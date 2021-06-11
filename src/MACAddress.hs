module MACAddress
  ( MACAddress( MACAddress ), mac, macAddress )
where

import Prelude  ( Double, (+), (*), fromInteger )

-- aeson -------------------------------

import Data.Aeson.Types  ( FromJSON( parseJSON ), Parser, ToJSON( toJSON )
                         , Value( Array, Number, String ), typeMismatch )

-- base --------------------------------

import Control.Applicative  ( pure )
import Control.Monad        ( fail, return )
import Data.Bool            ( otherwise )
import Data.Char            ( Char, isHexDigit )
import Data.Either          ( Either( Left, Right ) )
import Data.Eq              ( Eq )
import Data.Foldable        ( toList )
import Data.Function        ( ($), (&) )
import Data.List            ( all, head )
import Data.Ord             ( Ord, (<), (>) )
import Data.Tuple           ( fst )
import Data.Void            ( Void )
import Data.Word            ( Word8 )
import GHC.Generics         ( Generic )
import Numeric              ( readHex )
import Text.Show            ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-default ------------------------

import Data.Default  ( def )

-- data-textual ------------------------

import qualified  Data.Textual.Integral  as  TI

import Data.Textual  ( Printable( print ), Textual( textual ), toText )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- dhall -------------------------------

import qualified  Dhall.Core  as  DC

import Dhall       ( Decoder( Decoder, expected, extract ), Expector, Extractor
                   , FromDhall( autoWith ), typeError )
import Dhall.Core  ( Expr )
import Dhall.Src   ( Src )

-- either ------------------------------

import Data.Either.Validation  ( Validation( Success ) )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⋪), (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Lens         ( (⊩) )
import Data.MoreUnicode.Maybe        ( pattern 𝕵 )

-- parsec ------------------------------

import qualified  Text.Parsec.Char  as  TC

-- parsers -----------------------------

import qualified  Text.Parser.Char  as  RC

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary ) )

-- scientific --------------------------

import Data.Scientific  ( floatingOrInteger )

-- template-haskell --------------------

import Language.Haskell.TH        ( appE, litE, stringL, varE )
import Language.Haskell.TH.Quote  ( QuasiQuoter )

-- text --------------------------------

import Data.Text  ( Text, unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- textual-plus ------------------------

import TextualPlus  ( parseTextM )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

import ParsecPlus       ( Parsecable( parser ), __parsecN__ )
import QuasiQuoting        ( exp, mkQQ )
-- import TextualPlus      ( parseTextM )

--------------------------------------------------------------------------------

data MACAddress = MACAddress Word8 Word8 Word8 Word8 Word8 Word8
  deriving (Eq, Generic, NFData, Ord, Show)

----------------------------------------

instance Printable MACAddress where
  print (MACAddress a b c d e f) = P.text $ [fmt|%02x-%02x-%02x-%02x-%02x-%02x|]
                                                    a    b    c    d    e    f

----------------------------------------

instance Textual MACAddress where
  textual = let hexByte = (\ (a,b) → a*16+b) ⊳ ((,) ⊳ TI.hexDigit ⊵ TI.hexDigit)
             in MACAddress ⊳ hexByte ⋪ (RC.oneOf ":-")
                           ⊵ hexByte ⋪ (RC.oneOf ":-")
                           ⊵ hexByte ⋪ (RC.oneOf ":-")
                           ⊵ hexByte ⋪ (RC.oneOf ":-")
                           ⊵ hexByte ⋪ (RC.oneOf ":-")
                           ⊵ hexByte

----------------------------------------

{- | Read a Word8, encoded as two 'hex' chars (a-f,A-F,0-9).  Use of any other
     characters will lead to an undefined result -}
hexRead ∷ Char → Char → Word8
hexRead a b = fst ∘ head $ readHex [a,b]

parseJSONHexByte ∷ Value → Parser Word8
parseJSONHexByte (String t) =
  case unpack t of
    []      → fail "empty hexbyte"
    s@[c]   → if isHexDigit c
              then return $ hexRead '0' c
              else fail $ [fmt|bad hex char '%s'|] s
    s@[a,b] → if all isHexDigit s
              then return $ hexRead a b
              else fail $ [fmt|bad hex char '%s'|] s
    _       → fail $ [fmt|hexbyte too long '%t'|] t
parseJSONHexByte (Number n) =
  case floatingOrInteger n of
    Left (f ∷ Double) → fail $ [fmt|floating hexbyte: %w|] f
    Right i | i < 0     → fail $ [fmt|negative hexbyte: %d|] i
            | i > 255   → fail $ [fmt|hexbyte > 255: %d|] i
            | otherwise → return (fromInteger i)

parseJSONHexByte invalid = typeMismatch "hexbyte" invalid

instance FromJSON MACAddress where
  parseJSON (String t) = parseTextM "MACAddress" t
  parseJSON (Array (toList → [a,b,c,d,e,f])) = MACAddress ⊳ parseJSONHexByte a
                                                          ⊵ parseJSONHexByte b
                                                          ⊵ parseJSONHexByte c
                                                          ⊵ parseJSONHexByte d
                                                          ⊵ parseJSONHexByte e
                                                          ⊵ parseJSONHexByte f
  parseJSON (Array xs) = fail $ [fmt|MACAddress must have 6 elements: %w|] xs
  parseJSON invalid    = typeMismatch "MACAddress" invalid

----------------------------------------

instance ToJSON MACAddress where
  toJSON = String ∘ toText

----------------------------------------

instance Parsecable MACAddress where
  parser = let hexByte = hexRead ⊳ TC.hexDigit ⊵ TC.hexDigit
            in MACAddress ⊳ hexByte ⋪ TC.oneOf ":-"
                          ⊵ hexByte ⋪ TC.oneOf ":-"
                          ⊵ hexByte ⋪ TC.oneOf ":-"
                          ⊵ hexByte ⋪ TC.oneOf ":-"
                          ⊵ hexByte ⋪ TC.oneOf ":-"
                          ⊵ hexByte

----------------------------------------

__macAddress ∷ Text → MACAddress
__macAddress = __parsecN__

{-| quasi-quoter for MACAddresses -}
macAddress ∷ QuasiQuoter
macAddress =
  let parseExp t = 𝕵 $ appE (varE '__macAddress) (litE $ stringL t)
   in mkQQ "MACAddress" $ def & exp ⊩ parseExp

mac ∷ QuasiQuoter
mac = macAddress

instance FromDhall MACAddress where
{-
  autoWith _ = Decoder {..}
               where extract (DC.TextLit (DC.Chunks [] t)) = pure $ __parsecN t
                     extract _                             = empty
                     expected = DC.Text
-}
  autoWith _ = let expected ∷Expector (Expr Src Void)
                   expected = Success DC.Text
                   extract ∷ Expr Src Void → Extractor Src Void MACAddress
                   extract (DC.TextLit (DC.Chunks [] t)) = pure $ __parsecN__ t
                   extract x                             = typeError expected x
                in Decoder{..}

instance Arbitrary MACAddress where
  arbitrary = MACAddress ⊳ arbitrary ⊵ arbitrary ⊵ arbitrary
                         ⊵ arbitrary ⊵ arbitrary ⊵ arbitrary

-- that's all, folks! ----------------------------------------------------------
