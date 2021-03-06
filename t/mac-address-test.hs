-- base --------------------------------

import System.IO  ( IO )

-- tasty -------------------------------

import Test.Tasty           ( defaultIngredients )
import Test.Tasty.Runners   ( defaultMainWithIngredients )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import T.MACAddress  ( tests )

--------------------------------------------------------------------------------

main ∷ IO ()
main = defaultMainWithIngredients defaultIngredients tests

-- that's all, folks! ----------------------------------------------------------
