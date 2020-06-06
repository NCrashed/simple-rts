module Game.Simple.Rts(
    fibModule
  ) where

import Ivory.Language

fibLoop :: Def ('[Ix 1000] :-> Uint32)
fibLoop = proc "fibLoop" $ \n -> body $ do
  a <- local (ival 0)
  b <- local (ival 1)

  n `times` \_ -> do
    a' <- deref a
    b' <- deref b
    store a b'
    store b (a' + b')

  result <- deref a
  ret result

fibModule :: Module
fibModule = package "fib_test" $ do
  incl fibLoop
