module Game.Simple.Rts(
    mainModule
  ) where

import Ivory.Language
import Ivory.SDL

runGame :: Def ('[] :-> Sint32)
runGame = proc "run_game" $ body $ do
  checkFail "SDL_Init Error: %s\n" $ call sdlInit sdlInitEverything
  ret 0

checkFail :: GetReturn eff ~ Returns Sint32 => IString -> Ivory eff Sint32 -> Ivory eff ()
checkFail msg f = do
  res <- f
  ifte_ (res /=? 0) failCase (pure ())
  where
    failCase = do
      er <- call sdlGetError
      call_ sdlLog1 msg er
      ret 1

  -- a <- local (ival 0)
  -- b <- local (ival 1)
  --
  -- n `times` \_ -> do
  --   a' <- deref a
  --   b' <- deref b
  --   store a b'
  --   store b (a' + b')
  --
  -- result <- deref a
  -- ret result

mainModule :: Module
mainModule = package "main_game" $ do
  depend sdlModule
  incl runGame
