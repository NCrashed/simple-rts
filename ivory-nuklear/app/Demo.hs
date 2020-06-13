module Main where

import Ivory.Language
import Ivory.Language.Cast
import Ivory.Language.Pointer
import Ivory.Malloc
import Ivory.SDL
import Ivory.String
import Ivory.Nuklear
import Ivory.Nuklear.SDL

import Ivory.Artifact
import Ivory.Artifact.Location
import Ivory.Compile.C.CmdlineFrontend
import Paths_ivory_nuklear

main :: IO ()
main = runCompiler modules artifacts initialOpts { outDir = Just "cgen" }
  where
    modules = [
        sdlModule
      , nuklearModule
      , nuklearSdlModule
      , mainModule
      , stringModule
      , mallocModule
      ]
    artifacts = nuklearArtifacts
      ++ [
        cabalArtifact "main.c"
      , cabalArtifact "Makefile"
      ]
    cabalArtifact = Src . artifactCabalFile getDataDir

runGame :: Def ('[IString] :-> Sint32)
runGame = proc "run_game" $ \path -> body $ do
  res <- bracketRet "SDL_Init Error: %s\n" (call sdlInit sdlInitEverything) $
    bracketNull "SDL_CreateWindow Error: %s\n" 0
      (call sdlCreateWindow "Hello World!" 100 100 620 387 sdlWindowShown)
      (call_ sdlDestroyWindow) $ \win ->
        bracketNull "SDL_CreateRenderer Error: %s\n" 0
          (call sdlCreateRenderer win (-1) (sdlRendererAccelerated .| sdlRendererPresentVSync))
          (call_ sdlDestroyRenderer) $ \ren -> withMalloc pathMax $ \grumpy_src -> do
            call_ strcpy grumpy_src path
            call_ strcat grumpy_src "/../img/grumpy-cat.bmp"
            call_ sdlLog1 "Path to image is %s\n" grumpy_src
            bracketNull "SDL_LoadBMP Error: %s\n" 0
              (call sdlLoadBMP (ivoryCast grumpy_src))
              (call_ sdlFreeSurface) $ \bmp ->
                bracketNull "SDL_CreateTextureFromSurface Error: %s\n" 0
                  (call sdlCreateTextureFromSurface ren bmp)
                  (call_ sdlDestroyTexture) $ \tex -> do
                    (20 :: Ix 21) `times` \_ -> do
                      call_ sdlRenderClear ren
                      call_ sdlRenderCopy ren tex nullPtr nullPtr
                      call_ sdlRenderPresent ren
                      call_ sdlDelay 100
                    pure 0
  call_ sdlQuit
  ret res

bracketRet :: GetAlloc eff ~ Scope s => IString -> Ivory eff Sint32 -> Ivory eff Sint32 -> Ivory eff Sint32
bracketRet msg initf bodyf = do
  res <- initf
  ecode <- local (ival 0)
  ifte_ (res /=? 0) (printAndFail msg >>= store ecode) (bodyf >>= store ecode)
  deref ecode

bracketNull :: (KnownConstancy mut, IvoryType a, IvoryInit c, GetAlloc eff ~ Scope s, IvoryStore c)
  => IString
  -> c -- ^ Default value for return type
  -> Ivory eff (Pointer Nullable mut area ('Stored a)) -- ^ Init resource
  -> (Pointer Nullable mut area ('Stored a) -> Ivory eff ()) -- ^ Destroy resource
  -> (Pointer Nullable mut area ('Stored a) -> Ivory eff c) -- ^ Body
  -> Ivory eff c
bracketNull msg defRet initf destf bodyf = do
  retRef <- local $ ival defRet
  res <- initf
  ifte_ (res ==? nullPtr) (printAndFail msg >> destf res) (bodyf res >>= store retRef)
  destf res
  deref retRef

printAndFail :: IString -> Ivory eff Sint32
printAndFail msg = do
  er <- call sdlGetError
  call_ sdlLog1 msg er
  pure 1

mainModule :: Module
mainModule = package "main_game" $ do
  depend sdlModule
  depend nuklearModule
  depend nuklearSdlModule
  depend mallocModule
  depend stringModule
  incl runGame
