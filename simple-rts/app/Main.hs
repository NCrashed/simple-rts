module Main where

import Game.Simple.Rts
import Ivory.Malloc
import Ivory.SDL
import Ivory.String

import Ivory.Artifact
import Ivory.Artifact.Location
import Ivory.Compile.C.CmdlineFrontend
import Paths_simple_rts

main :: IO ()
main = runCompiler modules artifacts initialOpts { outDir = Just "cgen" }
  where
    modules = [
        sdlModule
      , mainModule
      , stringModule
      , mallocModule
      ]
    artifacts = [
        cabalArtifact "main.c"
      , cabalArtifact "Makefile"
      ]
    cabalArtifact = Src . artifactCabalFile getDataDir
