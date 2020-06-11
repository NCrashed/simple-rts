module Main where

import Game.Simple.Rts
import Ivory.SDL

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
      ]
    artifacts = [
        cabalArtifact "main.c"
      , cabalArtifact "Makefile"
      ]
    cabalArtifact = Src . artifactCabalFile getDataDir
