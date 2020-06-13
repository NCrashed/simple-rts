module Ivory.Nuklear(
    nuklearModule
  , nuklearArtifacts
  , NKContext
  , NKFontAtlas
  , NkAntiAliasing
  , nkAntiAliasingOff
  , nkAntiAliasingOn
  ) where

import Ivory.Artifact
import Ivory.Artifact.Location
import Ivory.Language
import Paths_ivory_nuklear

type NKContext = Ptr Global (Stored ())
type NKFontAtlas = Ptr Global (Stored ())

type NkAntiAliasing = Sint32

nkAntiAliasingOff, nkAntiAliasingOn  :: NkAntiAliasing
nkAntiAliasingOff = extern "NK_ANTI_ALIASING_OFF" "nuklear.h"
nkAntiAliasingOn = extern "NK_ANTI_ALIASING_ON" "nuklear.h"

nuklearModule :: Module
nuklearModule = package "ivory_nuklear" $ do
  inclSym nkAntiAliasingOff
  inclSym nkAntiAliasingOn
  pure ()

nuklearArtifacts :: [Located Artifact]
nuklearArtifacts = [
    cabalArtifact "nuklear.h"
  , cabalArtifact "nuklear_sdl_gl3.h"
  ]
  where
    cabalArtifact = Src . artifactCabalFile getDataDir
