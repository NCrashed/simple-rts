module Ivory.String(
    pathMax
  , strcpy
  , strcat
  , stringModule
  ) where

import Ivory.Language
import Ivory.Malloc (MString)

pathMax :: Uint32
pathMax = extern "PATH_MAX" "limits.h"

strcpy :: Def ('[MString, IString] :-> ())
strcpy = importProc "strcpy" "string.h"

strcat :: Def ('[MString, IString] :-> ())
strcat = importProc "strcat" "string.h"

stringModule :: Module
stringModule = package "ivory_string" $ do
  incl strcpy
  incl strcat
  inclSym pathMax
