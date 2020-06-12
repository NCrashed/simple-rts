module Ivory.Malloc(
    MString
  , malloc
  , free
  , withMalloc
  , mallocModule
  ) where

import Ivory.Language

type MString = Ptr Global (Stored IChar)

malloc :: Def ('[Uint32] :-> MString)
malloc = importProc "malloc" "stdlib.h"

free :: Def ('[MString] :-> ())
free = importProc "free" "stdlib.h"

withMalloc :: Uint32 -> (MString -> Ivory eff a) -> Ivory eff a
withMalloc n ma = do
  ptr <- call malloc n
  a <- ma ptr
  call_ free ptr
  pure a

mallocModule :: Module
mallocModule = package "ivory_malloc" $ do
  incl malloc
  incl free
