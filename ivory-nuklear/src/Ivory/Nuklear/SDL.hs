module Ivory.Nuklear.SDL(
    nuklearSdlModule
  , NKContext
  , NKFontAtlas
  , SDLContext
  , nkSdlInit
  , nkSdlFontStashBegin
  , nkSdlFontStashEnd
  , nkSdlHandleEvent
  , nkSdlRender
  , nkSdlShutdown
  , nkSdlDeviceDestroy
  , nkSdlDeviceCreate
  ) where

import Ivory.Language
import Ivory.Nuklear
import Ivory.SDL

nkSdlInit :: Def ('[SDLContext] :-> NKContext)
nkSdlInit = importProc "nk_sdl_init" "nuklear_sdl_gl3.h"

nkSdlFontStashBegin :: Def ('[Ptr Global (Stored NKFontAtlas)] :-> ())
nkSdlFontStashBegin = importProc "nk_sdl_font_stash_begin" "nuklear_sdl_gl3.h"

nkSdlFontStashEnd :: Def ('[] :-> ())
nkSdlFontStashEnd = importProc "nk_sdl_font_stash_end" "nuklear_sdl_gl3.h"

nkSdlHandleEvent :: Def ('[SDLEvent] :-> Sint32)
nkSdlHandleEvent = importProc "nk_sdl_handle_event" "nuklear_sdl_gl3.h"

nkSdlRender :: Def ('[NkAntiAliasing, Sint32, Sint32] :-> ())
nkSdlRender = importProc "nk_sdl_render" "nuklear_sdl_gl3.h"

nkSdlShutdown :: Def ('[] :-> ())
nkSdlShutdown = importProc "nk_sdl_shutdown" "nuklear_sdl_gl3.h"

nkSdlDeviceDestroy :: Def ('[] :-> ())
nkSdlDeviceDestroy = importProc "nk_sdl_device_destroy" "nuklear_sdl_gl3.h"

nkSdlDeviceCreate :: Def ('[] :-> ())
nkSdlDeviceCreate = importProc "nk_sdl_device_create" "nuklear_sdl_gl3.h"

nuklearSdlModule :: Module
nuklearSdlModule = package "ivory_nuklear_sdl" $ do
  incl nkSdlInit
  incl nkSdlFontStashBegin
  incl nkSdlFontStashEnd
  incl nkSdlHandleEvent
  incl nkSdlRender
  incl nkSdlShutdown
  incl nkSdlDeviceDestroy
  incl nkSdlDeviceCreate
