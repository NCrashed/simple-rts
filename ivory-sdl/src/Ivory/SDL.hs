module Ivory.SDL(
    sdlModule
  -- * Initialization
  , sdlInit
  , sdlQuit
  -- ** Init flags
  , InitFlag
  , sdlInitTimer
  , sdlInitAudio
  , sdlInitVideo
  , sdlInitJoystick
  , sdlInitHaptic
  , sdlInitGameController
  , sdlInitEvents
  , sdlInitEverything
  -- * Logging
  , SdlLog(..)
  , sdlGetError
  -- * Window
  , SDLWindow
  , sdlCreateWindow
  , sdlWindowPosCentered
  , sdlWindowPosUndefined
  , sdlDestroyWindow
  -- ** Window flags
  , WindowFlag
  , sdlWindowFullscreen
  , sdlWindowFullscreenDesktop
  , sdlWindowOpenGL
  , sdlWindowVulkan
  , sdlWindowHidden
  , sdlWindowBorderless
  , sdlWindowResizable
  , sdlWindowMinimized
  , sdlWindowMaximized
  , sdlWindowInputGrabbed
  , sdlWindowAllowHighDpi
  -- * Renderer
  , SDLRenderer
  , sdlCreateRenderer
  , sdlDestroyRenderer
  , sdlRenderClear
  , SDLRect
  , sdlRenderCopy
  , sdlRenderPresent
  , sdlDelay
  -- ** Renderer flags
  , RenderFlag
  , sdlRendererSoftware
  , sdlRendererAccelerated
  , sdlRendererPresentVSync
  , sdlRendererTargetTexture
  -- * Surface
  , SDLSurface
  , sdlLoadBMP
  , sdlFreeSurface
  -- * Texture
  , SDLTexture
  , sdlCreateTextureFromSurface
  , sdlDestroyTexture
  ) where

import Ivory.Language
import Ivory.Language.Pointer

type InitFlag = Uint32

sdlInitTimer, sdlInitAudio, sdlInitVideo, sdlInitJoystick, sdlInitHaptic,
  sdlInitGameController, sdlInitEvents, sdlInitEverything :: InitFlag
sdlInitTimer          = extern "SDL_INIT_TIMER" "SDL.h"
sdlInitAudio          = extern "SDL_INIT_AUDIO" "SDL.h"
sdlInitVideo          = extern "SDL_INIT_VIDEO" "SDL.h"
sdlInitJoystick       = extern "SDL_INIT_JOYSTICK" "SDL.h"
sdlInitHaptic         = extern "SDL_INIT_HAPTIC" "SDL.h"
sdlInitGameController = extern "SDL_INIT_GAMECONTROLLER" "SDL.h"
sdlInitEvents         = extern "SDL_INIT_EVENTS" "SDL.h"
sdlInitEverything     = extern "SDL_INIT_EVERYTHING" "SDL.h"

sdlInit :: Def ('[InitFlag] :-> Sint32)
sdlInit = importProc "SDL_Init" "SDL.h"

class SdlLog a where
  sdlLog :: Def ((IString ': a) ':-> ())

instance SdlLog '[] where
  sdlLog = importProc "SDL_Log" "SDL.h"

instance IvoryType a => SdlLog '[a] where
  sdlLog = importProc "SDL_Log" "SDL.h"

instance (IvoryType a, IvoryType b) => SdlLog '[a, b] where
  sdlLog = importProc "SDL_Log" "SDL.h"

instance (IvoryType a, IvoryType b, IvoryType c) => SdlLog '[a, b, c] where
  sdlLog = importProc "SDL_Log" "SDL.h"

instance (IvoryType a, IvoryType b, IvoryType c, IvoryType d) => SdlLog '[a, b, c, d] where
  sdlLog = importProc "SDL_Log" "SDL.h"

instance (IvoryType a, IvoryType b, IvoryType c, IvoryType d, IvoryType e) => SdlLog '[a, b, c, d, e] where
  sdlLog = importProc "SDL_Log" "SDL.h"

instance (IvoryType a, IvoryType b, IvoryType c, IvoryType d, IvoryType e, IvoryType f) => SdlLog '[a, b, c, d, e, f] where
  sdlLog = importProc "SDL_Log" "SDL.h"

instance (IvoryType a, IvoryType b, IvoryType c, IvoryType d, IvoryType e, IvoryType f, IvoryType g) => SdlLog '[a, b, c, d, e, f, g] where
  sdlLog = importProc "SDL_Log" "SDL.h"

sdlGetError :: Def ('[] :-> IString)
sdlGetError = importProc "SDL_GetError" "SDL.h"

type SDLWindow = Pointer Nullable Mutable Global (Stored OpaqueType)

type WindowFlag = Uint32

sdlCreateWindow :: Def ('[IString, Sint32, Sint32, Sint32, Sint32, WindowFlag] :-> SDLWindow)
sdlCreateWindow = importProc "SDL_CreateWindow" "SDL.h"

sdlWindowPosCentered :: Sint32
sdlWindowPosCentered = extern "SDL_WINDOWPOS_CENTERED" "SDL.h"

sdlWindowPosUndefined :: Sint32
sdlWindowPosUndefined = extern "SDL_WINDOWPOS_UNDEFINED" "SDL.h"

sdlWindowFullscreen, sdlWindowFullscreenDesktop, sdlWindowOpenGL, sdlWindowVulkan,
  sdlWindowHidden, sdlWindowBorderless, sdlWindowResizable, sdlWindowMinimized,
  sdlWindowMaximized, sdlWindowInputGrabbed, sdlWindowAllowHighDpi :: WindowFlag
sdlWindowFullscreen         = extern "SDL_WINDOW_FULLSCREEN" "SDL.h"
sdlWindowFullscreenDesktop  = extern "SDL_WINDOW_FULLSCREEN_DESKTOP" "SDL.h"
sdlWindowOpenGL             = extern "SDL_WINDOW_OPENGL" "SDL.h"
sdlWindowVulkan             = extern "SDL_WINDOW_VULKAN" "SDL.h"
sdlWindowHidden             = extern "SDL_WINDOW_HIDDEN" "SDL.h"
sdlWindowBorderless         = extern "SDL_WINDOW_BORDERLESS" "SDL.h"
sdlWindowResizable          = extern "SDL_WINDOW_RESIZABLE" "SDL.h"
sdlWindowMinimized          = extern "SDL_WINDOW_MINIMIZED" "SDL.h"
sdlWindowMaximized          = extern "SDL_WINDOW_MAXIMIZED" "SDL.h"
sdlWindowInputGrabbed       = extern "SDL_WINDOW_INPUT_GRABBED" "SDL.h"
sdlWindowAllowHighDpi       = extern "SDL_WINDOW_ALLOW_HIGHDPI" "SDL.h"

sdlDestroyWindow :: Def ('[SDLWindow] :-> ())
sdlDestroyWindow = importProc "SDL_DestroyWindow" "SDL.h"

sdlQuit :: Def ('[] :-> ())
sdlQuit = importProc "SDL_Quit" "SDL.h"

type SDLRenderer = Pointer Nullable Mutable Global (Stored OpaqueType)

type RenderFlag = Uint32

sdlRendererSoftware, sdlRendererAccelerated, sdlRendererPresentVSync, sdlRendererTargetTexture :: RenderFlag
sdlRendererSoftware = extern "SDL_RENDERER_SOFTWARE" "SDL.h"
sdlRendererAccelerated = extern "SDL_RENDERER_ACCELERATED" "SDL.h"
sdlRendererPresentVSync = extern "SDL_RENDERER_PRESENTVSYNC" "SDL.h"
sdlRendererTargetTexture = extern "SDL_RENDERER_TARGETTEXTURE" "SDL.h"

sdlCreateRenderer :: Def ('[SDLWindow, Sint32, RenderFlag] :-> SDLRenderer)
sdlCreateRenderer = importProc "SDL_CreateRenderer" "SDL.h"

sdlDestroyRenderer :: Def ('[SDLRenderer] :-> ())
sdlDestroyRenderer = importProc "SDL_DestroyRenderer" "SDL.h"

type SDLSurface = Pointer Nullable Mutable Global (Stored OpaqueType)

sdlLoadBMP :: Def ('[IString] :-> SDLSurface)
sdlLoadBMP = importProc "SDL_LoadBMP" "SDL.h"

sdlFreeSurface :: Def ('[SDLSurface] :-> ())
sdlFreeSurface = importProc "SDL_FreeSurface" "SDL.h"

type SDLTexture = Pointer Nullable Mutable Global (Stored OpaqueType)

sdlCreateTextureFromSurface :: Def ('[IString] :-> SDLTexture)
sdlCreateTextureFromSurface = importProc "SDL_CreateTextureFromSurface" "SDL.h"

sdlDestroyTexture :: Def ('[SDLTexture] :-> ())
sdlDestroyTexture = importProc "SDL_DestroyTexture" "SDL.h"

sdlRenderClear :: Def ('[SDLRenderer] :-> Sint32)
sdlRenderClear = importProc "SDL_RenderClear" "SDL.h"

type SDLRect = Pointer Nullable Mutable Global (Stored OpaqueType)

sdlRenderCopy :: Def ('[SDLRenderer, SDLTexture, SDLRect, SDLRect] :-> Sint32)
sdlRenderCopy = importProc "SDL_RenderCopy" "SDL.h"

sdlRenderPresent :: Def ('[SDLRenderer] :-> Sint32)
sdlRenderPresent = importProc "SDL_RenderPresent" "SDL.h"

sdlDelay :: Def ('[Uint32] :-> ())
sdlDelay = importProc "SDL_Delay" "SDL.h"

sdlModule :: Module
sdlModule = package "ivory_sdl" $ do
  incl sdlInit
  incl sdlQuit
  incl (sdlLog :: Def ('[IString] :-> ()))
  incl sdlGetError
  incl sdlCreateWindow
  incl sdlDestroyWindow
  incl sdlCreateRenderer
  incl sdlDestroyRenderer
  incl sdlLoadBMP
  incl sdlFreeSurface
  incl sdlCreateTextureFromSurface
  incl sdlDestroyTexture
  incl sdlRenderClear
  incl sdlRenderCopy
  incl sdlRenderPresent
  incl sdlDelay
