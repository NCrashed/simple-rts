module Ivory.SDL(
    sdlModule
  -- * Initialization
  , sdlInit
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
  -- ** Renderer flags
  , RenderFlag
  , sdlRendererSoftware
  , sdlRendererAccelerated
  , sdlRendererPresentVSync
  , sdlRendererTargetTexture
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

sdlInit :: Def ('[InitFlag] :-> IBool)
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

type SDLRenderer = Pointer Nullable Mutable Global (Stored OpaqueType)

type RenderFlag = Uint32

sdlRendererSoftware, sdlRendererAccelerated, sdlRendererPresentVSync, sdlRendererTargetTexture :: RenderFlag
sdlRendererSoftware = extern "SDL_RENDERER_SOFTWARE" "SDL.h"
sdlRendererAccelerated = extern "SDL_RENDERER_ACCELERATED" "SDL.h"
sdlRendererPresentVSync = extern "SDL_RENDERER_PRESENTVSYNC" "SDL.h"
sdlRendererTargetTexture = extern "SDL_RENDERER_TARGETTEXTURE" "SDL.h"

sdlCreateRenderer :: Def ('[SDLWindow, Sint32, RenderFlag] :-> SDLRenderer)
sdlCreateRenderer = importProc "SDL_CreateRenderer" "SDL.h"

sdlModule :: Module
sdlModule = package "ivory_sdl" $ do
  incl sdlInit
  incl (sdlLog :: Def ('[IString] :-> ()))
  incl sdlGetError
  incl sdlCreateWindow
  incl sdlCreateRenderer
