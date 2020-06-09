module Ivory.SDL(
    sdlModule
  -- * Initialization
  , sdlInit
  -- ** Init flags
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
  -- * Window creation
  , SDLWindow
  , sdlCreateWindow
  , sdlWindowPosCentered
  , sdlWindowPosUndefined
  -- ** Window flags
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
  ) where

import Ivory.Language
import Ivory.Language.Pointer

sdlInitTimer, sdlInitAudio, sdlInitVideo, sdlInitJoystick, sdlInitHaptic,
  sdlInitGameController, sdlInitEvents, sdlInitEverything :: Uint32
sdlInitTimer          = extern "SDL_INIT_TIMER" "SDL.h"
sdlInitAudio          = extern "SDL_INIT_AUDIO" "SDL.h"
sdlInitVideo          = extern "SDL_INIT_VIDEO" "SDL.h"
sdlInitJoystick       = extern "SDL_INIT_JOYSTICK" "SDL.h"
sdlInitHaptic         = extern "SDL_INIT_HAPTIC" "SDL.h"
sdlInitGameController = extern "SDL_INIT_GAMECONTROLLER" "SDL.h"
sdlInitEvents         = extern "SDL_INIT_EVENTS" "SDL.h"
sdlInitEverything     = extern "SDL_INIT_EVERYTHING" "SDL.h"

sdlInit :: Def ('[Uint32] :-> IBool)
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

sdlCreateWindow :: Def ('[IString, Sint32, Sint32, Sint32, Sint32, Uint32] :-> SDLWindow)
sdlCreateWindow = importProc "SDL_CreateWindow" "SDL.h"

sdlWindowPosCentered :: Sint32
sdlWindowPosCentered = extern "SDL_WINDOWPOS_CENTERED" "SDL.h"

sdlWindowPosUndefined :: Sint32
sdlWindowPosUndefined = extern "SDL_WINDOWPOS_UNDEFINED" "SDL.h"

sdlWindowFullscreen, sdlWindowFullscreenDesktop, sdlWindowOpenGL, sdlWindowVulkan,
  sdlWindowHidden, sdlWindowBorderless, sdlWindowResizable, sdlWindowMinimized,
  sdlWindowMaximized, sdlWindowInputGrabbed, sdlWindowAllowHighDpi :: Sint32
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

sdlModule :: Module
sdlModule = package "ivory_sdl" $ do
  incl sdlInit
  incl (sdlLog :: Def ('[IString] :-> ()))
  incl sdlGetError
  incl sdlCreateWindow
