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
  , sdlLog0
  , sdlLog1
  , sdlLog2
  , sdlLog3
  , sdlLog4
  , sdlLog5
  , sdlLog6
  , sdlLog7
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
  , sdlWindowShown
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
  -- * Events
  , SDLEvent
  -- * Context
  , SDLContext
  -- ** GL attribues
  , SDLGlAttr
  , sdlGlSetAttribute
  , sdlGlRedSize
  , sdlGlGreenSize
  , sdlGlBlueSize
  , sdlGlAlphaSize
  , sdlGlBufferSize
  , sdlGlDoublebuffer
  , sdlGlDepthSize
  , sdlGlStencilSize
  , sdlGlAccumRedSize
  , sdlGlAccumGreenSize
  , sdlGlAccumBlueSize
  , sdlGlAccumAlphaSize
  , sdlGlStereo
  , sdlGlMultisamplebuffers
  , sdlGlMultisamplesamples
  , sdlGlAcceleratedVisual
  , sdlGlContextMajorVersion
  , sdlGlContextMinorVersion
  , sdlGlContextFlags
  , sdlGlContextProfileMask
  , sdlGlShareWithCurrentContext
  , sdlGlFramebufferSrgbCapable
  , sdlGlContextReleaseBehavior
  -- ** GL context flags
  , SDLGlContextFlag
  , sdlGlContextDebugFlag
  , sdlGlContextForwardCompatibleFlag
  , sdlGlContextRobustAccessFlag
  , sdlGlContextResetIsolationFlag
  -- ** GL profile
  , SDLGlProfile
  , sdlGlContextProfileCore
  , sdlGlContextProfileCompatibility
  , sdlGlContextProfileEs
  -- * Hints
  , sdlSetHint
  , SDLHint
  , sdlHintAccelerometerAsJoystick
  , sdlHintAndroidApkExpansionMainFileVersion
  , sdlHintAndroidApkExpansionPatchFileVersion
  , sdlHintAndroidSeparateMouseAndTouch
  , sdlHintAppleTvControllerUiEvents
  , sdlHintAppleTvRemoteAllowRotation
  , sdlHintBmpSaveLegacyFormat
  , sdlHintEmscriptenKeyboardElement
  , sdlHintFramebufferAcceleration
  , sdlHintGamecontrollerconfig
  , sdlHintGrabKeyboard
  , sdlHintIdleTimerDisabled
  , sdlHintImeInternalEditing
  , sdlHintJoystickAllowBackgroundEvents
  , sdlHintMacBackgroundApp
  , sdlHintMacCtrlClickEmulateRightClick
  , sdlHintMouseFocusClickthrough
  , sdlHintMouseRelativeModeWarp
  , sdlHintNoSignalHandlers
  , sdlHintOrientations
  , sdlHintRenderDirect3d11Debug
  , sdlHintRenderDirect3dThreadsafe
  , sdlHintRenderDriver
  , sdlHintRenderOpenglShaders
  , sdlHintRenderScaleQuality
  , sdlHintRenderVsync
  , sdlHintRpiVideoLayer
  , sdlHintThreadStackSize
  , sdlHintTimerResolution
  , sdlHintVideoAllowScreensaver
  , sdlHintVideoHighdpiDisabled
  , sdlHintVideoMacFullscreenSpaces
  , sdlHintVideoMinimizeOnFocusLoss
  , sdlHintVideoWindowSharePixelFormat
  , sdlHintVideoWinD3dcompiler
  , sdlHintVideoX11NetWmPing
  , sdlHintVideoX11Xinerama
  , sdlHintVideoX11Xrandr
  , sdlHintVideoX11Xvidmode
  , sdlHintWindowsDisableThreadNaming
  , sdlHintWindowsEnableMessageloop
  , sdlHintWindowsNoCloseOnAltF4
  , sdlHintWindowFrameUsableWhileCursorHidden
  , sdlHintWinrtHandleBackButton
  , sdlHintWinrtPrivacyPolicyLabel
  , sdlHintWinrtPrivacyPolicyUrl
  , sdlHintXinputEnabled
  , sdlHintXinputUseOldJoystickMapping
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

sdlLog0 :: Def ('[IString] :-> ())
sdlLog0 = importProc "SDL_Log" "SDL.h"

sdlLog1 :: IvoryType a => Def ('[IString, a] :-> ())
sdlLog1 = importProc "SDL_Log" "SDL.h"

sdlLog2 :: (IvoryType a, IvoryType b) => Def ('[IString, a, b] :-> ())
sdlLog2 = importProc "SDL_Log" "SDL.h"

sdlLog3 :: (IvoryType a, IvoryType b, IvoryType c) => Def ('[IString, a, b, c] :-> ())
sdlLog3 = importProc "SDL_Log" "SDL.h"

sdlLog4 :: (IvoryType a, IvoryType b, IvoryType c, IvoryType d) => Def ('[IString, a, b, c, d] :-> ())
sdlLog4 = importProc "SDL_Log" "SDL.h"

sdlLog5 :: (IvoryType a, IvoryType b, IvoryType c, IvoryType d, IvoryType e) => Def ('[IString, a, b, c, d, e] :-> ())
sdlLog5 = importProc "SDL_Log" "SDL.h"

sdlLog6 :: (IvoryType a, IvoryType b, IvoryType c, IvoryType d, IvoryType e, IvoryType f) => Def ('[IString, a, b, c, d, e, f] :-> ())
sdlLog6 = importProc "SDL_Log" "SDL.h"

sdlLog7 :: (IvoryType a, IvoryType b, IvoryType c, IvoryType d, IvoryType e, IvoryType f, IvoryType g) => Def ('[IString, a, b, c, d, e, f, g] :-> ())
sdlLog7 = importProc "SDL_Log" "SDL.h"

sdlGetError :: Def ('[] :-> IString)
sdlGetError = importProc "SDL_GetError" "SDL.h"

type SDLWindow = Ptr Global (Stored ())

type WindowFlag = Uint32

sdlCreateWindow :: Def ('[IString, Sint32, Sint32, Sint32, Sint32, WindowFlag] :-> SDLWindow)
sdlCreateWindow = importProc "SDL_CreateWindow" "SDL.h"

sdlWindowPosCentered :: Sint32
sdlWindowPosCentered = extern "SDL_WINDOWPOS_CENTERED" "SDL.h"

sdlWindowPosUndefined :: Sint32
sdlWindowPosUndefined = extern "SDL_WINDOWPOS_UNDEFINED" "SDL.h"

sdlWindowFullscreen, sdlWindowFullscreenDesktop, sdlWindowOpenGL, sdlWindowVulkan,
  sdlWindowHidden, sdlWindowShown, sdlWindowBorderless, sdlWindowResizable,
  sdlWindowMinimized, sdlWindowMaximized, sdlWindowInputGrabbed,
  sdlWindowAllowHighDpi :: WindowFlag
sdlWindowFullscreen         = extern "SDL_WINDOW_FULLSCREEN" "SDL.h"
sdlWindowFullscreenDesktop  = extern "SDL_WINDOW_FULLSCREEN_DESKTOP" "SDL.h"
sdlWindowOpenGL             = extern "SDL_WINDOW_OPENGL" "SDL.h"
sdlWindowVulkan             = extern "SDL_WINDOW_VULKAN" "SDL.h"
sdlWindowShown              = extern "SDL_WINDOW_SHOWN" "SDL.h"
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

type SDLRenderer = Ptr Global (Stored ())

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

type SDLSurface = Ptr Global (Stored ())

sdlLoadBMP :: Def ('[IString] :-> SDLSurface)
sdlLoadBMP = importProc "SDL_LoadBMP" "SDL.h"

sdlFreeSurface :: Def ('[SDLSurface] :-> ())
sdlFreeSurface = importProc "SDL_FreeSurface" "SDL.h"

type SDLTexture = Ptr Global (Stored ())

sdlCreateTextureFromSurface :: Def ('[SDLRenderer, SDLSurface] :-> SDLTexture)
sdlCreateTextureFromSurface = importProc "SDL_CreateTextureFromSurface" "SDL.h"

sdlDestroyTexture :: Def ('[SDLTexture] :-> ())
sdlDestroyTexture = importProc "SDL_DestroyTexture" "SDL.h"

sdlRenderClear :: Def ('[SDLRenderer] :-> Sint32)
sdlRenderClear = importProc "SDL_RenderClear" "SDL.h"

type SDLRect = Ptr Global (Stored ())

sdlRenderCopy :: Def ('[SDLRenderer, SDLTexture, SDLRect, SDLRect] :-> Sint32)
sdlRenderCopy = importProc "SDL_RenderCopy" "SDL.h"

sdlRenderPresent :: Def ('[SDLRenderer] :-> Sint32)
sdlRenderPresent = importProc "SDL_RenderPresent" "SDL.h"

sdlDelay :: Def ('[Uint32] :-> ())
sdlDelay = importProc "SDL_Delay" "SDL.h"

type SDLEvent = Ptr Global (Stored ())

type SDLContext = Ptr Global (Stored ())

sdlGlSetAttribute :: Def ('[SDLGlAttr, Sint32] :-> Sint32)
sdlGlSetAttribute = importProc "SDL_GL_SetAttribute" "SDL.h"

type SDLGlAttr = Sint32

sdlGlRedSize :: SDLGlAttr
sdlGlRedSize = extern "SDL_GL_RED_SIZE " "SDL.h"

sdlGlGreenSize :: SDLGlAttr
sdlGlGreenSize = extern "SDL_GL_GREEN_SIZE" "SDL.h"

sdlGlBlueSize :: SDLGlAttr
sdlGlBlueSize = extern "SDL_GL_BLUE_SIZE" "SDL.h"

sdlGlAlphaSize :: SDLGlAttr
sdlGlAlphaSize = extern "SDL_GL_ALPHA_SIZE" "SDL.h"

sdlGlBufferSize :: SDLGlAttr
sdlGlBufferSize = extern "SDL_GL_BUFFER_SIZE" "SDL.h"

sdlGlDoublebuffer :: SDLGlAttr
sdlGlDoublebuffer = extern "SDL_GL_DOUBLEBUFFER" "SDL.h"

sdlGlDepthSize :: SDLGlAttr
sdlGlDepthSize = extern "SDL_GL_DEPTH_SIZE" "SDL.h"

sdlGlStencilSize :: SDLGlAttr
sdlGlStencilSize = extern "SDL_GL_STENCIL_SIZE" "SDL.h"

sdlGlAccumRedSize :: SDLGlAttr
sdlGlAccumRedSize = extern "SDL_GL_ACCUM_RED_SIZE" "SDL.h"

sdlGlAccumGreenSize :: SDLGlAttr
sdlGlAccumGreenSize = extern "SDL_GL_ACCUM_GREEN_SIZE" "SDL.h"

sdlGlAccumBlueSize :: SDLGlAttr
sdlGlAccumBlueSize = extern "SDL_GL_ACCUM_BLUE_SIZE" "SDL.h"

sdlGlAccumAlphaSize :: SDLGlAttr
sdlGlAccumAlphaSize = extern "SDL_GL_ACCUM_ALPHA_SIZE" "SDL.h"

sdlGlStereo :: SDLGlAttr
sdlGlStereo = extern "SDL_GL_STEREO" "SDL.h"

sdlGlMultisamplebuffers :: SDLGlAttr
sdlGlMultisamplebuffers = extern "SDL_GL_MULTISAMPLEBUFFERS" "SDL.h"

sdlGlMultisamplesamples :: SDLGlAttr
sdlGlMultisamplesamples = extern "SDL_GL_MULTISAMPLESAMPLES" "SDL.h"

sdlGlAcceleratedVisual :: SDLGlAttr
sdlGlAcceleratedVisual = extern "SDL_GL_ACCELERATED_VISUAL" "SDL.h"

sdlGlContextMajorVersion :: SDLGlAttr
sdlGlContextMajorVersion = extern "SDL_GL_CONTEXT_MAJOR_VERSION" "SDL.h"

sdlGlContextMinorVersion :: SDLGlAttr
sdlGlContextMinorVersion = extern "SDL_GL_CONTEXT_MINOR_VERSION" "SDL.h"

sdlGlContextFlags :: SDLGlAttr
sdlGlContextFlags = extern "SDL_GL_CONTEXT_FLAGS" "SDL.h"

sdlGlContextProfileMask :: SDLGlAttr
sdlGlContextProfileMask = extern "SDL_GL_CONTEXT_PROFILE_MASK" "SDL.h"

sdlGlShareWithCurrentContext :: SDLGlAttr
sdlGlShareWithCurrentContext = extern "SDL_GL_SHARE_WITH_CURRENT_CONTEXT" "SDL.h"

sdlGlFramebufferSrgbCapable :: SDLGlAttr
sdlGlFramebufferSrgbCapable = extern "SDL_GL_FRAMEBUFFER_SRGB_CAPABLE" "SDL.h"

sdlGlContextReleaseBehavior :: SDLGlAttr
sdlGlContextReleaseBehavior = extern "SDL_GL_CONTEXT_RELEASE_BEHAVIOR" "SDL.h"

type SDLGlContextFlag = IString

sdlGlContextDebugFlag :: SDLGlContextFlag
sdlGlContextDebugFlag = extern "SDL_GL_CONTEXT_DEBUG_FLAG" "SDL.h"

sdlGlContextForwardCompatibleFlag :: SDLGlContextFlag
sdlGlContextForwardCompatibleFlag = extern "SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG" "SDL.h"

sdlGlContextRobustAccessFlag :: SDLGlContextFlag
sdlGlContextRobustAccessFlag = extern "SDL_GL_CONTEXT_ROBUST_ACCESS_FLAG" "SDL.h"

sdlGlContextResetIsolationFlag :: SDLGlContextFlag
sdlGlContextResetIsolationFlag = extern "SDL_GL_CONTEXT_RESET_ISOLATION_FLAG" "SDL.h"

type SDLGlProfile = IString

sdlGlContextProfileCore :: SDLGlProfile
sdlGlContextProfileCore = extern "SDL_GL_CONTEXT_PROFILE_CORE" "SDL.h"

sdlGlContextProfileCompatibility :: SDLGlProfile
sdlGlContextProfileCompatibility = extern "SDL_GL_CONTEXT_PROFILE_COMPATIBILITY" "SDL.h"

sdlGlContextProfileEs :: SDLGlProfile
sdlGlContextProfileEs = extern "SDL_GL_CONTEXT_PROFILE_ES" "SDL.h"

type SDLHint = IString

sdlSetHint :: Def ('[SDLHint, IString] :-> IBool)
sdlSetHint = importProc "SDL_SetHint" "SDL.h"

sdlHintAccelerometerAsJoystick :: SDLHint
sdlHintAccelerometerAsJoystick = extern "SDL_HINT_ACCELEROMETER_AS_JOYSTICK" "SDL.h"

sdlHintAndroidApkExpansionMainFileVersion :: SDLHint
sdlHintAndroidApkExpansionMainFileVersion = extern "SDL_HINT_ANDROID_APK_EXPANSION_MAIN_FILE_VERSION" "SDL.h"

sdlHintAndroidApkExpansionPatchFileVersion :: SDLHint
sdlHintAndroidApkExpansionPatchFileVersion = extern "SDL_HINT_ANDROID_APK_EXPANSION_PATCH_FILE_VERSION" "SDL.h"

sdlHintAndroidSeparateMouseAndTouch :: SDLHint
sdlHintAndroidSeparateMouseAndTouch = extern "SDL_HINT_ANDROID_SEPARATE_MOUSE_AND_TOUCH" "SDL.h"

sdlHintAppleTvControllerUiEvents :: SDLHint
sdlHintAppleTvControllerUiEvents = extern "SDL_HINT_APPLE_TV_CONTROLLER_UI_EVENTS" "SDL.h"

sdlHintAppleTvRemoteAllowRotation :: SDLHint
sdlHintAppleTvRemoteAllowRotation = extern "SDL_HINT_APPLE_TV_REMOTE_ALLOW_ROTATION" "SDL.h"

sdlHintBmpSaveLegacyFormat :: SDLHint
sdlHintBmpSaveLegacyFormat = extern "SDL_HINT_BMP_SAVE_LEGACY_FORMAT" "SDL.h"

sdlHintEmscriptenKeyboardElement :: SDLHint
sdlHintEmscriptenKeyboardElement = extern "SDL_HINT_EMSCRIPTEN_KEYBOARD_ELEMENT" "SDL.h"

sdlHintFramebufferAcceleration :: SDLHint
sdlHintFramebufferAcceleration = extern "SDL_HINT_FRAMEBUFFER_ACCELERATION" "SDL.h"

sdlHintGamecontrollerconfig :: SDLHint
sdlHintGamecontrollerconfig = extern "SDL_HINT_GAMECONTROLLERCONFIG" "SDL.h"

sdlHintGrabKeyboard :: SDLHint
sdlHintGrabKeyboard = extern "SDL_HINT_GRAB_KEYBOARD" "SDL.h"

sdlHintIdleTimerDisabled :: SDLHint
sdlHintIdleTimerDisabled = extern "SDL_HINT_IDLE_TIMER_DISABLED" "SDL.h"

sdlHintImeInternalEditing :: SDLHint
sdlHintImeInternalEditing = extern "SDL_HINT_IME_INTERNAL_EDITING" "SDL.h"

sdlHintJoystickAllowBackgroundEvents :: SDLHint
sdlHintJoystickAllowBackgroundEvents = extern "SDL_HINT_JOYSTICK_ALLOW_BACKGROUND_EVENTS" "SDL.h"

sdlHintMacBackgroundApp :: SDLHint
sdlHintMacBackgroundApp = extern "SDL_HINT_MAC_BACKGROUND_APP" "SDL.h"

sdlHintMacCtrlClickEmulateRightClick :: SDLHint
sdlHintMacCtrlClickEmulateRightClick = extern "SDL_HINT_MAC_CTRL_CLICK_EMULATE_RIGHT_CLICK" "SDL.h"

sdlHintMouseFocusClickthrough :: SDLHint
sdlHintMouseFocusClickthrough = extern "SDL_HINT_MOUSE_FOCUS_CLICKTHROUGH" "SDL.h"

sdlHintMouseRelativeModeWarp :: SDLHint
sdlHintMouseRelativeModeWarp = extern "SDL_HINT_MOUSE_RELATIVE_MODE_WARP" "SDL.h"

sdlHintNoSignalHandlers :: SDLHint
sdlHintNoSignalHandlers = extern "SDL_HINT_NO_SIGNAL_HANDLERS" "SDL.h"

sdlHintOrientations :: SDLHint
sdlHintOrientations = extern "SDL_HINT_ORIENTATIONS" "SDL.h"

sdlHintRenderDirect3d11Debug :: SDLHint
sdlHintRenderDirect3d11Debug = extern "SDL_HINT_RENDER_DIRECT3D11_DEBUG" "SDL.h"

sdlHintRenderDirect3dThreadsafe :: SDLHint
sdlHintRenderDirect3dThreadsafe = extern "SDL_HINT_RENDER_DIRECT3D_THREADSAFE" "SDL.h"

sdlHintRenderDriver :: SDLHint
sdlHintRenderDriver = extern "SDL_HINT_RENDER_DRIVER" "SDL.h"

sdlHintRenderOpenglShaders :: SDLHint
sdlHintRenderOpenglShaders = extern "SDL_HINT_RENDER_OPENGL_SHADERS" "SDL.h"

sdlHintRenderScaleQuality :: SDLHint
sdlHintRenderScaleQuality = extern "SDL_HINT_RENDER_SCALE_QUALITY" "SDL.h"

sdlHintRenderVsync :: SDLHint
sdlHintRenderVsync = extern "SDL_HINT_RENDER_VSYNC" "SDL.h"

sdlHintRpiVideoLayer :: SDLHint
sdlHintRpiVideoLayer = extern "SDL_HINT_RPI_VIDEO_LAYER" "SDL.h"

sdlHintThreadStackSize :: SDLHint
sdlHintThreadStackSize = extern "SDL_HINT_THREAD_STACK_SIZE" "SDL.h"

sdlHintTimerResolution :: SDLHint
sdlHintTimerResolution = extern "SDL_HINT_TIMER_RESOLUTION" "SDL.h"

sdlHintVideoAllowScreensaver :: SDLHint
sdlHintVideoAllowScreensaver = extern "SDL_HINT_VIDEO_ALLOW_SCREENSAVER" "SDL.h"

sdlHintVideoHighdpiDisabled :: SDLHint
sdlHintVideoHighdpiDisabled = extern "SDL_HINT_VIDEO_HIGHDPI_DISABLED" "SDL.h"

sdlHintVideoMacFullscreenSpaces :: SDLHint
sdlHintVideoMacFullscreenSpaces = extern "SDL_HINT_VIDEO_MAC_FULLSCREEN_SPACES" "SDL.h"

sdlHintVideoMinimizeOnFocusLoss :: SDLHint
sdlHintVideoMinimizeOnFocusLoss = extern "SDL_HINT_VIDEO_MINIMIZE_ON_FOCUS_LOSS" "SDL.h"

sdlHintVideoWindowSharePixelFormat :: SDLHint
sdlHintVideoWindowSharePixelFormat = extern "SDL_HINT_VIDEO_WINDOW_SHARE_PIXEL_FORMAT" "SDL.h"

sdlHintVideoWinD3dcompiler :: SDLHint
sdlHintVideoWinD3dcompiler = extern "SDL_HINT_VIDEO_WIN_D3DCOMPILER" "SDL.h"

sdlHintVideoX11NetWmPing :: SDLHint
sdlHintVideoX11NetWmPing = extern "SDL_HINT_VIDEO_X11_NET_WM_PING" "SDL.h"

sdlHintVideoX11Xinerama :: SDLHint
sdlHintVideoX11Xinerama = extern "SDL_HINT_VIDEO_X11_XINERAMA" "SDL.h"

sdlHintVideoX11Xrandr :: SDLHint
sdlHintVideoX11Xrandr = extern "SDL_HINT_VIDEO_X11_XRANDR" "SDL.h"

sdlHintVideoX11Xvidmode :: SDLHint
sdlHintVideoX11Xvidmode = extern "SDL_HINT_VIDEO_X11_XVIDMODE" "SDL.h"

sdlHintWindowsDisableThreadNaming :: SDLHint
sdlHintWindowsDisableThreadNaming = extern "SDL_HINT_WINDOWS_DISABLE_THREAD_NAMING" "SDL.h"

sdlHintWindowsEnableMessageloop :: SDLHint
sdlHintWindowsEnableMessageloop = extern "SDL_HINT_WINDOWS_ENABLE_MESSAGELOOP" "SDL.h"

sdlHintWindowsNoCloseOnAltF4 :: SDLHint
sdlHintWindowsNoCloseOnAltF4 = extern "SDL_HINT_WINDOWS_NO_CLOSE_ON_ALT_F4" "SDL.h"

sdlHintWindowFrameUsableWhileCursorHidden :: SDLHint
sdlHintWindowFrameUsableWhileCursorHidden = extern "SDL_HINT_WINDOW_FRAME_USABLE_WHILE_CURSOR_HIDDEN" "SDL.h"

sdlHintWinrtHandleBackButton :: SDLHint
sdlHintWinrtHandleBackButton = extern "SDL_HINT_WINRT_HANDLE_BACK_BUTTON" "SDL.h"

sdlHintWinrtPrivacyPolicyLabel :: SDLHint
sdlHintWinrtPrivacyPolicyLabel = extern "SDL_HINT_WINRT_PRIVACY_POLICY_LABEL" "SDL.h"

sdlHintWinrtPrivacyPolicyUrl :: SDLHint
sdlHintWinrtPrivacyPolicyUrl = extern "SDL_HINT_WINRT_PRIVACY_POLICY_URL" "SDL.h"

sdlHintXinputEnabled :: SDLHint
sdlHintXinputEnabled = extern "SDL_HINT_XINPUT_ENABLED" "SDL.h"

sdlHintXinputUseOldJoystickMapping :: SDLHint
sdlHintXinputUseOldJoystickMapping = extern "SDL_HINT_XINPUT_USE_OLD_JOYSTICK_MAPPING" "SDL.h"

sdlModule :: Module
sdlModule = package "ivory_sdl" $ do
  incl sdlInit
  incl sdlQuit
  incl sdlLog0
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
  incl sdlSetHint
  incl sdlGlSetAttribute
  inclSym sdlInitTimer
  inclSym sdlInitAudio
  inclSym sdlInitVideo
  inclSym sdlInitJoystick
  inclSym sdlInitHaptic
  inclSym sdlInitGameController
  inclSym sdlInitEvents
  inclSym sdlInitEverything
  inclSym sdlWindowFullscreen
  inclSym sdlWindowFullscreenDesktop
  inclSym sdlWindowOpenGL
  inclSym sdlWindowVulkan
  inclSym sdlWindowShown
  inclSym sdlWindowHidden
  inclSym sdlWindowBorderless
  inclSym sdlWindowResizable
  inclSym sdlWindowMinimized
  inclSym sdlWindowMaximized
  inclSym sdlWindowInputGrabbed
  inclSym sdlWindowAllowHighDpi
  inclSym sdlRendererSoftware
  inclSym sdlRendererAccelerated
  inclSym sdlRendererPresentVSync
  inclSym sdlRendererTargetTexture
  inclSym sdlGlRedSize
  inclSym sdlGlGreenSize
  inclSym sdlGlBlueSize
  inclSym sdlGlAlphaSize
  inclSym sdlGlBufferSize
  inclSym sdlGlDoublebuffer
  inclSym sdlGlDepthSize
  inclSym sdlGlStencilSize
  inclSym sdlGlAccumRedSize
  inclSym sdlGlAccumGreenSize
  inclSym sdlGlAccumBlueSize
  inclSym sdlGlAccumAlphaSize
  inclSym sdlGlStereo
  inclSym sdlGlMultisamplebuffers
  inclSym sdlGlMultisamplesamples
  inclSym sdlGlAcceleratedVisual
  inclSym sdlGlContextMajorVersion
  inclSym sdlGlContextMinorVersion
  inclSym sdlGlContextFlags
  inclSym sdlGlContextProfileMask
  inclSym sdlGlShareWithCurrentContext
  inclSym sdlGlFramebufferSrgbCapable
  inclSym sdlGlContextReleaseBehavior
  inclSym sdlGlContextDebugFlag
  inclSym sdlGlContextForwardCompatibleFlag
  inclSym sdlGlContextRobustAccessFlag
  inclSym sdlGlContextResetIsolationFlag
  inclSym sdlGlContextProfileCore
  inclSym sdlGlContextProfileCompatibility
  inclSym sdlGlContextProfileEs
  inclSym sdlHintAccelerometerAsJoystick
  inclSym sdlHintAndroidApkExpansionMainFileVersion
  inclSym sdlHintAndroidApkExpansionPatchFileVersion
  inclSym sdlHintAndroidSeparateMouseAndTouch
  inclSym sdlHintAppleTvControllerUiEvents
  inclSym sdlHintAppleTvRemoteAllowRotation
  inclSym sdlHintBmpSaveLegacyFormat
  inclSym sdlHintEmscriptenKeyboardElement
  inclSym sdlHintFramebufferAcceleration
  inclSym sdlHintGamecontrollerconfig
  inclSym sdlHintGrabKeyboard
  inclSym sdlHintIdleTimerDisabled
  inclSym sdlHintImeInternalEditing
  inclSym sdlHintJoystickAllowBackgroundEvents
  inclSym sdlHintMacBackgroundApp
  inclSym sdlHintMacCtrlClickEmulateRightClick
  inclSym sdlHintMouseFocusClickthrough
  inclSym sdlHintMouseRelativeModeWarp
  inclSym sdlHintNoSignalHandlers
  inclSym sdlHintOrientations
  inclSym sdlHintRenderDirect3d11Debug
  inclSym sdlHintRenderDirect3dThreadsafe
  inclSym sdlHintRenderDriver
  inclSym sdlHintRenderOpenglShaders
  inclSym sdlHintRenderScaleQuality
  inclSym sdlHintRenderVsync
  inclSym sdlHintRpiVideoLayer
  inclSym sdlHintThreadStackSize
  inclSym sdlHintTimerResolution
  inclSym sdlHintVideoAllowScreensaver
  inclSym sdlHintVideoHighdpiDisabled
  inclSym sdlHintVideoMacFullscreenSpaces
  inclSym sdlHintVideoMinimizeOnFocusLoss
  inclSym sdlHintVideoWindowSharePixelFormat
  inclSym sdlHintVideoWinD3dcompiler
  inclSym sdlHintVideoX11NetWmPing
  inclSym sdlHintVideoX11Xinerama
  inclSym sdlHintVideoX11Xrandr
  inclSym sdlHintVideoX11Xvidmode
  inclSym sdlHintWindowsDisableThreadNaming
  inclSym sdlHintWindowsEnableMessageloop
  inclSym sdlHintWindowsNoCloseOnAltF4
  inclSym sdlHintWindowFrameUsableWhileCursorHidden
  inclSym sdlHintWinrtHandleBackButton
  inclSym sdlHintWinrtPrivacyPolicyLabel
  inclSym sdlHintWinrtPrivacyPolicyUrl
  inclSym sdlHintXinputEnabled
  inclSym sdlHintXinputUseOldJoystickMapping
