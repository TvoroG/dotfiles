import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeysP)
import Graphics.X11.ExtraTypes.XF86
import System.IO
import System.IO.Unsafe (unsafePerformIO)


myKeys = [("<XF86AudioLowerVolume>", spawn "amixer set Master 2%-"),
          ("<XF86AudioRaiseVolume>", spawn "amixer set Master 2%+"),
          ("<XF86AudioMute>", spawn "amixer set Master toggle"),
          ("<XF86MonBrightnessDown>", spawn "xbacklight - 5"),
          ("<XF86MonBrightnessUp>", spawn "xbacklight + 5"),
          ("<XF86AudioPlay>", spawn "cmus-remote --pause"),
          ("C-M-<Right>", spawn "cmus-remote --next"),
          ("C-M-<Left>", spawn "cmus-remote --prev")]


myManageHook = composeAll
  [ manageDocks
  , isFullscreen --> doFullFloat
  , className =? "Emacs" --> doShift "2:dev"
  , className =? "chromium-browser" --> doShift "1:web"
  , manageHook defaultConfig
  ]


myLayout = tiled ||| Mirror tiled ||| Full
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100


myConfig xmproc = defaultConfig
  { workspaces = ["1:web", "2:dev", "3:term", "4", "5", "6", "7"]
  , modMask = mod4Mask
  , logHook = dynamicLogWithPP xmobarPP
    { ppOutput = hPutStrLn xmproc
    , ppTitle = xmobarColor "green" "" . shorten 50
    }
  , terminal = "urxvt"
  , manageHook = myManageHook
  , layoutHook = avoidStruts $ smartBorders $ myLayout
  } `additionalKeysP` myKeys


main = do
  xmproc <- spawnPipe "/run/current-system/sw/bin/xmobar"
  xmonad =<< xmobar (myConfig xmproc)
