import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import System.IO

main = do
  xmproc <- spawnPipe "/run/current-system/sw/bin/xmobar"
  xmonad =<< xmobar defaultConfig {
    modMask = mod4Mask
    , logHook = dynamicLogWithPP xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppTitle = xmobarColor "green" "" . shorten 50
      }
    , terminal  = "urxvt"
    }
