-- vim: set ts=2 sw=2 et:

import System.IO

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Util.Run(spawnPipe)

myModMask = mod4Mask -- left super key
myTerminal = "urxvt"
myWorkspaces = [] ++ map show [5..10]


main :: IO ()
main = do
  xmproc <- spawnPipe "/usr/bin/xmobar"
  xmonad $ defaultConfig
    { terminal    = myTerminal
    , modMask     = myModMask
    , borderWidth = 0
    , focusFollowsMouse = True
    , manageHook = manageDocks <+> manageHook defaultConfig
    , layoutHook = avoidStruts $ myLayout
    , logHook = myLogHook xmproc
    , workspaces = myWorkspaces
    }

myLayout = spacing 8 $
  gaps [(U, 2), (D, 1), (L, 1), (R, 1)] $
    (ResizableTall 1 (1/55) (1/2) [])
  ||| Simplest

myLogHook h = dynamicLogWithPP $ xmobarPP
  { ppOutput = hPutStrLn h
  , ppTitle = xmobarColor "green" "" . shorten 80
  }
