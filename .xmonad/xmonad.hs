-- vim: set ts=2 sw=2 et:

import qualified Data.Map as M

import System.Exit
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
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)

myModMask = mod4Mask -- left super key
myTerminal = "urxvt"
myWorkspaces = ["term", "web", "mail", "ide"] ++ map show [5..10]

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
    , keys = myKeys
    , mouseBindings = myMouseBindings
    }

myLayout = spacing 8 $
  gaps [(U, 2), (D, 1), (L, 1), (R, 1)] $
    (ResizableTall 1 (1/55) (1/2) [])
  ||| Simplest

myLogHook h = dynamicLogWithPP $ xmobarPP
  { ppOutput = hPutStrLn h
  , ppTitle = xmobarColor "green" "" . shorten 80
  }

-- http://xmonad.org/xmonad-docs/xmonad/src/XMonad-Config.html
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  [ (( modMask .|. shiftMask,     xK_Return ), spawn $ XMonad.terminal conf)
  , (( modMask,                   xK_p      ), spawn "dmenu_run -p \"(*'-') < \" -fn mikachan -sb black -sf green")
  , (( modMask .|. controlMask,   xK_l      ), spawn "xscreensaver-command -lock")
  , (( modMask .|. shiftMask,     xK_c      ), kill)

  , (( modMask,                   xK_space  ), sendMessage NextLayout)
  , (( modMask .|. shiftMask,     xK_space  ), setLayout $ XMonad.layoutHook conf)
  , (( modMask,                   xK_n      ), refresh)

  , (( modMask,                   xK_Tab    ), windows W.focusDown)
  , (( modMask .|. shiftMask,     xK_Tab    ), windows W.focusUp)
  , (( modMask,                   xK_j      ), windows W.focusDown)
  , (( modMask,                   xK_k      ), windows W.focusUp)
  , (( modMask,                   xK_m      ), windows W.focusMaster)

  , (( modMask,                   xK_Return ), windows W.swapMaster)
  , (( modMask .|. shiftMask,     xK_j      ), windows W.swapDown)
  , (( modMask .|. shiftMask,     xK_k      ), windows W.swapUp)

  , (( modMask,                   xK_h      ), sendMessage Shrink)
  , (( modMask,                   xK_l      ), sendMessage Expand)

  , (( modMask,                   xK_t      ), withFocused $ windows . W.sink)

  , (( modMask,                   xK_comma  ), sendMessage $ IncMasterN 1)
  , (( modMask,                   xK_period ), sendMessage $ IncMasterN (-1))

  , (( modMask .|. shiftMask,     xK_q      ), io $ exitWith ExitSuccess)
  , (( modMask,                   xK_q      ), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")

  , (( modMask .|. shiftMask,     xK_slash  ), spawn $ "echo \"" ++ help ++ "\" | xmessage -file -")
  , (( modMask,                   xK_question), spawn $ "echo \"" ++ help ++ "\" | xmessage -file -")
  ]
  ++
  [((m .|. modMask, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++
  [((m .|. modMask, k), screenWorkspace s >>= flip whenJust (windows . f))
    | (k, s) <- zip [xK_w, xK_e, xK_r] [0..]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList
  [ (( modMask, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
  , (( modMask, button2), windows . (W.shiftMaster .) . W.focusWindow)
  , (( modMask, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
  , (( modMask, button4), \w -> sendMessage Shrink)
  , (( modMask, button5), \w -> sendMessage Expand)
  ]

help :: String
help = unlines
  [ "key bindings"
  , ""
  , "Mod-Shift-Enter    launch terminal"
  , "Mod-p              launch dmenu"
  , "Mod-Control-l      lock screen"
  , "Mod-Shift-c        close window"
  , "Mod-Space          rotate layouts"
  , "Mod-Shift-Space    reset layouts"
  , "Mod-n              resize/refresh windows"
  , ""
  , "Mod-Tab            move focus to next window"
  , "Mod-Shift-Tab      move focus to prev window"
  , "Mod-j              move focus to next window"
  , "Mod-k              move focus to prev window"
  , "Mod-m              move focus to master window"
  , ""
  , "Mod-Return         swap the focused window and the master window"
  , "Mod-Shift-j        swap the focused window and the master window"
  , "Mod-Shift-k        swap the focused window and the master window"
  , ""
  , "Mod-h              shrink the master area"
  , "Mod-l              expand the master area"
  , ""
  , "Mod-t              unfloat window"
  , ""
  , "Mod-Comma          increment the number of windows in the master area"
  , "Mod-period         decrement the number of windows in the master area"
  , ""
  , "Mod-Shift-q        quit xmonad"
  , "Mod-q              restart xmonad"
  , ""
  , "Mod-[1..9]         switch to workspace N"
  , "Mod-Shift-[1..9]   move window to workspace N"
  , "Mod-{w,e,r}        switch to screen N"
  , "Mod-Shift-{w,e,r}  move window to screen N"
  , ""
  , "mouse bindings"
  , ""
  , "Mod-LeftClick      drag the window"
  , "Mod-MiddleClick    move the window to top"
  , "Mod-RightClick     resize the window"
  , "Mod-ScrollWheel    resize the master area"
  ]
