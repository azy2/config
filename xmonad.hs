
--
-- xmonad example config file for xmonad-0.9
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--
-- NOTE: Those updating from earlier xmonad versions, who use
-- EwmhDesktops, safeSpawn, WindowGo, or the simple-status-bar
-- setup functions (dzen, xmobar) probably need to change
-- xmonad.hs, please see the notes below, or the following
-- link for more details:
--
-- http://www.haskell.org/haskellwiki/Xmonad/Notable_changes_since_0.8
--

import           Data.Monoid
import           System.Exit
import           XMonad
import           XMonad.Actions.SpawnOn
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.BinarySpacePartition

import           XMonad.Util.EZConfig        (additionalKeys)

import           Control.Monad               (liftM2)
import           Data.List
import qualified Data.Map                    as M
import qualified XMonad.StackSet             as W

import           XMonad.Hooks.EwmhDesktops

import           XMonad.Util.Run
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "konsole"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Width of the window border in pixels.
--
myBorderWidth   = 4

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- NOTE: from 0.9.1 on numlock mask is set automatically. The numlockMask
-- setting should be removed from configs.
--
-- You can safely remove this even on earlier xmonad versions unless you
-- need to set it to something other than the default mod2Mask, (e.g. OSX).
--
-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
-- myNumlockMask   = mod2Mask -- deprecated in xmonad-0.9.1
------------------------------------------------------------


-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = map show [1..9]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#AAFFFF"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal"
    [ ((modm, xK_Return), spawn "emacsclient -c \"$@\" -e \"(multi-term)\"")

    --  Launch dmenu
    , ((modm, xK_e     ), spawn "rofi -show run")

    --  Launch emacs
    , ((modm, xK_o     ), spawn "emacsclient -c")

    --  Launch dmenu
    , ((modm, xK_r     ), spawn "chromium --force-device-scale-factor=1.25")
    , ((modm, xK_l     ), spawn "dwb")
    , ((modm, xK_s     ), spawn "pavucontrol")

    , ((modm, xK_Menu  ), spawn "/home/ben/bin/switchlayouts toggle")

    -- Change wallpaper
    , ((modm, xK_w     ), spawn "/home/ben/bin/randomWallpaper")

    -- close focused window
    , ((modm, xK_a     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    --, ((modm,               xK_p     ), refresh)

    -- Move focus to the next window
    --, ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_h     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_t     ), windows W.focusUp  )

    -- Swap the focused window and the master window
    --, ((modm .|. shiftMask, xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_h     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_t     ), windows W.swapUp    )

    , ((modm, xK_g ), sendMessage $ MoveSplit U)

    , ((modm, xK_m), sendMessage $ MoveSplit D)

    , ((modm, xK_d), sendMessage $ MoveSplit L)

    , ((modm, xK_n), sendMessage $ MoveSplit R)

    , ((modm, xK_f), sendMessage $ Rotate)

    , ((modm, xK_c), sendMessage $ Swap)

    -- Shrink the master area
    --, ((modm,               xK_d     ), sendMessage Shrink)

    -- Expand the master area
    --, ((modm,               xK_n     ), sendMessage Expand)

    --, ((modm,               xK_g     ), sendMessage MirrorExpand)

    --, ((modm,               xK_m     ), sendMessage MirrorShrink)

    -- Push window back into tiling
    , ((modm,               xK_y     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    --, ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    --, ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm              , xK_z     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_a     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")


    -- change volume
    , ((0, 0x1008FF11 ), spawn "pulseaudio-ctl down 5")

    -- change volume
    , ((0, 0x1008FF13 ), spawn "pulseaudio-ctl up 5")

    -- change brightness
    , ((0, 0x1008FF02 ), spawn "sudo brightness up")

    -- change brightness
    , ((0, 0x1008FF03 ), spawn "sudo brightness down")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_exclam, xK_at, xK_braceleft, xK_braceright, xK_percent, xK_asciicircum, xK_ampersand, xK_asterisk, xK_parenleft]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- * NOTE: XMonad.Hooks.EwmhDesktops users must remove the obsolete
-- ewmhDesktopsLayout modifier from layoutHook. It no longer exists.
-- Instead use the 'ewmh' function from that module to modify your
-- defaultConfig as a whole. (See also logHook, handleEventHook, and
-- startupHook ewmh notes.)
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
-- myLayout = smartBorders $ spacing 8 $ ResizableTall 1 (5/100) (1/2) [] ||| ThreeCol 1 (5/100) (1/3) ||| noBorders Full
myLayout = smartBorders $ spacing 8 $ emptyBSP ||| noBorders Full

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = manageSpawn <+> composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Mathematica"    --> doFloat
    , className =? "Calculator"         --> doFloat
    , className =? "Figure" --> doFloat
    , className =? "QEMU" --> doFloat
    , isFullscreen --> doFullFloat
    , resource =? "desktop_window" --> doIgnore
    , fmap ("float" `isInfixOf`) title --> doFloat]

pbManageHook = composeAll $ concat
   [ [ manageDocks ]
   , [ manageHook defaultConfig ]
   , [ isDialog --> doCenterFloat ]
   , [ isFullscreen --> (doF W.focusDown <+> doFullFloat) ]
   , [ fmap not isDialog --> doF avoidMaster ]]

avoidMaster = W.modify' $ \c -> case c of
   W.Stack t [] (r:rs) -> W.Stack t [r] rs
   otherwise -> c


------------------------------------------------------------------------
-- Event handling

-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH event handling to your custom event hooks by
-- combining them with ewmhDesktopsEventHook.
--
myEventHook = fullscreenEventHook

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH logHook actions to your custom log hook by
-- combining it with ewmhDesktopsLogHook.
--
-- myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add initialization of EWMH support to your custom startup
-- hook by combining it with ewmhDesktopsStartup.
--
myStartupHook = do
  setWMName "LG3D"
  spawn "/home/ben/bin/emacs-hidpi"



spawnToWorkspace :: String -> String -> X ()
spawnToWorkspace program workspace = do
  spawn program
  windows $ W.greedyView workspace

-- myXmonadBar = "conky -c /home/ben/.xmonad/.conky_dzen | dzen2 -fn 'Deja Vu Sans Mono:13' -p -fg '#FFFFFF' -bg '#000000' -x '0' -y '0' -h '24' -w '1920'"
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = xmonad =<< xmobar defaults {
  startupHook = startupHook defaults >> setWMName "LG3D"
                                  }
-- main = xmonad defaults

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.

defaults = ewmh defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        -- numlockMask deprecated in 0.9.1
        -- numlockMask        = myNumlockMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = avoidStruts myLayout,
        -- manageHook         = pbManageHook <+> myManageHook
        --                                   <+> manageDocks
        --                                   <+> manageSpawn
        --                                   <+> manageHook defaultConfig,
        manageHook         = myManageHook,
        handleEventHook    = ewmhDesktopsEventHook <+> myEventHook,
        logHook            = ewmhDesktopsLogHook,
        startupHook        = myStartupHook <+> ewmhDesktopsStartup
    }
