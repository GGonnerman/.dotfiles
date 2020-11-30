    -- My configs
import Custom.MyKeys
import Custom.MyLayouts
import Custom.MyScratchpads
import Custom.MyVariables

    -- Base
import XMonad
import System.IO
import System.Exit

    -- Actions

    -- Data
import Data.Monoid

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)

--    -- Layouts
--import XMonad.Layout.GridVariants (Grid(Grid))
--import XMonad.Layout.SimplestFloat
--import XMonad.Layout.Spiral
--import XMonad.Layout.ResizableTile
--import XMonad.Layout.ThreeColumns
--
--    -- Layouts modifiers
--import XMonad.Layout.LayoutModifier
--import XMonad.Layout.LimitWindows (limitWindows)
--import XMonad.Layout.Magnifier
--import XMonad.Layout.MultiToggle (mkToggle, single)
--import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
--import XMonad.Layout.NoBorders
--import XMonad.Layout.Renamed (renamed, Rename(Replace))
--import XMonad.Layout.Spacing

    -- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce

-- Autostart
myStartupHook :: X ()
myStartupHook = do
            spawnOnce "nitrogen --restore &"
	    --spawnOnce "wallset --video ~/Videos/Retro.mp4"
            spawnOnce "picom &"
            spawnOnce "urxvtd &"
            spawnOnce "nm-applet &"
            --spawnOnce "xautolock -time 10 -corners -+-- -cornerdelay 1 -cornerredelay 10 -locker lock &"
            spawnOnce "/home/twoonesecond/bin/slack-cmds/slack-start &"
            spawnOnce "/home/twoonesecond/.screenlayout/dual-monitor.sh &"

-- Manage Hook
myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
    [ className =? "MPlayer"            --> doFloat
    , className =? "Gimp"               --> doFloat
    , className =? "Shotgun"            --> doFloat
    , className =? "mpv"                --> doFullFloat
    , className =? "Minecraft Launcher" --> doFloat
    , resource  =? "desktop_window"     --> doIgnore
    , resource  =? "kdesktop"           --> doIgnore
    ] <+> namedScratchpadManageHook scratchpads

-- Log hook
myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 1.0

-- Event hook
myEventHook = mempty

-- Main
main :: IO ()
main = do
    xmproc0 <- spawnPipe "xmobar -x 0 /home/twoonesecond/.xmonad/xmobarrc.0"
    xmproc1 <- spawnPipe "xmobar -x 1 /home/twoonesecond/.xmonad/xmobarrc.1"
    xmonad $ ewmh def
        { modMask               = myModMask
        , terminal              = myTerminal
        , workspaces            = myWorkspaces
        , borderWidth           = myBorderWidth
        , normalBorderColor     = myNormalColor
        , focusedBorderColor    = myFocusColor
        , startupHook           = myStartupHook
        , layoutHook            = myLayoutHook
        , handleEventHook       = docksEventHook <+> myEventHook
        , manageHook            = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageDocks
        , logHook               = myLogHook <+> dynamicLogWithPP xmobarPP
            { ppOutput          = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x
            , ppCurrent         = xmobarColor "#c3e88d" "" . wrap "[" "]"  -- current workspace in xmobar
            , ppVisible         = xmobarColor "#c3e88d" ""                 -- visible but not current workspace
            , ppHidden          = xmobarColor "#82aaff" "" . wrap "*" ""   -- hidden workspaces in xmobar
            , ppHiddenNoWindows = xmobarColor "#b3afc2" ""                 -- hidden workspaces (no windows)
            , ppTitle           = xmobarColor "#ffffff" "" . shorten 60    -- title of active window in xmobar
            , ppSep             = "<fc=#666666> | </fc>"                    -- separators in xmobar
            , ppUrgent          = xmobarColor "#c45500" "" . wrap "!" "!"  -- urgent workspace
            , ppExtras          = [windowCount]                            -- # of windows current workspace
            , ppOrder           = \(ws:l:t:ex) -> [ws,l]++ex++[t]
            }
    } `additionalKeysP` myKeys
