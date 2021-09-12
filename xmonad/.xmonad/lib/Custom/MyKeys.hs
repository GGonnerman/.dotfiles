module Custom.MyKeys where

    -- Custom (my personal configs)
import Custom.MyGridMenu
import Custom.MyScratchpads
import Custom.MyVariables

    -- XMonad
import XMonad
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Actions
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.CopyWindow (copyToAll, kill1, killAllOtherCopies)
import XMonad.Actions.GridSelect
import XMonad.Actions.WithAll (sinkAll, killAll)

    -- Data
import Data.Maybe (isJust)

    -- Hooks
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))

    -- Layout modifiers
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

    -- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad

myKeys :: [(String, X ())]
myKeys =
    -- Xmonad
        [ ("M-<Backspace>", spawn "xmonad --recompile && xmonad --restart") -- Recompile and restart xmonad
        , ("M-C-S-<Backspace>", io exitSuccess)                             -- Quits xmonad

    -- Open my preferred terminal
        , ("M-<Return>", spawn myTerminal)

    -- Scratchpads
        , ("M-S-<Return>", namedScratchpadAction scratchpads "terminal")
        , ("M-S-m", namedScratchpadAction scratchpads "mocp")
        , ("M-S-s", namedScratchpadAction scratchpads "slack")
        , ("M-S-l", namedScratchpadAction scratchpads "live-server")
        , ("M-S-h", namedScratchpadAction scratchpads "htop")
        , ("M-S-n", namedScratchpadAction scratchpads "newsboat")
        , ("M-S-a", namedScratchpadAction scratchpads "alsamixer")

    -- Launch dmenu
        , ("M-d", spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")

    -- Screenshots
        , ("M-p", spawn "sh /home/twoonesecond/bin/copyscreenshot")
        , ("M-S-p", spawn "sh /home/twoonesecond/bin/savescreenshot")

    -- Monitors
        , ("M-C-s", spawn "sh /home/twoonesecond/.screenlayout/1.sh && xmonad --restart")
        , ("M-C-a", spawn "sh /home/twoonesecond/.screenlayout/2.sh && xmonad --restart")

    -- Windows
        , ("M-q", kill1)                           -- Kill the currently focused client
        , ("M-S-r", killAll)                       -- Kill all windows on current workspace

    -- Grid Select (CTRL-g followed by a key)
        , ("C-g g", spawnSelected' myAppGrid)
        , ("C-M1-g", spawnSelected' myAppGrid)
        , ("C-g t", goToSelected $ myGridConfig myColorizer)
        , ("C-g b", bringSelected $ myGridConfig myColorizer)

    -- Windows navigation
--      , ("M-m", windows W.focusMaster)     -- Move focus to the master window
        , ("M-j", windows W.focusDown)       -- Move focus to the next window
        , ("M-k", windows W.focusUp)         -- Move focus to the prev window
--      , ("M-S-m", windows W.swapMaster)    -- Swap the focused window and the master window
        , ("M-S-j", windows W.swapDown)      -- Swap focused window with next window
        , ("M-S-k", windows W.swapUp)        -- Swap focused window with prev window
--      , ("M-<Backspace>", promote)         -- Moves focused window to master, others maintain order
--      , ("M1-S-<Tab>", rotSlavesDown)      -- Rotate all windows except master and keep focus in place
--      , ("M1-C-<Tab>", rotAllDown)         -- Rotate all the windows in the current stack
        , ("M-0", windows copyToAll)         -- Make the client 'sticky' (show on all workspaces)
        , ("M-S-0", killAllOtherCopies)      -- Remove the client from all workspaces besides current

        -- Layouts
        , ("M-<Tab>", sendMessage NextLayout)                -- Switch to next layout
--      , ("M-C-M1-<Up>", sendMessage Arrange)
--      , ("M-C-M1-<Down>", sendMessage DeArrange)
        , ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
--      , ("M-S-<Space>", sendMessage ToggleStruts)         -- Toggles struts
--      , ("M-S-n", sendMessage $ MT.Toggle NOBORDERS)      -- Toggles noborder
        , ("M-o", sendMessage (IncMasterN 1))              -- Increase number of clients in master pane
        , ("M-S-o", sendMessage (IncMasterN (-1)))         -- Decrease number of clients in master pane
--      , ("M-S-<KP_Multiply>", increaseLimit)              -- Increase number of windows
--      , ("M-S-<KP_Divide>", decreaseLimit)                -- Decrease number of windows

        , ("M-h", sendMessage Shrink)                       -- Shrink horiz window width
        , ("M-l", sendMessage Expand)                       -- Expand horiz window width
--      , ("M-C-j", sendMessage MirrorShrink)               -- Shrink vert window width
--      , ("M-C-k", sendMessage MirrorExpand)               -- Exoand vert window width

    -- Floating windows
        , ("M-f", sendMessage (T.Toggle "floats")) -- Toggles my 'floats' layout
        , ("M-t", withFocused $ windows . W.sink)  -- Push floating window back to tile
        , ("M-S-t", sinkAll)                       -- Push ALL floating windows to tile

    -- Workspaces
        , ("M-.", nextScreen)  -- Switch focus to next monitor
        , ("M-,", prevScreen)  -- Switch focus to prev monitor

    -- Custom youtube-viewer script
        , ("M-M1-o", spawn "/home/twoonesecond/bin/youtube-viewer-clip")

    -- Controls for mocp music player.
        , ("M-M1-p", spawn "mocp --play")
        , ("M-M1-l", spawn "mocp --next")
        , ("M-M1-h", spawn "mocp --previous")
        , ("M-M1-<Space>", spawn "mocp --toggle-pause")

    -- Volume controls
        , ("M-M1-m", spawn "amixer set Master toggle")
        , ("M-M1-k", spawn "amixer set Master 5%+ unmute")
        , ("M-M1-j", spawn "amixer set Master 5%- unmute")

    -- Switch keyboard layout
        , ("M-C-1", spawn "setxkbmap -v us -variant colemak")    -- Enable colemak layout
        , ("M-C-2", spawn "setxkbmap us")         -- Enable us layout

    --- My Applications (Super+Alt+Key)
        , ("M-c", spawn (myTerminal ++ " --hold -e cal"))
--        , ("M-M1-a", spawn (myTerminal ++ " -e ncpamixer"))
--        , ("M-M1-b", spawn "surf www.youtube.com/c/DistroTube/")
--        , ("M-M1-e", spawn (myTerminal ++ " -e neomutt"))
--        , ("M-M1-f", spawn (myTerminal ++ " -e sh ./.config/vifm/scripts/vifmrun"))
--        , ("M-M1-i", spawn (myTerminal ++ " -e irssi"))
--        , ("M-M1-j", spawn (myTerminal ++ " -e joplin"))
--        , ("M-M1-l", spawn (myTerminal ++ " -e lynx -cfg=~/.lynx/lynx.cfg -lss=~/.lynx/lynx.lss gopher://distro.tube"))
--        , ("M-M1-m", spawn (myTerminal ++ " -e mocp"))
--        , ("M-M1-n", spawn (myTerminal ++ " -e newsboat"))
--        , ("M-M1-p", spawn (myTerminal ++ " -e pianobar"))
--        , ("M-M1-r", spawn (myTerminal ++ " -e rtv"))
--        , ("M-M1-t", spawn (myTerminal ++ " -e toot curses"))
--        , ("M-M1-w", spawn (myTerminal ++ " -e wopr report.xml"))
--        , ("M-M1-y", spawn (myTerminal ++ " -e youtube-viewer"))

    -- Multimedia Keys
	, ("<XF86AudioPlay>", spawn "mocp --toggle-pause")
	, ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
	, ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
--        , ("<XF86AudioPrev>", spawn "cmus prev")
--        , ("<XF86AudioNext>", spawn "cmus next")
        , ("<XF86AudioMute>",   spawn "amixer set Master toggle")  -- Bug prevents it from toggling correctly in 12.04.
        , ("<XF86MonBrightnessUp>", spawn "light -A 5")
        , ("<XF86MonBrightnessDown>", spawn "light -U 5")
--        , ("<XF86HomePage>", spawn "firefox")
--        , ("<XF86Search>", safeSpawn "firefox" ["https://www.google.com/"])
--        , ("<XF86Mail>", runOrRaise "geary" (resource =? "thunderbird"))
--        , ("<XF86Calculator>", runOrRaise "gcalctool" (resource =? "gcalctool"))
--        , ("<XF86Eject>", spawn "toggleeject")
--        , ("<Print>", spawn "scrotd 0")
        ]
        -- Appending some extra xprompts to keybindings list.
        -- Look at "xprompt settings" section this of config for values for "k".
        -- ++ [("M-p " ++ k, f dtXPConfig') | (k,f) <- promptList ]
        -- ++ [("M-p " ++ k, f dtXPConfig' g) | (k,f,g) <- promptList' ]

        -- The following lines are needed for named scratchpads.
          where nonNSP          = WSIs (return (\ws -> W.tag ws /= "nsp"))
                nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))

--import XMonad
--import System.IO (hPutStrLn)
--import System.Exit (exitSuccess)
--import qualified XMonad.StackSet as W

    -- Actions
--import XMonad.Actions.GridSelect
--import XMonad.Actions.MouseResize
--import XMonad.Actions.Promote
--import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
--import qualified XMonad.Actions.TreeSelect as TS
--import XMonad.Actions.WindowGo (runOrRaise)
--import qualified XMonad.Actions.Search as S

    -- Data
--import Data.Char (isSpace)
--import Data.Monoid
--import Data.Maybe (isJust)
--import Data.Tree
--import qualified Data.Tuple.Extra as TE
--import qualified Data.Map as M

    -- Hooks
--import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
--import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
--import XMonad.Hooks.FadeInactive
--import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
--import XMonad.Hooks.ServerMode
--import XMonad.Hooks.SetWMName
--import XMonad.Hooks.WorkspaceHistory

    -- Layouts
--import XMonad.Layout.GridVariants (Grid(Grid))
--import XMonad.Layout.SimplestFloat
--import XMonad.Layout.Spiral
--import XMonad.Layout.ResizableTile
--import XMonad.Layout.Tabbed
--import XMonad.Layout.ThreeColumns

    -- Layouts modifiers
--import XMonad.Layout.LayoutModifier
--import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
--import XMonad.Layout.Magnifier
--import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
--import XMonad.Layout.NoBorders
--import XMonad.Layout.Renamed (renamed, Rename(Replace))
--import XMonad.Layout.ShowWName
--import XMonad.Layout.Spacing
--import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))

    -- Prompt
--import XMonad.Prompt
--import XMonad.Prompt.Input
--import XMonad.Prompt.FuzzyMatch
--import XMonad.Prompt.Man
--import XMonad.Prompt.Pass
--import XMonad.Prompt.Shell (shellPrompt)
--import XMonad.Prompt.Ssh
--import XMonad.Prompt.XMonad
--import Control.Arrow (first)

    -- Utilities
--import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
--import XMonad.Util.SpawnOnce

