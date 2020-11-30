module Custom.MyVariables where

    -- Imports
import XMonad
import qualified XMonad.StackSet as W

-- Variables

myFont :: String
myFont = "xft:Mononoki Nerd Font:bold:size=9:antialias=true:hinting=true"

-- Sets modkey to super/windows key
myModMask :: KeyMask
myModMask = mod4Mask

-- Sets default terminal
myTerminal :: String
myTerminal = "urxvt"

-- Sets border width for  windows
myBorderWidth :: Dimension
myBorderWidth = 3

-- Sets border color for normal windows
myNormalColor :: String
myNormalColor = "#494d5e"

-- Sets border color for focused windows
myFocusColor :: String
myFocusColor = "#bbc5ff"

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- Workspaces
xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
    where
        doubleLts '<' = "<<"
        doubleLts x   = [x]

myWorkspaces :: [String]
myWorkspaces = clickable . (map xmobarEscape)
                $ ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
    where
        clickable  l = ["<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                        (i,ws) <- zip [1..9] l,
                        let n = i ]
