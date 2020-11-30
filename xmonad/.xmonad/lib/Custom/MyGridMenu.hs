module Custom.MyGridMenu where

  -- imports
import XMonad
import XMonad.Actions.GridSelect
import Custom.MyVariables

-- Grid select colors
myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
                    (0x29, 0x2d, 0x3e) -- Lowest inactive bg
                    (0x29, 0x2d, 0x3e) -- Highest inactive bg
                    (0xc7, 0x92, 0xea) -- Active bg
                    (0xc0, 0xa7, 0x9a) -- Inactive fg
                    (0x29, 0x2d, 0x3e) -- Active fg

-- Gridselect menu layout
myGridConfig :: p -> GSConfig Window
myGridConfig colorizer  = (buildDefaultGSConfig myColorizer)
      { gs_cellheight   = 40
      , gs_cellwidth    = 200
      , gs_cellpadding  = 6
      , gs_originFractX = 0.5
      , gs_originFractY = 0.5
      , gs_font         = myFont
      } 

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = def
                    { gs_cellheight   = 40
                    , gs_cellwidth    = 200
                    , gs_cellpadding  = 6
                    , gs_originFractX = 0.5
                    , gs_originFractY = 0.5
                    , gs_font         = myFont
                    }

myAppGrid = [ ("Audio", myTerminal ++ " -e alsamixer")
            , ("Mocp",  myTerminal ++ " -e mocp")
            , ("Browser", "brave")
            , ("Gimp", "gimp")
            , ("Slack",  "slack")
            , ("Bluetooth", "blueman-manager")
            , ("Nemo", "nemo")
            , ("Kazam", "kazam")
            , ("Keepass", "keepassxc")
            , ("DWService", "$HOME/Applications/dwagent.sh")
            ]
