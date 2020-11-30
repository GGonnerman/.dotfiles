module Custom.MyScratchpads where

import XMonad
import Custom.MyVariables
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W
import XMonad.ManageHook

scratchpads :: [NamedScratchpad]

scratchpads = [  NS "terminal" "urxvt -name scratchpad" (resource =? "scratchpad") (customFloating $ center 0.5 0.5)
               , NS "mocp" "urxvt -name mocp -e mocp" (resource =? "mocp") (customFloating $ center 0.5 0.5 )
               , NS "slack" "urxvt -name slack -e slack-term" (resource =? "slack") (customFloating $ center 0.65 0.35)
               , NS "live-server" "urxvt -name live-server" (resource =? "live-server") (customFloating $ center 0.5 0.5)
               , NS "htop" "urxvt -name htop -e htop" (resource =? "htop") (customFloating $ center 0.5 0.5)
               , NS "newsboat" "urxvt -name newsboat -e newsboat" (resource =? "newsboat") (customFloating $ center 0.8 0.8)
               , NS "alsamixer" "urxvt -name alsamixer -e alsamixer" (resource =? "alsamixer") (customFloating $ center 0.8 0.8)
               ] where center w h = W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h
