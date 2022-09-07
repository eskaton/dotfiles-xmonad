import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Search hiding (hoogle)
import XMonad.Actions.Submap
import XMonad.Actions.WindowGo
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog hiding (xmobar)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers
import XMonad.Prompt.Man
import XMonad.Layout.ResizableTile
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Util.XSelection
import System.Directory(getHomeDirectory)
import System.IO
import XMonad.Additions.Actions
import XMonad.Additions.Keys
import XMonad.Additions.Layout

switch_ws ws = case filter (\(_,w) -> w == ws) $ zip (map show [0..]) myWorkspaces of
                  [(n,_)] -> wrap ("<action=xdotool set_desktop " ++ n ++ ">") "</action>" ws

main = do
   home <- getHomeDirectory
   xmproc <- spawnPipe $ xmobar home
   xmonad $ ewmh . docks $ def {focusFollowsMouse = True}
      { modMask = mod4Mask
      , manageHook = manageDocks <+> myManageHook
      , layoutHook = myLayout
      , handleEventHook = handleEventHook def
      , logHook = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppHidden = xmobarColor "#a0a0a0" "" . switch_ws
            , ppHiddenNoWindows = switch_ws
            }
      , startupHook        = setWMName "LG3D"
      , borderWidth        = 1
      , terminal           = "xterm"
      , normalBorderColor  = "#343434"
      , focusedBorderColor = "#193375" 
      , workspaces         = myWorkspaces
      } `additionalKeys` ([ 
        ((mod4Mask .|. shiftMask, xK_o),   promptSelection browser)
      , ((mod4Mask .|. shiftMask, xK_p),   submap $ programsMap home)
      , ((mod4Mask,               xK_F1),  manPrompt def)
      , ((mod4Mask,               xK_s),   submap $ searchEngineMap $ promptSearchBrowser def browser)
      , ((mod4Mask .|. shiftMask, xK_s),   submap $ searchEngineMap $ selectSearchBrowser browser)
      , ((mod4Mask,               xK_Tab), toggleWS)
      , ((mod1Mask,               xK_b),   sendMessage ToggleStruts)
      ] ++ extraKeys)
      `additionalMouseBindings` extraMouseBindings

dictCc = searchEngine "dictcc" "https://dict.cc/?s="
hoogle = searchEngine "hoogle" "https://hoogle.haskell.org/?hoogle="
duckDuckGo = searchEngine "duckduckgo" "https://duckduckgo.com/?q="
googleMaps = searchEngine "googlemaps" "https://www.google.ch/maps/place/"

searchEngineMap method = M.fromList $
      [ ((0, xK_e), method dictCc)
      , ((0, xK_d), method duckDuckGo)
      , ((0, xK_g), method google)
      , ((0, xK_i), method imdb)
      , ((0, xK_h), method hoogle)
      , ((0, xK_m), method googleMaps)
      , ((0, xK_w), method wikipedia)
      ]
