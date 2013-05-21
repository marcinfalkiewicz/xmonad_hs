--  xmonad >>=
--
--  deps:
--      xmonad
--      xmonad-contrib
--
--  vim:ft=haskell

-- core
import XMonad
import XMonad.Config
import qualified XMonad.StackSet
-- window managment
import XMonad.Hooks.ManageHelpers (isDialog, isFullscreen, isInProperty,
                                doCenterFloat, doFullFloat, composeOne, (-?>))
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.Script
import XMonad.Hooks.Minimize

import XMonad.Actions.UpdateFocus

-- layout
import XMonad.Layout.Named
import XMonad.Layout.TrackFloating

import XMonad.Layout.IM
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.Grid
import XMonad.Layout.Minimize
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.SimplestFloat
import XMonad.Layout.NoBorders (smartBorders, noBorders)

-- prompt
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
-- keys
import XMonad.Util.EZConfig
import XMonad.Util.Run (safeSpawn, runInTerm)
import XMonad.Actions.CycleWS

import Data.Monoid (mconcat) -- merge multiple event hooks

termEmulator = "urxvtc"
modKey = mod4Mask -- Super key
toggleStrutsKey XConfig {XMonad.modMask = mod4Mask} = (mod4Mask, xK_b)

-- core config
main = do
    xmonad =<< statusBar "xmobar" statusBarPP toggleStrutsKey conf

conf =
    withUrgencyHook NoUrgencyHook $
    ewmh $ defaultConfig {
          borderWidth           = 1
        , normalBorderColor     = "#0C4D48"
        , focusedBorderColor    = "#00BFFE"
        , terminal              = termEmulator
        , modMask               = modKey
        , workspaces            = ["irc", "web" ] ++ map show [ 3 .. 8 ] ++ ["comm", "steam", "gimp"]
        , startupHook           = mconcat
                                    [ execScriptHook "startup"
                                    , adjustEventInput ]
        , layoutHook            = windowLayout
        , manageHook            = mconcat
                                    [ windowManagment
                                    , manageDocks ]
        , logHook               = mconcat
                                    [ takeTopFocus ]
        , handleEventHook       = mconcat
                                    [ fullscreenEventHook
                                    , minimizeEventHook
                                    , focusOnMouseMove ]
    }
    `additionalKeysP`   keyBinds

-- windows layout -- layout [nmaster] [delta] [ratio]
windowLayout =
    avoidStruts     $ smartBorders  $ trackFloating $
    onWorkspace "comm" im           $
    onWorkspace "steam" float       $
    onWorkspace "gimp" gimp         $
    tiled ||| mirror ||| full ||| float
        where
            im    = named "IM"    $ reflectHoriz $ noBorders
                (gridIM (1/5) (And (ClassName ("Gajim")) (Role "roster")))
            tiled   = named "<icon=/home/dweller/.xmobar/icons/tall.xbm/>"  $
                Tall 1 (5/100) (1/2)
            mirror  = named "<icon=/home/dweller/.xmobar/icons/mirror.xbm/>" $
                Mirror tiled
            full    = named "<icon=/home/dweller/.xmobar/icons/full.xbm/>"  $
                noBorders Full
            float   = named "<icon=/home/dweller/.xmobar/icons/float.xbm/>" $
                simplestFloat
            gimp    = named "gimp" $ withIM (1/5) (Role "gimp-toolbox") $
                reflectHoriz $ withIM (1/5) (Role "gimp-dock") Full

windowManagment = composeOne . concat $
  [ [ className =? browser          -?> doShift "web"   | browser   <- browsers     ],
    [ className =? im               -?> doShift "comm"  | im        <- ims          ],
    [ className =? graphics         -?> doShift "8"     | graphics  <- imageproc    ],

    -- firefox specific
    [ title     =? firefox          -?> doFloat         | firefox   <- fxdialog     ],

    [ isFullscreen                  -?> doFullFloat
    , isDialog                      -?> doCenterFloat

    , className =? "net-ftb-mclauncher-MinecraftLauncher" -?> doFloat

    , title     =? "MPlayer"        -?> doFullFloat
    , resource  =? "gnome-mplayer"  -?> doCenterFloat
    , className =? "mpv"            -?> doFloat
    , title     =? "Kerbal Space Program"   -?> doFullFloat
    , className =? "Nitrogen"       -?> doFloat

    , resource  =? "gimp"           -?> doShift "gimp"
    , className =? "fontforge"      -?> doFloat

    , resource  =? "Steam"          -?> doShift "steam"

    , resource  =? "stalonetray"    -?> doIgnore

    -- thunar
    , title     =? "File Operation Progress" -?> doCenterFloat

  ] ]
        where
            browsers    = ["Firefox", "Chrome", "Midori"]
            ims         = ["Gajim", "Pidgin", "Kadu", "Empathy"]
            imageproc   = ["Inkscape", "Blender"]
            --
            fxdialog    = ["Firefox Preferences", "About Mozilla Firefox"]

-- prompt settings
promptConfig = defaultXPConfig {
      font              = "xft:PF Tempesta Five:size=5"
    , defaultText       = ""
    , fgColor           = "#FEFEFE"
    , fgHLight          = "#00BFFE"
    , bgColor           = "#07171F"
    , bgHLight          = "#07171F"
    , promptBorderWidth = 0
    , position          = Bottom
    , height            = 16
    , historySize       = 128
    }

statusBarPP = xmobarPP {
      ppCurrent         = xmobarColor "#00BFFE" ""
    , ppHiddenNoWindows = xmobarColor "#505050" ""
    , ppUrgent          = xmobarColor "#E34E25" ""
    , ppSep             = xmobarColor "#FEFEFE" "" "   "
    , ppTitle           = wrap (xmobarColor"#FEB300" "" "<") (xmobarColor "#FEB300" "" ">") . shorten 60
}

-- key bindings
-- emacs-like notation
keyBinds = [
      ("M-q"        , safeSpawn "xmonad"    ["--restart"])
    , ("M-S-e"      , safeSpawn "gvim"      ["/home/dweller/.xmonad/xmonad.hs"])
    , ("M-S-Enter"  , safeSpawn termEmulator [])

    , ("M-p"        , shellPrompt promptConfig) -- dmenu-like prompt
    , ("M-S-p"      , sshPrompt promptConfig)   -- ssh prompt


    , ("M-f"        , safeSpawn "luakit"   [])
    , ("M-S-f"      , safeSpawn "firefox" ["-private"])

    , ("M-e"        , runInTerm "" "$HOME/.xmonad/scripts/tmux_wrapper mc mc")
    , ("M-m"        , runInTerm "" "$HOME/.xmonad/scripts/tmux_wrapper cmus cmus")
    , ("M-h"        , runInTerm "" "top")
    , ("<Print>"    , safeSpawn "scrot" [])
    , ("M-S-l"      , safeSpawn "xlock" ["-mode", "grav"])
    , ("M-="        , nextWS)
    , ("M--"        , prevWS)
    -- xmonad specific
    --
    , ("M-S-="      , sendMessage Expand)
    , ("M-S--"      , sendMessage Shrink)
    , ("M-b"        , sendMessage ToggleStruts)

    -- notebook specific
    -- lvds backlight (external keyboard)
    , ("M-S-<Up>"   , safeSpawn "xbacklight" ["-inc", "10"])
    , ("M-S-<Down>" , safeSpawn "xbacklight" ["-dec", "10"])

    -- mixer options (FreeBSD specific)
    --, ("<XF86AudioLowerVolume>",    safeSpawn "mixer" ["vol", "-5"])
    --, ("<XF86AudioRaiseVolume>",    safeSpawn "mixer" ["vol", "+5"])
    --, ("<XF86AudioMute>",           safeSpawn "mixer" ["vol", "0"])

    -- mixer options (Linux specific)
    , ("<XF86AudioLowerVolume>",    safeSpawn "amixer" ["-q", "set", "Master", "1%-"])
    , ("<XF86AudioRaiseVolume>",    safeSpawn "amixer" ["-q", "set", "Master", "1%+"])
    , ("<XF86AudioMute>",           safeSpawn "amixer" ["-q", "set", "Master", "toggle"])


    , ("M-<XF86AudioLowerVolume>",  safeSpawn "cmus-remote" ["-r"]) -- prev track
    , ("M-<XF86AudioRaiseVolume>",  safeSpawn "cmus-remote" ["-n"]) -- next track

    , ("M-<XF86AudioMute>",         safeSpawn "cmus-remote" ["-u"]) -- toggle play
    , ("<XF86AudioPlay>",         safeSpawn "cmus-remote" ["-u"]) -- toggle play
    ]

