-- xmonad >>=
--
-- deps:
--  xmonad
--  xmonad-contrib
--

-- core
import XMonad
import XMonad.Config

-- window managment
import XMonad.Hooks.ManageHelpers (isDialog, isFullscreen, doCenterFloat, doFullFloat, composeOne, (-?>))
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
-- layout
import XMonad.Layout.Named
import XMonad.Layout.IM
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.Grid
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.SimplestFloat
import XMonad.Layout.NoBorders (smartBorders, noBorders)
-- prompt
import XMonad.Prompt
import XMonad.Prompt.Shell
-- keys
import XMonad.Util.EZConfig
import XMonad.Util.Run (safeSpawn, runInTerm)

modKey = mod4Mask -- Super key

-- core config
main =
    xmonad $ defaultConfig {
          borderWidth           = 2
        , normalBorderColor     = "#C3C9C9"
        , focusedBorderColor    = "#C93531"
        , terminal              = "xterm"
        , modMask               = modKey
        , workspaces            = ["1", "web" ] ++ map show [ 3 .. 8 ] ++ ["comm"]
        , layoutHook            = windowLayout
        , manageHook            = windowManagment
        , startupHook           = setWMName "LG3D" -- java compatibility
    }
    `additionalKeysP` keyBinds
    `additionalKeys`  keyMedia

-- windows layout
windowLayout = 
    --avoidStruts $
    onWorkspace "comm" im $
    smartBorders (tiled ||| Mirror tiled ||| noBorders Full ||| float)
        where
            im    = named "IM"    $ reflectHoriz $ noBorders (gridIM (1/5) (And (ClassName ("Gajim")) (Role "roster")))  -- instant messenger -- modifier (modifier [ratio] [class])
            tiled = named "Tile"  $ Tall 1 (5/100) (1/2)    -- tiling -- layout [nmaster] [delta] [ratio]
            float = named "Float" $ simplestFloat           -- float


windowManagment = composeOne . concat $
  [ [ className =? browser          -?> doShift "web"   | browser   <- browsers ],
    [ className =? im               -?> doShift "comm"  | im        <- ims      ],
    [ className =? wine             -?> doCenterFloat   | wine      <- wineproc ],
    [ isFullscreen                  -?> doFullFloat
    , isDialog                      -?> doCenterFloat
    , className =? "MPlayer"        -?> doCenterFloat
    --, className =? "Wine"           --> doCenterFloat
    , className =? "Nitrogen"       -?> doFloat
    , className =? "Gimp"           -?> doFloat

    , className =? "dzen2"          -?> doIgnore
    , resource  =? "stalonetray"    -?> doIgnore
    , resource  =? "xfce4-notifyd"  -?> doIgnore 
    , className =? "net-minecraft-MinecraftLauncher" -?> doCenterFloat

  ] ]
        where
            browsers = ["Firefox", "Midori", "Namoroka", "Chrome", "Arora"]
            ims      = ["Gajim", "Pidgin", "Kadu", "Empathy"]
            wineproc = ["Wine", "explorer.exe"]

-- prompt settings
promptConfig = defaultXPConfig {
--      font              = "-*-andale mono-medium-r-*-*-10-*-*-*-*-*-iso10646-1"
      font              = "xft:Acknowledge:size=9"
    , defaultText       = ""
    , fgColor           = "#C3C9C9"
    , fgHLight          = "#C5292D"
    , bgColor           = "#343434"
    , bgHLight          = "#343434"
    , promptBorderWidth = 0
    , position          = Top
    , height            = 12
    , historySize       = 16
    }

-- key bindings
-- emacs-like notation
keyBinds = [
      ("M-<F1>" , safeSpawn "sudo"      ["pm-suspend"])                         -- equivalent of Fn-<F1> (for external keyboard)
    , ("M-q"    , safeSpawn "xmonad"    ["--restart"])                          -- rebuild and restart xmonad
    , ("M-f"    , safeSpawn "firefox"   [])
    , ("M-S-e"  , safeSpawn "gvim"      ["/home/dweller/.xmonad/xmonad.hs"])    -- edit config

    , ("M-p"    , shellPrompt promptConfig)                     -- start apps

    , ("M-e"    , runInTerm "" "mc $HOME")

    -- xmonad specific
    --
    , ("M-S-=", sendMessage Expand)
    , ("M-S--", sendMessage Shrink)
    , ("M-b"  , sendMessage ToggleStruts)

    -- notebook specific
    --
    , ("M-S-<Up>"   , safeSpawn "xbacklight" ["-inc", "10"])
    , ("M-S-<Down>" , safeSpawn "xbacklight" ["-dec", "10"])

    ]

-- xmonad notation
keyMedia = [
      ((0      , 0x1008ff11), spawn "amixer -q set Master 1%-")    -- XF86AudioLowerVolume
    , ((0      , 0x1008ff13), spawn "amixer -q set Master 1%+")    -- XF86AudioRaiseVolume
    , ((0      , 0x1008ff12), spawn "amixer -q set Master toggle") -- XF86AudioMute
    , ((modKey , 0x1008ff11), spawn "mpc prev")                    -- mpd Prev (binded to modKey-XF86AudioLowerVolume)
    , ((modKey , 0x1008ff13), spawn "mpc next")                    -- mpd Next (binded to modKey-XF86AudioRaiseVolume)
    , ((modKey , 0x1008ff12), spawn "mpc toggle")                  -- mpd Play/Pause (XF86AudioMute)
    , ((0      , 0x1008ff14), spawn "mpc toggle")                  -- mpd Play/Pause (XF86AudioPlay)
    ]
