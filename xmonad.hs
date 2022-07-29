--Import Xmonad modules
import XMonad
--Layouts
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace

--Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.CopyWindow
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.GridSelect
import XMonad.Actions.UpdatePointer

--Hooks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog

--Utils
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare
import XMonad.Util.Replace
import XMonad.Util.NamedWindows (getName)
--Other
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageHelpers

--Import other modules
import GHC.IO.Handle.Types (Handle)
import Data.Monoid
import Data.Ord
import Data.Maybe (fromJust)
import Data.List (sortBy)
import Data.Function (on)
import Control.Monad (forM_, join)
import System.Exit
import Graphics.X11.ExtraTypes
import Graphics.X11.Xinerama (getScreenInfo)
import qualified Data.Map as M
import Data.List

--Sound Settings
playBeep             = "canberra-gtk-play -i audio-volume-change"
playScreen           = "canberra-gtk-play -i camera-shutter"
playOpen             = "canberra-gtk-play -i dialog-information"

--Launchers
screenshotPath       = "$HOME/Pictures/Screenshots"

myTerminal           = "urxvt"
myTerminalOpen       = "urxvt -e "
myFileManager        = myTerminalOpen++"ranger"
myActionMenu         = myTerminalOpen++"bash-greeter"
mySettingsMenu       = myTerminalOpen++"settings-menu"
myIDE                = "emacsclient -c"
myBrowser            = "qutebrowser"
myQuickFileManager   = "rofi -show file-browser-extended -config $HOME/.xmonad/rofi/config.rasi"
myLauncher           = "rofi -show drun -config $HOME/.xmonad/rofi/config.rasi"
myTasklist           = "rofi -show window -config $HOME/.xmonad/rofi/config.rasi"
myCalculator         = "rofi -show calc -config $HOME/.xmonad/rofi/config.rasi"
myGameLauncher       = "lutris &"++playOpen
myScreenshooter      = "flameshot full -p "++screenshotPath++" & "++playScreen
myAdvancedScreenshooter = "flameshot gui &"++playScreen
myBookLibrary        = "calibre"
myNotes              = "flatpak run net.cozic.joplin_desktop"
myRss                = "/opt/fluent-reader/fluent-reader"
mySystemMonitor      = myTerminalOpen++"gotop"
myMusicPlayer        = "youtube-music &"++playOpen
--Tray (tint is hex -> int)
myTray               = "$HOME/.xmonad/scripts/toggle-tray"
--Environment
increaseTemperature  = "redshift -O 4000K"
restoreTemperature   = "redshift -x"
increaseBrightness   = "$HOME/.xmonad/dunst/dunst-brightness -inc 10"
fullBrightness       = "$HOME/.xmonad/dunst/dunst-brightness -set 100"
decreaseBrightness   = "$HOME/.xmonad/dunst/dunst-brightness -dec 10"
offBrightness        = "$HOME/.xmonad/dunst/dunst-brightness -set 0"
increaseVolume       = "$HOME/.xmonad/dunst/dunst-volume -i 5"
decreaseVolume       = "$HOME/.xmonad/dunst/dunst-volume -d 5"
muteVolume           = "$HOME/.xmonad/dunst/dunst-volume -t"
myWallpaper          = "feh --bg-scale $HOME/.wallpaper.jpg"
myRestart            = "killall xmobar & xmonad --recompile; xmonad --restart"
myLocker             = "dbus-send --type=method_call --dest=org.gnome.ScreenSaver /org/gnome/ScreenSaver org.gnome.ScreenSaver.Lock"

-- Window border
myBorderWidth = 2
myNormalBorderColor = "#282828"
myFocusedBorderColor = "#ebdbb2"

--Xmobar stdin colors
currentColor = "#ebdbb2" -- "#d65d0e"
hiddenColor = "#928374"
visibleColor = "#ebdbb2" -- "#d79921"
emptyColor = "#504945"
textColor = "#ebdbb2"
backgroundColor = "#282828"

--Screen Icons
screenIcon1 = "<box type=Bottom width=4 mb2 color=#fb4934><fc=#fb4934>\xf109</fc></box>"
screenIcon2 = "<box type=Bottom width=4 mb2 color=#b8bb26><fc=#b8bb26>\xf108</fc></box>"
screenIcon3 = "<box type=Bottom width=2 mb2 color=#fe8019><fc=#fe8019>c</fc></box>"

--Gap and bar size
gap = 0
barSize = 30



-- Mod key (Win)
myModMask            = mod4Mask

--My workspaces
myWorkspaces         = ["\xf121", "\xf0ac", "\xf304", "\xf11b", "\xf87c", "\xf086", "\xf02d", "\xf7d9", "\xf233"]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..]
clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
  where i = fromJust $ M.lookup ws myWorkspaceIndices
-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Tabbed Layout Theme
myTabConf :: Theme
myTabConf = def {
  fontName = "xft:Hack Nerd Font:size=10:antialias=true"
  , activeColor = backgroundColor
  , inactiveColor = backgroundColor
  , activeTextColor = currentColor
  , inactiveTextColor = hiddenColor
  , activeBorderColor = myFocusedBorderColor
  , inactiveBorderColor = myNormalBorderColor
}
------------------------------------------------------------------------
-- Key bindings.

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
 
  --StartCommentedKeys
  
  --
  -- Layout manipulations
  --

  -- Reset the layouts on the current workspace to default
  [  ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
  -- Next layout
  , ((modm,               xK_o ), sendMessage NextLayout)
  -- First layout
  --, ((modm,               xK_z ), sendMessage FirstLayout)
  -- Toggle Full layout
  , ((modm,               xK_f     ), sendMessage $ Toggle NBFULL)

  --
  -- Window manipulations
  --

  -- Move focus to the next window
  , ((modm,               xK_j     ), windows W.focusDown)
  -- Move focus to the previous window
  , ((modm,               xK_k     ), windows W.focusUp  )
  -- Swap the focused window with the next window
  , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
  -- Swap the focused window with the previous window
  , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
  -- Shrink the master area
  , ((modm,               xK_h     ), sendMessage Shrink)
  -- Expand the master area
  , ((modm,               xK_l     ), sendMessage Expand)
  -- Increase number of windows in the master area
  , ((modm,               xK_i     ), sendMessage (IncMasterN 1))
  -- Decrease number of windows in the master area
  , ((modm,               xK_d     ), sendMessage (IncMasterN (-1)))

  -- Push window back into tiling
  , ((modm,               xK_t     ), withFocused $ windows . W.sink)
  -- Close focused window
  , ((modm,               xK_c     ), kill)

  --
  -- Launchers
  --

  -- Terminal
  , ((modm,               xK_Return), spawn $ XMonad.terminal conf)
  -- Launcher
  , ((modm,               xK_r     ), spawn $ myLauncher)
  -- Tasks
  , ((modm,               xK_Tab   ), spawn $ myTasklist)
  -- System Tray
  , ((modm,               xK_y     ), spawn $ myTray)
  -- Browser
  , ((modm,               xK_b     ), spawn $ myBrowser)
  -- Calculator
  , ((modm,               xK_u     ), spawn $ myCalculator)
  -- File Manager
  , ((modm,               xK_v     ), spawn $ myFileManager)
  -- Screenshooter
  , ((modm,               xK_s     ), spawn $ myScreenshooter) 
  -- Aadvanced screenshooter
  , ((modm .|. shiftMask, xK_s     ), spawn $ myAdvancedScreenshooter)
  -- Games
  , ((modm,               xK_g     ), spawn $ myGameLauncher)
  --, ((modm,               xK_g     ), goToSelected def)
  -- Action menu
  --, ((modm,               xK_a     ), spawn $ myActionMenu)
  -- Settings menu
  , ((modm,               xK_w     ), spawn $ mySettingsMenu)
  -- Books
  --, ((modm,               xK_d     ), spawn $ myBookLibrary)
  -- Notes
  --, ((modm,               xK_n     ), spawn $ myNotes)
  -- Rss
  --, ((modm,               xK_e     ), spawn $ myRss)
  -- IDE
  , ((modm,               xK_e     ), spawn $ myIDE)
  -- System Monitor
  , ((modm,               xK_p     ), spawn $ mySystemMonitor)
  -- Music Player
  , ((modm,               xK_m     ), spawn $ myMusicPlayer)

  --
  -- Environment
  --

  -- Increase temperature
  , ((modm,               xK_equal), spawn $ increaseTemperature)
  -- Decrease temperature
  , ((modm,               xK_minus), spawn $ restoreTemperature)
  -- Increase brightness
  , ((0, xF86XK_MonBrightnessUp   ), spawn $ increaseBrightness)
  , ((modm,                 xK_F12), spawn $ increaseBrightness)
  -- Full brightness
  , ((modm, xF86XK_MonBrightnessUp), spawn $ fullBrightness)
  , ((modm .|. shiftMask,   xK_F12), spawn $ fullBrightness)
  -- Decrease brightness
  , ((0, xF86XK_MonBrightnessDown ), spawn $ decreaseBrightness)
  , ((modm,                 xK_F11), spawn $ decreaseBrightness)
  -- Off brightness
  , ((modm, xF86XK_MonBrightnessDown), spawn $ offBrightness)
  -- Increase volume
  , ((0, xF86XK_AudioRaiseVolume  ), spawn $ increaseVolume)
  , ((modm,                  xK_F3), spawn $ increaseVolume)
  -- Decrease volume
  , ((0, xF86XK_AudioLowerVolume  ), spawn $ decreaseVolume)
  , ((modm,                  xK_F2), spawn $ decreaseVolume)
  -- Mute volume
  , ((0, xF86XK_AudioMute         ), spawn $ muteVolume)
  , ((modm,                  xK_F1), spawn $ muteVolume)

  --
  -- Global Events
  --
 
  -- Lock Session
  , ((modm              , xK_q    ), spawn $ myLocker)
  -- Quit xmonad
  , ((modm .|. shiftMask, xK_q    ), io (exitWith ExitSuccess))
  -- Restart xmonad
  , ((modm .|. shiftMask, xK_r    ), spawn $ myRestart)

  --
  -- Switching Workspaces
  --
 
  -- Cycle between workspaces
  -- Focus next workspace
  , ((modm,               xK_Right ),  nextWS)
  -- Focus previous workspace
  , ((modm,               xK_Left  ),  prevWS)
  -- Shift to next workspace
  , ((modm .|. shiftMask, xK_Right ),  shiftToNext)
  -- Shift to previous workspace
  , ((modm .|. shiftMask, xK_Left  ),  shiftToPrev)

  -- Cycle between workspaces (ATL)
  -- Focus next workspace
  , ((modm,         xK_bracketright),  nextWS)
  -- Focus Previous workspace
  , ((modm,          xK_bracketleft),  prevWS)
  -- Shift to next workspace
  , ((modm .|. shiftMask, xK_bracketright),  shiftToNext)
  -- Shift to previous workspace
  , ((modm .|. shiftMask, xK_bracketleft ),  shiftToPrev)
 
 -- Cycle between screens
 -- Focus next screen
  , ((modm,               xK_Down  ),  nextScreen)
 -- Focus previous screen
  , ((modm,               xK_Up    ),  prevScreen)

 -- Cycle between screens (ALT)
 -- Focus next screen
  , ((modm              , xK_semicolon     ),  nextScreen)
 -- Focus previous screen
  , ((modm              , xK_apostrophe    ),  prevScreen)

  --EndCommentedKeys

  ]
  ++

   -- mod-[1..9], Switch to workspace N
   -- mod-shift-[1..9], Move client to workspace N
   --
  [((m .|. modm, k), windows $ f i)
    | (i, k) <- zip (workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++
  [((m .|. modm, k), windows $ f i)
    | (i, k) <- zip (workspaces conf) [xK_KP_End, xK_KP_Down, xK_KP_Next, xK_KP_Left, xK_KP_Begin, xK_KP_Right, xK_KP_Home, xK_KP_Up, xK_KP_Prior]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
-- [((m .|. modm, k), windows $ onCurrentScreen f i)
--    | (i, k) <- zip (workspaces' conf) [xK_KP_1 .. xK_KP_9]
--    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

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

-- Force Tiling
doSink :: ManageHook
doSink = ask >>= doF . W.sink


-- Layouts:
myLayout =
  mkToggle (NBFULL ?? EOT)
  $ smartBorders
  $ gaps [(U,barSize)]
  $ onWorkspace (myWorkspaces !! 0) tallFirst
  $ onWorkspace (myWorkspaces !! 1) tallFirst
  $ onWorkspace (myWorkspaces !! 2) tabbedFirst
  $ onWorkspace (myWorkspaces !! 3) tabbedFirst
  $ onWorkspace (myWorkspaces !! 4) wideFirst
  $ onWorkspace (myWorkspaces !! 5) tabbedFirst
  $ onWorkspace (myWorkspaces !! 6) tallFirst
  $ onWorkspace (myWorkspaces !! 7) tabbedFirst
  $ onWorkspace (myWorkspaces !! 8) tabbedFirst
  $ tallFirst
          where
           spacing = spacingRaw False (Border gap gap gap gap) True (Border gap gap gap gap) True
           tall_big = spacing $ Tall nmaster delta master_ratio
           nmaster = 1
           master_ratio = 3/4
           even_ratio = 1/2
           delta = 1/10

           tabbedLayout = renamed [Replace "\xf488"] $ spacing $ tabbedBottom shrinkText myTabConf
           tall = renamed [Replace "\xf5cb"] $ spacing $ Tall nmaster delta even_ratio
           wide =  renamed [Replace "\xf5d0"] $ spacing $ Mirror tall_big
           bottomWide = renamed [Replace "\xf5c7"] $ spacing $ reflectVert wide
           --grid = renamed [Replace "\xf5c6"] $ spacing $ Grid
           --leftTall = renamed [Replace "T"] $ spacing $ reflectHoriz tall

           tallFirst = ( tall ||| tabbedLayout ||| wide ||| bottomWide)
           tabbedFirst = ( tabbedLayout ||| tall ||| wide ||| bottomWide)
           wideFirst = ( wide ||| tall ||| tabbedLayout ||| bottomWide)



------------------------------------------------------------------------
-- Window rules:
myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = manageDocks <+> (isFullscreen --> doFullFloat) <+> composeAll
    [ className =? "MPlayer"         --> doFloat
    , className =? "ulauncher"       --> doIgnore
    , className =? "ueberzug"        --> doIgnore

    , className =? "Atom"            --> doShift ( myWorkspaces !! 0 )
    --, className =? "Emacs"           --> doShift ( myWorkspaces !! 0 )
    , className =? "Code"            --> doShift ( myWorkspaces !! 0 )
    , className =? "code-oss"            --> doShift ( myWorkspaces !! 0 )

    --, className =? "qutebrowser"    --> doShift "0_2\xfa9e"
    , className =? "firefox"         --> doShift ( myWorkspaces !! 1 )
    , className =? "Chromium"        --> doShift ( myWorkspaces !! 1 )
    , className =? "Chromium-browser"--> doShift ( myWorkspaces !! 1 )

    , className =? "Gimp-2.10"       --> doShift ( myWorkspaces !! 2 )
    , className =? "Gimp"            --> doShift ( myWorkspaces !! 2 )
    , className =? "Darktable"       --> doShift ( myWorkspaces !! 2 )
    , className =? "krita"           --> doShift ( myWorkspaces !! 2 )
    , className =? "Aseprite"        --> doShift ( myWorkspaces !! 2 )
    , className =? "Inkscape"        --> doShift ( myWorkspaces !! 2 )
    , className =? "kdenlive"        --> doShift ( myWorkspaces !! 2 )
    , className =? "Cinelerra"       --> doShift ( myWorkspaces !! 2 )
    , className =? "Blender"         --> doShift ( myWorkspaces !! 2 )
    , className =? "Audacity"        --> doShift ( myWorkspaces !! 2 )

    , className =? "Godot"           --> doShift ( myWorkspaces !! 3 )
    , className =? "UnityHub"        --> doShift ( myWorkspaces !! 3 )
    , title     =? "GameHub"         --> doShift ( myWorkspaces !! 3 )
    , className =? "Lutris"           --> doShift ( myWorkspaces !! 3 )
    , className =? "Steam"           --> doShift ( myWorkspaces !! 3 )
    , className =? "itch"            --> doShift ( myWorkspaces !! 3 )
    , className =? "com.gitlab.librebob.Athenaeum"  --> doShift ( myWorkspaces !! 3 )

    , className =? "mpv"             --> doShift ( myWorkspaces !! 4 )
    , className =? "Stremio"             --> doShift ( myWorkspaces !! 4 )
    , className =? "FreeTube"        --> doShift ( myWorkspaces !! 4 )
    , className =? "YouTube Music"   --> doShift ( myWorkspaces !! 4 )
    , className =? "fluent-reader"   --> doShift ( myWorkspaces !! 4 )

    , title     =? "WeChat"          --> doShift ( myWorkspaces !! 5 )
    , className =? "discord"         --> doShift ( myWorkspaces !! 5 )
    , className =? "TelegramDesktop" --> doShift ( myWorkspaces !! 5 )

    , className =? "calibre"         --> doShift ( myWorkspaces !! 6 )
    , className =? "Wps"             --> doShift ( myWorkspaces !! 6 )
    , className =? "Et"              --> doShift ( myWorkspaces !! 6 )
    , className =? "Wpp"             --> doShift ( myWorkspaces !! 6 )
    , className =? "Evince"          --> doShift ( myWorkspaces !! 6 )
    , className =? "Komikku"         --> doShift ( myWorkspaces !! 6 )

    , className =? "obs"             --> doShift ( myWorkspaces !! 7 )

    , className =? "Uget-gtk"        --> doShift ( myWorkspaces !! 7 )

    , className =? "Virt-manager"    --> doShift ( myWorkspaces !! 8 )
    , className =? "Vmplayer"        --> doShift ( myWorkspaces !! 8 )
    , className =? "org.remmina.Remmina" --> doShift ( myWorkspaces !! 8 )

    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , resource  =? "Jitsi Meet"     --> doIgnore
    ] -- <+> manageDocks -- <+> (isFullscreen --> doFullFloat)


------------------------------------------------------------------------
-- Event handling

myEventHook = mempty

------------------------------------------------------------------------
-- startup hook

myStartupHook :: X ()
myStartupHook = do
              spawn myWallpaper

-------------------------------------------------
-- Xmobar integration
-------------------------------------------------
physicalScreens :: X [Maybe ScreenId]
physicalScreens = withWindowSet $ \windowSet -> do
    let numScreens = length $ W.screens windowSet
    mapM (\s -> getScreen def (P s)) [0..numScreens]

getPhysicalScreen :: ScreenId -> X (Maybe PhysicalScreen)
getPhysicalScreen sid = do
    pscreens <- physicalScreens
    return $ (Just sid) `elemIndex` pscreens >>= \s -> Just (P s)

getScreenIcon :: String -> String
getScreenIcon "0" = screenIcon1++" "
getScreenIcon "1" = screenIcon2++" "
getScreenIcon "2" = screenIcon3++" "

getActiveScreen :: X String
getActiveScreen = withWindowSet $ \windowSet -> do
    let sid = W.screen (W.current windowSet)
    pscreen <- getPhysicalScreen sid
    let screenid = case pscreen of
                Just (P s) -> show s
    return $ getScreenIcon screenid


screenCount :: X Int
screenCount = withDisplay (io.fmap length.getScreenInfo)
-------------------------------------------------
-- Running
-------------------------------------------------

myLogHook :: X ()
myLogHook = do
              spawn myWallpaper



main = do
 --replace
 n <- countScreens
 xmprocs <- mapM (\i -> spawnPipe $ "xmobar -x " ++ show i ++ " $HOME/.xmonad/xmobarrc") [0..n-1] -- ++ show i ++ " -x " ++ show i) [0..n-1]
 -- xmprocs <- mapM (\i -> spawnPipe $ "xmobar $HOME/.xmonad/xmobar/0" ++ " -x " ++ show i) [0..n-1] -- ++ show i ++ " -x " ++ show i) [0..n-1]
 -- xmprocs  <- mapM (\i -> spawnPipe $ "xmobar -x " ++ show i ++ " $HOME/.xmonad/xmobar/0")  [0..1]
 -- xmproc0 <- spawnPipe "xmobar -x 0 $HOME/.xmonad/xmobar/0"
 -- xmproc1 <- spawnPipe "xmobar -x 1 $HOME/.xmonad/xmobar/1"

 xmonad $  ewmh $ def
  {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts, PPs
        logHook            =  mapM_ (\handle -> dynamicLogWithPP $ xmobarPP{
          ppOutput = hPutStrLn handle
        , ppCurrent = xmobarColor currentColor "" . wrap "<box type=Bottom width=4 mb2 color=#ebdbb2>" "</box>"
        , ppHidden = xmobarColor  hiddenColor "" . wrap "" "" . clickable
        , ppHiddenNoWindows = xmobarColor emptyColor "" . wrap "" "" . clickable
        , ppVisible = xmobarColor visibleColor "" . wrap "" "" . clickable
        , ppWsSep = " "
        , ppSep = " "
        , ppExtras = [Just <$> getActiveScreen]
        , ppTitle = xmobarColor textColor "" . shorten 20
        , ppOrder = \(ws:_:t:ex) -> [pad ws]++ex++[t]
        }) xmprocs >> updatePointer (0.5, 0.5) (0, 0),
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook, -- <+> EWMH.fullscreenEventHook,
        startupHook        = myStartupHook
    }
