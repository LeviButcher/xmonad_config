import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Magnifier
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.Loggers
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing

main :: IO ()
main = xmonad 
    . ewmh 
    . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
    $ myConfig

layoutSpacing = 5

myConfig = def {
    modMask = mod4Mask,
    terminal = "alacritty",
    normalBorderColor = "#D3D3D3",
    focusedBorderColor = "#1F51FF",
    layoutHook = spacingWithEdge layoutSpacing $ myLayout,
    manageHook = manageHook def <+> myManageHook,
    workspaces = ["info", "dev", "web", "media"] <+> (show <$> [5..9])
  }
  `additionalKeysP` 
  [
    ("M-f", spawn "firefox"),
    ("M-d", spawn "rofi -show run")
  ]

myManageHook :: ManageHook
myManageHook = composeAll 
    [
        isDialog --> doFloat,
        className =? "discord" --> doShift "media"
    ]

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = renamed [Replace "ThreeCol"] . magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100


myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " " " . green 
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    green, blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#4469e3" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
    green = xmobarColor "#44e35c" ""

