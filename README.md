# Xmonad dotfiles. Gruvbox theme
My xmonad, xmobar and rofi configuration

<img src="https://github.com/alex-karev/homepage/raw/main/screenshot.png">

## Features
* Using same tags on multiple screens
* Indicator showing currently focused screen
* Tabbed mode and fullscreen mode (mod+f)
* Clickable application menu using rofi

## Installation
1. Install xmonad: `sudo pacman -S xmonad xmonad-contrib`
2. Install optional dependencies: `sudo pacman -S rofi redshift dunst feh urxvt pamixer xbacklight`
3. Clone this repo to your .xmonad directory
4. Compile xmonad: `xmonad --recompile`
5. Done!

## Usage
Default mod key is set to `Win`

You can run apps via rofi (mod+r)

Configure `xmonad.hs` and `xmobarrc` to change hotkeys and applications

Set wallpaper by copying it to $HOME/.wallpaper.jpg

## Contributing
You are welcome to fork this repo and to create pull requests

## License
Distributed under the MIT License. See LICENSE for more information
