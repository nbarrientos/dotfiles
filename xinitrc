#!/bin/bash

# Thinkpad USB keyboard
#setxkbmap -option "ctrl:nocaps" -option "altwin:swap_alt_win" -layout us
#xset m 7 1;
#xmodmap -e "keycode 9 = backslash backslash backslash"
#xmodmap -e "keycode 49 = Escape"

# Kinesis Advantage
setxkbmap -option -option "caps:escape" -option "compose:rctrl" -layout us
xmodmap -e "keycode 119 = Super_L"

# Xscreensaver
xscreensaver -nosplash &
# Mute/unmute sound after locking/unlocking
perl .dotfiles/xscreensaver/bin/xscreensaver-mute.pl &

autocutsel -f
autocutsel -f -s PRIMARY

xrdb ~/.Xresources

xrandr --output DP-1 --mode 2560x1440 --rotate normal --output HDMI-2 --mode 1920x1200 --left-of DP-1 --rotate left --primary &

xbindkeys

exec i3
