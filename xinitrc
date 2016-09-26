#!/bin/bash

# Thinkpad USB keyboard
#setxkbmap -option "ctrl:nocaps" -option "altwin:swap_alt_win" -layout us
#xset m 7 1;
#xmodmap -e "keycode 9 = backslash backslash backslash"
#xmodmap -e "keycode 49 = Escape"

# Kinesis Advantage
setxkbmap -option "caps:escape" -layout us
xmodmap -e "keycode 119 = Super_L"

# Xscreensaver
xscreensaver -nosplash &

xrdb ~/.Xresources

xrandr --output HDMI-1 --mode 1920x1200 --rotate normal --output HDMI-2 --mode 1920x1200 --left-of HDMI-1 --rotate left &

exec i3
