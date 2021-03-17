#!/bin/bash

# Kinesis Advantage
setxkbmap -option -option "caps:escape" -option "compose:rctrl" -layout us
xmodmap -e "keycode 119 = Super_L"

# Xscreensaver
xscreensaver -nosplash &

#autocutsel -f
#autocutsel -f -s PRIMARY

xrdb ~/.Xresources

xrandr --output DP-0 --mode 2560x1440 -r 144 --primary &

#xbindkeys

keepassxc &

exec emacs
