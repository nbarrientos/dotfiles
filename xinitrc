#!/bin/bash

# Kinesis Advantage
setxkbmap -option -option "caps:escape" -option "compose:rctrl" -layout us
xmodmap -e "keycode 119 = Super_L"

# Xscreensaver
xscreensaver -nosplash &

autocutsel -f
autocutsel -f -s PRIMARY

xrdb ~/.Xresources

xrandr --output DP-1 --mode 2560x1440 --rotate normal --output HDMI-2 --mode 1920x1200 --left-of DP-1 --rotate left --primary &

xbindkeys

keepassxc &

exec i3
