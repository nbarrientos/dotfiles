#!/bin/bash

# Disable Touchpad
xinput set-int-prop "SynPS/2 Synaptics TouchPad" "Synaptics Off" 8 1

xrdb ~/.Xresources

xrandr | grep "VGA1 connected" -q && xrandr --output VGA1 --dpi 95 --mode 1920x1080 --left-of LVDS1 --primary --output LVDS1 --off

xset -b

setxkbmap -option "ctrl:nocaps" -layout us
#xmodmap -e "keycode 9 = backslash backslash backslash"
#xmodmap -e "keycode 49 = Escape"

exec i3
