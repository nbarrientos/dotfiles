#!/bin/bash

# If an external monitor is connected, make it the default
xrandr | grep VGA1 | grep " connected "
if [ $? -eq 0 ]; then
    xrandr --output LVDS1 --off --output VGA1 --mode 1920x1080
fi

# -- Keyboard remappings

# Built-in keyboard
setxkbmap -option "altwin:swap_lalt_lwin" -option "ctrl:nocaps"

if [ -f /usr/bin/lsusb ]; then
  # Aluminum Apple keyboard
  lsusb | grep -q "05ac:1006" \
    && setxkbmap -option -model macintosh -layout es \
    && xmodmap ~/.xmodmaprc.apple
fi

# Bootstrap Awesome
exec awesome
