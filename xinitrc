#!/bin/bash

# If an external monitor is connected, make it the default
xrandr | grep VGA1 | grep " connected "
if [ $? -eq 0 ]; then
    xrandr --output LVDS1 --off --output VGA1 --mode 1920x1080
fi

# -- Keyboard remappings

if [ -f /usr/bin/lsusb ]; then
  if [ ! `lsusb | grep -q "05ac:1006"`]; then
    # Aluminum Apple keyboard
    setxkbmap -model macintosh -layout es -option "ctrl:nocaps";
    xmodmap ~/.xmodmaprc.apple;
  else
    # Built-in keyboard
    setxkbmap -option "altwin:swap_lalt_lwin" -option "ctrl:nocaps" -layout es;
  fi
fi

# Disable Touchpad
xinput set-int-prop "SynPS/2 Synaptics TouchPad" "Synaptics Off" 8 1

# Bootstrap Awesome
exec awesome
