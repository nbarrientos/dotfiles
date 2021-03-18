#!/bin/bash

# Xscreensaver
xscreensaver -nosplash &

xrdb ~/.Xresources

xrandr --output DP-0 --mode 2560x1440 -r 144 --primary &

#xbindkeys

keepassxc &

pasystray &

exec emacs
