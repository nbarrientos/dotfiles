#!/bin/bash

setxkbmap -option "ctrl:nocaps" -option "altwin:swap_lalt_lwin" -layout es;

xmodmap -e "keycode 9 = backslash backslash backslash"
xmodmap -e "keycode 49 = Escape"

# Bootstrap Awesome
exec awesome -c .config/awesome/rc.lua
