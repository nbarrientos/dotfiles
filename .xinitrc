#!/bin/bash

# Xscreensaver
xscreensaver -nosplash &

xrdb ~/.Xresources

autorandr $(cat /etc/hostname)

keepassxc &

pasystray &

dunst &

exec emacs
