#!/bin/bash

# Xscreensaver
xscreensaver -nosplash &

xrdb ~/.Xresources

autorandr $(cat /etc/hostname)

exec emacs
