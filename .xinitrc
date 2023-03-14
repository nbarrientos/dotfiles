#!/bin/bash

# Xscreensaver
xscreensaver -nosplash &

xrdb ~/.Xresources

autorandr -l $(cat /etc/hostname) --force

exec emacs
