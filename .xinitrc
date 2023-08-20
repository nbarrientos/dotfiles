#!/bin/bash

# Xscreensaver
xscreensaver -nosplash &

xrdb ~/.Xresources

autorandr -l $(cat /etc/hostname) --force

if /usr/bin/lsusb | grep -q Kinesis; then
  xkbcomp -I/home/nacho/.xkb -w 0 /home/nacho/.xkb/keymap/kinesis $DISPLAY
fi

exec emacs
