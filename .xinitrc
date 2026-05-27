#!/bin/bash

xinitdir="/etc/X11/xinit"

# Xscreensaver
xscreensaver -nosplash &

xrdb ~/.Xresources

autorandr -l $(cat /etc/hostname) --force

if /usr/bin/lsusb | grep -q Kinesis; then
  xkbcomp -I$HOME/.xkb -w 0 ~/.xkb/keymap/kinesis $DISPLAY
fi

if [ -d "$xinitdir"/xinitrc.d ] ; then
	for f in "$xinitdir/xinitrc.d"/?*.sh ; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

exec emacs
