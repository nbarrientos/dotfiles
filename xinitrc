#!/bin/bash

setxkbmap -option "ctrl:nocaps" -option "altwin:swap_lalt_lwin" -layout es;

# Bootstrap Awesome
exec awesome -c .config/awesome/rc.lua
