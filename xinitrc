# Keyboard remappings
# Built-in keyboard
#setxkbmap -option "altwin:swap_lalt_lwin" -option "ctrl:nocaps" &
# Apple keyboard
setxkbmap -option "ctrl:nocaps"
xmodmap -e "keycode 49 = less greater less greater bar brokenbar"
xmodmap -e "keycode 37 = "

# If an external monitor is connected, make it the default display
xrandr | grep VGA1 | grep " connected "
if [ $? -eq 0 ]; then
    xrandr --output LVDS1 --off --output VGA1 --mode 1920x1080
fi

# Bootstrap Awesome
exec awesome
