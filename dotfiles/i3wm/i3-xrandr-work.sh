#!/bin/sh

xrandr --output DP-2 --auto --left-of DP-1  --rotate left
xrandr --output DP-3 --auto --right-of DP-1 --rotate left

# position middle screen at 600 pixels down, compared to others screens.
xrandr --output DP-1 --panning 2560x1440+1440+600
