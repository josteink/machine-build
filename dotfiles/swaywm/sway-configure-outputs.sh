#!/bin/bash

# DP-3 or DP-5
if [ `swaymsg -t get_outputs | grep DP-3 | wc -l` -ge 1 ]; then
    swaymsg 'set $fujitsu DP-3'
else
    swaymsg 'set $fujitsu DP-5'
fi
swaymsg 'bindsym $mod+Shift+f3 move workspace to output $fujitsu'



# DP-4 or DP-6
if [ `swaymsg -t get_outputs | grep DP-4 | wc -l` -ge 1 ]; then
    swaymsg 'set $dell DP-4'
else
    swaymsg 'set $dell DP-6'
fi
swaymsg 'bindsym $mod+Shift+f2 move workspace to output $dell'

