#!/bin/bash

# Waybar lives next to this script in the dotfiles tree. HYPR_THEME=cde
# selects the Common Desktop Environment front panel, HYPR_THEME=indigo
# selects the SGI Indigo Magic panel, and HYPR_THEME=dracula-sgi selects the
# Dracula-coloured SGI panel. Anything else, including an unset variable,
# keeps the Dracula bar.

DOTFILES="$(cd "$(dirname "$(readlink -f "$0")")/.." && pwd)"

case "${HYPR_THEME:-dracula}" in
    cde)
        CONFIG="$DOTFILES/waybar/config.cde"
        STYLE="$DOTFILES/waybar/style.cde.css"
        ;;
    indigo)
        CONFIG="$DOTFILES/waybar/config.indigo"
        STYLE="$DOTFILES/waybar/style.indigo.css"
        ;;
    dracula-sgi)
        CONFIG="$DOTFILES/waybar/config.dracula-sgi"
        STYLE="$DOTFILES/waybar/style.dracula-sgi.css"
        ;;
    *)
        CONFIG="$DOTFILES/waybar/config"
        STYLE="$DOTFILES/waybar/style.css"
        ;;
esac

export HYPR_DOTFILES="$DOTFILES"
export HYPR_THEME="${HYPR_THEME:-dracula}"

CONFIG_FILES="$CONFIG $STYLE"

trap "killall waybar" EXIT

while true; do
    waybar -c "$CONFIG" -s "$STYLE" &
    inotifywait -e create,modify $CONFIG_FILES
    killall waybar
done
