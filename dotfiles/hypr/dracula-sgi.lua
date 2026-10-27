-- Dracula-SGI skin: SGI 4Dwm frame geometry with the Dracula palette.
-- Active frame #bd93f9, inactive #6272a4, desktop #282a36.
-- The frame bevel is generated from the border colour, so the six-step
-- ramp and the 1px black edges need no special shader work.
--
-- Companion files: waybar/config.dracula-sgi, waybar/style.dracula-sgi.css,
-- rofi/dracula-sgi.rasi, dunst/dunstrc.dracula-sgi,
-- hyprlock.dracula-sgi.conf, kitty/dracula-sgi.conf.

hl.config({
    general = {
        gaps_in = 3,
        gaps_out = { top = 4, right = 4, bottom = 4, left = 4 },
        border_size = 8,

        col = {
            active_border   = "rgb(bd93f9)",
            inactive_border = "rgb(6272a4)",
        },

        resize_on_border = true,
        extend_border_grab_area = 15,
        hover_icon_on_border = true,
        allow_tearing = false,
        layout = "dwindle",
    },

    decoration = {
        rounding = 0,

        active_opacity = 1.0,
        inactive_opacity = 1.0,

        shadow = {
            enabled = false,
        },

        blur = {
            enabled = false,
        },

        dim_inactive = false,

        border_bevel = true,
        titlebar = {
            enabled = true,
            height = 24,
            font_family = "Nimbus Sans",
            font_size = 12,
            col = {
                active = -1,        -- follow the active border colour
                inactive = -1,      -- follow the inactive border colour
            },
            text_color          = 0xff282a36,
            text_color_inactive = 0xfff8f8f2,
            text_shadow         = false,
            close_button        = true,
        },
    },

    misc = {
        disable_hyprland_logo = true,
        disable_splash_rendering = true,
    },
})

hl.curve("linear", { type = "bezier", points = { {0, 0}, {1, 1} } })

-- "speed" is the duration in deciseconds. 6 is 0.6s, ten times shorter than 60.
hl.animation({ leaf = "global",        enabled = true,  speed = 6, bezier = "linear" })
hl.animation({ leaf = "border",        enabled = false })
hl.animation({ leaf = "windows",       enabled = true,  speed = 6, bezier = "linear" })
hl.animation({ leaf = "windowsIn",     enabled = true,  speed = 6, bezier = "linear" })
hl.animation({ leaf = "windowsOut",    enabled = true,  speed = 6, bezier = "linear" })
hl.animation({ leaf = "fadeIn",        enabled = true,  speed = 6, bezier = "linear" })
hl.animation({ leaf = "fadeOut",       enabled = true,  speed = 6, bezier = "linear" })
hl.animation({ leaf = "fade",          enabled = true,  speed = 6, bezier = "linear" })
hl.animation({ leaf = "layers",        enabled = true,  speed = 6, bezier = "linear" })
hl.animation({ leaf = "layersIn",      enabled = true,  speed = 6, bezier = "linear", style = "fade" })
hl.animation({ leaf = "layersOut",     enabled = true,  speed = 6, bezier = "linear", style = "fade" })
hl.animation({ leaf = "fadeLayersIn",  enabled = true,  speed = 6, bezier = "linear" })
hl.animation({ leaf = "fadeLayersOut", enabled = true,  speed = 6, bezier = "linear" })
hl.animation({ leaf = "workspaces",    enabled = true,  speed = 6, bezier = "linear", style = "fade" })
hl.animation({ leaf = "workspacesIn",  enabled = true,  speed = 6, bezier = "linear", style = "fade" })
hl.animation({ leaf = "workspacesOut", enabled = true,  speed = 6, bezier = "linear", style = "fade" })
hl.animation({ leaf = "zoomFactor",    enabled = true,  speed = 6, bezier = "linear" })

-- Stock Hyprland has no decoration:border_bevel. Asking for it there is a config error.
if hl.get_config then
    local _, bevelErr = hl.get_config("decoration:border_bevel")
    if bevelErr == nil then
        hl.config({
            decoration = {
                border_bevel = true,
            },
        })
    end
end
