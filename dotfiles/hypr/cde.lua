-- CDE skin, Crimson palette (upstream Crimson.dp, high byte of each channel).
-- Color set 1 active frame #b24d7a
-- Color set 2 inactive frame #aeb2c3
-- Color set 3 backdrop #718ba5 (swaybg, not this file)
-- Window frames use decoration.border_bevel (outer raised, face, inner sunken).
-- The key exists only in the patched Hyprland tree. The stock compositor
-- skips it, so a reload there still draws a flat frame and does not error.

hl.config({
    general = {
        gaps_in = 3,
        gaps_out = { top = 4, right = 4, bottom = 4, left = 4 },
        border_size = 8,

        col = {
            active_border   = "rgb(b24d7a)",
            inactive_border = "rgb(aeb2c3)",
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
            font_family = "Nimbus Sans",       -- empty = misc:font_family
            font_size = 12,
            --col = {
            --    active = -1,        -- -1 = follow active border color
            --    inactive = -1,      -- -1 = follow inactive border color
            --},
            text_color          = 0xffffffff,
            text_color_inactive = -1,
            text_shadow         = true,
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

-- Stock Hyprland 0.56 has no decoration:border_bevel. Asking for it there is a config error.
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
