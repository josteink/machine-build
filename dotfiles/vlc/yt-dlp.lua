-- yt-dlp.lua — use yt-dlp to resolve YouTube URLs in VLC

-- place in home-folder
--
-- Linux: ~/.local/share/vlc/lua/playlist/
-- MacOS: ~/Library/Application Support/org.videolan.vlc/lua/playlist/

function probe()
    return (vlc.access == "http" or vlc.access == "https") and
           string.match(vlc.path, "youtube%.com/watch") or
           string.match(vlc.path, "youtu%.be/")
end

function parse()
    local url = vlc.path
    local command = 'yt-dlp -f best -g "' .. url .. '"'
    local handle = io.popen(command)
    if not handle then
        vlc.msg.err("Failed to run yt-dlp")
        return nil
    end

    local stream_url = handle:read("*l")
    handle:close()

    if not stream_url or stream_url == "" then
        vlc.msg.err("yt-dlp did not return a URL")
        return nil
    end

    return { { path = stream_url } }
end
