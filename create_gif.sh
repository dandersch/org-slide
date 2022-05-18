#!/usr/bin/bash

# ffmpeg screencapture
ffmpeg -video_size 640x400 -framerate 25 -f x11grab -i :0.0+960,10 out.webm

# convert to gif
# ffmpeg -ss 30 -t 3 -i input.mp4 -vf "fps=10,scale=320:-1:flags=lanczos,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 output.gif
ffmpeg -i out.webm -loop 0 -r 5 -vf "fps=10,scale=640:-1:flags=lanczos,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -s 640x400 example.gif

# optional: smaller gif filesize
# convert example.gif -fuzz 8% -layers Optimize example.gif

# clean up
rm out.webm
