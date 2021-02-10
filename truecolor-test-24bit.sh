#!/bin/bash
# Based on: https://gist.github.com/XVilka/8346728
# Check terminal emulator for 24-bit (True Color) support
# = 8-bits per channel (R,G,B, and possibly alpha channel)
# = 16,777,216 color variants

# Termianl emulators might support any number of colors
# 2, 8, 16, 88 and 256 are common values
# See ANSI/VT100 Terminal Control Escape Sequences
# VT100 was one of the first terminals to support ANSI escape codes for cursor control
# and other tasks, and added a number of extended codes for special features like
# controlling the status lights on the keyboard
# https://stackoverflow.com/questions/4842424/list-of-ansi-color-escape-sequences

# 8 colors + "default" = actually 9
# 16 colors (incl. bright variants) + "default" and "bright default" = actually 18
# 256 color mode colors + 18 of previous colors = actuall 274
# https://www.lukeshu.com/blog/term-colors.html

# Compare to HDR 10-bit color depth:
# = 1,073,741,824 colors

# Compare to Dolby Vision 12-bit color depth:
# = 68,719,476,736 colors


# iTerm2 has True Color support, even though my Preferences set
# environment varialbe TERM=xterm-256color

# https://www.iterm2.com/features.html
# "In version 3, 24-bit color is supported."
# https://www.iterm2.com/version3.html
# https://www.iterm2.com/3.0/documentation-one-page.html#documentation-highlights.html

# Emacs 26.1 can support True Color inside Terminal Mode (also known as Console/-nw mode)
# https://www.gnu.org/software/emacs/manual/html_node/efaq/Colors-on-a-TTY.html
# "Currently there's no standard way to determine whether a terminal supports direct color mode. "
# TERM=xterm-24bit emacs -nw


# tmux can be configured with True Color
# https://tomlankhorst.nl/iterm-tmux-vim-true-color/

awk -v term_cols="${width:-$(tput cols || echo 80)}" 'BEGIN{
    s="/\\";
    for (colnum = 0; colnum<term_cols; colnum++) {
        r = 255-(colnum*255/term_cols);
        g = (colnum*510/term_cols);
        b = (colnum*255/term_cols);
        if (g>255) g = 510-g;
        printf "\033[48;2;%d;%d;%dm", r,g,b;
        printf "\033[38;2;%d;%d;%dm", 255-r,255-g,255-b;
        printf "%s\033[0m", substr(s,colnum%2+1,1);
    }
    printf "\n";
}'


# HDR images?
# https://netflixtechblog.com/enhancing-the-netflix-ui-experience-with-hdr-1e7506ad3e8


# https://github.com/syl20bnr/spacemacs/wiki/Terminal
# https://emacs.stackexchange.com/questions/45093/set-24-bit-colors-terminal-truecolor-for-solarized-theme-on-mac/45162
# /usr/bin/tic -x -o ~/.terminfo xterm-24bit.terminfo

# since on my machine,
# (base) ➜  dotfiles_work git:(master) ✗ which tic
# /usr/local/anaconda3/bin/tic

# TERM=xterm-24bit emacs -nw
# M-x list-colors-display
# shows 556 colors, more than the 256 colors shown in a normal console Emacs session
# But the Solarized theme is messed up.

# See https://www.reddit.com/r/emacs/comments/9vqhsz/true_24bit_color_in_terminal_emacs/
