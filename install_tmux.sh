#!/usr/bin/env bash
# tmux v1.8 (2013-03-26) available on all Princeton clusters as of June 2020
# tmux not available on ALCF Theta as Fall 2019

# v3.2 recovers the behavior in https://github.com/tmux/tmux/wiki/Getting-Started
# for "C-\ s" in my config, "Tree mode splits the window into two sections:
# the top half has a tree of sessions, windows and panes and
# the bottom half has a preview of the area around the cursor in each pane. "

# Frank Willmore: try Spack-built version of Tmux
# module load spack-curated/latest
# module load utilities/tmux/2.8


# on ALCF Theta:
module load gcc || true

# libevent
cd $HOME
git clone git@github.com:libevent/libevent.git
cd libevent
sh autogen.sh
./configure --prefix=$HOME
make
make install
# make verify


# tmux
cd $HOME
git clone git@github.com:tmux/tmux.git
cd tmux
sh autogen.sh
# export LIBEVENT_CFLAGS=-I${HOME}/include
# export LIBEVENT_LIBS="-L${HOME}/lib -levent"
# ./configure --prefix=$HOME/mytmux LDFLAGS='-L$HOME/lib' CPPFLAGS='-I$HOME/include'

# Only solution that circumvents "libevent not found" error relies on PKG_CONFIG_PATH
# https://gist.github.com/mbreese/b0630195e57874c87ef3611d059d1bc2
PKG_CONFIG_PATH=$HOME/lib/pkgconfig ./configure --prefix=$HOME
make
make install
# cp tmux $HOME/bin


# install terminfo entries from ncurses library
cd $HOME
curl -LO https://invisible-island.net/datafiles/current/terminfo.src.gz && gunzip terminfo.src.gz
/usr/bin/tic -xe tmux,tmux-256color terminfo.src
