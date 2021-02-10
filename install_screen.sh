#!/usr/bin/env bash
# Needed to replace weird version of GNU Screen on ALCF Theta
# (Screen version 4.00.04devel (GNU) 8-Jun-1)
# which was not correctly interfacing with Emacs scrolling
# Compare to (Screen version 4.01.00devel (GNU) 2-May-06) on Traverse, Tiger, JLSE, etc.
version_str=4.8.0

cd $HOME
wget https://ftp.gnu.org/gnu/screen/screen-${version_str}.tar.gz
tar xvf screen-${version_str}.tar.gz
cd screen-${version_str}/
# on ALCF Theta
module load gcc || true
./autogen.sh
# no real documentation on configuration options...
# GNU screen is slowly / poorly maintianed--- need to switch to tmux
./configure --enable-colors256 # --prefix=$HOME/myscreen (it seems to ignore this)
make
cp screen $HOME/bin
cd ../
rm screen-${version_str}.tar.gz
