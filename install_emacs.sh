# PICSciE Tiger has system GNU Emacs 23, Della has v24.3.1
# Download and build autoconf 2.69 (2012-04-24), required to build Emacs 25.3

set -e
cd $HOME
# wget https://ftp.gnu.org/gnu/autoconf/autoconf-2.69.tar.gz
# # could download autoconf-latest.tar.gz, instead
# tar xvf autoconf-2.69.tar.gz
# cd autoconf-2.69
# ./configure --prefix=$HOME
# make
# make install
# cd ..

sudo apt-get install cmake libtool libtool-bin libncurses-dev libgnutls-dev autoconf

version_str=27.1
# Download and install GNU Emacs 27.1 (2020-08-11) from source
# Originally, this script installed v25.3 (2017-09-11)
# May exceed 1 GB $HOME disk quota on Tiger, etc. PICSciE systems
wget http://ftp.gnu.org/gnu/emacs/emacs-${version_str}.tar.gz
tar xvf emacs-${version_str}.tar.gz
cd emacs-${version_str}
./autogen.sh
./configure --prefix=$HOME/myemacs --with-x-toolkit=no \
            --with-gif=no --with-png=no --with-jpeg=no \
	    --with-xpm=ifavailable --with-tiff=ifavailable \
            --with-gnutls=yes \
            --with-modules
make
make install
cd ..
rm emacs-${version_str}.tar.gz autoconf-2.69.tar.gz
