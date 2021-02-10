# For example, PICSciE Perseus has system git v1.8.3.1 (2013-06-10)
# Replace with user-installed version 2.28.0 (2020-07-26)
version_str=2.28.0

cd $HOME
# Using GitHub mirror of kernel.org main repository:
wget https://github.com/git/git/archive/v${version_str}.tar.gz
tar xvf v${version_str}.tar.gz
cd git-${version_str}/
make configure
./configure --prefix=$HOME/mygit
make
make install
cd ..
rm v${version_str}.tar.gz
