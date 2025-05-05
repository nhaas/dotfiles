#!/bin/bash -ex
# Inspired by: https://www.reddit.com/r/emacs/comments/hkoksm/best_way_to_get_emacs_27_on_ubuntu_well_actually/

pushd ${WORKSPACE}/nhaas

mkdir -p emacs-mirror
cd emacs-mirror
# git init
# git remote add origin https://github.com/emacs-mirror/emacs.git
# git fetch --depth 1 origin emacs-28.2
# git reset --hard FETCH_HEAD
# sudo apt install autoconf make gcc texinfo libgtk-3-dev libxpm-dev libjpeg-dev libgif-dev libtiff5-dev libgnutls28-dev libncurses5-dev libjansson-dev libharfbuzz-dev libharfbuzz-bin
# 
# # Following taken from https://practical.li/blog/posts/build-emacs-28-on-ubuntu/
# # Had to switch gccjit-10 to gccjit-9 so that it matches gcc version on this machine
# sudo apt install libgccjit0 libgccjit-9-dev libjansson4 libjansson-dev gnutls-bin
# 
# ./autogen.sh

./configure --with-native-compilation

# Utilize all the cores on this machine
make -j$(nproc)

# Install into the default area WITHOUT su privilege
sudo make install

