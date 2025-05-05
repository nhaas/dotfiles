#!/bin/bash -ex
# Inspired by: https://ryanfleck.ca/2025/compiling-emacs-30/

sudo apt update

pushd ${WORKSPACE}/nhaas

if [ -d emacs-mirror ]; then
    cd emacs-mirror
else
    mkdir -p emacs-mirror
    cd emacs-mirror
    git init
    git remote add origin https://github.com/emacs-mirror/emacs.git
fi
git fetch --depth 1 origin emacs-30.1
git reset --hard FETCH_HEAD

# install dependencies before building
# sudo apt install autoconf make gcc texinfo libgtk-3-dev libxpm-dev libjpeg-dev libgif-dev libtiff5-dev libgnutls28-dev libncurses5-dev libjansson-dev libharfbuzz-dev libharfbuzz-bin libtree-sitter0 libtree-sitter-dev

# Following taken from https://practical.li/blog/posts/build-emacs-28-on-ubuntu/
# Had to switch gccjit-10 to gccjit-9 so that it matches gcc version on this machine
sudo apt install libgccjit0 libgccjit-11-dev # libjansson4 libjansson-dev gnutls-bin

./autogen.sh

./configure --with-native-compilation=aot \
            --with-tree-sitter \
            --with-modules \
            --with-threads

# Make single-threaded to avoid races
make bootstrap

# Install into the default area WITHOUT su privilege
sudo make install

