#!/bin/bash

cd $(dirname $0)

ln -s ./profile $HOME/.profile
ln -s ./bashrc $HOME/.bashrc
ln -s ./gitconfig $HOME/.gitconfig
ln -s ./emacs $HOME/.emacs
