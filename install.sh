#!/bin/bash

cd $(dirname $0)

ln -rs ./profile $HOME/.profile
ln -rs ./bashrc $HOME/.bashrc
ln -rs ./gitconfig $HOME/.gitconfig
ln -rs ./emacs $HOME/.emacs
