#!/bin/bash

cd $(dirname $0)

# backup old files
BACKUP=dotfiles.bak

mkdir $BACKUP
mv $HOME/.profile ./$BACKUP/profile
mv $HOME/.bashrc ./$BACKUP/bashrc
mv $HOME/.gitconfig ./$BACKUP/gitconfig
mv $HOME/.emacs ./$BACKUP/emacs
mv $HOME/.emacs.d ./$BACKUP/emacs.d

# install dotfiles
ln -rs ./profile $HOME/.profile
ln -rs ./bashrc $HOME/.bashrc
ln -rs ./gitconfig $HOME/.gitconfig
ln -rs ./emacs $HOME/.emacs
