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
mv $HOME/.Xdefaults ./$BACKUP/Xdefaults
mv $HOME/.Xmodmap ./$BACKUP/Xmodmap
mv $HOME/.xmonad ./$BACKUP/xmonad
mv $HOME/.xinitrc ./$BACKUP/xinitrc
mv $HOME/.xsession ./$BACKUP/xsession
mv $HOME/.compton.conf ./$BACKUP/compton.conf

# install dotfiles
ln -s `pwd`/profile $HOME/.profile
ln -s `pwd`/bashrc $HOME/.bashrc
ln -s `pwd`/gitconfig $HOME/.gitconfig
ln -s `pwd`/emacs $HOME/.emacs
ln -s `pwd`/Xdefaults $HOME/.Xdefaults
ln -s `pwd`/Xmodmap $HOME/.Xmodmap
ln -s `pwd`/xmonad $HOME/.xmonad
ln -s `pwd`/xinitrc $HOME/.xinitrc
ln -s `pwd`/xsession $HOME/.xsession
ln -s `pwd`/compton.conf $HOME/.compton.conf
