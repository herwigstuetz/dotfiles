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
mv $HOME/.xmonad ./$BACKUP/xmonad
mv $HOME/.xmobarrc ./$BACKUP/xmobarrc
mv $HOME/.xinitrc ./$BACKUP/xinitrc
mv $HOME/.xsession ./$BACKUP/xsession
mv $HOME/.compton.conf ./$BACKUP/compton.conf

# install dotfiles
ln -rs ./profile $HOME/.profile
ln -rs ./bashrc $HOME/.bashrc
ln -rs ./gitconfig $HOME/.gitconfig
ln -rs ./emacs $HOME/.emacs
ln -rs ./Xdefaults $HOME/.Xdefaults
ln -rs ./xmonad $HOME/.xmonad
ln -rs ./xmobarrc $HOME/.xmobarrc
ln -rs ./xinitrc $HOME/.xinitrc
ln -rs ./xsession $HOME/.xsession
ln -rs ./compton.conf $HOME/.compton.conf
