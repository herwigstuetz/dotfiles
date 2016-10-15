#!/usr/bin/env bash

if [ -s ~/.Xmodmap ]; then
    xmodmap ~/.Xmodmap
fi

emacs --daemon
compton --config ~/.compton.conf &

urxvt &
exec xmonad
