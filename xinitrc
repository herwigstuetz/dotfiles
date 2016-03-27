#!/usr/bin/env bash

emacs --daemon
compton --config ~/.compton.conf &
urxvt &
exec xmonad
