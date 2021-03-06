# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

if [ -d "/usr/local/bin" ] ; then
    PATH=/usr/local/bin:$PATH
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

# custom variables
export EDITOR=emacs

# if stack is installed add autocompletion
if command -v stack >/dev/null 2>&1; then
   eval "$(stack --bash-completion-script stack)"
fi

if [ `uname` == "Darwin" ]; then
    alias ls="ls -G"
elif [ `uname` == "Linux" ]; then
    alias ls="ls --color=auto"
fi
