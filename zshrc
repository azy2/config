#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
    source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Customize to your needs...
alias make="make -j9"

alias off="sudo shutdown -h now"
alias restart="sudo reboot"

function e {
    nohup emacsclient -c $@ &>/dev/null &
    disown
    exit
}

export EDITOR="e"

# alias ls="ls -h "

alias clear="clear && ls"

function chpwd() {
    emulate -L zsh
    clear
}

clear
