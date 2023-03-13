#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
export TERM=emacs
