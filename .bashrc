#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
export TERM=rxvt-unicode

# Wrap ssh to set the window title to the hostname being SSH'ed into
# so we don't rely on having to set PROMPT_COMMAND/PS1 on the other
# end. Shamefully stolen from (thanks!):
# https://ondergetekende.nl/setting-terminal-title-to-ssh-hostname.html
function ssh {
  local saved_args="$@"
  local title=""

  if [ -z "$COMP_LINE" ] ; then
    while [[ -n "$1" && -z "$title" ]] ; do
      local arg="$1"
      shift
      if [[ "$arg" =~ ^[^-] ]] ; then
        title="${arg%%.*}"
      fi
    done
  fi

  if [ -n "$title" ] ; then
    echo -ne "\033]0;$title\007"
  fi

  /usr/bin/ssh $saved_args
  local status=$?

  return $status
}
