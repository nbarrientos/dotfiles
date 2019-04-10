#!/bin/sh
# Dumps the fullname and e-mail addr of CERN users and e-groups in a way that
# can be consumed by Mutt's query_command.
#
# Inspired by https://github.com/svend/home-bin/blob/master/mutt-uw-search
# and https://github.com/simonjbeaumont/.dotfiles/tree/master/mutt
#
# This can then be used by Mutt in the following way:
#  set query_command = "echo; grep -i '%s' ~/.mutt/xldap.db"
#
# Execute from time to time in a cronjob:
#  pull-ldap.sh > ../.mutt/xldap.db

host=xldap.cern.ch

print_results()
{
  mail=
  name=

  while read s; do
    case "$s" in
    dn:*)
      # New entry
      if [ -n "$mail" -a -n "$name" ]; then
        echo -e "$mail\t$name"
      fi

      # Clear all variables
      mail=
      name=
      ;;
    mail:*)
      mail=${s#mail:[   ]*}
      ;;
    displayName:*)
      name=${s#displayName:[   ]*}
      ;;
    esac
  done

  # Catch last entry
  if [ -n "$mail" -a -n "$name" ]; then
    echo -e "$mail\t$name"
  fi
}

ldapsearch -LLL -h $host -x -b "OU=Users,OU=Organic Units,DC=cern,DC=ch" -E pr=500/noprompt "cernAccountType=Primary" mail displayname | print_results | sort | uniq
ldapsearch -LLL -h $host -x -b "OU=e-groups,OU=Workgroups,DC=cern,DC=ch" -E pr=500/noprompt mail displayname | print_results | sort | uniq
