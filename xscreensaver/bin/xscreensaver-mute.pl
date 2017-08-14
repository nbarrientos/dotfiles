#!/usr/bin/env perl
#
# Taken literally from xscreensaver-command(1)

my $blanked = 0;
open (IN, "xscreensaver-command -watch |");
while (<IN>) {
  if (m/^(BLANK|LOCK)/) {
    if (!$blanked) {
      system "amixer set Master toggle mute";
      $blanked = 1;
    }
  } elsif (m/^UNBLANK/) {
      system "amixer set Master toggle unmute";
      $blanked = 0;
  }
}
