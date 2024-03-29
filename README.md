# What's this?

Here you can find most of the configuration files that I use everyday
at work and when I hack in my free time. I keep some bits in private
repositories to which you may find references. This is mainly for
privacy reasons.

As you can see I mostly live inside Emacs, which apart from being a
magnificent editor, I also use as my window manager, my news reader,
my shell, my e-mail client, my calendar/planner, my web browser
(combined with Firefox) and as an interface to many non-Emacs-native
utilities like Git or Ripgrep.

Most of the keybindings declared by this configuration are aimed to be
comfortable when typing on a [Kinesis
Advantage](https://kinesis-ergo.com/products/#advantage2) keyboard.

This repository is constantly evolving and receives new patches as I
learn about new things (mostly Emacs packages). Feel free to explore
the contents and to make use of any ideas that you might find
interesting.

# Where to find this repository

  * [Sourcehut](https://git.sr.ht/~nbarrientos/dotfiles) (main)
  * [GitHub](https://github.com/nbarrientos/dotfiles) (mirror)
  * [CERN's GitLab](https://gitlab.cern.ch/ibarrien/dotfiles) (mirror)

# How to deploy

Clone the dotfiles:

```
sudo pacman -S --noconfirm git stow
git clone https://git.sr.ht/~nbarrientos/dotfiles ~/.dotfiles
```

Now create the necessary directories so some trees are unfolded and
[Stow](https://www.gnu.org/software/stow/manual/stow.html) it! or,

``` 1c-enterprise
$ bash ~/.dotfiles/init.sh
```

# Screencasts

From time to time I record screencasts showcasing some custom or
interesting Emacs configurations that I have in my dotfiles. They can
be found in my [YouTube
channel](https://www.youtube.com/channel/UCCEr9T0kxVIaNnkdLfj5C3g).
