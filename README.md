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

# How to deploy

Clone the dotfiles, create the necessary directories so some trees are
unfolded and
[Stow](https://www.gnu.org/software/stow/manual/stow.html) it!

``` 1c-enterprise
$ git clone git@github.com:nbarrientos/dotfiles.git ~/.dotfiles
$ mkdir -p ~/.config/systemd/user ~/.config/gtk-3.0
$ mkdir -p ~/.emacs.d ~/.emacs.d/eshell ~/.emacs.d/transient
$ mkdir -p ~/.gnupg
$ mkdir -p ~/.local/share/applications
$ mkdir -p ~/.ssh/controlmasters
$ mkdir -p ~/venvs
$ cd .dotfiles
$ stow -v .
```
