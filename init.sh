#!/bin/bash
sudo pacman -S --needed - < .dotfiles/PKGLIST.native.arch
mkdir -p ~/.config/systemd/user ~/.config/gtk-3.0
mkdir -p ~/.config/keepassxc ~/.config/dunst
mkdir -p ~/.config/pipewire ~/.config/git
mkdir -p ~/.emacs.d ~/.emacs.d/eshell ~/.emacs.d/transient
mkdir -p ~/.gnupg
mkdir -p ~/.local/share/applications
mkdir -p ~/.ssh/controlmasters
mkdir -p ~/venvs
mkdir -p ~/Downloads
mkdir -p ~/mail/cern ~/mail/criptonita
pushd .dotfiles
stow -v .
popd
