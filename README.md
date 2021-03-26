# Deployment

Clone the dotfiles, create the necessary directories so some trees are
unfolded and
[Stow](https://www.gnu.org/software/stow/manual/stow.html) it!

``` 1c-enterprise
$ git clone git@github.com:nbarrientos/dotfiles.git ~/.dotfiles
$ mkdir -p ~/.config/systemd/user
$ mkdir -p ~/.emacs.d ~/.emacs.d/eshell ~/.emacs.d/transient
$ mkdir -p ~/.gnupg
$ cd .dotfiles
$ stow -v .
```
