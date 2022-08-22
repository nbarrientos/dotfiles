.PHONY: packagelists packagelist-native packagelist-foreign

packagelists: packagelist-native packagelist-foreign

packagelist-native:
	pacman -Qqne > PKGLIST.native.arch

packagelist-foreign:
	pacman -Qqme > PKGLIST.foreign.arch
