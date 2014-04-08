#!/bin/sh

ensure_link() {
    test -L "$2" || ln -sfv "$(pwd)/$1" "$2"
}

configure_vim() {
    echo "vim configuration"
    ensure_link "vimrc" "$HOME/.vimrc"

    if [ ! -d "$HOME/.vim/bundle/vundle" ]; then
        echo "Vundle installation"
        git clone https://github.com/gmarik/vundle.git "$HOME/.vim/bundle/vundle"
        vim -u "$HOME/.vim/vundles.vim" +BundleInstall +qa
    fi
    ensure_link "vundles.vim" "$HOME/.vim/vundles.vim"
}

configure_zsh() {
    echo "zsh configuration"
    ensure_link "zshrc" "$HOME/.zshrc"

    if [ ! -d "$HOME/.oh-my-zsh" ]; then
        echo "oh-my-zsh installation"
        git clone git://github.com/robbyrussell/oh-my-zsh.git "$HOME/.oh-my-zsh"
    fi
}

configure_X() {
    echo "X configuration"
    ensure_link "Xdefaults" "$HOME/.Xdefaults"
}

configure_vim
echo 
configure_zsh
echo
configure_X
