#!/bin/sh

ensure_link() {
    test -L "$2" || ln -srv "$1" "$2"
}

configure_vim() {
    echo "vim configuration"
    ensure_link "vimrc" "$HOME/.vimrc"

    if [ ! -d "$HOME/.vim/bundle/vundle" ]; then
        echo "Vundle installation"
        git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
    fi
    ensure_link "vundles.vim" "$HOME/.vim/vundles.vim"
}

configure_wmii() {
    echo "wmii configuration"
    if [ ! -d "$HOME/.wmii" ]; then
        echo "copy default wmiirc file"
        mkdir -v "$HOME/.wmii"
        cp -v "/etc/wmii/wmiirc" "$HOME/.wmii"
    fi
    ensure_link "wmiirc_local" "$HOME/.wmii/wmiirc_local"
}

configure_zsh() {
    echo "zsh configuration"
    ensure_link "zshrc" "$HOME/.zshrc"

    if [ ! -d "$HOME/.oh-my-zsh" ]; then
        echo "oh-my-zsh installation"
        git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
    fi
}

configure_X() {
    echo "X configuration"
    ensure_link "Xdefaults" "$HOME/.Xdefaults"
}

configure_vim
echo 
configure_wmii
echo
configure_zsh
echo
configure_X
