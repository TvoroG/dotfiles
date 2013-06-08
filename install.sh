#!/bin/sh

configure_vim() {
    echo "vim configuration"
    cp -v "vimrc" "$HOME/.vimrc"

    if [ ! -d "$HOME/.vim/bundle/vundle" ]; then
        echo "Vundle installation"
        git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
    fi
    cp -v "vundles.vim" "$HOME/.vim"
}

configure_wmii() {
    echo "wmii configuration"
    if [ ! -d "$HOME/.wmii" ]; then
        echo "copy default wmiirc file"
        mkdir -v "$HOME/.wmii"
        cp -v "/etc/wmii/wmiirc" "$HOME/.wmii"
    fi
    cp -v "wmiirc_local" "$HOME/.wmii"
}

configure_zsh() {
    echo "zsh configuration"
    cp -v "zshrc" "$HOME/.zshrc"

    if [ ! -d "$HOME/.oh-my-zsh" ]; then
        echo "oh-my-zsh installation"
        git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
    fi
}

configure_X() {
    echo "X configuration"
    cp -v "Xdefaults" "$HOME/.Xdefaults"
}

read -p "All files wil be overwritten. Are you sure [yn]? " is_ok
if [ "$is_ok" == "y" ]; then
    configure_vim
    echo 
    configure_wmii
    echo
    configure_zsh
    echo
    configure_X
fi
