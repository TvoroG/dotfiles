filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
Bundle 'gmarik/vundle'

" My Bundles here
Bundle 'JavaBrowser'
Bundle 'javacomplete'
Bundle 'L9'
Bundle 'FuzzyFinder'
Bundle 'VimClojure'
let g:vimclojure#HighlightBuiltins = 1
let g:vimclojure#ParenRainbow = 1

Bundle 'altercation/vim-colors-solarized'
set background=dark
colorscheme solarized

filetype plugin indent on

