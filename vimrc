set nocompatible	"Use Vim settings, rather then Vi settings

" General config
set visualbell	"No sounds
set autoread	"Reload files changed outside vim
set incsearch	"Do incremental searching
syntax on

" Vundle initialization
if filereadable(expand("~/.vim/vundles.vim"))
	source ~/.vim/vundles.vim
endif


" Indentation
set autoindent
set smartindent
set smarttab
set shiftwidth=4
set softtabstop=4
set tabstop=4
set expandtab

filetype plugin on
filetype indent on

" Keymaps
noremap <C-x><C-f> :FufFileWithCurrentBufferDir<ENTER>
noremap <C-x><C-b> :FufBuffer<ENTER>
