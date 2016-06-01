" General settings
set backspace=2
set nocompatible
syntax enable


" Turn off swap and backups
set nobackup
set nowb
set noswapfile


" Tabs and indentation
set expandtab " use spaces
set smarttab
set shiftwidth=2 " tab == 2 spaces
set tabstop=2
set autoindent
set smartindent

" Status line
set statusline=2

" line numbers
set relativenumber
set number

" Show matching braces
set showmatch

" Search and menu
set wildmenu
set incsearch " search while typing
set hlsearch " highlight search

" Remaps
let mapleader=","
map <F2> :FZF <CR>
map <F1> :NERDTreeToggle <CR>
map <F3> :TagbarToggle <CR>
imap jj <Esc>


" Plugins
execute pathogen#infect()
filetype plugin indent on

" Fuzzy Finder
set rtp+=~/.fzf


"Colorscheme
"set t_Co=256
set background=dark
"colorscheme solarized
colorscheme murphy
