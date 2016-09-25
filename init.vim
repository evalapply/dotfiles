" General settings
set backspace=2
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

" Autosave when building
set autowrite

" Remaps
let mapleader=","
map <F2> :FZF <CR>
map <F1> :NERDTreeToggle <CR>
map <F3> :TagbarToggle <CR>
imap jj <Esc>
nnoremap <A-h> <c-w>h
nnoremap <A-j> <c-w>j
nnoremap <A-k> <c-w>k
nnoremap <A-l> <c-w>l
nnoremap <A-+> <c-w>+
nnoremap <A--> <c-w>-
nnoremap <A->> <c-w>>
nnoremap <A-<> <c-w><
nnoremap <A-t> :10Term<CR>
tnoremap <A-q> <C-\><C-n>:q<CR>

" Plugins
"execute pathogen#infect()
filetype plugin indent on
call plug#begin('~/.config/nvim/autoload/plug')
" General use plugins
Plug 'scrooloose/nerdtree'
Plug 'majutsushi/tagbar'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'vimlab/split-term.vim'
Plug 'SirVer/ultisnips'
Plug 'scrooloose/syntastic'
" Language specific
Plug 'fatih/vim-go'
Plug 'nsf/gocode'
Plug 'StanAngeloff/php.vim'
Plug 'rust-lang/rust.vim'
Plug 'cakebaker/scss-syntax.vim'
Plug 'jwalton512/vim-blade'
Plug 'pangloss/vim-javascript'
Plug 'digitaltoad/vim-pug'
call plug#end()

" Enable python3
let g:python3_host_prog = '/usr/bin/python3'

" UltiSnips directory
let g:UltiSnipsSnippetDirectories=["~/.config/nvim/snippets"]
" Set ultisnips triggers
let g:UltiSnipsExpandTrigger="<tab>"                                            
let g:UltiSnipsJumpForwardTrigger="<tab>"                                       
let g:UltiSnipsJumpBackwardTrigger="<s-tab>" 

" Fuzzy Finder
set rtp+=~/.fzf

" Terminal - split
set splitbelow

" Vim-Go
let g:gp_list_type = "quickfix"
au FileType go nmap <leader>r <Plug>(go-run)
au FileType go nmap <leader>b <Plug>(go-build)
au FileType go nmap <leader>t <Plug>(go-test)
au FileType go nmap <leader>c <Plug>(go-coverage)
au FileType go nmap <Leader>gv <Plug>(go-def-vertical)
map <C-n> :cnext<CR>
map <C-m> :cprevious<CR>
nnoremap <leader>a :cclose<CR>
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_fields = 1
let g:go_highlight_types = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1
let g:go_fmt_command = "goimports"

" Vim Rust
let g:rustfmt_autosave = 1
au FileType rs nmap <leader>t RustTest

" Syntastic fix conflict with vim-go
let g:syntastic_mode_map = { 'mode': 'active',
      \ 'active_filetypes': [],
      \ 'passive_filetypes': ['go'] }
let g:syntastic_go_checkers = ['golint', 'govet', 'errcheck']

"Colorscheme
set t_Co=256
colorscheme lucario
