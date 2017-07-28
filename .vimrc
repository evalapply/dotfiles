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
imap <C-l> <Esc>
nmap <S-Enter> O<Esc>
nmap <CR> o<Esc>
nmap ta a<space><Esc>
nmap ti i<space><Esc>


" Plugins
filetype plugin indent on
call plug#begin('~/.vim/autoload/plugged')
Plug 'scrooloose/nerdtree'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install -all' }
Plug 'majutsushi/tagbar'
Plug 'Shougo/neocomplete'

Plug 'fatih/vim-go'
Plug 'nsf/gocode'
Plug 'elixir-lang/vim-elixir'
Plug 'udalov/kotlin-vim'
Plug 'mattn/emmet-vim'
Plug 'artur-shaik/vim-javacomplete2'
Plug 'rust-lang/rust.vim'
Plug 'racer-rust/vim-racer'
Plug 'rhysd/vim-crystal'
Plug 'python-mode/python-mode'
Plug 'dracula/vim'
call plug#end()

" NeoComplete
let g:acp_enableAtStartup = 0
" " Use neocomplete.
let g:neocomplete#enable_at_startup = 1
" " Use smartcase.
let g:neocomplete#enable_smart_case = 1
" " Set minimum syntax keyword length.
let g:neocomplete#sources#syntax#min_keyword_length = 3
" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" " <C-h>: close popup and delete backword char.
inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
" <CR>: close popup and save indent.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
  "return (pumvisible() ? "\<C-y>" : "" ) . "\<CR>"
  " For no inserting <CR> key.
  return pumvisible() ? "\<C-y>" : "\<CR>"
endfunction
" Close preview window
autocmd CompleteDone * pclose
" Do not autoselect first match
set completeopt=longest,menuone
let g:jedi#popup_select_first=0

" JavaComplete
nmap <F4> <Plug>(JavaComplete-Imports-AddSmart)
imap <F4> <Plug>(JavaComplete-Imports-AddSmart)
nmap <F5> <Plug>(JavaComplete-Imports-Add)
imap <F5> <Plug>(JavaComplete-Imports-Add)
nmap <F6> <Plug>(JavaComplete-Imports-AddMissing)
imap <F6> <Plug>(JavaComplete-Imports-AddMissing)
nmap <F7> <Plug>(JavaComplete-Imports-RemoveUnused)
imap <F7> <Plug>(JavaComplete-Imports-RemoveUnused)
let g:JavaComplete_ClosingBrace = 1

let g:go_fmt_command = "gofmt"

" OmniCompletions
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
autocmd FileType java setlocal omnifunc=javacomplete#Complete

" Fuzzy Finder
set rtp+=~/.fzf

"Colorscheme
color dracula
set t_Co=256
set background=dark

" Python
let g:pymode_python = 'python3'
let g:pymode_folding = 0
let g:pymode_rope_complete_on_dot = 0

" Golang
let g:go_fmt_command = "goimports"

" Rust Racer
let g:racer_cmd = "~/.cargo/bin/racer"

