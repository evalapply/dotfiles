" jk to exit insert mode
inoremap jk <Esc>

set backspace=indent,eol,start
filetype plugin indent on

" Tabs and indentation
" ####################
set expandtab " use spaces
set smarttab
set shiftwidth=2 " <TAB> == 2 spaces
set tabstop=2
set autoindent
set smartindent

" Search
" ####################
" Search for command mode
set wildmenu
" Incremental search
set incsearch
" Highlight search
set hlsearch
" Don't care about case
set ignorecase
" Search by camelcase
set smartcase
" Fuzzy Finder
set rtp+=~/.fzf
" Auto completion settings
set completeopt=longest,menuone

" Text Help
set number
set list

" Leader
map <space> <leader>

" web assembly wabt files
autocmd BufRead,BufNewFile *.wat set ft=lisp


call plug#begin('~/.vim/plugged')
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
  Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
  Plug 'jdonaldson/vaxe'
  Plug 'w0rp/ale'
  Plug 'pangloss/vim-javascript'
  Plug 'mattn/emmet-vim'

  Plug 'metakirby5/codi.vim'
  Plug 'sheerun/vim-polyglot'

  Plug 'tpope/vim-sensible'
  Plug 'tpope/vim-dispatch'
  Plug 'tpope/vim-scriptease'
  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-commentary'
  Plug 'tpope/vim-vinegar'
  Plug 'tpope/vim-endwise'
  Plug 'tpope/vim-unimpaired'
call plug#end()

" Show git in Status, via fugitive
"set statusline+=%{FugitiveStatusline()}

"
" Aliases
"

" Tabs
map <C-t>n :tabnew<CR>
map <C-t>e :tabedit 
map <C-t>l :tabn<CR>
map <C-t>h :tabp<CR>

imap <C-t>n <ESC>:tabnew<CR>
imap <C-t>e <ESC>:tabedit 
imap <C-t>l <ESC>:tabn<CR>
imap <C-t>h <ESC>:tabp<CR>


" Open terminal with ,t !TODO: Extract out to function
nmap <leader>t <C-w>s<C-w>j:term ++curwin<CR>

map <leader>f :FZF<CR>

" Close quickfix window
map <leader>cc :ccl<CR>

" Open quickfix window
map <leader>co :cope<CR>

" open netrw
map <leader>e :Explore<CR>
