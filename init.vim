" Specify directory for plugins
" Begin vim-plug load
call plug#begin('~/.config/nvim/plugged')

  "           Themes
  "##############################
  Plug 'arcticicestudio/nord-vim'

  "      File Navigation
  "##############################
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install -all' }
  Plug 'tpope/vim-projectionist'
  Plug 'scrooloose/nerdtree'
  Plug 'wincent/Command-T'
  Plug 'brookhong/ag.vim'


  "          Editing
  "##############################
  Plug 'tpope/vim-commentary'
  Plug 'kien/rainbow_parentheses.vim'
  Plug 'guns/vim-surround'
  Plug 'jiangmiao/auto-pairs'
  Plug 'tpope/vim-endwise'
  Plug 'junegunn/vim-easy-align'



  "         Coding/IDE
  "##############################
  Plug 'janko-m/vim-test'
  " Plug 'tpope/vim-dispatch'
  " Provides commands, check frequently
  " :help unimpaired
  Plug 'tpope/vim-unimpaired'
  " Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
  Plug 'roxma/nvim-completion-manager'
  " Vim repeat note:
  " To add support for specific plugins
  " silent! call repeat#set("\<Plug>MyWonderfulMap", v:count)
  Plug 'tpope/vim-repeat' 



  "    Code Syntax/Navigation
  "##############################
  Plug 'ludovicchabant/vim-gutentags'
  Plug 'neomake/neomake'
  " Plug 'w0rp/ale'



  "         Terminal
  "##############################
  Plug 'kassio/neoterm'


  "       Version Control
  "##############################
  Plug 'tpope/vim-fugitive'
  Plug 'airblade/vim-gitgutter'



  "         Languages
  "##############################
  " A bunch of languages
  Plug 'sheerun/vim-polyglot'

  " Web
  Plug 'mattn/emmet-vim'

  " Elixir
  Plug 'slashmili/alchemist.vim'
  Plug 'c-brenn/phoenix.vim'
  Plug 'elixir-lang/vim-elixir'
  Plug 'thinca/vim-ref'
  Plug 'awetzel/elixir.nvim', { 'do': 'yes \| ./install.sh' }

  " Ruby
  Plug 'tpope/vim-bundler'
  Plug 'tpope/vim-rake'
  Plug 'tpope/vim-rails'

  " Clojure
  " Plug 'tpope/vim-fireplace'
  Plug 'clojure-vim/acid.nvim'
  " Add [grimvim \"0.1.0"] to project.clj
  Plug 'jebberjeb/grimoire.vim'
  Plug 'clojure-vim/async-clj-omni'

  " Java
  Plug 'artur-shaik/vim-javacomplete2'
  
  " CPP
  Plug 'Rip-Rip/clang_complete'

  "Lispy
  Plug 'jpalardy/vim-slime'


  "       Misc Utilities
  "##############################
  Plug 'wakatime/vim-wakatime'
call plug#end()

" Basic
" ####################
filetype plugin indent on
syntax on
" utf8
set encoding=utf-8
" Line Numbers
set number
" set relativenumber
" Show matching braces
set showmatch
" Set backup directory
" // <- means filenames should be built from complete path
set backupdir=~/.config/nvim/.backup// 
" Set swap directory
set directory=~/.config/nvim/.swp//
" Set undo directory
set undodir=~/.config/nvim/.undo//



" Remaps
" ####################
let mapleader="," " Leader key is ,
" Cursor line
set cul
"
" Get Rid of Escape!
"
" jk to exit insert mode
imap jk <Esc> 
" jk to exit command mode
cmap <C-g> <C-c>
" v to exit visual mode
vmap v <Esc>
" jk to exit terminal mode
tmap jk <C-\><C-n>

" ,s to save
map <leader>s :w<CR>
" ,q to save and quit
map <leader>q :q<CR>
" Get rid of search highlight
nmap <leader>n :noh<CR> 
" Close quickfix window
map <leader>cc :ccl<CR>
" Open quickfix window
map <leader>co :cope<CR>
" Goto vimrc
nmap <leader>vr :e ~/.config/nvim/init.vim<CR> 
" Faster open file
nmap <leader>o :e 
" Open netrw
nmap <leader>e :Explore<CR> 
" List Buffers
" map <leader>b :buffers<CR>
" Using CommandT as it allows to select buffer
" instead of just listing
map <leader>b :CommandTBuffer<CR>
" Help tags
map <leader>h :CommandTBuffer<CR>
" Use CtrlG to cancel
let g:CommandTCancelMap='<C-g>'
" New tab with current file
map <leader>w :tabnew %<CR>
" Next Tab
map ]w :tabnext<CR>
" Previous Tab
map [w :tabprevious<CR>
" Toggle NERDTree
imap <F1> <Esc>:NERDTreeToggle<CR> 
" Toggle NERDTree from insert mode
nmap <F1> :NERDTreeToggle<CR> 
" FZF FuzzyFind
map <leader>f :FZF<CR>



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



" Editing
" ####################
" Tags
let g:gutentags_cache_dir = '~/.config/nvim/.tags_cache'
" " Neomake
" " Checks whether running on battery
" function! MyOnBattery()
"   return readfile('/sys/class/power_supply/AC/online') == ['0']
" endfunction
" " Changes when Neomake runs based on whether
" " running on battery
" if MyOnBattery()
"   call neomake#configure#automake('w')
" else
"   call neomake#configure#automake('nw', 1000)
" endif
" " Open errors automatically
" let g:neomake_open_list = 2

" Alchemist
" Disable semi-tags functionality
let g:alchemist_tag_disable = 1

" Vim-Test make tests use dispatch.vim
let test#strategy = "dispatch"

" Command-T Faster file indexing
" `git ls-files`, `find` if not in git project
let g:CommandTFileScanner = "git"
" increase max files
let g:CommandTMaxFiles=200000

" Deoplete
let g:deoplete#enable_at_startup = 1

" Neoterm
let g:neoterm_position = 'horizontal'
let g:neoterm_automap_keys = ',tm'
" Useful maps
" Toggle last terminal, new if none are open
nnoremap <silent> ,to :Ttoggle<CR>
" Consider :Topen - opens a new terminal, or the current
" Opens a new terminal
nnoremap <silent> ,tn :Tnew<CR>
" hide/close terminal
nnoremap <silent> ,th :call neoterm#close()<cr>
" clear terminal
nnoremap <silent> ,tl :call neoterm#clear()<cr>
" kills the current job (send a <c-c>)
nnoremap <silent> ,tc :call neoterm#kill()<cr>
" Repl mappings
nmap gx <Plug>(neoterm-repl-send)
xmap gx <Plug>(neoterm-repl-send)
nmap gxx <Plug>(neoterm-repl-send-line)
" now can use gx{text-objects} such as gxip
let g:slime_target = "neovim"

" OmniCompletions
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
autocmd FileType java setlocal omnifunc=javacomplete#Complete



" Theming
" ####################
colorscheme nord
set t_Co=256
set termguicolors
