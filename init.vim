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
  Plug 'tpope/vim-speeddating'



  "         Coding/IDE
  "##############################
  Plug 'janko-m/vim-test'
  Plug 'tpope/vim-dispatch'
  " Provides commands, check frequently
  " :help unimpaired
  Plug 'tpope/vim-unimpaired'
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
  " Plug 'roxma/nvim-completion-manager'
  " Vim repeat note:
  " To add support for specific plugins
  " silent! call repeat#set("\<Plug>MyWonderfulMap", v:count)
  Plug 'tpope/vim-repeat' 
  Plug 'majutsushi/tagbar'
  Plug 'christoomey/vim-tmux-navigator'
  Plug 'justinmk/vim-sneak'
  Plug 'hkupty/iron.nvim'



  "    Code Syntax/Navigation
  "##############################
  Plug 'ludovicchabant/vim-gutentags'
  " Plug 'neomake/neomake'
  Plug 'w0rp/ale'
  Plug 'nathanaelkane/vim-indent-guides'
  Plug 'Yggdroot/indentLine'



  "         Terminal
  "##############################
  Plug 'kassio/neoterm'


  "       Version Control
  "##############################
  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-rhubarb'
  Plug 'airblade/vim-gitgutter'
  Plug 'mhinz/vim-signify'
  Plug 'jreybert/vimagit'



  "         Languages
  "##############################
  " A bunch of languages
  Plug 'sheerun/vim-polyglot'

  " Web
  Plug 'mattn/emmet-vim'

  " Elixir
  Plug 'elixir-lang/vim-elixir'
  Plug 'slashmili/alchemist.vim'
  Plug 'c-brenn/phoenix.vim'

  " Ruby
  Plug 'tpope/vim-bundler'
  Plug 'tpope/vim-rake'
  Plug 'tpope/vim-rails'

  " Python
  Plug 'zchee/deoplete-jedi'
  Plug 'Vimjas/vim-python-pep8-indent'

  " Clojure
  " Plug 'tpope/vim-salve'
  Plug 'tpope/vim-fireplace'
  Plug 'clojure-vim/vim-jack-in'
  " Add [grimvim \"0.1.0"] to project.clj
  Plug 'jebberjeb/grimoire.vim'
  " Plug 'clojure-vim/async-clj-omni'
  " Plug 'clojure-vim/acid.nvim'

  " Java
  Plug 'artur-shaik/vim-javacomplete2'
  
  " CPP
  Plug 'Rip-Rip/clang_complete'

  " Racket
  Plug 'wlangstroth/vim-racket'
  Plug 'MicahElliott/vrod'

  " Rust
  Plug 'rust-lang/rust.vim'
  Plug 'racer-rust/vim-racer'

  " Go
  Plug 'fatih/vim-go'

  " Common-lisp
  Plug 'l04m33/vlime', {'rtp': 'vim/'}


  "       Misc Utilities
  "##############################
  Plug 'wakatime/vim-wakatime'
  Plug 'jceb/vim-orgmode'
  Plug 'vimwiki/vimwiki'
  Plug 'mhinz/vim-startify'
call plug#end()

" Basic
" ####################
filetype plugin indent on
syntax on
set hidden
" utf8
set encoding=utf-8
" Line Numbers
set number
" set relativenumber
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
" Enable folding
set foldmethod=indent
set foldlevel=99
" Fast tty for slow term like Konsole
set ttyfast

" Rainbow Parentheses
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces



" Remaps
" ####################
let mapleader="," " Leader key is ,
let maplocalleader = ","
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

" -----CommandT
" Buffers
map <leader>b :CommandTBuffer<CR>
" Help tags
map <leader>? :CommandTHelp<CR>
" Command tags
map <leader>c :CommandTCommand<CR>
" History
map <leader>h :CommandTHistory<CR>
" Files (current directory)
map <leader>t :CommandT<CR>
" Jumps
map <leader>j :CommandTJump<CR>
" Use CtrlG to cancel
let g:CommandTCancelMap='<C-g>'

" New tab with current file
map <leader>w :tabnew %<CR>
" Next Tab
map ]w :tabnext<CR>
" Previous Tab
map [w :tabprevious<CR>
" Toggle NERDTree
nmap <F1> <Esc>:NERDTreeToggle<CR> 
" Toggle NERDTree from insert mode
imap <F1> :NERDTreeToggle<CR> 
" Toggle Tagbar
nmap <F2> :TagbarToggle<CR>
" Toggle Tagbar from insert
imap <F2> :TagbarToggle<CR>
" FZF FuzzyFind
map <leader>f :FZF<CR>

nmap <leader>dv <C-w>v
nmap <leader>ds <C-w>s
nmap <leader>dd <C-w>w

" Tabs and indentation
" ####################
"set expandtab " use spaces
set smarttab
set tabstop=4
set shiftwidth=0 " <TAB> == 2 spaces
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
" let g:alchemist_tag_disable = 1

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
"let g:neoterm_position = 'horizontal'
let g:neoterm_default_mod = 'belowright'
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
" BadWhitespace
" au BufRead,BufNewFile *.* match BadWhitespace /\s\+$/
" IronRepl
nmap <leader>r :IronRepl<CR><C-w>w
"
let g:AutoPairsMapCR = 0

" Rust Racer
let g:racer_cmd = "~/.cargo/bin/racer"
autocmd BufRead *.rs :setlocal tags=./rusty-tags.vi;/
autocmd BufWritePost *.rs :silent! exec "!rusty-tags vi --quiet --start-dir=" . expand('%:p:h') . "&" | redraw!
autocmd BufRead *.rs :setlocal tags=./rusty-tags.vi;/,$RUST_SRC_PATH/rusty-tags.vi
au FileType rust nmap gd <Plug>(rust-def)
au FileType rust nmap gs <Plug>(rust-def-split)
au FileType rust nmap gx <Plug>(rust-def-vertical)
au FileType rust nmap K <Plug>(rust-doc)

" Go
au FileType go nmap <leader>gr <Plug>(go-run)
au FileType go nmap <leader>gb <Plug>(go-build)
au FileType go nmap <leader>gt <Plug>(go-test)
au FileType go nmap <leader>gc <Plug>(go-coverage)

" Python 
" pep8
" au BufNewFile,BufRead *.py
"     \ set tabstop=4
"     \ set softtabstop=4
"     \ set shiftwidth=4
"     \ set textwidth=79
"     \ set expandtab
"     \ set autoindent
"     \ set fileformat=unix

" Common-lisp
let g:vlime_cl_use_terminal = 1
let g:vlime_cl_impl = "clisp"
function! VlimeBuildServerCommandFor_clisp(vlime_loader, vlime_eval)
  return ["clisp"]
endfunction

" vimwiki
let g:vimwiki_folding='syntax'

highlight! link TermCursor Cursor
highlight! TermCursorNC guibg=red guifg=white ctermbg=1 ctermfg=15



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
