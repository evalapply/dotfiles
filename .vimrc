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
imap <C-j> <Esc>
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
Plug 'mattn/emmet-vim'
Plug 'artur-shaik/vim-javacomplete2'
call plug#end()

" NeoComplete
" Disable AutoComplPop.
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

" JavaComplete
autocmd FileType java setlocal omnifunc=javacomplete#Complete
nmap <F4> <Plug>(JavaComplete-Imports-AddSmart)
imap <F4> <Plug>(JavaComplete-Imports-AddSmart)
nmap <F5> <Plug>(JavaComplete-Imports-Add)
imap <F5> <Plug>(JavaComplete-Imports-Add)
nmap <F6> <Plug>(JavaComplete-Imports-AddMissing)
imap <F6> <Plug>(JavaComplete-Imports-AddMissing)
nmap <F7> <Plug>(JavaComplete-Imports-RemoveUnused)
imap <F7> <Plug>(JavaComplete-Imports-RemoveUnused)
let g:JavaComplete_ClosingBrace = 1

" OmniCompletions
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

" Fuzzy Finder
set rtp+=~/.fzf


"Colorscheme
colorscheme onedark
"set t_Co=256
set background=dark

"Use 24-bit (true-color) mode in Vim/Neovim when outside tmux.
"If you're using tmux version 2.2 or later, you can remove the outermost $TMUX check and use tmux's 24-bit color support
"(see < http://sunaku.github.io/tmux-24bit-color.html#usage > for more information.)
if (empty($TMUX))
  if (has("nvim"))
    "For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
    let $NVIM_TUI_ENABLE_TRUE_COLOR=1
  endif
  "For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
  "Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
  " < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
  if (has("termguicolors"))
    set termguicolors
  endif
endif
