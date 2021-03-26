imap jk <Esc>

call plug#begin('~/.local/share/nvim/plugged')
	Plug 'tpope/vim-sensible'
	Plug 'tpope/vim-surround'
	Plug 'tpope/vim-unimpaired'
	Plug 'tpope/vim-repeat'
	Plug 'tpope/vim-projectionist'
	Plug 'tpope/vim-fugitive'
	Plug 'tpope/vim-endwise'

	Plug 'tpope/vim-salve'
	Plug 'tpope/vim-dispatch'
	" :Plug 'tpope/vim-fireplace'
	" Clojure plugin
	Plug 'Olical/conjure', {'tag': 'v4.16.0'}

	Plug 'clojure-vim/vim-jack-in'
	Plug 'radenling/vim-dispatch-neovim'

	Plug 'prabirshrestha/vim-lsp'
	Plug 'mattn/vim-lsp-settings'

	Plug 'Shougo/deoplete.nvim'
	Plug 'lighttiger2505/deoplete-vim-lsp'
call plug#end()

let g:deoplete#enable_at_startup = 1
