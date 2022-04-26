local fn = vim.fn

-- Automatically install packer
local install_path = fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
  PACKER_BOOTSTRAP = fn.system {
    "git",
    "clone",
    "--depth",
    "1",
    "https://github.com/wbthomason/packer.nvim",
    install_path,
  }
  print "Installing packer close and reopen Neovim..."
  vim.cmd [[packadd packer.nvim]]
end

-- Autocommand that reloads neovim whenever you save the plugins.lua file
vim.cmd [[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerSync
  augroup end
]]

-- Use a protected call so we don't error out on first use
local status_ok, packer = pcall(require, "packer")
if not status_ok then
  return
end

-- Have packer use a popup window
packer.init {
  display = {
    open_fn = function()
      return require("packer.util").float { border = "rounded" }
    end,
  },
}

-- Install your plugins here
return packer.startup(function(use)
  -- My plugins here
  use "wbthomason/packer.nvim" -- Have packer manage itself
  use "nvim-lua/popup.nvim" -- An implementation of the Popup API from vim in Neovim
  use "nvim-lua/plenary.nvim" -- Useful lua functions used ny lots of plugins
  use "kyazdani42/nvim-web-devicons" -- Needed for NvimTree

  -- Editor functionality
  -- use "numToStr/Comment.nvim"
  use "JoosepAlviste/nvim-ts-context-commentstring"
  -- Autopairs, integrates with both cmp and lsp
  use { 'windwp/nvim-autopairs',
    after = {'nvim-treesitter', 'nvim-cmp'},
    config = function ()
      require('nvim-autopairs').setup {}
    end
  }
  use { 'p00f/nvim-ts-rainbow', after = {'nvim-treesitter'}}
  -- Colorschemes
  use "lunarvim/colorschemes" -- A bunch of colorschemes you can try out
  use "lunarvim/darkplus.nvim"
  use "folke/tokyonight.nvim"

  -- Aesthetics
  use "beauwilliams/statusline.lua"
  -- use {
  --   "glepnir/galaxyline.nvim",
  --   requires = {"kyazdani42/nvim-web-devicons", opt = true}
  -- }

  -- Project-Editor functionality
  use "kyazdani42/nvim-tree.lua" -- Project Explorer
  use {"akinsho/toggleterm.nvim"}  -- MultiTerminal
  -- TODO: Setup which-key
  use "folke/which-key.nvim"
  -- TODO: Setup Dashboard
  -- use "glepnir/dashboard-nvim"
  -- TODO: Setup vim-unimpaired and tpope plugins needed
  use "tpope/vim-sensible"
  use "tpope/vim-surround"
  use "tpope/vim-dispatch"
  use "tpope/vim-scriptease"
  use "tpope/vim-fugitive"
  use "junegunn/gv.vim"
  use "tpope/vim-commentary"
  -- use "tpope/vim-vinegar"
  use "tpope/vim-endwise"
  use "tpope/vim-unimpaired"
  use "radenling/vim-dispatch-neovim"
  -- Move at the speed of light!
  use {
    "ggandor/lightspeed.nvim",
    config = function ()
      require('lightspeed').setup {
        ignore_case = true,
      }
    end
  }

  -- cmp plugins
  use "hrsh7th/nvim-cmp" -- The completion plugin
  use "hrsh7th/cmp-buffer" -- buffer completions
  use "hrsh7th/cmp-path" -- path completions
  use "hrsh7th/cmp-cmdline" -- cmdline completions
  use "saadparwaiz1/cmp_luasnip" -- snippet completions
  use "hrsh7th/cmp-nvim-lsp"
  use "hrsh7th/cmp-nvim-lua"

  -- snippets
  use "L3MON4D3/LuaSnip" --snippet engine
  use "rafamadriz/friendly-snippets" -- a bunch of snippets to use

  -- LSP
  use "neovim/nvim-lspconfig" -- enable LSP
  use "williamboman/nvim-lsp-installer" -- simple to use language server installer
  use "tamago324/nlsp-settings.nvim" -- language server settings defined in json for specific languages

  -- Telescope
  use "nvim-telescope/telescope.nvim"
  use "nvim-telescope/telescope-file-browser.nvim"

  -- Treesitter
  use {
    "nvim-treesitter/nvim-treesitter",
    run = ":TSUpdate",
  }
  use "nvim-treesitter/playground"

  -- Git
  use "lewis6991/gitsigns.nvim"
  -- Decide between these two:
  -- use "tpope/vim-fugitive"
  -- use "sindrets/diffview.nvim"

  -- Language specific
  use "Olical/conjure"
  use "crispgm/nvim-go"
  use "jaawerth/fennel-nvim"
  use "clojure-vim/clojure.vim"
  use "clojure-vim/vim-jack-in"
  use "elixir-editors/vim-elixir"
  -- use "mattn/emmet-vim"
  use {"nvim-orgmode/orgmode", after = {"nvim-treesitter"}}

  -- DBs
  use "tpope/vim-dadbod"
  use "kristijanhusak/vim-dadbod-ui"

  -- Automatically set up your configuration after cloning packer.nvim
  -- Put this at the end after all plugins
  if PACKER_BOOTSTRAP then
    require("packer").sync()
  end
end)
