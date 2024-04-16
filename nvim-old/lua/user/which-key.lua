-- use {
--   "folke/which-key.nvim",
--   config = function()
--     require("which-key").setup {
--       -- your configuration comes here
--       -- or leave it empty to use the default settings
--       -- refer to the configuration section below
--     }
--   end
-- }
local status_ok, wk = pcall(require, "which-key")
if not status_ok then
  return
end

wk.setup({

})

wk.register({
  ["<leader>f"] = {
    name = "+file",
    f = { "<cmd>Telescope find_files<cr>", "Find File"},
    g = { "<cmd>Telescope live_grep<cr>", "Grep Files"},
    r = { "<cmd>Telescope oldfiles<cr>", "Open Recent Files"},
    n = { "<cmd>enew<cr>", "New File" },
    h = { "<cmd>Telescope help_tags<cr>", "Help Tags" },
    m = { "<cmd>Telescope man_pages<cr>", "Man Pages" },
    q = { "<cmd>Telescope quickfix<cr>", "Quickfix" },
    t = { "<cmd>Telescope treesitter<cr>", "Treesitter" },
  },
  ["<leader>g"] = {
    name = "+git",
    f = { "<cmd>Telescope git_files<cr>", "Find File"},
    c = { "<cmd>Telescope git_commits<cr>", "Find Commit"},
    b = { "<cmd>Telescope git_branches<cr>", "Find Branch"},
    s = { "<cmd>Telescope git_status<cr>", "Status"},
    x = { "<cmd>Telescope git_stash<cr>", "Stash"},
  },
  ["<leader>w"] = {
    name = "+window",
    v = { "<cmd>vsp<cr>", "Vertical Split"},
    s = { "<cmd>sp<cr>", "Horizontal Split"},
    d = { "<cmd>q<cr>", "Delete Window" },
    w = { "<cmd>wincmd w<cr>", "Other Window" },
    h = { "<cmd>wincmd h<cr>", "Left Window" },
    j = { "<cmd>wincmd j<cr>", "Down Window" },
    k = { "<cmd>wincmd k<cr>", "Up Window" },
    l = { "<cmd>wincmd l<cr>", "Right Window" },
    D = { "<cmd>only<cr>", "Close Others" },
  },
  ["<leader>b"] = {
    name = "+buffer",
    n = { "<cmd>bnext<cr>", "Buffer Next"},
    p = { "<cmd>bprevious<cr>", "Buffer Previous"},
    l = { "<cmd>Telescope buffers", "Buffer List"},
  },
  ["<leader>o"] = {
    name = "+open",
    t = { "<cmd>ToggleTerm<cr>", "Terminal Popup"},
    e = { "<cmd>NvimTreeToggle<cr>", "Explorer"}
  },
  ["<leader>n"] = {
    name = "+close",
    h = { "<cmd>noh<cr>", "Highlight"},
  },
})
