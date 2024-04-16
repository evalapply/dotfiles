vim.g.user_emmet_install_global = 0

-- vim.api.nvim_command([[
-- autocmd FileType html,css,heex,leex,jsx,ts,xml,less,sass,scss EmmetInstall
-- ]])

vim.api.nvim_create_autocmd("FileType", {
  pattern = {"html,css,heex,leex,jsx,ts,xml,less,sass,scss"},
  callback = EmmetInstall,
})
