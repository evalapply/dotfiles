local path_to_elixirLS = vim.fn.expand("~/ThirdParty/elixir-ls/release/language_server.sh")

return {
  cmd = {path_to_elixirLS},
  settings = {
    elixirLS = {
      dialyzerEnabled = false,
      fetchDeps = false,
    },
  },
}
