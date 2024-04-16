local function create_window_with_filetype(filetype)
  local width = vim.api.nvim_get_option 'columns'
  local height = vim.api.nvim_get_option 'lines'

  local win_height = math.ceil(height * 0.8 - 4)
  local win_width = math.ceil(width * 0.8)

  local row = math.ceil((height - win_height) / 2 - 1)
  local col = math.ceil((width - win_width) / 2)

  local opts = {
    relative = 'editor',
    style = 'minimal',
    -- relative = 'win',
    row = row,
    col = col,
    border = 'rounded',
    title = 'Popout',
    title_pos = 'center',
    width = win_width,
    height = win_height,
  }

  local buf_id = vim.api.nvim_create_buf(false, false)
  vim.api.nvim_buf_set_option(buf_id, 'filetype', filetype)

  local win_id = vim.api.nvim_open_win(buf_id, true, opts)
  vim.api.nvim_win_set_option(win_id, 'cursorline', true)
end

local function create_window_with_current_file()
  local width = vim.api.nvim_get_option 'columns'
  local height = vim.api.nvim_get_option 'lines'

  local win_height = math.ceil(height * 0.8 - 4)
  local win_width = math.ceil(width * 0.8)

  local row = math.ceil((height - win_height) / 2 - 1)
  local col = math.ceil((width - win_width) / 2)

  local opts = {
    relative = 'editor',
    style = 'minimal',
    -- relative = 'win',
    row = row,
    col = col,
    border = 'rounded',
    title = 'Popout',
    title_pos = 'center',
    width = win_width,
    height = win_height,
  }

  local buf_id = vim.api.nvim_create_buf(false, false)
  local file_path = vim.api.nvim_buf_get_name(0)

  local win_id = vim.api.nvim_open_win(buf_id, true, opts)
  vim.api.nvim_win_set_option(win_id, 'cursorline', true)

  vim.cmd('$read' .. file_path)
end

local function create_window_with_current_buffer()
  local width = vim.api.nvim_get_option 'columns'
  local height = vim.api.nvim_get_option 'lines'

  local win_height = math.ceil(height * 0.8 - 4)
  local win_width = math.ceil(width * 0.8)

  local row = math.ceil((height - win_height) / 2 - 1)
  local col = math.ceil((width - win_width) / 2)

  local opts = {
    relative = 'editor',
    style = 'minimal',
    -- relative = 'win',
    row = row,
    col = col,
    border = 'rounded',
    title = 'Popout',
    title_pos = 'center',
    width = win_width,
    height = win_height,
  }

  -- local file_path = vim.api.nvim_buf_get_name(0)
  local buf_id = vim.api.nvim_get_current_buf()

  local win_id = vim.api.nvim_open_win(buf_id, true, opts)
  vim.api.nvim_win_set_option(win_id, 'cursorline', true)

  -- vim.cmd('$read' .. file_path)
end

local function close_window(win_id, buf_id)
  if vim.api.nvim_win_is_valid(win_id) then
    vim.api.nvim_win_close(win_id, true)
  end

  if vim.api.nvim_buf_is_valid(buf_id) then
    vim.api.nvim_buf_delete(buf_id, { force = true })
  end
end
