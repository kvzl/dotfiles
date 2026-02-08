return {
  "folke/snacks.nvim",
  opts = {
    dashboard = { enabled = false },
    picker = {
      hidden = true,
      sources = {
        files = {
          hidden = true,
          ignored = true,
        },
      },
      grep = {
        hidden = true,
      },
    },
  },
  keys = {
    {
      "<leader>G",
      function()
        Snacks.picker.grep({ hidden = true })
      end,
      desc = "Grep custom",
    },
  },
}
