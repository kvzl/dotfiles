return {
  "zaldih/themery.nvim",
  lazy = false,
  config = function()
    require("themery").setup({
      themes = {
        {
          name = "Nordic",
          colorscheme = "nordic",
        },
        {
          name = "Catppuccin",
          colorscheme = "catppuccin",
        },
        {
          name = "Kanagawa",
          colorscheme = "kanagawa-dragon",
        },
      },
    })
  end,
}
