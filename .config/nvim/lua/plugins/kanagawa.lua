return {
  "rebelot/kanagawa.nvim",
  lazy = false,
  priority = 1000,
  config = function()
    -- local colors = require("kanagawa.colors").setup()
    -- local palette = colors.palette

    require("kanagawa").setup({
      theme = "dragon",
      background = {
        light = "dragon",
        dark = "dragon",
      },

      transparent = true,

      colors = {
        palette = {
          dragonOrange = "#ffffff",
          roninYellow = "#ffffff",
        },
        theme = {
          all = {
            ui = {
              bg_gutter = "none",
            },
          },
        },
      },
    })
  end,
}
