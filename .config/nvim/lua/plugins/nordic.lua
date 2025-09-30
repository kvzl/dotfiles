return {
  "AlexvZyl/nordic.nvim",
  lazy = false,
  priority = 1000,
  config = function()
    require("nordic").load({
      on_highlight = function(highlights, palette)
        highlights.CursorLine.bg = palette.black0
      end,
      transparent = {
        bg = true,
        float = true,
      },
      bold_keywords = true,
      bright_border = true,
      reduced_blue = true,
      cursorline = {
        theme = "light",
      },
    })
  end,
}
