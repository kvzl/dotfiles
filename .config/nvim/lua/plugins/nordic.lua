return {
  "AlexvZyl/nordic.nvim",
  lazy = false,
  priority = 1000,
  config = function()
    require("nordic").load({
      transparent = {
        bg = true,
        float = true,
      },
      bright_border = true,
      reduced_blue = true,
      swap_backgrounds = true,
      cursorline = {
        theme = "dark",
      },
      telescope = {
        style = "classic",
      },
    })
  end,
}
