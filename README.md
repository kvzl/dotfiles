# dotfiles

## Environment

Currently:

- OS: macOS ARM
- Terminal: [Ghostty](https://ghostty.org/)
- Shell: Zsh + [Zim](https://github.com/zimfw/zimfw) + [p10k](https://github.com/romkatv/powerlevel10k)
- Editor: Neovim + [LazyVim](https://github.com/LazyVim/LazyVim)
- Package manager: [Homebrew](https://brew.sh/) + [Brewfile](https://github.com/Homebrew/homebrew-bundle)
- Dotfile manager: [Dotbot](https://github.com/anishathalye/dotbot)
- WM: [Amethyst](https://ianyh.com/amethyst/)

## Prerequisite

To begin, please install Homebrew by running the following command in your shell:

```shell
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

## Installation

Obtain the dotfiles and initialize [Dotbot](https://github.com/anishathalye/dotbot):

```shell
git clone git@github.com:kvzl/dotfiles.git
cd dotfiles
./install
```

Install applications and utilities using Homebrew Bundle:

```shell
brew bundle
```

