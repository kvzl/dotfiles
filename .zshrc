# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# zimfw
ZIM_CONFIG_FILE=$HOME/.zimrc
ZIM_HOME=${ZDOTDIR:-${HOME}}/.zim

# Download zimfw plugin manager if missing.
if [[ ! -e ${ZIM_HOME}/zimfw.zsh ]]; then
  curl -fsSL --create-dirs -o ${ZIM_HOME}/zimfw.zsh \
      https://github.com/zimfw/zimfw/releases/latest/download/zimfw.zsh
fi

# Install missing modules and update ${ZIM_HOME}/init.zsh if missing or outdated.
if [[ ! ${ZIM_HOME}/init.zsh -nt ${ZIM_CONFIG_FILE:-${ZDOTDIR:-${HOME}}/.zimrc} ]]; then
  source ${ZIM_HOME}/zimfw.zsh init
fi

# Initialize modules.
source ${ZIM_HOME}/init.zsh

fpath+=~/.zfunc

# Enable vi mode for zsh
bindkey -v

# apply aliases
if [ -f "$HOME/.zsh_aliases" ]; then
	zsh-defer source "$HOME/.zsh_aliases"
fi

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# fzf
[[ ! "$(command -v fzf)" ]] || zsh-defer _evalcache fzf --zsh

# zoxide
[[ ! "$(command -v zoxide)" ]] || zsh-defer _evalcache zoxide init zsh --cmd z

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
zsh-defer _evalcache pyenv init - zsh

# zellij
zellij_tab_name_update() {
  if [[ -n $ZELLIJ ]]; then
    local current_dir=$PWD
    if [[ $current_dir == $HOME ]]; then
      current_dir="~"
    else
      current_dir=${current_dir##*/}
    fi
    (command nohup zellij action rename-tab $current_dir >/dev/null 2>&1 &)
  fi
}

[[ -n $ZELLIJ ]] && {
  zsh-defer zellij_tab_name_update
  chpwd_functions+=(zellij_tab_name_update) 
}

