# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# zplug
export ZPLUG_HOME="$HOME/.zplug"


# Check if zplug is installed
if [[ ! -d $ZPLUG_HOME ]]; then
  git clone https://github.com/zplug/zplug $ZPLUG_HOME
  source $ZPLUG_HOME/init.zsh && zplug update --self
else
  source $ZPLUG_HOME/init.zsh
fi

source $HOME/.zplugrc

fpath+=~/.zfunc

# Enable vi mode for zsh
bindkey -v

# apply aliases
if [ -f "$HOME/.zsh_aliases" ]; then
	source "$HOME/.zsh_aliases"
fi

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

[[ $commands[kubectl] ]] && source <(kubectl completion zsh) # add autocomplete permanently to your zsh shell

# fzf
[[ ! "$(command -v fzf)" ]] || eval "$(fzf --zsh)"

# zoxide
[[ ! "$(command -v zoxide)" ]] || eval "$(zoxide init zsh --cmd z)"
#k9s
export K9S_CONFIG_DIR="$HOME/.config/k9s"
# zellij
zellij_tab_name_update() {
    if [[ -n $ZELLIJ ]]; then
        local current_dir=$PWD
        if [[ $current_dir == $HOME ]]; then
            current_dir="~"
        else
            current_dir=${current_dir##*/}
        fi
        command nohup zellij action rename-tab $current_dir >/dev/null 2>&1
    fi
}

zellij_tab_name_update
chpwd_functions+=(zellij_tab_name_update)
