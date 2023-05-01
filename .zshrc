# zmodload zsh/zprof

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

export LANG=en_US.UTF-8

# antigen
# source /usr/local/share/antigen/antigen.zsh
# source $HOME/.antigenrc

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


# nvm
# NODE_VERSION=v14.8.0
# export PATH="$HOME/.nvm/versions/node/$NODE_VERSION/bin:$PATH"
#
# export NVM_DIR="$HOME/.nvm"
# [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" --no-use # This loads nvm
# [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion


# yarn
export PATH="$HOME/.yarn/bin:$PATH"

fpath+=~/.zfunc


# zsh
HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000


# composer
export PATH="$HOME/.composer/vendor/bin:$PATH"


# apply aliases
if [ -f "$HOME/.zsh_aliases" ]; then
	source "$HOME/.zsh_aliases"
fi

# doom
export PATH="$HOME/.emacs.d/bin:$PATH"

# custom scripts
export PATH="$HOME/Scripts:$PATH"

# shopify
[ -f "$HOME/.shopify-app-cli/shopify.sh" ] && source "$HOME/.shopify-app-cli/shopify.sh"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# Use vim keybinding
bindkey -v
export KEYTIMEOUT=1

# sdkman
#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$SDKMAN_DIR/bin/sdkman-init.sh" ]] && source "$SDKMAN_DIR/bin/sdkman-init.sh"

# opam configuration
test -r "$HOME/.opam/opam-init/init.zsh" && . "$HOME/.opam/opam-init/init.zsh" > /dev/null 2> /dev/null || true

# kubectl krew
export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"

# disable dotnet cli telemetry
DOTNET_CLI_TELEMETRY_OPTOUT=1

export PATH="/opt/homebrew/bin:$PATH"
export PATH="/usr/local/bin:$PATH"

# zprof