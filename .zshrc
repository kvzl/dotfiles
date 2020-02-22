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
# export NVM_DIR="$HOME/.nvm"
# [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm


# rvm
# export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting


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

# tabtab source for slss package
# uninstall by removing these lines or running `tabtab uninstall slss`
[[ -f /Users/ucfan/.nvm/versions/node/v10.13.0/pnpm-global/1/node_modules/.registry.npmjs.org/tabtab/2.2.2/node_modules/tabtab/.completions/slss.zsh ]] && . /Users/ucfan/.nvm/versions/node/v10.13.0/pnpm-global/1/node_modules/.registry.npmjs.org/tabtab/2.2.2/node_modules/tabtab/.completions/slss.zsh


# doom
export PATH="$HOME/.emacs.d/bin:$PATH"

# custom scripts
export PATH="$HOME/Scripts:$PATH"


[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
