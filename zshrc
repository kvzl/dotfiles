# antigen
# source /usr/local/share/antigen/antigen.zsh
# source $HOME/.antigenrc

# zplug
export ZPLUG_HOME=/usr/local/opt/zplug
source $ZPLUG_HOME/init.zsh
source $HOME/.zplugrc


# nvm
export NVM_DIR="/Users/ucfan/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm


# rvm
export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting


# android
export ANDROID_HOME=~/Library/Android/sdk
export PATH=${PATH}:${ANDROID_HOME}/tools:${ANDROID_HOME}/platform-tools


# tns
###-tns-completion-start-###
#if [ -f /Users/ucfan/.tnsrc ]; then
#    source /Users/ucfan/.tnsrc
#fi
###-tns-completion-end-###


# yarn
export PATH="$HOME/.yarn/bin:$PATH"

fpath+=~/.zfunc

# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
[[ -f /Users/ucfan/.config/yarn/global/node_modules/tabtab/.completions/serverless.zsh ]] && . /Users/ucfan/.config/yarn/global/node_modules/tabtab/.completions/serverless.zsh
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
[[ -f /Users/ucfan/.config/yarn/global/node_modules/tabtab/.completions/sls.zsh ]] && . /Users/ucfan/.config/yarn/global/node_modules/tabtab/.completions/sls.zsh


# AWS Profile
export AWS_PROFILE=kevin@evolabs
# export AWS_PROFILE=kevin.l
# export AWS_PROFILE=kevin@sandbox



# emsdk
export PATH="$HOME/mlib/emsdk-portable:$PATH"
export EMSDK="$HOME/mlib/emsdk-portable"
export EM_CONFIG="$HOME/.emscripten"


# pyenv
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi
export PATH="~/.pyenv/bin:$PATH"

export PATH="$HOME/.local/bin:$PATH"

# pyenv-virtualenv
if which pyenv-virtualenv-init > /dev/null; then eval "$(pyenv virtualenv-init -)"; fi


# zsh
HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000


# opam
. $HOME/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true


# webdev
export PATH="$HOME/.pub-cache/bin:$PATH"


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


