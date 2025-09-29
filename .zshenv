[[ -s "$HOME/.profile" ]] && . "$HOME/.profile"

export PATH="/opt/homebrew/bin:$PATH"
export PATH="/opt/homebrew/opt/mysql-client/bin:$PATH"
export PATH="/usr/local/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"

export LANG=en_US.UTF-8

export EDITOR=vim

# yarn
export PATH="$HOME/.yarn/bin:$PATH"

# zsh
HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

# composer
export PATH="$HOME/.composer/vendor/bin:$PATH"

# opam configuration
test -r "$HOME/.opam/opam-init/init.zsh" && . "$HOME/.opam/opam-init/init.zsh" > /dev/null 2> /dev/null || true

# kubectl krew
export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"

# disable dotnet cli telemetry
DOTNET_CLI_TELEMETRY_OPTOUT=1

# go
export GOPATH="$HOME/go"
export PATH="${PATH}:${GOPATH}/bin"

# rust
# . "$HOME/.cargo/env"

# pnpm
export PNPM_HOME="$HOME/.pnpm"
export PATH="$PNPM_HOME:$PATH"

# k9s
export K9S_CONFIG_DIR="$HOME/.config/k9s"

# gam
export PATH="$HOME/bin:$PATH"
alias gam="$HOME/bin/gam7/gam"

# zsh-autosuggestions
export ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE="20"
export ZSH_AUTOSUGGEST_USE_ASYNC=1

# nvm
export NVM_LAZY_LOAD=true
export NVM_COMPLETION=true


# Added by OrbStack: command-line tools and integration
# This won't be added again if you remove it.
source ~/.orbstack/shell/init.zsh 2>/dev/null || :

timezsh() {
  shell=${1-$SHELL}
  for i in $(seq 1 10); do /usr/bin/time $shell -i -c exit; done
}

# terraform helpers
tfa() {
  workspace=$(terraform workspace show)
  stack=$(basename "$PWD")
  echo "running: terraform apply -var-file ../../config/$workspace/$stack.tfvars $@"
  terraform apply -var-file "../../config/$workspace/$stack.tfvars" $@
}

tfp() {
  workspace=$(terraform workspace show)
  stack=$(basename "$PWD")
  echo "running: terraform plan -var-file ../../config/$workspace/$stack.tfvars $@"
  terraform plan -var-file "../../config/$workspace/$stack.tfvars" $@
}

tfi() {
  workspace=$(terraform workspace show)
  stack=$(basename "$PWD")
  echo "running: terraform import -var-file ../../config/$workspace/$stack.tfvars $@"
  terraform import -var-file "../../config/$workspace/$stack.tfvars" $@
}

tfws() {
  # format of $1 should be $scope-$workspace, e.g. internal-stag
  workspace=$(echo $1 | cut -d- -f2)
  export AWS_PROFILE=$1 && terraform workspace select $workspace
}


