export PATH="/opt/homebrew/bin:$PATH"
export PATH="/opt/homebrew/opt/mysql-client/bin:$PATH"
export PATH="/usr/local/bin:$PATH"

export LANG=en_US.UTF-8

# yarn
export PATH="$HOME/.yarn/bin:$PATH"


# zsh
HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

# composer
export PATH="$HOME/.composer/vendor/bin:$PATH"

# doom
export PATH="$HOME/.emacs.d/bin:$PATH"

# custom scripts
export PATH="$HOME/Scripts:$PATH"

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

# go
export GOPATH="$HOME/go"
export PATH="${PATH}:${GOPATH}/bin"

# rust
. "$HOME/.cargo/env"
