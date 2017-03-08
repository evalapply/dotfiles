# ZPLUG plugin manager
if [[ ! -d ~/.zplug ]];then
    git clone https://github.com/b4b4r07/zplug ~/.zplug
fi

source ~/.zplug/init.zsh
# Async for zsh, used by pure
zplug "mafredri/zsh-async", from:github, defer:0
# Load completion library for those sweet [tab] squares
zplug "lib/completion", from:oh-my-zsh
# Up -> History search! Who knew it wasn't a built in?
zplug "lib/key-bindings", from:oh-my-zsh
# History defaults
zplug "lib/history", from:oh-my-zsh
# Adds useful aliases for things dealing with directories
zplug "lib/directories", from:oh-my-zsh
# gst, gco, gc -> All the git shortcut goodness
zplug "plugins/git", from:oh-my-zsh, if:"hash git"
# Syntax highlighting for commands
zplug "zsh-users/zsh-syntax-highlighting", from:github, defer:3
# Theme!
zplug "sindresorhus/pure", use:pure.zsh, from:github, as:theme
# Actually install plugins, prompt user input
if ! zplug check --verbose; then
    printf "Install zplug plugins? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

zplug load

# FZF fuzzy finder
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# PATH VARIABLES
export GOPATH=$HOME/Work/code/go
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:$HOME/Work/servers/wildfly/bin
export PATH=$PATH:$HOME/.npm-global/bin
export PATH="$HOME/.cargo/bin:$PATH"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/asqrd/Work/google-cloud-sdk/path.zsh.inc' ]; then source '/home/asqrd/Work/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/home/asqrd/Work/google-cloud-sdk/completion.zsh.inc' ]; then source '/home/asqrd/Work/google-cloud-sdk/completion.zsh.inc'; fi

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/asqrd/.sdkman"
[[ -s "/home/asqrd/.sdkman/bin/sdkman-init.sh" ]] && source "/home/asqrd/.sdkman/bin/sdkman-init.sh"

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
export PATH="$PATH:$HOME/.rvm/gems/ruby-2.4.0/bin"
