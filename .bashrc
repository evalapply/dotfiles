source /usr/share/defaults/etc/profile
# Add KIEX to PATH
test -s "/home/asqrd/.kiex/scripts/kiex" && source "/home/asqrd/.kiex/scripts/kiex"
# Add Elixir to PATH
source /home/asqrd/.kiex/elixirs/elixir-1.5.1.env

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
test -s "/home/asqrd/.rvm/scripts/rvm" && source "/home/asqrd/.rvm/scripts/rvm"
export PATH="$PATH:$HOME/.rvm/bin"


# Add GOPATH
export GOPATH="$HOME/work/code/go"

### ALIASES ###

# yauto alias for solus packaging
alias fetchYml="$HOME/packages/common/Scripts/yauto.py"

# cd aliases
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."

### PROMPT ###

# Colors and Reset
RED="\[$(tput setaf 1)\]"
GREEN="\[$(tput setaf 2)\]"
YELLOW="\[$(tput setaf 3)\]"
BLUE="\[$(tput setaf 4)\]"
PURPLE="\[$(tput setaf 5)\]"
TEAL="\[$(tput setaf 6)\]"
WHITE="\[$(tput setaf 7)\]"

RESET="\[$(tput sgr0)\]"

export PS1="${BLUE}\u${YELLOW}@${TEAL}\h${WHITE}| ${PURPLE}\W ${WHITE}|${GREEN} $ ${RESET}"
