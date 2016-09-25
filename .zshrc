#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Customize to your needs...
#
# User configuration

  export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games"
  export PATH=${PATH}:~/Android/Sdk/tools
  export PATH=${PATH}:~/Android/Sdk/platform-tools
  export PATH=${PATH}:~/work/idea/bin/
  export PATH=${PATH}:~/scripts/
  export PATH=${PATH}:~/.cargo/bin/

export EDITOR="/usr/bin/nvim"
export VISUAL=$EDITOR

# Resty REST API Curl wrapper
source ~/resty

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Add environment variable for composer
export PATH="$HOME/.composer/vendor/bin:$PATH"

# Add environment variable NDK_ROOT for cocos2d-x
export NDK_ROOT=/home/apuesto89/work/android/Ndk
export PATH=$NDK_ROOT:$PATH

# Add environment variable ANDROID_SDK_ROOT for cocos2d-x
export ANDROID_SDK_ROOT=/home/apuesto89/work/android/Sdk
export PATH=$ANDROID_SDK_ROOT:$PATH
export PATH=$ANDROID_SDK_ROOT/tools:$ANDROID_SDK_ROOT/platform-tools:$PATH

# Add environment variable ANT_ROOT for cocos2d-x
export ANT_ROOT=/usr/share/ant/bin
export PATH=$ANT_ROOT:$PATH

# Add environment variable for go
export GOPATH="$HOME/work/go"
# optionally add bin dir
export PATH="$GOPATH/bin:$PATH"
# Rust bin
export RUST_SRC_PATH="/usr/local/src/rust/src"
export PATH="/home/apuesto89/.multirust/toolchains/stable/cargo/bin:$PATH"

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
