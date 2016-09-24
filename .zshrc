if [ ! -d "$HOME/.antigen" ]; then 
    git clone https://github.com/zsh-users/antigen.git "$HOME/.antigen"
fi
source $HOME/.antigen/antigen.zsh

antigen use oh-my-zsh

# libraries
antigen bundle mafredri/zsh-async

# plugins
antigen bundle robbyrussell/oh-my-zsh plugins/z
antigen bundle robbyrussell/oh-my-zsh plugins/git
antigen bundle robbyrussell/oh-my-zsh plugins/gpg-agent
antigen bundle robbyrussell/oh-my-zsh plugins/bgnotify
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle unixorn/autoupdate-antigen.zshplugin
#antigen bundle l04m33/dot-files .oh-my-zsh_custom/plugins/my-env/

# themes
antigen bundle sindresorhus/pure

antigen apply

source $HOME/.dot-files/.oh-my-zsh_custom/plugins/my-env/my-env.plugin.zsh
