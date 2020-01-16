if [ ! -d "$HOME/.antigen" ]; then 
    git clone --branch master https://github.com/zsh-users/antigen.git "$HOME/.antigen"
fi
source $HOME/.antigen/antigen.zsh

antigen use oh-my-zsh

# libraries
antigen bundle mafredri/zsh-async

# oh-my-zsh bundled plugins
antigen bundle z
antigen bundle git
antigen bundle bgnotify

# external plugins
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle fanzeyi/zsh-at
antigen bundle unixorn/autoupdate-antigen.zshplugin
#antigen bundle l04m33/dot-files .oh-my-zsh_custom/plugins/my-env/

# themes
antigen bundle sindresorhus/pure

antigen apply

source $HOME/.dot-files/.oh-my-zsh_custom/plugins/my-env/my-env.plugin.zsh
source $HOME/.dot-files/.oh-my-zsh_custom/plugins/my-gpg-agent/my-gpg-agent.plugin.zsh
