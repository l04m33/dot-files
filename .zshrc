if [ ! -d "$HOME/.antigen" ]; then 
    git clone https://github.com/zsh-users/antigen.git "$HOME/.antigen"
fi
source $HOME/.antigen/antigen.zsh

antigen use oh-my-zsh

antigen bundle robbyrussell/oh-my-zsh plugins/git
antigen bundle robbyrussell/oh-my-zsh plugins/ssh-agent
antigen bundle zsh-users/zsh-syntax-highlighting
#antigen bundle l04m33/dot-files .oh-my-zsh_custom/plugins/my-env/

antigen theme robbyrussell/oh-my-zsh themes/pure

antigen apply

source $HOME/.dot-files/.oh-my-zsh_custom/plugins/my-env/my-env.plugin.zsh
