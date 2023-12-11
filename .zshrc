#autoload -U compinit && compinit

export ZSH_RC_MGR_HOME="${HOME}/.zgenom"
if [ ! -d "${ZSH_RC_MGR_HOME}" ]; then
    git clone --branch main 'https://github.com/jandamm/zgenom.git' "${ZSH_RC_MGR_HOME}"
fi
source ${ZSH_RC_MGR_HOME}/zgenom.zsh

if ! zgenom saved; then

    zgenom load mafredri/zsh-async . main

    zgenom ohmyzsh
    zgenom ohmyzsh plugins/z
    zgenom ohmyzsh plugins/bgnotify

    zgenom load zsh-users/zsh-syntax-highlighting
    zgenom load sindresorhus/pure pure.zsh main

    zgenom save
fi

if command -V emacsclient > /dev/null 2>&1; then
    alias ec='emacsclient -a "" -t'
    alias ecc='emacsclient -a "" -c'
fi

source $HOME/.dot-files/.oh-my-zsh_custom/plugins/my-gpg-agent/my-gpg-agent.plugin.zsh
