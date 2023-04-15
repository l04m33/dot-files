autoload -U compinit && compinit

export ZSH_RC_MGR_HOME="${HOME}/.zgen"
if [ ! -d "${ZSH_RC_MGR_HOME}" ]; then
    git clone --branch master 'https://github.com/tarjoilija/zgen.git' "${ZSH_RC_MGR_HOME}"
fi
source ${ZSH_RC_MGR_HOME}/zgen.zsh

if ! zgen saved; then

    zgen load mafredri/zsh-async . main

    zgen oh-my-zsh
    zgen oh-my-zsh plugins/z
    zgen oh-my-zsh plugins/bgnotify

    zgen load zsh-users/zsh-syntax-highlighting
    zgen load sindresorhus/pure pure.zsh main

    zgen save
fi

if command -V emacsclient > /dev/null 2>&1; then
    alias ec='emacsclient -a "" -t'
    alias ecc='emacsclient -a "" -c'
fi

source $HOME/.dot-files/.oh-my-zsh_custom/plugins/my-gpg-agent/my-gpg-agent.plugin.zsh
