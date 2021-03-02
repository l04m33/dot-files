autoload -U compinit && compinit

export ZSH_RC_MGR_HOME="${HOME}/.zgen"
if [ ! -d "${ZSH_RC_MGR_HOME}" ]; then
    git clone --branch master 'https://github.com/tarjoilija/zgen.git' "${ZSH_RC_MGR_HOME}"
fi
source ${ZSH_RC_MGR_HOME}/zgen.zsh

if ! zgen saved; then

    zgen load mafredri/zsh-async

    zgen oh-my-zsh
    zgen oh-my-zsh plugins/z
    zgen oh-my-zsh plugins/bgnotify

    zgen load zsh-users/zsh-syntax-highlighting
    zgen load sindresorhus/pure

    zgen save
fi

source $HOME/.dot-files/.oh-my-zsh_custom/plugins/my-env/my-env.plugin.zsh
source $HOME/.dot-files/.oh-my-zsh_custom/plugins/my-gpg-agent/my-gpg-agent.plugin.zsh
