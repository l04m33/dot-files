if [ -z "$__ZSH_ENV_INITIALIZED__" ]; then
    GLOBAL_ETC_PROFILE="/etc/profile"
    if [ -f "$GLOBAL_ETC_PROFILE" ]; then
        source "$GLOBAL_ETC_PROFILE"
    fi

    source $HOME/.dot-files/.oh-my-zsh_custom/plugins/my-env/my-env.plugin.zsh

    export __ZSH_ENV_INITIALIZED__=y
fi
