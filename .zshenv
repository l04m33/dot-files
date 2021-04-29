#
# When I log in to a non-login and non-interactive shell via SSH, e.g.
#
#     ssh me@server 'echo $PATH'
#
# This is the ONLY init file that would be executed by zsh. I have no
# choice but to assemble the PATH right here, so that all profiles from
# my Guix setup can be accessed properly.
#
# Yet, this file would be executed EVERYTIME a zsh is spawned, hence
# the __ZSH_ENV_INITIALIZED__ variable. To update the PATH, one needs
# to log out and then log back in, or unset this variable.
#

if [ -z "$__ZSH_ENV_INITIALIZED__" ]; then
    GLOBAL_ETC_PROFILE="/etc/profile"
    if [ -f "$GLOBAL_ETC_PROFILE" ]; then
        source "$GLOBAL_ETC_PROFILE"
    fi

    source $HOME/.dot-files/.oh-my-zsh_custom/plugins/my-env/my-env.plugin.zsh

    export __ZSH_ENV_INITIALIZED__=y
fi

#
# The user-defined functions are NOT automatically "propagated" to
# child shells, so we need to define them everytime a new shell is
# spawned. Just... keep the functions defined here slim and simple.
#
source $HOME/.dot-files/.oh-my-zsh_custom/plugins/my-functions/my-functions.plugin.zsh
