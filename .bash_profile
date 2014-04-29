# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs

export PATH=$PATH:$HOME/.local/bin:$HOME/bin
if [ -d "$HOME/app_inst" ]; then
    export APPINST_PATH=$HOME/app_inst
    APPINST_BINS=$(echo $APPINST_PATH/*/bin | sed 's/[ \t]\+/:/g')
fi
if [ -n "$APPINST_BINS" ]; then
    export PATH=$APPINST_BINS:$PATH
fi

export EDITOR=vim

# history options
export HISTCONTROL=ignoredups:erasedups
shopt -s histappend

# virtualenvwrapper
if which virtualenvwrapper.sh > /dev/null 2>&1; then
    export WORKON_HOME=$HOME/.virtualenvs
    if which python3 > /dev/null 2>&1; then
        export VIRTUALENVWRAPPER_PYTHON="`which python3`"
        export VIRTUALENVWRAPPER_VIRTUALENV="`which pyvenv`"
    fi

    if [ ! -d "$WORKON_HOME" ]; then
        mkdir -p "$WORKON_HOME"
    fi

    source `which virtualenvwrapper.sh`
fi
