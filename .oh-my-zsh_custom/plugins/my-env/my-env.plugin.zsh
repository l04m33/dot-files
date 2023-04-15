APPINST_PATH="$HOME/app_inst"

if [ -d "$APPINST_PATH" ]; then
    export APPINST_PATH
    APPINST_BINS=$(echo $APPINST_PATH/*/bin | tr -s '[:blank:]' ':')
fi
if [ -n "$APPINST_BINS" ]; then
    export PATH=$APPINST_BINS:$PATH
fi

if command -V emacsclient > /dev/null 2>&1; then
    export EDITOR="emacsclient -a '' -t"
    export VISUAL="emacsclient -a '' -c"
    alias ec='emacsclient -a "" -t'
    alias ecc='emacsclient -a "" -c'
else
    if command -V kak > /dev/null 2>&1; then
        export EDITOR="kak"
    else
        export EDITOR="vim"
        export VISUAL="gvim"
    fi
fi

if [ -e "$HOME/.local_env" ]; then
    . "$HOME/.local_env"
fi
