APPINST_PATH="$HOME/app_inst"

if [ -d "$APPINST_PATH" ]; then
    export APPINST_PATH
    APPINST_BINS=$(echo $APPINST_PATH/*/bin | tr -s '[:blank:]' ':')
fi
if [ -n "$APPINST_BINS" ]; then
    export PATH=$APPINST_BINS:$PATH
fi

export EDITOR=$(if which kak > /dev/null 2>&1; then echo kak; else echo vim; fi)
