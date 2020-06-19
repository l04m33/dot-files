APPINST_PATH="$HOME/app_inst"

if [ -d "$APPINST_PATH" ]; then
    export APPINST_PATH
    APPINST_BINS=$(echo $APPINST_PATH/*/bin | tr -s '[:blank:]' ':')
fi
if [ -n "$APPINST_BINS" ]; then
    export PATH=$APPINST_BINS:$PATH
fi

GUIX_ENV_FILE="$HOME/.config/guix/current/etc/profile"
GUIX_PROFILE_ENV_FILE="$HOME/.guix-profile/etc/profile"
for ef in "$GUIX_ENV_FILE" "$GUIX_PROFILE_ENV_FILE"; do
    if [ -f "$ef" ]; then
        source "$ef"
        export GUIX_PROFILE=${ef:h:h}
    fi
done
if [ -n "$GUIX_PROFILE" ]; then
    export GUIX_LOCPATH=$GUIX_PROFILE/lib/locale
fi

export EDITOR=$(if which kak > /dev/null 2>&1; then echo kak; else echo vim; fi)

# Aliases
