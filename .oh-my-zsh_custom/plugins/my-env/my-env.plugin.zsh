APPINST_PATH="$HOME/app_inst"

if [ -d "$APPINST_PATH" ]; then
    export APPINST_PATH
    APPINST_BINS=$(echo $APPINST_PATH/*/bin | tr -s '[:blank:]' ':')
fi
if [ -n "$APPINST_BINS" ]; then
    export PATH=$APPINST_BINS:$PATH
fi

export EDITOR=$(if which kak > /dev/null 2>&1; then echo kak; else echo vim; fi)

function load_guix_profile {
    if [ $# -gt 0 ]; then
        local pf_name=$1; shift
        local GUIX_PROFILE="${HOME}/.adhoc-profiles/${pf_name}"
        if [ -f "${GUIX_PROFILE}/etc/profile" ]; then
            source "${GUIX_PROFILE}/etc/profile"
            printf "Profile loaded: %s\n" "${GUIX_PROFILE}"
        else
            printf "Not a loadable profile: %s\n" "${GUIX_PROFILE}" 1>&2
        fi
    else
        printf "No profile specified.\n" 1>&2
    fi
}

alias lgp=load_guix_profile
