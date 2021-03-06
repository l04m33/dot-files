function load_guix_profile {
    if [ $# -gt 0 ]; then
        for pf_name in $@; do
            local GUIX_PROFILE="${HOME}/.adhoc-profiles/${pf_name}"
            if [ -f "${GUIX_PROFILE}/etc/profile" ]; then
                source "${GUIX_PROFILE}/etc/profile"
                printf "Profile loaded: %s\n" "${GUIX_PROFILE}"
            else
                printf "Not a loadable profile: %s\n" "${GUIX_PROFILE}" 1>&2
            fi
        done
    else
        printf "No profile specified.\n" 1>&2
    fi
}

alias lgp=load_guix_profile
