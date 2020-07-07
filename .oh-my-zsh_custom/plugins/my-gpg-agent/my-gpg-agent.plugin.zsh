local GPG_AGENT_BIN=$(which gpg-agent)
local GPG_AGENT_ENV="$HOME/.gnupg/gpg-agent.env"
local GPG_CONNECT_AGENT_ERR_MSG="gpg-connect-agent: no gpg-agent running in this session"

function start_agent_withssh {
    ${GPG_AGENT_BIN} --quiet --enable-ssh-support --daemon 2> /dev/null > "${GPG_AGENT_ENV}"
    chmod 600 "${GPG_AGENT_ENV}"
    . "${GPG_AGENT_ENV}" > /dev/null
}

if [[ "$(gpg-connect-agent --no-autostart --quiet /bye 2>&1)" == "$GPG_CONNECT_AGENT_ERR_MSG" ]]; then
    start_agent_withssh
elif [[ -f "${GPG_AGENT_ENV}" ]]; then
    . "${GPG_AGENT_ENV}" > /dev/null
fi

GPG_TTY=$(tty)
export GPG_TTY
