export XMODIFIERS=@im=ibus
export GTK_IM_MODULE=ibus
export QT_IM_MODULE=ibus

if [ -e "$HOME/.profile_local" ]; then
    . "$HOME/.profile_local"
fi
