export XMODIFIERS=@im=ibus
export GTK_IM_MODULE=ibus
export QT_IM_MODULE=ibus

export GDK_CORE_DEVICE_EVENTS=1

if [ -e "$HOME/.profile_local" ]; then
    . "$HOME/.profile_local"
fi
