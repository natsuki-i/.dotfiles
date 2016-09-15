#!/bin/sh

export LANG=ja_JP.UTF-8

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ x "$f" ] && . "$f"
  done
fi

# input method
if type fcitx ; then
  export GTK_IM_MODULE=fcitx
  export QT_IM_MODULE=fcitx
  export XMODIFIERS=@im=fcitx
  export DefaultIMModule=fcitx
  fcitx -d
fi

# https://wiki.haskell.org/Xmonad/Frequently_asked_questions#Problems_with_Java_applications.2C_Applet_java_console
export _JAVA_AWT_WM_NONREPARENTING=1

xsetroot -cursor_name left_ptr

# background
if [ -d $HOME/.wallpaper ]; then
  feh --bg-center --randomize --recursive $HOME/.wallpaper &
fi

# screen saver
if type xscreensaver ; then
  xscreensaver -no-splash &
fi

exec xmonad
