
autoload -U compinit
compinit
autoload -U colors
colors

setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt correct
setopt list_packed
setopt nolistbeep
setopt noautoremoveslash

export LANG=ja_JP.UTF-8
export EDITOR=vim
export LESS="-R"
export PATH="$HOME/.local/bin:$PATH"

# ターミナルの色
if [ -e /usr/share/terminfo/*/xterm-256color ]; then
  export TERM=xterm-256color
else
  export TERM=xterm-color
fi

# 補完
zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'
zstyle ':completion:*' list-colors ''

# prompt
case ${UID} in
0)
	# root
	PROMPT="%B%{$fg[red]%}%m%{$fg[blue]%} %~ %{$fg[red]%}# %{$reset_color%}"
	;;
*)
	PROMPT="%B%{$fg[green]%}%n@%m%{$fg[blue]%} %~ $ %{$reset_color%}"
	;;
esac

# 履歴
HISTFILE=~/.zsh_history
HISTSIZE=100000000
SAVEHIST=100000000
setopt hist_ignore_dups
setopt hist_ignore_space
setopt share_history
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end

# key binding
bindkey -v
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

# alias
alias l=ls ls='ls -h --color=auto' la='ls -a' ll='ls -l' lla='ls -al'
alias df='df -h' du='du -h'
alias tf='tail -f'
alias grep='grep --color'
alias gs='git status'
alias gd='git diff'
alias gdc='git diff --cached'
alias gl='git log -5'
alias gl1='git log --oneline'
alias bexec='bundle exec'
alias bruby='bexec ruby'
alias bspring='bexec spring'
alias brails='bundle exec rails'
alias bpadrino='bundle exec padrino'
alias brake='noglob bundle exec rake'
alias bpry='bundle exec pry'
alias srails='bundle exec spring rails'
alias srake='noglob bundle exec spring rake'
alias spec='bspring rspec'
alias q='exit'
alias curl='curl -s'
alias -g G='| grep --line-buffered' H='| head' T='| tail' L='| less'
alias -g JQ='| noglob jq' JQ.='JQ .'

# rbenv
if [ -e ~/.rbenv ] ; then
  export PATH="$HOME/.rbenv/bin:$PATH"
  eval "$(rbenv init -)"
fi

# direnv
if (( $+commands[direnv] )) ; then
  eval "$(direnv hook zsh)"
fi

# keychain
if (( $+commands[keychain] )); then
  eval $(keychain --eval)
fi

function use_gnu_tools() {
  # BSD系が面倒になったらこの関数を呼び出す
  cmds=(base64 basename cat chcon chgrp chmod chown chroot cksum comm cp csplit cut date dd dir dircolors md5sum)
  cmds=($cmds dirname du echo env expand expr factor false find fmt fold head hostid id install join link ln logname)
  cmds=($cmds mkdir mkfifo mknod mktemp mv nice nl nohup nproc numfmt od paste pathchk pinky pr printenv printf ptx pwd)
  cmds=($cmds readlink realpath rm rmdir runcon seq sha1sum sha224sum sha256sum sha384sum sha512sum shred shuf sleep)
  cmds=($cmds sort split stat stdbuf stty sum sync tac tail tee test timeout touch tr true truncate tsort tty uname)
  cmds=($cmds unexpand uniq unlink users vdir wc who whoami yes)
  for cmd in $cmds; do
    if (( $+commands[g$cmd] )) ; then
      alias $cmd=g$cmd
    fi
  done

  # g[
  if (( ${+commands[g\[]} )) ; then
    alias "["="g\["
  fi

  # df
  if (( $+commands[gdf] )) ; then
    alias df="gdf -h"
  fi

  # gls
  if (( $+commands[gls] )) ; then
    alias ls="gls -h --color"
  fi

  # ggrep
  if (( $+commands[ggrep] )) ; then
    alias grep="ggrep --color"
  fi
}

function do_enter() {
  if [ -n "$BUFFER" ]; then
    zle accept-line
    return 0
  fi
  echo

  ls
  if [ "$(git rev-parse --is-inside-work-tree 2> /dev/null)" = true ]; then
    echo
    git status -sb .
  fi
  echo
  echo
  zle reset-prompt
  return 0
}

env_dev() {

  precmd () {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
  }

  autoload -Uz vcs_info
  zstyle ':vcs_info:*' formats '[%b]'
  zstyle ':vcs_info:*' actionformats '[%b|%a]'
  zstyle ':vcs_info:(svn|bzr):*' branchformat '%Qb:r%r'
  zstyle ':vcs_info:bzr:*' use-simple true

  autoload -Uz is-at-least
  if is-at-least 4.3.10; then
    zstyle ':vcs_info:git:*' check-for-changes true
    zstyle ':vcs_info:git:*' stagedstr '+'
    zstyle ':vcs_info:git:*' unstagedstr '-'
    zstyle ':vcs_info:git:*' formats '[%b] %c%u'
    zstyle ':vcs_info:git:*' actionformats '[%b|%a] %c%u'
  fi

  zle -N do_enter
  bindkey '^m' do_enter

  case ${UID} in
  0)
    # root
    PROMPT="%{$fg[red]%}[%n@%m]%{$reset_color%}"
    ;;
  *)
    PROMPT="%{$fg[green]%}[%n@%m]%{$reset_color%}"
    ;;
  esac
  PROMPT="
$PROMPT %{${fg[blue]}%}%~%{${reset_color}%}
%(?.%{$fg[green]%}(*'-') <.%{$fg[red]%}(*;-;%)?<)%{${reset_color}%} "
  PROMPT2="       %(?.%{$fg[green]%}.%{$fg[red]%})<%{${reset_color}%} "
  SPROMPT="%{$fg[red]%}%{$suggest%}(*'~'%)?< もしかして? %B%r%b %{$fg[red]%} [nyae]:${reset_color} "
  RPROMPT="%1(v|%F{green%1v%f|)"
}

# 開発環境ではzshrc_localに以下を記述する
#env_dev

if [ -e ~/.zshrc_local ] ; then
	source ~/.zshrc_local
fi

