# {{{ EXPORTS

# Setting default programs
export EDITOR="nvim"			# Set nvim as default text editor

# Setting proper history exports
export HISTFILESIZE=1000000		# Allow large history file size
export HISTSIZE=1000000			# Allow large number of commands to be stored
export HISTCONTROL=ignoredups:erasedups	# Ignore duplicate commands
export HISTTIMEFORMAT='%F %T '		# Set time format to be 'YYYY-MM-DD HH:MM:SS '
export PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r" # After each command, append to history and reread it

# }}}

# Set vi mode in shell
set -o vi

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Prompt
PS1='[\u@\h \W]\$ '

# {{{ Adding binaries to path

# Add ~/.local/bin
if [ -d "$HOME/.local/bin" ]; then
	PATH="$HOME/.local/bin${PATH:+:${PATH}}"
fi
# Add ~/bin
if [ -d "$HOME/bin" ]; then
	PATH="$HOME/bin${PATH:+:${PATH}}"
fi
# Add npm
if [ -d "$HOME/.npm-global/bin" ]; then
	PATH="$HOME/.npm-global/bin:$PATH"
fi

# }}}

# {{{ SHOPT

shopt -s autocd		# Change dirs without "cd"
shopt -s cdspell	# Autocorrect mispelled dir names
shopt -s cmdhist	# Save multi-line commands as single line in history
shopt -s dotglob	# ???
shopt -s histappend	# Append history instead of overwriting
shopt -s expand_aliases	# Expand aliases

# }}}

bind "set completion-ignore-case on"	# Ignore upper/lower case for tab completion

# {{{ FILE EXTRACTION

ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1   ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

# }}}

# {{{ ALIASES

# Package management
alias pacsyu='sudo pacman -Syyu'
alias yaysyu='yay -Syu --noconfirm'
alias cleanup='sudo pacman -Rsn $(pacman -Qtdq)'
alias update='yay -Syu && xmonad --recompile && xmonad --restart'

# Add color
alias grep='grep --color=auto'

# ls shortcuts
alias ls='ls --color=auto --group-directories-first'
alias sl='ls'
alias  l='ls'
alias ll='ls -l'
alias la='ls -a'

# Confirm before overwriting
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i --one-file-system'

# Use trash
alias r='trash'

# List installed packages
alias installed-packages='comm -23 <(pacman -Qqett | sort) <(pacman -Qqg base-devel | sort | uniq)'

# Youtube best audio/video formats, ignore errors
alias youtube-m4a='yt-dlp --audio-quality 0 --audio-format m4a --extract-audio --ignore-errors'
alias youtube-mp3='yt-dlp --audio-quality 0 --audio-format mp3 --extract-audio --ignore-errors'
alias youtube-mp4='yt-dlp --format mp4 --ignore-errors'

# Locally serve a website
alias serve='browser-sync start -s -f . --no-notify --host 127.0.0.1 --port 9000'

# Make keepass readable
alias keepass='QT_QPA_PLATFORMTHEME=qt5ct keepassxc'

# Mount/unmount android easily
alias mount-android='simple-mtpfs --device 1 ~/mnt'
alias unmount-android='fusermount -u ~/mnt'

# Set brightness for day/night manual
alias night='redshift -x >/dev/null && redshift -O 4500 -g 0.65 >/dev/null'
alias day='redshift -x >/dev/null'

# Terminal bin
alias tb='nc termbin.com 9999'

alias sourcebash='source ~/.bashrc'

# Terminal rickroll
alias rr='curl -s -L https://raw.githubusercontent.com/keroserene/rickrollrc/master/roll.sh | bash'

# Move to school folder
alias sc='cd /home/twoonesecond/School; ls'
alias scc='cd /home/twoonesecond/School/Composition-1; ls'
alias sce='cd /home/twoonesecond/School/Env-Sci; ls'
alias scl='cd /home/twoonesecond/School/Env-Sci-Lab; ls'
alias scu='cd /home/twoonesecond/School/US-History; ls'
alias scw='cd /home/twoonesecond/School/Western-Civ; ls'

# Move to tor folder
alias tr='cd /home/twoonesecond/.local/share/torbrowser/tbb/x86_64/tor-browser_en-US/Browser/Downloads; ls'

# SSH Into raspberrypi
alias pi='ssh pi@192.168.1.223'

alias td='nvim ~/todo'

alias gstatus='git status'
alias gadd='git add'
alias gcommit='git commit'
alias gpush='git push'
alias gpull='git pull'
alias gdiff='git diff'

function calc {
  python -c "print($@)"
}

# }}}
