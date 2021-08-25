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

# ls shortcuts
alias ls='ls --color=auto --group-directories-first'
alias sl='ls'
alias  l='ls'
alias ll='ls -l'
alias la='ls -a'

# Confirm before overwriting
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'

# List installed packages
alias installed-packages='comm -23 <(pacman -Qqett | sort) <(pacman -Qqg base-devel | sort | uniq)'

# Youtube best audio/video formats, ignore errors
alias youtube-mp3='youtube-dl -f bestaudio --audio-quality 0 --audio-format mp3 -i -x --extract-audio'
alias youtube-mp4='youtube-dl -f "bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best" -i'

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

# Terminal rickroll
alias rr='curl -s -L https://raw.githubusercontent.com/keroserene/rickrollrc/master/roll.sh | bash'

# Move to school folder
alias sc='cd /home/twoonesecond/School; ls'

# SSH Into raspberrypi
alias pi='sshpass -p 0165 ssh pi@192.168.1.208'

# }}}
