# {{{ EXPORTS

# I believe this fixes android-studio gray screen on arch linux
export _JAVA_AWT_WM_NONREPARENTING=1

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

# TODO: You have to manually set npm global install to specified path with the command blow:
# npm config set prefix '~/.npm-global'

# Add npm bin path
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
alias cleanup='sudo pacman -Rsn $(pacman -Qtdq)'
alias update='yay -Syu && xmonad --recompile && xmonad --restart'

# List installed packages
alias installed-packages='comm -23 <(pacman -Qqett | sort) <(pacman -Qqg base-devel | sort | uniq)'

# Add color
alias grep='grep --color=auto'

# ls shortcuts
alias ls='ls --human-readable --color=auto --group-directories-first'
alias sl='ls'
alias  l='ls'
alias ll='ls -l'
alias la='ls -A'
alias lla='ls -llA'

# Confirm before overwriting
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i --one-file-system'

# Use trash
alias r='trash'

# Youtube best audio/video formats, ignore errors
alias youtube-m4a='yt-dlp --audio-quality 0 --audio-format m4a --extract-audio --ignore-errors'
alias youtube-mp3='yt-dlp --audio-quality 0 --audio-format mp3 --extract-audio --ignore-errors'
alias youtube-mp4='yt-dlp --format mp4 --ignore-errors'

# Locally serve a website
alias serve='browser-sync start --server --files . --no-notify --host 127.0.0.1 --port 9000'

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
alias scc='cd /home/twoonesecond/School/Composition-II; ls'
alias sce='cd /home/twoonesecond/School/East-Asian-Cultures; ls'
alias scs='cd /home/twoonesecond/School/Statistics; ls'
alias scb='cd /home/twoonesecond/School/Intro-to-Business; ls'
alias scw='cd /home/twoonesecond/School/World-Regional-Geography; ls'

# Move to tor folder
alias tr='cd /home/twoonesecond/.local/share/torbrowser/tbb/x86_64/tor-browser_en-US/Browser/Downloads; ls'

# SSH Into raspberrypi
alias pi='ssh pi@192.168.1.223'

alias td='nvim ~/todo'

alias gstatus='git status'
alias gclone='git clone'
alias gadd='git add'
alias gcommit='git commit'
alias gpush='git push'
alias gpull='git pull'
alias gdiff='git diff'

function calc {
  python -c "print($@)"
}

function generate-key {
  string=$( ls -lA ~/.ssh )
  if [[ $string == *"pub"* ]]; then
      echo "Public key already exists"
      return
  fi
  ssh-keygen -t ed25519 -C "gaston95g@gmail.com"
  eval "$(ssh-agent -s)"
  ssh-add ~/.ssh/id_ed25519
  xclip -sel clip ~/.ssh/id_ed25519.pub && echo "Copied public key to clipboard"
  echo "To copy again: xclip -sel clip ~/.ssh/id_ed25519.pub"
  echo "https://github.com/settings/ssh/new"
}

function mount_media {
    sudo cryptsetup luksOpen UUID=6f151bf8-1f72-452d-b16f-5e4782d7c910 Files
    sudo mount /dev/mapper/Files /mnt/Files
}

function bind_media {
    sudo chgrp -R media media_files/
    find media_files/ -type f -exec chmod 664 {} +
    find media_files/ -type d -exec chmod 775 {} +
    find media_files/ -type d -exec chmod g+s {} +

function translate_ep_ts {
    original_name=$( ls | grep -Pi "\[S$1E0?$2\] .*ts" )
    new_name="${original_name%%.ts}.mp4"

    if [ -z "$original_name" ]; then
	echo "No string found for S$1E$2"
	return
    fi

    echo "Season $1, Episode $2: $original_name"
    echo ""
    echo "ffmpeg -i \"$original_name\" -map 0 -c copy \"$new_name\""

    ffmpeg -i "$original_name" -map 0 -c copy "$new_name"
    mv "$original_name" "old/$original_name"
}

function translate_ts {
    original_name="$1"
    new_name="${original_name%%.ts}.mp4"
    #ffmpeg -i "$original_name" -map 0 -c copy "$new_name"
    ffmpeg -i "$original_name" -c copy "$new_name" && trash "$original_name" || rm -f "$new_name"
}

function translate_mkv {
    original_name="$1"
    new_name="${original_name%%.mkv}.mp4"
    #ffmpeg -i "$original_name" -map 0 -c copy "$new_name"
    ffmpeg -i "$original_name" -c copy "$new_name" && trash "$original_name" || rm -f "$new_name"
}

# }}}
