#setopt PUSHD_INGORE_DUPS
bindkey "\e" vi-cmd-mode
bindkey "jk" vi-cmd-mode
bindkey '^O' push-line
autoload -U url-quote-magic
zle -N self-insert url-quote-magic
export EDITOR="vim"

exec 2>>(while read line; do
print '\e[91m'${(q)line}'\e[0m' > /dev/tty;
done &)

local knownhosts
knownhosts=( ${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[0-9]*}%%\ *}%%,*} )
zstyle ':completion:*:(ssh|scp|sftp):*' hosts $knownhosts
zstyle ':completion:*' use-ip true

# number of lines kept in history
export HISTSIZE=10000
# number of lines saved in the history after logout
export SAVEHIST=10000
# location of history
export HISTFILE=~/.zhistory
# append command to history file once executed
setopt inc_append_history
#setopt noautomenu

#export LD_LIBRARY_PATH=/usr/local/lib
#export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig




#PS1
BLACK="%{"$'\033[01;30m'"%}"
GREEN="%{"$'\033[01;32m'"%}"
RED="%{"$'\033[01;31m'"%}"
YELLOW="%{"$'\033[01;33m'"%}"
BLUE="%{"$'\033[01;34m'"%}"
BOLD="%{"$'\033[01;39m'"%}"
NORM="%{"$'\033[00m'"%}"
#export PS1="${RED}%n${NORM} AT ${BLUE}%m ${GREEN}(%T)${NORM} %#"
#will look like this (with appropriate colors) %T is the current time
#user AT mycomputer (22:33) %

export PS1='%n@%m [ %~ ]%0(?..:%?) %#'
#user@mycomputer [ /current/working/dir ] numero de error si != 0%

autoload -U compinit promptinit
compinit
promptinit;
setopt AUTO_CD               # implicate cd for non-commands
setopt CD_ABLE_VARS       # read vars in cd
setopt CORRECT            # correct spelling
setopt COMPLETE_IN_WORD    # complete commands anywhere in the word
setopt NOTIFY              # Notify when jobs finish
setopt C_BASES             # 0xFF
setopt BASH_AUTO_LIST      # Autolist options on repeition of ambiguous args
setopt CHASE_LINKS         # Follow links in cds
setopt AUTO_PUSHD          # Push dirs into history
setopt ALWAYS_TO_END       # Move to the end on complete completion
setopt LIST_ROWS_FIRST     # Row orientation for menu
setopt MULTIOS             # Allow Multiple pipes
setopt MAGIC_EQUAL_SUBST   # Expand inside equals
setopt EXTENDED_GLOB
setopt SHARE_HISTORY
setopt HIST_IGNORE_DUPS
setopt prompt_subst


autoload colors
colors

(ls --help 2>/dev/null |grep -- --color=) >/dev/null && \

alias -g HL='| head -20'
alias -g H='| head'
alias -g TL='| tail -20'
alias -g T='| tail'
alias -g tr='-ltr'
alias -g X='| xclip'
alias -g L='| less'
alias -g G='| grep '

export JAVA_HOME="/usr/lib/java"
export PATH="${JAVA_HOME}/bin:${PATH}"
#alias java='/usr/lib/java/bin/java'
#alias javac='/usr/lib/java/bin/javac'
alias \:q=exit
alias lsd='ls -d *(/)'
alias gvi='gvim'
alias lst='ls -ltr'
alias ls='ls  --color=auto'
alias lsç='ls  --color=auto'
alias ll='ls -las  --color=auto'
alias -g perl6='~/rakudo/perl6'
alias ks='ls'
alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'


alias ts="prove --state=slow,save -j3 -r"
alias tf="prove --state=failed,save -v -r"


alias sshuni='ssh alu9854@exthost.etsii.ull.es'      
# 4oU8zN
alias screenuni='screen -t sshuni -c ~/.screenrc2'

#alias hide='echo -en "\033]50;nil2\007"'
alias tiny='echo -en "\033]50;-misc-fixed-medium-r-normal--8-80-75-75-c-50-iso10646-1\007"'
alias small='echo -en "\033]50;6x10\007"'
alias default='echo -e "\033]50;-misc-fixed-medium-r-semicondensed--12-*-*-*-*-*-iso10646-1\007"'
alias medium='echo -en "\033]50;-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso10646-1\007"'
alias large='echo -en "\033]50;-misc-fixed-medium-*-*-*-15-*-*-*-*-*-iso10646-1\007"'
## This is a large font that has a corresponding double-width font for
## CJK and other characters, useful for full-on utf-8 goodness.
alias larger='echo -en "\033]50;-misc-fixed-medium-r-normal--18-*-*-*-*-*-iso10646-1\007"'
alias huge='echo -en "\033]50;-misc-fixed-medium-r-normal--20-200-75-75-c-100-iso10646-1\007"'
alias normal=default




#screen integration to set caption bar dynamically
function title {
if [[ $TERM == "screen"  || $TERM == "screen-256color-bce"|| $TERM == "screen-256color" || $TERM == "screen.linux" ]]; then
	# Use these two for GNU Screen:
	print -nR $'\033k'$1$'\033'\\\

	print -nR $'\033]0;'$2$'\a'
elif [[ $TERM == "xterm" ||$TERM == "rxvt-unicode" || $TERM == "urxvt" ]]; then
	# Use this one instead for XTerms:
	print -nR $'\033]0;'$*$'\a'
fi
}

function precmd {
title zsh "urxvt $PWD"
echo -ne '\033[?17;0;127c'
}

#precmd () {
    #print -n '\017'
#}



function preexec {
emulate -L zsh
local -a cmd; cmd=(${(z)1})
if [[ $cmd[1]:t == "ssh" ]]; then
    title "@"$cmd[2] "urxvt $cmd"
elif [[ $cmd[1]:t == "sudo" ]]; then
    title "#"$cmd[2]:t "urxvt $cmd[3,-1]"
elif [[ $cmd[1]:t == "for" ]]; then
    title "()"$cmd[7] "urxvt $cmd"
elif [[ $cmd[1]:t == "svn" ]]; then
    title "$cmd[1,2]" "urxvt $cmd"
else
    title $cmd[1]:t "urxvt $cmd[2,-1]"
fi
}

function joinpdf {
gs -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile=merged.pdf "$@"
}

#SSHAGENT=/usr/bin/ssh-agent
#SSHAGENTARGS="-s"
#if [ -z "$SSH_AUTH_SOCK" -a -x "$SSHAGENT" ]; then
#    eval `$SSHAGENT $SSHAGENTARGS`
#    trap "kill $SSH_AGENT_PID" 0
#    fi

function xpwd () { pwd | xclip -i ; xclip -o ; }
function xcd () { cd `xclip -o` ; }

export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

#function zle-line-init zle-keymap-select {
#RPS1="${${KEYMAP/vicmd/-- NORMAL --}/(main|viins)/-- INSERT --}"
#RPS2=$RPS1
#RPS3=$RPS1
#RPS4=$RPS1
#zle reset-prompt
   #}

   #zle -N zle-line-init

zstyle ':completion:*' menu select=3

function cd () {
if [[ -f $1 ]]; then
	builtin cd $1:h
else
	builtin cd $1
fi
}

export CPLUS_INCLUDE_PATH=/usr/local/include/eo:"$CPLUS_INCLUDE_PATH"
export LIBRARY_PATH=/usr/local/lib:"$LIBRARY_PATH"

#function hg_prompt_info {
    #hg prompt --angle-brackets "\
#< on %{$fg[magenta]%}<branch>%{$reset_color%}>\
#< at %{$fg[yellow]%}<tags|%{$reset_color%}, %{$fg[yellow]%}>%{$reset_color%}>\
#%{$fg[green]%}<status|modified|unknown><update>%{$reset_color%}<
##patches: <patches|join( → )|pre_applied(%{$fg[yellow]%})|post_applied(%{$reset_color%})|pre_unapplied(%{$fg_bold[black]%})|post_unapplied(%{$reset_color%})>>" 2>/dev/null
#}

function hg_prompt_info {
    hg prompt --angle-brackets "\
< on %{$fg[magenta]%}<branch>%{$reset_color%}>\
%{$fg[green]%}<status|modified|unknown><update>%{$reset_color%}%{$reset_color%}" 2>/dev/null
}

function prompt_char {
    hg root >/dev/null 2>/dev/null && echo '☿' && return
    git branch >/dev/null 2>/dev/null && echo '±' && return
    echo '○'
}

PROMPT='%{$fg[magenta]%}%n%{$reset_color%} at %{$fg[yellow]%}%m%{$reset_color%} in %{$fg_bold[green]%}${PWD/#$HOME/~}%{$reset_color%}$(hg_prompt_info)'
#RPROMPT='$(prompt_char)'
# ls -l **/hgrc(-D)  locates hgrc files
