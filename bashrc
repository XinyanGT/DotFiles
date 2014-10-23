############################################################################
############################# Bash RC file #################################
############################################################################

# Mostly by whoever wrote the default debian/ubuntu bashrc, but a few tweaks
# and additions by Saul Reynolds-Haertle.

# If not running interactively, don't do anything
[ -z "$PS1" ] && return


############################################################################
####################### Environment Variable setup #########################
############################################################################
# setup editor variable
export EDITOR="vim"
export VISUAL="vim"

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
# ... or force ignoredups and ignorespace
export HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"






############################################################################
################################ Colors ####################################
############################################################################

# If we're in an xterm, change the colors and stuff. Otherwise leave them
# as is.
case "$TERM" in
    xterm*)
        # Color the primary prompt, and set it to something useful and short.
        #                command                    # color   part
        export     PS1="\[\e[1;31m\]["              # red     [
        export PS1=$PS1"\[\e[1;32m\]\u"             # green   user
        export PS1=$PS1"\[\e[1;32m\]@"              # green   @
        export PS1=$PS1"\[\e[1;32m\]\h"             # green   hostname
        export PS1=$PS1"\[\e[1;34m\] \W"            # cyan    directory
        export PS1=$PS1"\[\e[1;31m\]]$\["           # red     ]$
        export PS1=$PS1"\e[0;0m\] "                 # white   _
	    # export PS1="\[\e[31m\][\u@\h \W]\$\[\e[0m\] "

    	# attempt to change the xterm title.
	    export PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}\007"'
	    export TERM=xterm-256color
        ;;
    screen*)
        #                command                      color   part
        export     PS1="\[\e[1;31m\]["              # red     [
        export PS1=$PS1"\[\e[1;32m\]\u@\h"          # green   user@host
        export PS1=$PS1"\[\e[1;36m\] \W"            # blue    directory
        export PS1=$PS1"\[\e[1;31m\]]$\["           # red     ]$
        export PS1=$PS1"\e[0;0m\] "                 # white   _
	    # export PS1="\[\e[31m\][\u@\h \W]\$\[\e[0m\] "

    	# attempt to change the xterm title.
	    export PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}\007"'
        export TERM=screen-256color
        ;;
esac



############################################################################
################################ Aliases ###################################
############################################################################

# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable color support for ls some other programs where color is useful.
if [ -x /usr/bin/dircolors ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# A few very random shortcut aliases I use
alias more='less'
alias la='ls -a'
alias ll='ls -l'
alias lal='ls -al'
alias lla='ls -al'

# Some these are just plain weird, but I like them.
#alias irssi='su -c vebyast irssi'
alias octave='octave -q'

# display a recursive tree view of the filesystem
alias t='tree'
alias tp='tree -p'

# working with acls
alias ga='getfacl'
alias sa='setfacl'



###########################################################################
alias lm="ll --block-size=MB"
alias lk="ll --block-size=KB"
alias rm="rm -i"
alias sc='cd /lustre/atlas2/csc025/scratch/voidp'
alias bl='bpls -lav'
alias mu="cd ..; make; cd -"
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/ccs/home/voidp/Software/opencv/lib:/ccs/home/voidp/Software/fastbit-1-3-8/lib
export SC=/lustre/atlas2/csc025/scratch/voidp
export AD=$HOME/Software/adios-1.3.1
export ADS=$AD/src
export ADT=$AD/tests
module unload mxml
module unload adios
module load mxml
module load adios/1.7.0
module unload emacs
module load emacs/24.4

############################################################################
####################### Upgrade bash completion ############################
############################################################################

# enable programmable completion features.
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi



