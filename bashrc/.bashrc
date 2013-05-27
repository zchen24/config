# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi





# load cisstvars.sh if exist
if [ -f ~/dev/cisst/build/cisstvars.sh ]; then
    . ~/dev/cisst/build/cisstvars.sh
fi


# uDrawGraph
if [ -f /usr/local/uDrawGraph-3.1/README.txt ]; then
    export UDG_HOME=/usr/local/uDrawGraph-3.1
    export PATH=$PATH:/usr/local/uDrawGraph-3.1/bin
fi


# .bash_zihan
if [ -f ~/.bash_zihan ]; then
    . ~/.bash_zihan
fi



# ROS RELATED
#source /opt/ros/fuerte/setup.bash
#source ~/fuerte/setup.bash
source ~/ros/groovy/setup.bash
export ROS_WORKSPACE=~/ros/groovy

alias rosresetup="source ./setup.sh"

export EDITOR='emacs -nw'
export ROS_PARALLEL_JOBS='-j6 -l6'
# export ROS_PACKAGE_PATH=$ROS_PACKAGE_PATH:~/dev/wyvern_lair
# export ROS_PACKAGE_PATH=$ROS_PACKAGE_PATH:~/dev/wyvern_lair/ros

# LAIR 
# export LAIR_CONFIG_PATH=~/dev/wyvern_lair/lair/config
# export LAIR_APP_PATH=~/dev/wyvern_lair/lair/apps
# export LAIR_DATABASE_PATH=~/dev/wyvern_lair/lair/sample_images
# export LAIR_ROOT_PATH=~/dev/wyvern_lair/lair


# ROS Setting
export ROS_PACKAGE_PATH=/home/$USER/wall/ros_common:/home/$USER/wall/lair_workspace:$ROS_PACKAGE_PATH
export ROS_PACKAGE_PATH=/home/zihan/dev/cisst/source/saw/components/sawROS:$ROS_PACKAGE_PATH


# export ROS_HOME=/home/$USER/.ros
#export ROS_IP=10.164.225.245
# export ROS_IP=$(ifconfig  | grep 'inet addr:'| grep -v '127.0.0.1' | cut -d: -f2 | awk '{ print $1}')
export ROS_HOSTNAME=localhost
# export ROS_MASTER_URI=http://10.162.34.52:11311

# LAIR Setting
export LAIR_CONFIG_PATH=/home/$USER/wall/lair_workspace/lair_dev/lair_platform/config
export LAIR_APP_PATH=/home/$USER/wall/lair_workspace/lair_dev/lair_platform/apps
export LAIR_DATABASE_PATH=/home/$USER/wall/dev/lair/lair_image_database
export LAIR_ROOT_PATH=/home/$USER/wall/lair_workspace/lair_dev/lair_platform




# cdbuild
function cdbuild() {
    buildPath=${PWD/source/build}
    buildPath=${buildPath/Dropbox\//}
    cd $buildPath
}

# cdsource
function cdsource() {
    sourcePath=${PWD/build/source}
    sourcePath=${sourcePath/dev/Dropbox\/dev}
    cd $sourcePath
}

#mkdirbuild
function mkdirbuild() {
    sourcePath=$PWD
    buildPath=${PWD/%source/}
    buildPath=${buildPath/Dropbox\//}
    cd $buildPath
    mkdir build
}

#ccmakeDropbox
function ccmakeDropbox() {
    sourcePath=${PWD/%build/source}
    sourcePath=${sourcePath/dev/Dropbox\/dev}
    ccmake $sourcePath
}


# Xilinx
function loadXilinx() {
    if [ -f /opt/Xilinx/13.4/ISE_DS/settings64.sh ]; then
        . /opt/Xilinx/13.4/ISE_DS/settings64.sh
    fi
}


# for emacs color in terminal
export TERM=xterm-256color