
# auto completion
if [ -f `brew --prefix`/etc/bash_completion ]; then
    . `brew --prefix`/etc/bash_completion
fi

# color
export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced