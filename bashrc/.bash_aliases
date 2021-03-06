alias sinstall='sudo apt-get install'
alias ssearch='apt-cache search'
alias sremove='sudo apt-get remove'
alias nemacs='emacs -nw'
alias semacs='sudo emacs -nw'
alias matlab='matlab -nodisplay'
alias trash='trash-put'

# Useful Aliases
alias resource-bash='. ~/.bashrc'
alias rebash='nemacs ~/.bashrc'



# JHConnect VPN 
alias jconnect='jhu_nc.sh start'
alias djconnect='jhu_nc.sh stop'

# apparix bookmark system
function to () {
   if test "$2"; then
     cd "$(apparix "$1" "$2" || echo .)";
   else
     cd "$(apparix "$1" || echo .)";
   fi
   pwd
}
function bm () {
   if test "$2"; then
      apparix --add-mark "$1" "$2";
   elif test "$1"; then
      apparix --add-mark "$1";
   else
      apparix --add-mark;
   fi
}
function portal () {
   if test "$1"; then
      apparix --add-portal "$1";
   else
      apparix --add-portal;
   fi
}
# function to generate list of completions from .apparixrc
function _apparix_aliases ()
{   cur=$2
    dir=$3
    COMPREPLY=()
    if [ "$1" == "$3" ]
    then
        COMPREPLY=( $( cat $HOME/.apparix{rc,expand} | \
                       grep "j,.*$cur.*," | cut -f2 -d, ) )
    else
        dir=`apparix -favour rOl $dir 2>/dev/null` || return 0
        eval_compreply="COMPREPLY=( $(
            cd "$dir"
            \ls -d *$cur* | while read r
            do
                [[ -d "$r" ]] &&
                [[ $r == *$cur* ]] &&
                    echo \"${r// /\\ }\"
            done
            ) )"
        eval $eval_compreply
    fi
    return 0
}
# command to register the above to expand when the 'to' command's args are
# being expanded
complete -F _apparix_aliases to



# rebuild source code 
function crebuild {
    rm -rf build
    mkdir build
    cd build
    ccmake ../source
}


# download cisst
function crecisst {
    mkdir source
    mkdir build
    cd source
    svn co https://svn.lcsr.jhu.edu/cisst/trunk .
    cd ../build
    ccmake ../source
}





