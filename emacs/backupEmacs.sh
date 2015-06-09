#!/bin/bash 
# This script file is used to backup emacs
# config file & folder from current user 
# dir(~) to current dir 

cp ~/.emacs ./.emacs
cp -R ~/.emacs.d/pkgs ./.emacs.d/pkgs

