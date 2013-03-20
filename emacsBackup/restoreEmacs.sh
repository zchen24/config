#!/bin/bash 
# This script file is used to restore emacs
# config file & folder from current dir to 
# current user dir(~) 

cp ./.emacs ~/.emacs
cp -R ./.emacs.d ~/
