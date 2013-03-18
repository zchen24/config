#!/usr/bin/env bash

if [ -f ~/.bashrc ]; then
    cp ~/.bashrc .
fi

if [ -f ~/.bash_aliases ]; then
    cp ~/.bash_aliases .
fi

if [ -f ~/.bash_zihan ]; then
    cp ~/.bash_zihan .
fi

