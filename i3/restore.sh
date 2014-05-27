#!/usr/bin/env bash

# copy config
if [ -f ./i3config/config ]; then
    cp -r ./i3config ~/.i3
fi

