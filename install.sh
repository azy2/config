#!/usr/bin/env bash

CLONE_DIR=$PWD

cd $HOME

for f in $CLONE_DIR/home/* $CLONE_DIR/home/.[^.]*; do
    echo "ln -s $(basename $f) $f"
done
