#!/usr/bin/env zsh

setopt EXTENDED_GLOB

CLONE_DIR=$PWD

cd $HOME

for f in $CLONE_DIR/home/.[^.]*; do
    LINK=$HOME/${f:t}
    [[ -a $LINK ]] && rm -rf $LINK
    ln -s $f $HOME/${f:t}
done

for f in $CLONE_DIR/home/.zprezto/runcoms/^README.md(.N); do
    LINK=$HOME/.${f:t}
    [[ -a $LINK ]] && rm -rf $LINK
    ln -s $f $HOME/.${f:t}
done
