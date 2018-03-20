#!/usr/bin/env zsh

setopt EXTENDED_GLOB

DOTFILE_DIR=$PWD/home

cd $HOME

DIRS=('.emacs.d' '.doom.d' '.zprezto' '.local/share/konsole' '.local/share/dolphin' '.config')

for f in $DIRS; do
    LINK=$HOME/$f
    TARGET=$DOTFILE_DIR/$f
    [[ -h $LINK ]] && rm $LINK
    ln -s $TARGET $LINK
done

for f in $CLONE_DIR/home/.zprezto/runcoms/^README.md(.N); do
    LINK=$HOME/$f
    [[ -h $LINK ]] && rm -rf $LINK
    ln -s $f $HOME/.${f:t}
done
