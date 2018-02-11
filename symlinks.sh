#!/bin/bash

cd dotfiles

for dir in $(find . -type d | sed 's/^..//'); do
	mkdir -p "$HOME/$dir";
done

for dotfile in $(find . -type f | sed 's/^..//'); do
	if [ -f "$HOME/$dotfile" ]; then
        rm "$HOME/$dotfile"
    fi
	ln -s "$PWD/$dotfile" "$HOME/$dotfile"
done
