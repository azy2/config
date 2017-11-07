#!/bin/bash

sudo add-apt-repository ppa:mozillateam/firefox-next
sudo apt update
sudo apt install -y zsh emacs25 git vim keepassx firefox

git clone git@github.com:azy2/config.git ~/config

git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
ln -s /home/ben/.spacemacs /home/ben/config/.spacemacs
emacs&

git clone --recursive git@github.com:azy2/prezto.git ~/.zprezto
rm ~/.zprezto/runcoms/README.md
for rcfile in $(ls ~/.zprezto/runcoms/); do
    ln -s "/home/ben/.zprezto/runcoms/$rcfile" "/home/ben/.${rcfile}"
done
chsh -s /bin/zsh
