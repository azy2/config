if [ ! -L "$HOME/.spacemacs" ]; then
    ln -s "$HOME/config/.spacemacs" "$HOME/.spacemacs"
fi
if [ ! -L "$HOME/.emacs.d" ]; then
    ln -s "$HOME/config/.emacs.d" "$HOME/.emacs.d"
fi

mkdir -p ~/.xmonad
if [ ! -L "$HOME/.xmonad/xmonad.hs" ]; then
    ln -s "$HOME/config/.xmonad.hs" "$HOME/.xmonad/xmonad.hs"
fi

if [ ! -d "$HOME/.oh-my-zsh" ]; then
    sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
    rm .zshrc
fi

if [ ! -L "$HOME/.zshrc" ]; then
    ln -s "$HOME/config/.zshrc" "$HOME/.zshrc"
fi

if [ ! -L "$HOME/.zsh_profile" ]; then
    ln -s "$HOME/config/.zsh_profile" "$HOME/.zsh_profile"
fi

if [ ! -L "$HOME/.oh-my-zsh/themes/ben2.zsh-theme" ]; then
    ln -s "$HOME/config/ben2.zsh-theme" "$HOME/.oh-my-zsh/themes/ben2.zsh-theme"
fi
