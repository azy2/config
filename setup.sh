if [ ! -L "$HOME/.emacs.d/init.el" ]; then
    mkdir -p "$HOME/.emacs.d/"
    ln -s "$HOME/config/init.el" "$HOME/.emacs.d/init.el"
fi


mkdir -p ~/.xmonad
if [ ! -L "$HOME/.xmonad/xmonad.hs" ]; then
    ln -s "$HOME/config/xmonad.hs" "$HOME/.xmonad/xmonad.hs"
fi

if [ ! -L "$HOME/.xmobarrc" ]; then
    ln -s "$HOME/config/.xmobarrc" "$HOME/.xmobarrc"
fi

if [ ! -d "$HOME/.oh-my-zsh" ]; then
    sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
fi

if [ -e "$HOME/.zshrc" ]; then
    rm "$HOME/.zshrc"
fi

if [ ! -L "$HOME/.zshrc" ]; then
    ln -s "$HOME/config/.zshrc" "$HOME/.zshrc"
fi

if [ ! -L "$HOME/.zlogin" ]; then
    ln -s "$HOME/config/.zlogin" "$HOME/.zlogin"
fi

if [ ! -L "$HOME/.oh-my-zsh/themes/ben2.zsh-theme" ]; then
    ln -s "$HOME/config/ben2.zsh-theme" "$HOME/.oh-my-zsh/themes/ben2.zsh-theme"
fi
