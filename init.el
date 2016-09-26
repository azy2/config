(keyboard-translate ?\C-x ?\C-u)
(keyboard-translate ?\C-u ?\C-x)

(setq gc-cons-threshold 200000000)

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(defvar package-list
  '(
    cl
    better-defaults
    auto-compile
    company
    company-c-headers
    async
    irony
    ido-ubiquitous
    ido-vertical-mode
    smex
    company-irony
    flycheck
    flycheck-pos-tip
    flycheck-irony
    ;; smartparens
    semantic
    srefactor
    stickyfunc-enhance
    projectile
    corral
    multi-term
    anaconda-mode
    company-anaconda
    golden-ratio
    guide-key
    multiple-cursors
    smart-mode-line
    auctex
    company-math
    web-mode
    disaster
    visual-fill-column
    rotate
    powerline
    airline-themes
    rainbow-delimiters
    spray
    avy
    ))

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Autocompile
(setq load-prefer-newer t)
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

;; Better Defaults
(require 'better-defaults)
;; Open scratch buffer by default
(setq initial-buffer-choice t)
(setq delete-old-versions -1)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq vc-follow-symlinks t)
(setq ring-bell-function 'ignore)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
(blink-cursor-mode 1)
(global-hl-line-mode)
(setq-default cursor-type 'bar)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(define-key global-map (kbd "RET") 'newline-and-indent)
(fset 'yes-or-no-p 'y-or-n-p)

(require 'cl)
(defvar dont-indent-modes
  '(makefile-makepp-mode makefile-bsdmake-mode makefile-imake-mode makefile-automake-mode makefile-mode makefile-gmake-mode verilog-mode)
  "A list of the makefile major modes")

(defun indent-whole-buffer ()
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))
(defun indent-file-when-save ()
  (make-local-variable 'after-save-hook)
  (unless (member major-mode dont-indent-modes)
    (add-hook 'after-save-hook
              (lambda ()
                (if (buffer-file-name)
                    (indent-whole-buffer))))))
(defun indent-file-when-visit ()
  (make-local-variable 'find-file-hook)
  (unless (member major-mode dont-indent-modes)
    (add-hook 'find-file-hook
              (lambda ()
                (if (buffer-file-name)
                    (indent-whole-buffer))))))

(add-hook 'prog-mode-hook 'indent-file-when-save)
(add-hook 'prog-mode-hook 'indent-file-when-visit)

(defun comment-or-uncomment-region-or-line ()
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))
(global-set-key "\M-;" 'comment-or-uncomment-region-or-line)

;; Color Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/local/noctilux-theme")
(load-theme 'noctilux t)

(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)
(setq bdf-directory-list '("~/.emacs.d/local/fonts"))
(set-face-attribute 'default nil :font "DejaVu Sans Mono 22")

;; Company mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)
(require 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-c-headers-path-system "/usr/include/c++/6.1.1/")
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

(require 'ido-vertical-mode)
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)
(require 'ido-ubiquitous)
(ido-ubiquitous-mode t)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(setq ido-vertical-show-count t)

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(require 'cc-mode)

;; Irony-mode
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(setq c-default-style "linux"
      c-basic-offset 4)

;; Flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
;; (setq-default flycheck-disabled-checkers
;;               (append flycheck-disabled-checkers
;;                       '(c/c++-gcc)))
(add-hook 'c++-mode-hook (lambda ()
                           (setq flycheck-gcc-language-standard "c++1y"
                                 flycheck-clang-language-standard "c++1y"
                                 flycheck-clang-standard-library "libc++")))

;; Smartparens
;; (require 'smartparens-config)
;; (smartparens-global-mode t)
;; (show-smartparens-global-mode t)
;; (setq sp-cancel-autoskip-on-backward-movement nil)

(electric-pair-mode t)

(require 'corral)
(global-set-key (kbd "M-(") 'corral-parentheses-backward)
(global-set-key (kbd "M-)") 'corral-parentheses-forward)
(global-set-key (kbd "M-[") 'corral-brackets-backward)
(global-set-key (kbd "M-]") 'corral-brackets-forward)
(global-set-key (kbd "M-{") 'corral-braces-backward)
(global-set-key (kbd "M-}") 'corral-braces-forward)
(global-set-key (kbd "M-\"") 'corral-double-quotes-backward)

(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)


;; (require 'semantic)
;; (global-semanticdb-minor-mode 1)
;; (global-semantic-idle-scheduler-mode 1)
;; (add-to-list 'semantic-default-submodes
;;              'global-semantic-stickyfunc-mode)
;; (add-to-list 'semantic-default-submodes
;;              'global-semantic-idle-summary-mode)
;; (semantic-mode 1)

(require 'srefactor)
(require 'srefactor-lisp)

(require 'multi-term)
(setq multi-term-program "/bin/zsh")
(setq multi-term-dedicated-window-height 25)
(setq multi-term-dedicated-select-after-open-p t)
(setq multi-term-dedicated-close-back-to-open-buffer-p t)
(global-set-key (kbd "C-<return>") 'multi-term-dedicated-toggle)

(defun smarter-move-beginning-of-line (arg)

  (interactive "^p")
  (setq arg (or arg 1))
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)


(line-number-mode t)
(column-number-mode t)

(require 'linum)

(defcustom linum-disabled-modes-list '(term-mode
                                       image-mode
                                       compilation-mode
                                       dired-mode
                                       text-mode)
  "* List of modes disabled when global linum mode is on"
  :type '(repeat (sexp :tag "Major mode"))
  :tag " Major modes where linum is disabled"
  :group 'linum)

(defcustom linum-disabled-starred-buffers 't
  "* Disable buffers that have stars in them like *Gnu Emacs*"
  :type 'boolean
  :group 'linum)

(defun linum-on ()
  "* When linum is running globally, disable line numbers in modes
defined in `linum-disabled-modes-list'. Changed by linum-off.
Also turns off numbering in starred modes like *scratch*."
  (unless (or (minibufferp)
              (member major-mode linum-disabled-modes-list)
              (string-match "*" (buffer-name))
              (> (buffer-size) 30000)) ;; Don't number huge files
    (linum-mode 1)))

(global-linum-mode t)

(setq auto-save-interval 500
      linum-delay t
      scroll-conservatively 10000
      mouse-wheel-scroll-amount '(1)
      mouse-wheel-progressive-speed nil)

(require 'sh-script)
(dolist (pattern '("\\.zsh\\'"
                   "zlogin\\'"
                   "zlogout\\'"
                   "zpreztorc\\'"
                   "zprofile\\'"
                   "zshenv\\'"
                   "zshrc\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'sh-mode)))
(add-hook 'sh-mode-hook (lambda ()
                          (when (and buffer-file-name
                                     (string-match-p "\\.zsh\\'" buffer-file-name))
                            (sh-set-shell "zsh"))))

(add-hook 'python-mode-hook 'anaconda-mode)
(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))

(require 'golden-ratio)
;; (golden-ratio-mode 1)
;; (setq golden-ratio-auto-scale t)

(require 'guide-key)
(setq guide-key/guide-key-sequence t
      guide-key/idle-delay 0.4
      guide-key/popup-window-position 'bottom
      guide-key/popup-window-size 25)
(guide-key-mode 1)

(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

(sml/setup)

(require 'tex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(TeX-global-PDF-mode t)
(add-to-list 'company-backends 'company-math-symbols-unicode)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.pro\\'" . fundamental-mode))

(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-css-colorization t)

(defun latex-compile ()
  (interactive)
  (save-buffer)
  (TeX-command "LaTeX" 'TeX-master-file))

(require 'disaster)
(define-key c-mode-base-map (kbd "C-c d") 'disaster)

;; (eval-after-load 'latex
;;   '(define-key LaTeX-mode-map (kbd "C-x C-s") 'latex-compile))

(defun current-dir ()
  (cadr (split-string (pwd))))

(defun exit-to-konsole ()
  (interactive)
  (shell-command (concat "nohup konsole --workdir " (current-dir) " > /dev/null &"))
  (shell-command "disown")
  (delete-frame))

(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "C-c #"))
(global-set-key (kbd "C-x C-c") 'server-edit)
(global-set-key (kbd "C-c #") 'save-buffers-kill-terminal)


(defun term-toggle-mode ()
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))

(global-set-key (kbd "C-c l") 'term-toggle-mode)

(setq term-buffer-maximum-size 1024)

(defun open-next-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun open-prior-line ()
  (interactive)
  (forward-line -1)
  (open-next-line))

(global-unset-key (kbd "C-o"))
(global-unset-key (kbd "C-S-o"))
(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "C-S-o") 'open-prior-line)

(require 'visual-fill-column)
(add-hook 'text-mode-hook 'flyspell-mode)
(setq visual-fill-column-width 100)
(setq visual-fill-column-center-text t)
(add-hook 'text-mode-hook 'toggle-word-wrap)
(add-hook 'text-mode-hook 'visual-fill-column-mode)
(add-hook 'text-mode-hook 'visual-line-mode)

(defun swap-with (dir)
  (interactive)
  (let ((other-window (window-find-other-window dir)))
    (when other-window
      (let* ((this-window (selected-window))
             (this-buffer (window-buffer this-window))
             (other-buffer (window-buffer other-window))
             (this-start (window-start this-window))
             (other-start (window-start other-window)))
        (set-window-buffer this-window other-buffer)
        (set-window-buffer other-window this-buffer)
        (set-window-start this-window other-start)
        (set-window-start other-window this-start)))))

(global-unset-key (kbd "M-j"))
(global-unset-key (kbd "M-k"))
(global-unset-key (kbd "M-h"))
(global-unset-key (kbd "M-l"))
(global-set-key (kbd "M-j") 'windmove-down)
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "M-l") 'windmove-right)

(global-unset-key (kbd "C-M-j"))
(global-unset-key (kbd "C-M-k"))
(global-unset-key (kbd "C-M-h"))
(global-unset-key (kbd "C-M-l"))
(global-set-key (kbd "C-M-j") (lambda () (interactive) (swap-with 'down)))
(global-set-key (kbd "C-M-k") (lambda () (interactive) (swap-with 'up)))
(global-set-key (kbd "C-M-h") (lambda () (interactive) (swap-with 'left)))
(global-set-key (kbd "C-M-l") (lambda () (interactive) (swap-with 'right)))

(require 'powerline)
(powerline-default-theme)
(require 'airline-themes)
(load-theme 'airline-papercolor t)
(setq airline-helm-colors nil)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'spray)
(setq spray-margin-top 15)
(setq spray-margin-left 15)

(require 'avy)
(global-set-key (kbd "C-' C-c") 'avy-goto-char)
(global-set-key (kbd "C-' C-w") 'avy-goto-word-0)
(global-set-key (kbd "C-' C-l") 'avy-goto-line)

(server-start)

(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("b5fe3893c8808466711c1b55bb7e66b9c6aa2a86811783375a43e1beabb1af33" "aab598c4d024d544b4e8b356a95ca693afa9de000b154bd2f86eed68c9e75557" "86a731bda96ed5ed69980b4cbafe45614ec3c288da3b773e4585101e7ece40d2" "133222702a3c75d16ea9c50743f66b987a7209fb8b964f2c0938a816a83379a0" "878e22a7fe00ca4faba87b4f16bc269b8d2be5409d1c513bb7eda025da7c1cf4" "cadc97db0173a0d0bfc40473cab4da462af0ba8d60befd0a4879b582bcbc092d" "0788bfa0a0d0471984de6d367bb2358c49b25e393344d2a531e779b6cec260c5" "977513781c8dd86f4f0a04dbf518df5ba496da42b71173368b305478703eea42" "6998bd3671091820a6930b52aab30b776faea41449b4246fdce14079b3e7d125" "51277c9add74612c7624a276e1ee3c7d89b2f38b1609eed6759965f9d4254369" "8e7ca85479dab486e15e0119f2948ba7ffcaa0ef161b3facb8103fb06f93b428" "fbcdb6b7890d0ec1708fa21ab08eb0cc16a8b7611bb6517b722eba3891dfc9dd" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "b04425cc726711a6c91e8ebc20cf5a3927160681941e06bc7900a5a5bfe1a77f" default)))
 '(verilog-auto-newline nil)
 '(verilog-indent-level 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(header-line ((t (:background "#292929" :foreground "#cccccc" :inverse-video nil :underline nil :slant normal :weight normal)))))
