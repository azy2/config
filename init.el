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
(setq initial-buffer-choice t)
(blink-cursor-mode 0)
(global-hl-line-mode)
(setq-default cursor-type 'bar)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(define-key global-map (kbd "RET") 'newline-and-indent)
(fset 'yes-or-no-p 'y-or-n-p)

(require 'cl)
(defun indent-whole-buffer ()
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
(defun indent-file-when-save ()
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (buffer-file-name)
                  (indent-whole-buffer))
              (save-buffer))))
(defun indent-file-when-visit ()
  (make-local-variable 'find-file-hook)
  (add-hook 'find-file-hook
            (lambda ()
              (if (buffer-file-name)
                  (indent-whole-buffer))
              (save-buffer))))
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

(set-face-attribute 'default nil :font "DejaVu Sans Mono 12")

;; Company mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)
(require 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-c-headers-path-system "/usr/include/c++/5.3.0/")
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


;; Irony-mode
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
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
(add-hook 'c++-mode-hook (lambda ()
                           (setq flycheck-gcc-language-standard "c++1y"
                                 flycheck-clang-language-standard "c++1y")))

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

(require 'cc-mode)
(require 'semantic)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(add-to-list 'semantic-default-submodes
             'global-semantic-stickyfunc-mode)
(add-to-list 'semantic-default-submodes
             'global-semantic-idle-summary-mode)
(semantic-mode 1)

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
              (> (buffer-size) 3000000)) ;; Don't number huge files
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
(golden-ratio-mode 1)
(setq golden-ratio-auto-scale t)

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


(server-start)

(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("b04425cc726711a6c91e8ebc20cf5a3927160681941e06bc7900a5a5bfe1a77f" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(header-line ((t (:background "#292929" :foreground "#cccccc" :inverse-video nil :underline nil :slant normal :weight normal)))))
