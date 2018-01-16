(define-key key-translation-map (kbd "C-x") (kbd "C-u"))
(define-key key-translation-map (kbd "C-u") (kbd "C-x"))
(define-key key-translation-map (kbd "M-x") (kbd "M-u"))
(define-key key-translation-map (kbd "M-u") (kbd "M-x"))

;; Tell emacs to stop putting customizations at the bottom of this file. It's annoying, messy, and I don't use them.
;; Instead, put them in their own file and, begrudgingly, load them.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(load "package")
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.milkbox.net/packages/") t)
;; (add-to-list 'package-archives
;; 	     '("gnu" . "https://elpa.gnu.org/packages"))
(add-to-list 'package-archives
	     '("marmalade" . "https://marmalade-repo.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq default-frame-alist '((font . "DejaVu Sans Mono 14")))

(use-package solarized-theme
  :ensure t
  :init
  (setq solarized-distinct-fringe-background nil)
  :config
  (load-theme 'solarized-dark))

;; Ew, no thanks
(menu-bar-mode -1)
(scroll-bar-mode 0)
(tool-bar-mode 0)

;; Follow symlinks. They're there for a reason.
(setq vc-follow-symlinks t)

;; Can I get a bar cursor, please?
(setq-default cursor-type 'bar)

;; Don't make me type yes, y is fine
(defalias 'yes-or-no-p 'y-or-n-p)

;; Show line numbers
(global-linum-mode 1)
(setq-default linum-format "%d")

;; Highlight the current line number
(use-package hlinum
  :ensure t
  :custom-face
  (linum-highlight-face ((t
                         (:foreground (face-attribute 'highlight :foreground))
                         (:background (face-attribute 'highlight :background)))))
  :config
  (hlinum-activate))

;; Highlight the current line
(global-hl-line-mode 1)

;; Show line number and column number in mode line
(line-number-mode 1)
(column-number-mode 1)

;; Get rid of startup screen and make the scratch buffer blank
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; When screen space permits, split vertically
(setq split-height-threshold nil)
(setq split-width-threshold 120)

;; Store all backup and autosave file in the tmp dir
(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

(bind-key* "C-t" 'other-window)

(use-package exec-path-from-shell
  :ensure t
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

(electric-pair-mode 1)

(global-visual-line-mode t)

(setq-default indent-tabs-mode nil)

(setq-default show-paren-delay 0)
(show-paren-mode 1)

;; Turn off the alarms. They're annoying.
(setq ring-bell-function 'ignore)

(defun comment-or-uncomment-region-or-line ()
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(bind-key* "M-;" 'comment-or-uncomment-region-or-line)

(use-package mwim
  :ensure t
  :bind (("C-a" . mwim-beginning)
         ("C-e" . mwim-end)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package company
  :ensure t
  :init
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2))
(add-hook 'after-init-hook 'global-company-mode)

(use-package flycheck
  :ensure t
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :config
  (global-flycheck-mode))

(use-package flycheck-pos-tip
  :ensure t
  :after (flycheck)
  :config
  (flycheck-pos-tip-mode))

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package rtags
  :ensure t
  :config
  (rtags-enable-standard-keybindings))

(use-package helm-rtags
  :after (helm rtags)
  :config
  (setq rtags-display-result-backend 'helm))

(use-package cmake-ide
  :ensure t
  :after (irony company flycheck rtags)
  :config
  (require 'rtags)
  (cmake-ide-setup))

(require 'cc-mode)
(setq c-default-style "linux"
      c-basic-offset 4)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defun end-of-block ()
  (interactive)
  (backward-up-list -1)
  (backward-char))

(defun beginning-of-block ()
  (interactive)
  (backward-up-list)
  (forward-char))

(bind-keys :map c-mode-map
           ("M-a" . beginning-of-block)
           ("M-e" . end-of-block)
           ("C-M-a" . c-beginning-of-defun)
           ("C-M-e" . c-end-of-defun))
(bind-keys :map c++-mode-map
           ("M-a" . beginning-of-block)
           ("M-e" . end-of-block)
           ("C-M-a" . c-beginning-of-defun)
           ("C-M-e" . c-end-of-defun))

(use-package helm
  :ensure t
  :bind (("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         :map helm-map
         ([tab] . helm-execute-persistent-action))
  :init
  (setq-default helm-autoresize-max-height 60)
  (setq-default helm-autoresize-min-height 20)
  (setq-default helm-M-x-fuzzy-match t)
  (setq-default helm-buffers-fuzzy-matching t
                helm-recentf-fuzzy-match t)
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1))

(use-package projectile
  :ensure t
  :config
  (projectile-mode))

(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :config
  (helm-projectile-on))

(use-package multi-term
  :ensure t
  :init
  (setq-default multi-term-dedicated-window-height 20)
  (defun multi-term-dedicated-toggle-and-switch ()
    (interactive)
    (multi-term-dedicated-toggle)
    (multi-term-dedicated-select))
  :bind (("<C-return>" . multi-term-dedicated-toggle-and-switch)))

(use-package treemacs
  :ensure t
  :config
  (treemacs-tag-follow-mode)
  (treemacs-git-mode 'extended)
  (treemacs-filewatch-mode)
  :bind (("C-o" . treemacs-toggle)))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs))

(use-package magit
  :ensure t
  :pin melpa
  :bind (("C-x g" . magit-status)))

(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'"
         "\\.js\\'"
         "\\.css\\'"
         "\\.xml\\'"
         "\\phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"))

(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

(use-package dashboard
  :ensure t
  :init
  (setq-default dashboard-startup-banner 'nil)
  (setq-default dashboard-items '((recents . 5)
                                  (bookmarks . 0)
                                  (projects . 50)
                                  (agenda . 0)
                                  (registers . 0)))
  :config
  (dashboard-setup-startup-hook))

(treemacs)
