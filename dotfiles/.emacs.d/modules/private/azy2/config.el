;;; private/default/config.el -*- lexical-binding: t; -*-

(setq doom-leader-key "C-c")
(setq doom-localleader-key "C-c m")

(define-key key-translation-map (kbd "C-x") (kbd "C-u"))
(define-key key-translation-map (kbd "C-u") (kbd "C-x"))
(define-key key-translation-map (kbd "M-x") (kbd "M-u"))
(define-key key-translation-map (kbd "M-u") (kbd "M-x"))

(load! +bindings)

(setq doom-font (font-spec :family "DejaVu Sans Mono" :size 22)
      doom-theme 'doom-vibrant)

(setq directory-abbrev-alist '(("^/home/ben/config/dotfiles/.emacs.d" . "/home/ben/.emacs.d")))

;; Function copied from `modules/lang/emacs-lisp/config.el' and modified to include my dotfiles dir
(defun +azy2|emacs-lisp|init-flycheck ()
  "Initialize flycheck-mode if not in emacs.d or dotfiles."
  (when (and buffer-file-name
             (and (not (file-in-directory-p buffer-file-name doom-emacs-dir))
                 (not (file-in-directory-p buffer-file-name "/home/ben/config/dotfiles/.emacs.d"))))
    (flycheck-mode +1)))

(remove-hook! 'emacs-lisp-mode-hook #'+emacs-lisp|init-flycheck)
(add-hook! 'emacs-lisp-mode-hook #'+azy2|emacs-lisp|init-flycheck)

(print emacs-lisp-mode-hook)

(require 'company)
(setq company-idle-delay 0
      company-minimum-prefix-length 2)

(def-package! cmake-ide
  :after (irony company flycheck rtags)
  :init
  (setq cmake-ide-build-pool-use-persistent-naming t)
  :config
  (require 'rtags)
  (cmake-ide-setup))

(require 'flycheck)
(setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
      flycheck-idle-change-delay 0.5)

(after! smartparens
  ;; Automatically indent a block when pressing enter inside curly braces,
  ;; square brackets or parentheses
  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil :post-handlers '(:add ("||\n[i]" "RET"))))
  (setq sp-autowrap-region t))

(after! hl-todo
  (setq hl-todo-keyword-faces
        `(("TODO"  . ,(face-foreground 'warning))
          ("FIXME" . ,(face-foreground 'error))
          ("XXX"   . ,(face-foreground 'error))
          ("HACK"  . ,(face-foreground 'error))
          ("NOTE"  . ,(face-foreground 'success)))))

(setq show-trailing-whitespace t
      which-key-idle-delay 0.1)

(setq counsel-find-file-ignore-regexp nil)

(add-hook! :append 'doom-init-ui-hook
  (blink-cursor-mode -1)
  (setq-default cursor-type 'bar))

(add-hook! minibuffer-setup (setq-local show-trailing-whitespace nil))

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(setq flycheck-pos-tip-timeout 60)

(add-hook 'doc-view-mode-hook #'auto-revert-mode)

(add-hook! 'emacs-lisp-mode-hook #'electric-pair-mode)
