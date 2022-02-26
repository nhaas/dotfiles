;; NOTE: init.el is now generated from config.org.  Please edit that file
;;       and init.el will be generated automatically when saving!

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa-stable" . "https://melpa.org/packages/")))

;; Set directory where ELisp Packages are to be installed. Normally this
;; defaults to <user-emacs-directory>/elpa/, but this pollutes the
;; configuration(s). Plus, it opens up the possibility for different
;; configurations to share the same packages.
(setq package-user-dir "~/elpa")
;; For 'package' to work properly, it needs to be able to find the GPG keys as
;; well. The default value is <user-emacs-directory>/elpa/gnupg.
(setq package-gnupghome-dir "~/elpa/gnupg")

(package-initialize)

;; bootstrap use-package
(setq package-enable-at-startup nil)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Silently add ':ensure t' to all instances of use-package macro. use-package
;; will attempt to download any package that isn't already present.
(setq use-package-always-ensure t)

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
;(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering)

;; Don't load or store any customizations
(setq custom-file null-device)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(setq inhibit-startup-screen t)   ; Go straight to *scratch* if no file is given
(setq visible-bell t)             ; Set up the visible bell instead of audible 'ding'
(menu-bar-mode -1)                ; Disable the menu bar
(column-number-mode)              ; Show column position of the cursor as well as the row like this: (R, C)
(show-paren-mode t)               ; Always highlight opposite parenthesis
(defalias 'yes-or-no-p 'y-or-n-p) ; Always Use `y-or-n-p', Never `yes-or-no-p'

;; Turn on line numbers for all modes...
(global-display-line-numbers-mode t)
;; ... except these modes.
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                ;; treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; (tool-bar-mode -1)          ; Disable the toolbar
;; (tooltip-mode -1)           ; Disable tooltips
;; (scroll-bar-mode -1)        ; Disable visible scrollbar
;; (set-fringe-mode 10)        ; Give some breathing room

;; ;; Set frame transparency
;; (set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
;; (add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
;; ;; Always begin in fullscreen
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package evil
  :ensure t
  :init
  ;; The following solves the issue where TAB doesn't map to org-cycle in emacs
  ;; -nw. It is necessary for this to occur *before* (require 'evil). See:
  ;; https://stackoverflow.com/questions/22878668/emacs-org-mode-evil-mode-tab-key-not-working
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  ;; 'Emacs way' to quite Insert mode
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(load-theme 'tango-dark)

(use-package which-key
  :defer 5 ; defer [N] causes package to be loaded -- if not already -- after N seconds of idle time.
  :diminish which-key-mode ; reduce clutter on the mode-line
  :config
  (which-key-mode) ; enable minor mode after loading
  )

(use-package magit)

(use-package rg)

(use-package toc-org
  :ensure t
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))
