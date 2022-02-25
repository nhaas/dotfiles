(require 'package) ;; You might already have this line

;; Set directory where ELisp Packages are to be installed. Normally this
;; defaults to <user-emacs-directory>/elpa/, but this pollutes the
;; configuration(s). Plus, it opens up the possibility for different
;; configurations to share the same packages.
(setq package-user-dir "~/elpa")
;; For 'package' to work properly, it needs to be able to find the GPG keys as
;; well. The default value is <user-emacs-directory>/elpa/gnupg.
(setq package-gnupghome-dir "~/elpa/gnupg")

(package-initialize)

(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa.org/packages/"))

;; define custom.el file to prevent Emacs from appending to this file
(setq custom-file (expand-file-name "custom.el" (file-name-directory load-file-name)))
(load custom-file)

;; bootstap use-package
(setq package-enable-at-startup nil)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; load evil
(use-package evil
  :ensure t ;; install the evil package if not installed
  :init ;; tweak evil's configuration before loading it
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil) ;; TAB is TAB, even in terminal mode at expense of evil-jump-forward (C-i)
  :config ;; tweak evil after loading it
  (evil-mode)

  ;; example how to map a command in normal mode (called 'normal state' in evil)
  (define-key evil-normal-state-map (kbd ", w") 'evil-window-vsplit))

;; This package emulates surround.vim
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; load evil-org for sensible org-mode keybindings
(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; PlantUML interface
(use-package plantuml-mode
  :ensure t
  :after org
  :init ;; evaluated before package is loaded
  (setq plantuml-default-exec-mode 'jar) ; Indicate to use Java for rendering
  (setq plantuml-jar-path "~/Downloads/plantuml.jar") ; Point to JAR file
  (setq org-plantuml-jar-path (eval 'plantuml-jar-path)) ; Tell Org where to file JAR file
  (defvar plantuml-indent-regexp-activate-start "NEVER MATCH THIS EXPRESSION") ; Don't indent on account of active region
  (defvar plantuml-indent-regexp-activate-end "NEVER MATCH THIS EXPRESSION")
  (setq plantuml-indent-level 2)
  :config ;; evaluated after loading it
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

;; I picked a theme that was better than the default for terminal usage
(load-theme 'tango-dark)
