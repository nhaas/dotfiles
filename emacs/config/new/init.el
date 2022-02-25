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

;; bootstrap use-package
(setq package-enable-at-startup nil)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; ;; Load up Org and Org-babel for tangling src code
;; (require 'org)
;; (require 'ob-tangle)

;; Load literate config
(org-babel-load-file (expand-file-name "config.org" (file-name-directory load-file-name)))

;; I picked a theme that was better than the default for terminal usage
(load-theme 'tango-dark)
