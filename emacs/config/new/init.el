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

;; define custom.el file to prevent Emacs from appending to this file
(setq custom-file (expand-file-name "custom.el" (file-name-directory load-file-name)))
(load custom-file)

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

;; Silently add ':ensure t' to all instances of use-package
;; (setq use-package-always-ensure t)

(load-theme 'tango-dark)

(use-package magit
  :ensure t)

(use-package rg
  :ensure t)

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
