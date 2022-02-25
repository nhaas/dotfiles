;; To replace `require' and `load-path' ?
(package-initialize)

(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa.org/packages/"))

;; (setq custom-file "~/.emacs.d/.emacs-custom.el")
;; (load custom-file)
(setq custom-file (expand-file-name ".emacs-custom.el" (file-name-directory load-file-name)))
(load custom-file)

;; (org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))
(org-babel-load-file (expand-file-name "myinit.org" (file-name-directory load-file-name)))
