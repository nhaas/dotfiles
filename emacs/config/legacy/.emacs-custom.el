(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style
   '((c-mode . "ssg")
     (c++-mode . "linux")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu")))
 '(comment-style 'multi-line nil nil "These look nice in the CSMCIExampleApp.c")
 '(csv-separators '("," " "))
 '(display-buffer-alist '(("*Async Shell Command*" display-buffer-no-window (nil))))
 '(indent-tabs-mode nil)
 '(normal-erase-is-backspace nil)
 '(org-agenda-files (list org-directory))
 '(org-cycle-emulate-tab nil)
 '(org-directory "~/Documents/org")
 '(org-hide-leading-stars t)
 '(org-log-into-drawer t)
 '(org-startup-folded 'overview)
 '(org-startup-indented t)
 '(org-tags-column -100)
 '(package-selected-packages
   '(plantuml-mode magit magit-delta dumb-jump rg ztree clang-format+ projectile-ripgrep yasnippet lsp-mode flycheck-rust rustic with-editor which-key use-package pkg-info flx-ido evil-surround evil-smartparens diminish))
 '(reb-re-syntax 'string)
 '(sh-basic-offset 4)
 '(tab-width 4)
 '(undo-tree-visualizer-diff t)
 '(x-select-enable-clipboard-manager nil))
 ;;'(load-theme 'tango-dark))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 98 :width normal))))
 '(hi-black-b ((t (:background "light salmon" :foreground "blue"))) nil "Added 07/20/2017")
 '(hi-black-hb ((t (:background "green"))) nil "Added 07/20/2017")
 '(hi-blue-b ((t (:background "cyan"))) nil "Added 07/20/2017")
 '(hi-green-b ((t (:background "aquamarine"))) nil "Added 07/20/2017")
 '(hi-red-b ((t (:background "dark orange"))) nil "Added 07/20/2017"))
