;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; ;; There are two ways to load a theme. Both assume the theme is installed and
;; ;; available. You can either set `doom-theme' or manually load a theme with the
;; ;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)

;; I picked a theme that was better than the default for terminal usage.
(load-theme 'tango-dark)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Disable highlighting current line. It makes the current line unreadable in
;; the terminal.
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

;; Disable confirmation to quiit Emacs
(setq confirm-kill-emacs nil)

;; Disabling persistent undo history
;; If you are using +tree:
(after! undo-tree
  (setq undo-tree-auto-save-history nil))

;;; Set-up PlantUML to my liking
;; Evaluate before loading
(defvar plantuml-indent-regexp-activate-start "NEVER MATCH THIS EXPRESSION") ; Don't indent on account of active region
(defvar plantuml-indent-regexp-activate-end "NEVER MATCH THIS EXPRESSION")
(setq plantuml-indent-level 2)

;; Configure visual-regexp
(use-package! visual-regexp
  :bind (("C-C r" . vr/replace)
         ("C-c q" . vr/query-replace)))

;; Don't continue the comment when pressing =o=/=O=
(setq +evil-want-o/O-to-continue-comments nil)

;;; format

;; Executbles must be found in PATH as stated in format-all--executable-table.
;; For example:
;; #+begin_src sh
;; ln -s /usr/bin/clang-format-3.9 /usr/bin/clang-format
;; #+end_src

;; clang-format (the executable) needs to find '.clang-format' in the current
;; directory or any parent directory. This method will copy the format file to
;; the current directory, then call whatever function it has been wrapped
;; around, then delete the file. This is exactly what the 'cformat' bash
;; function does. The advantage of this is that you never need to leave Emacs,
;; nor do you need to figure out how to find the file of the current buffer.
(defun prep-formatter (orig-fun &rest args)
  (pcase (derived-mode-p major-mode)
    ;; For C modes...
    ('c-mode (let((src-file "~/format/ABC.clang-format")
                  (dst-file ".clang-format"))
               ;; copy .clang-format to the cwd
               (copy-file src-file dst-file "-f")
               ;; let +format/region-or-buffer do it's thing
               (apply orig-fun args)
               ;; delete .clang-format
               (delete-file dst-file)))
    ;; For all other modes, let +format/region-or-buffer do it's thing
    (else-mode (apply orig-fun args))))

;; Wrap/"advise" my 'prep-formatter function AROUND Doom's 'format' module
;; function (bound to "SPC c f").
(advice-add '+format/region-or-buffer :around #'prep-formatter)
