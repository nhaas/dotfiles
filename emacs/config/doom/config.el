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

;; Log org TODOs into a LOGBOOK drawer. Doom default is to add entries as unsorted list.
(after! org
  (setq org-log-into-drawer 'LOGBOOK))

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

;; Explicitly add hook for ibuffer-projectile
;; TODO: why isn't Doom modules use-package :hook working?
(add-hook 'ibuffer-hook #'ibuffer-projectile-set-filter-groups)

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
(defun nh/prep-formatter (orig-fun &rest args)
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

;; Wrap/"advise" my 'nh/prep-formatter function AROUND Doom's 'format' module
;; function (bound to "SPC c f").
(advice-add '+format/region-or-buffer :around #'nh/prep-formatter)

;;;; Replace all at point
;; Grabs the symbol under point, prompts for a replacement string, and then does the replacement through the buffer. Point moves to original symbol (appears not to move in most instances). Borrowed from 'Isearch with symbol'.
;; TODO: Point to does not always return to its original place before invokation.
(defun nh/replace-all-symbol-current-buffer (x &optional partailp backward)
  "Grabs the symbol under point, prompts for a replacement
string, and then does the replacement through the buffer."
  (interactive "sReplace symbol at point with: ")
  (let (from to bound sym)
    (setq sym
          ;; this block taken directly from find-tag-default
          ;; we couldn't use the function because we need the internal from and to values
          (when (or (progn
                      ;; Look at text around `point'.
                      (save-excursion
                        (skip-syntax-backward "w_") (setq from (point)))
                      (save-excursion
                        (skip-syntax-forward "w_") (setq to (point)))
                      (> to from))
                    ;; Look between `line-beginning-position' and `point'.
                    (save-excursion
                      (and (setq bound (line-beginning-position))
                           (skip-syntax-backward "^w_" bound)
                           (> (setq to (point)) bound)
                           (skip-syntax-backward "w_")
                           (setq from (point))))
                    ;; Look between `point' and `line-end-position'.
                    (save-excursion
                      (and (setq bound (line-end-position))
                           (skip-syntax-forward "^w_" bound)
                           (< (setq from (point)) bound)
                           (skip-syntax-forward "w_")
                           (setq to (point)))))
            (buffer-substring-no-properties from to)))
    (if (null sym)
        (message "No symbol at point")
      (goto-char (1- to))
      ;; Once we have symbol, do the replacement
      (replace-string sym x t (point-min) (point-max)))))

(define-key search-map "r" 'nh/replace-all-symbol-current-buffer) ;; "M-s r"

;;; lookup
;; =dumb-jump= has a list of files/dirs that it uses to determine if a directory
;; is the project root. One of them is Makefile. Remove it from that list since
;; there are potentially many Makefiles in a project. If the real project root
;; cannot be found, add .dumbjump or .git/ to establish it as a project root.
(after! dumb-jump
  (delete "Makefile" dumb-jump-project-denoters))

;;;; query-swap-regexp
;; Swap A and B regexp matches in current buffer or region.
(defun nh/query-swap-regexp (regexp-a regexp-b)
  "Swap A and B regexp matches in current buffer or region."
  (interactive "sRegexp A: \nsRegexp B: ")
  (let ((match-a (save-excursion
                   (re-search-forward regexp-a nil t)
                   (match-string 0)))
        (match-b (save-excursion
                   (re-search-forward regexp-b nil t)
                   (match-string 0))))
    (query-replace-regexp
     (concat "\\(\\(" regexp-a "\\)\\|" regexp-b "\\)")
     `(replace-eval-replacement
       replace-quote
       (if (match-string 2) ,match-b ,match-a))
     nil
     (if (and transient-mark-mode mark-active) (region-beginning))
     (if (and transient-mark-mode mark-active) (region-end)))))

;; As of Org Mode 9.2, I can no longer TAB-complete src-code block abbreviation '<s'. Org mode now
;; provides some built-in template function that can be called with `C-c C-,`, but that doesn't work
;; across PuTTY + tmux for some reason. Instead, I found this solution to be quite helpful
;; http://wenshanren.org/?p=334.
(defun nh/org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    ;; (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

;; Add new key-binding in the =org-mode-hook=
(add-hook 'org-mode-hook
          (lambda ()
            ;; keybinding for inserting code blocks
            (local-set-key (kbd "C-c s i")
                           'nh/org-insert-src-block)
            ))
