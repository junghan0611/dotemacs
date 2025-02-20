;;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghanacs
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;;; spacemacs declare-prefix
;; 여기에 관리해야 하는 파일 더 넣어야 할 지도 glossary tags url people

(spacemacs/declare-prefix
  "!"   "shell cmd"
  ;; "TAB" "Last buf"
  "\""  "terminal here"
  "#"   "register"
  "'"   "open shell"
  "*"   "search w/"
  "/"   "search w/o"
  "0"   "treemacs"
  ";"   "latest-popup"
  "²"   "select-window"
  "`"   "select-window"
  ":"   "kill-last-popup"
  "?"   "show bindings"
  "a"   "applications"
  "b"   "buffers"
  "c"   "codes"
  "C"   "Capture/Colors"
  "d"   "debug"
  "D"   "Diff/Compare"
  "e"   "errors"
  ;; "E"   "Export"
  "f"   "files"
  "F"   "Frames"
  "g"   "git/vc"
  "h"   "help"
  "i"   "insertion"
  "j"   "jump/join"
  "k"   "lisp"
  "K"   "Macros"
  "l"   "layouts"
  "m"   "major-mode"
  "n"   "narrow/numbers"
  "N"   "Navigation"
  "o"   "user bindings"
  "p"   "projects"
  "P"   "Pandoc/export"
  "q"   "quit"
  "r"   "regs/rings"
  "s"   "search/symbol"
  "S"   "Spelling"
  "t"   "toggles<1>"
  "T"   "Themes/UI"
  "C-t" "toggles<2>"
  "C-v" "rectangles"
  "u"   "universals"
  "v"   "er/expand"
  "w"   "windows"
  "x"   "text"
  "z"   "zoom"

  "<up>"   "window↑"
  "<down>"   "window↓"
  "<left>"   "window←"
  "<right>"   "window→"
  )

;;; Spacemacs Global
;; 실수로 누르게 되는 빠른 종료 바인딩을 제거한다.
(spacemacs/set-leader-keys "qq" nil) ; prompt-kill-emacs

;;;; P +Pandoc/Export

;; export this file on buffer
(spacemacs/set-leader-keys "Pe" #'org-export-dispatch)
(spacemacs/set-leader-keys "Pr" #'org-reveal-export-to-html)
(spacemacs/set-leader-keys "Pp" #'org-hugo-export-wim-to-md)

;;;; custom and more

(spacemacs/set-leader-keys "o9" 'sort-lines)
;; (spacemacs/set-leader-keys "ow" 'eww)

;; Revert buffer - loads in .dir-locals.el changes
(spacemacs/set-leader-keys "oR" 'revert-buffer)

;; ;;;###autoload
;; (defun my/custom-switch-to-buffer ()
;;   "`consult-buffer' with buffers provided by persp."
;;   (interactive)
;;   (consult-buffer
;;    '(consult--source-hidden-buffer ; 'SPC'
;;      consult--source-persp-buffers ; l layout
;;      consult--source-modified-buffer ; M
;;      consult--source-buffer ; b -- ADD
;;      consult--source-recent-file ; f
;;      consult--source-bookmark ; m

;;      ;; consult--source-file-register ; r -- ADD
;;      ;; consult-projectile--source-projectile-buffer ; j -- ADD
;;      )))

;; (spacemacs/set-leader-keys "bb" 'my/custom-switch-to-buffer)
;; (spacemacs/set-leader-keys "bB" 'spacemacs/compleseus-switch-to-buffer)
(spacemacs/set-leader-keys "bB" 'consult-buffer)

(spacemacs/set-leader-keys "pm" 'magit-project-status)

;; C-x C-0 restores the default font size
(spacemacs/set-leader-keys "tS" #'text-scale-mode)

(spacemacs/declare-prefix "aoB"  "Bib/citar")
(spacemacs/set-leader-keys
  "aoBn" 'citar-create-note
  "aoBo" 'citar-open-note)

(spacemacs/declare-prefix "ia"  "abbrev-mode")
(spacemacs/set-leader-keys
  "iag" 'add-global-abbrev
  "ia'" 'expand-abbrev
  "ial" 'list-abbrevs
  "ia+" 'add-mode-abbrev)

;;;; Spacemacs Major

;;;;; emacs-lisp-mode

(spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode
  "te" 'ert/eval-and-run-all-tests-in-buffer)

;;;;; org-mode

(spacemacs/declare-prefix-for-mode 'org-mode "mB" "Bib/citar")
;; (spacemacs/declare-prefix-for-mode 'org-mode "mP" "Paragraph")
;; (spacemacs/set-leader-keys-for-major-mode 'org-mode

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "/" 'math-preview-at-point
  "M-/" 'math-preview-all
  "C-M-/" 'math-preview-clear-all
  "iT" 'bh/insert-inactive-timestamp
  "ic" 'citar-insert-citation
  "er" 'org-reveal-export-to-html
  ;; "ep" 'org-hugo-export-wim-to-md
  "Bi" 'citar-insert-citation
  "Br" 'citar-insert-reference
  "Bn" 'citar-create-note
  "Bo" 'citar-open-note
  "mt" 'org-tidy-mode
  "mB" 'palimpsest-move-region-to-bottom
  "mT" 'palimpsest-move-region-to-top
  "mD" 'palimpsest-move-region-to-trash
  "mc" 'org-columns
  "mC" 'org-columns-quit
  ;; "mN" 'org-next-visible-heading
  ;; "mP" 'org-previous-visible-heading
  )

;; (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode
;;   "mN" 'outline-next-visible-heading
;;   "mP" 'outline-previous-visible-heading
;;   )

;;;;; eww-mode

(spacemacs/set-leader-keys-for-major-mode 'eww-mode
  "s" 'ace-link-eww
  "c" 'eww-copy-page-url
  )

;; Bookmark with BM layer
(global-set-key (kbd "M-g b") 'spacemacs/bm-transient-state/body)
(spacemacs/set-leader-keys "f." 'spacemacs/bm-transient-state/body)

;;;;; 'SPC i' insertion

(spacemacs/set-leader-keys "it" 'hl-todo-insert)

;;;;; 'SPC a' applications

;; Hammy
;; (when (locate-library "hammy")
;;   (spacemacs/declare-prefix "ah"  "hammy-timer")
;;   (spacemacs/set-leader-keys
;;     "ahs" 'hammy-start
;;     "ahS" 'hammy-stop
;;     "ahn" 'hammy-next
;;     "ahl" 'hammy-view-log
;;     "ahr" 'hammy-reset
;;     "ahm" 'hammy-mode))

;; TMR Timer
(when (locate-library "tmr")
  (spacemacs/set-leader-keys "att" nil)
  (spacemacs/declare-prefix "att" "tmr")
  (spacemacs/set-leader-keys
    "attt" #'tmr
    "attl" #'tmr-tabulated-view
    "attc" #'tmr-clone
    "attk" #'tmr-cancel
    "atts" #'tmr-reschedule
    "atte" #'tmr-edit-description
    "attr" #'tmr-remove
    "attR" #'tmr-remove-finished
    "attT" #'tmr-with-description
    ))

;;;;; 'SPC g' git/vc

(when (locate-library "git-cliff")
  (spacemacs/set-leader-keys "g/" 'git-cliff-menu))

(when (locate-library "blamer")
  ;; (spacemacs/set-leader-keys "g9" 'blamer-show-commit-info)
  (spacemacs/set-leader-keys "g0" 'blamer-mode)
  (spacemacs/set-leader-keys "gb" 'blamer-mode)
  (spacemacs/set-leader-keys "gB" 'spacemacs/git-blame-transient-state/body)
  )

;;;;; 'SPC y'

;; Hypothesis
(when (locate-library "hypothesis")
  (spacemacs/declare-prefix "Ch"  "hypothesis")
  (spacemacs/set-leader-keys
    "Cho" #'hypothesis-to-org
    "Cha" #'hypothesis-to-archive
    )
  )

;;;;; 'SPC f'

;; (spacemacs/set-leader-keys "fed" 'goto-emacs-dotfiles.org)


;;;;; imenu treemacs

(global-set-key (kbd "<f8>") 'imenu-list-smart-toggle)
(global-set-key (kbd "M-<f8>") 'spacemacs/imenu-list-smart-focus)
(global-set-key (kbd "<f9>") 'spacemacs/treemacs-project-toggle)
(global-set-key (kbd "M-<f9>") 'treemacs-select-window)

;; (global-set-key (kbd "M-g l") 'spacemacs/toggle-relative-line-numbers) ; 'SPC t n r'

;;;; packages

;;;;; puni

(spacemacs/set-leader-keys
  "xs" #'puni-splice-killing-backward
  ;; "xT" #'transpose-sexps
  "xT" #'puni-transpose
  "xdd" #'delete-pair
  "xD" #'delete-pair
  "c;" #'pp-eval-expression
  ;; "fek" #'my/open-usr-key-file
  "js" #'puni-split
  "jj" #'avy-goto-char
  ;; "jD" nil
  ;; "jJ" 'avy-goto-char-timer
  "jn" #'electric-newline-and-maybe-indent
  "fel" #'find-library
  )

(spacemacs/declare-prefix
  "xs"   "⋇splice-backward-sexp"
  "xT"   "⋇transpose-sexps"
  "xD"   "⋇delete-pair"
  "c;"   "⋇pp-eval-expr"
  "ji"   "⋇jump/identifier"
  "jj"   "⋇jump/character"
  "jc"   "⋇jump/last-change"
  "jn"   "⋇jump/newline-indent"
  )

;;;;; markdown

(spacemacs/set-leader-keys-for-major-mode 'markdown-mode
  "/" 'math-preview-at-point
  "M-/" 'math-preview-all
  "C-M-/" 'math-preview-clear-all
  )

;;; only spacemacs

;;;; global

;; 파일 패스 복사 =SPC f y l=
;; (global-set-key (kbd "C-c f") 'spacemacs/copy-file-path)
(global-set-key (kbd "C-c f") 'spacemacs/projectile-copy-file-path)
(global-set-key (kbd "C-c F") 'spacemacs/copy-file-path-with-line)

;; (global-set-key (kbd "C-c j o t") 'org-make-toc)

;; (spacemacs/set-leader-keys "o9" 'sort-lines)
;; Kill this buffer now!
(global-set-key (kbd "M-S-q") 'spacemacs/kill-this-buffer)
(global-set-key (kbd "C-`") #'spacemacs/default-pop-shell) ; vscode

;;;; leetcode

(when (locate-library "leetcode")
  ;; (spacemacs/set-leader-keys "o9" 'sort-lines)
  ;; Labels the app as Leetcode so it doesn't appear as "prefix" in the menu
  (spacemacs/declare-prefix "a L" "Leetcode")
  ;; The remaining useful keybindings to using Leetcode
  (spacemacs/set-leader-keys
    "a L l" 'leetcode
    "a L d" 'leetcode-show-current-problem
    "a L r" 'leetcode-refresh
    "a L t" 'leetcode-try
    "a L u" 'leetcode-submit
    )
  )

;;;; devdocs-browser

(spacemacs/set-leader-keys "ar." #'devdocs-browser-open)

(spacemacs/set-leader-keys-for-major-mode 'python-mode "h." 'devdocs-browser-open)
(spacemacs/set-leader-keys-for-major-mode 'python-mode "h/" 'devdocs-browser-open-in)
(spacemacs/set-leader-keys-for-major-mode 'python-ts-mode "h." 'devdocs-browser-open)
(spacemacs/set-leader-keys-for-major-mode 'python-ts-mode "h/" 'devdocs-browser-open-in)

;;;; Global 'n' narrow  / numbers

(spacemacs/set-leader-keys "nm" #'my/split-and-indirect-orgtree)
(spacemacs/set-leader-keys "nM" #'my/kill-and-unsplit-orgtree)
(spacemacs/set-leader-keys "tM" #'my/org-toggle-emphasis-markers)

;;;; Global 'C' Capture

(spacemacs/set-leader-keys "Ca" #'orgabilize-org-archive) ; url to org
(spacemacs/set-leader-keys "Cl" #'orgabilize-insert-org-link)
(spacemacs/set-leader-keys "Cf" #'orgabilize-org-find-archived-file) ; html to org
(spacemacs/set-leader-keys "CA" #'orgabilize-org-archive-from-file)

(spacemacs/set-leader-keys "CA" #'orgabilize-org-archive-from-file)
(spacemacs/set-leader-keys "Cw" #'wikinforg)

;;;; Global 'j'

(spacemacs/set-leader-keys "j." 'evil-jump-forward)
(spacemacs/set-leader-keys "j," 'evil-jump-backward)

;;;; Global

(spacemacs/set-leader-keys "g g" 'magit-status) ; doom style

;;;; org-mode

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "E" 'org-set-effort
  "D" 'org-deadline
  "S" 'org-schedule
  "g" 'consult-org-heading
  "\\" 'org-tags-sparse-tree
  "TI" 'org-imgtog-mode
  "Tm" 'org-toggle-item
  "TM" 'my/org-toggle-emphasis-markers
  "Th" 'org-toggle-heading
  "Tp" 'org-tree-slide-mode
  "TP" 'org-present
  "i1" 'my/org-get-heading-title
  "i2" 'my/org-insert-heading-category
  "i3" 'my/org-insert-heading-categories-all
  "R" 'org-delete-link
  "F" 'org-insert-file-link
  "I" 'org-insert-link-dwim
  "mm" 'er/mark-paragraph
  )

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "CP" 'ash/org-pomodoro-til-meeting)
(spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
  "CP" 'ash/org-pomodoro-til-meeting) ;; "Start pomodoro til half hour"

;;;; TODO org-mode 'm'

;; (spacemacs/declare-prefix-for-mode 'org-mode "mo" "custom")
;; (spacemacs/set-leader-keys-for-major-mode 'org-mode
;;     "oc" 'my/genfile-timestamp
;;     "od" 'my/get-file-line
;;     "oe" 'my/get-file-link
;;     "of" 'my/encode-buffer-to-utf8
;;     "og" 'my/copy-word
;;     "oh" 'my/copy-line
;;     "oi" 'my/copy-paragraph
;;     "oj" 'my/copy-buffer
;;     "ok" 'my/backward-last-edit
;;     "ot" 'my/org-titlecase-level-1
;;     "ol" 'my/buffer-edit-hook
;;     "om" 'my/rename-file-and-buffer
;;     "on" 'my/grep-find
;;     "oo" 'my/open-external
;;     "op" 'my/open-external-pdf
;;     "oq" 'my/unfill-paragraph-or-region

;;     "i0" 'cc-todo-item
;;     "mi" 'org-id-get-create
;;     )

;;;; denote

(spacemacs/declare-prefix-for-mode 'org-mode "mn" "Denote")
(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "nf" 'denote
  "nc" 'citar-create-note
  "nC" 'citar-denote-open-note

  "nn" 'consult-notes
  "nt" 'denote-type

  "nz" 'denote-signature
  "ns" 'denote-subdirectory

  "nl" 'denote-link
  "nk" 'denote-keywords-add
  "nK" 'denote-keywords-remove
  "nR" 'denote-rename-file-using-front-matter
  )

;;;;; dired and denote

(spacemacs/declare-prefix-for-mode 'dired-mode "mn" "note")
(spacemacs/set-leader-keys-for-major-mode 'dired-mode
  "nn" 'consult-notes
  "nf" 'denote
  "nl" 'denote-link
  "nt" 'denote-type

  "nz" 'denote-signature
  "nc" 'citar-create-note
  "nC" 'citar-denote-open-note

  "nr" 'denote-dired-rename-files
  "nR" 'denote-dired-rename-marked-files-using-front-matter
  "nk" 'denote-dired-rename-marked-files-with-keywords

  "ns" 'denote-sort-dired
  )


;;;; Global 'o'

(spacemacs/set-leader-keys "oj" #'my/consult-org-all)
(spacemacs/set-leader-keys "ol" #'my/consult-org-links)
(spacemacs/set-leader-keys "of" #'my/consult-ripgrep-org-directory)

(spacemacs/set-leader-keys "Cr" #'remember) ;; org-capture ?!
(spacemacs/set-leader-keys "fn" #'consult-notes)

;;; spacemacs evil
;;;; evil-org

(with-eval-after-load 'evil-org
  ;; evil macro
  (define-key evil-normal-state-map (kbd "q") 'nil) ; evil macro disable
  (define-key evil-normal-state-map (kbd "Q") 'evil-record-macro)

  (define-key evil-normal-state-map "x" 'delete-forward-char)
  (define-key evil-normal-state-map "X" 'delete-backward-char)
  (evil-define-key 'normal 'evil-org-mode "x" 'delete-forward-char)
  (evil-define-key 'normal 'evil-org-mode "X" 'delete-backward-char)
  )

;;;; corfu

;; move to keys.el
;;  ;; (define-key corfu-map (kbd "S-TAB") 'jump-backward-pair)
;;  ;; (define-key prog-mode-map (kbd "S-TAB") 'jump-backward-pair)
;;  ;; (define-key prog-mode-map (kbd "S-<tab>") 'jump-backward-pair)
;;  ;; (define-key prog-mode-map (kbd "S-TAB") 'jump-backward-pair)

;; Use M-j to join lines. C-j splits them, so it's all good.
(evil-define-key '(insert) org-mode-map (kbd "M-j") 'join-line)

;;;; info

(define-key help-mode-map "o" 'ace-link-help)

;; preview info files
(add-to-list 'auto-mode-alist '("\\.info\\'" . Info-on-current-buffer))
(evil-define-key 'normal Info-mode-map "o" 'ace-link-info)

;;;; magic-section

(with-eval-after-load 'magit-section
  ;; (setq-default magit-section-highlighted-sections t)
  ;; (setq-default magit-section-highlight-overlays t)
  (define-key magit-section-mode-map (kbd "M-<tab>") 'other-window)
  (define-key magit-section-mode-map (kbd "C-<tab>") 'tab-next)
  )
;;; user-keybindings.el ends here
