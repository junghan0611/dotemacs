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

;;; Code:

;;; Global-Hydra

(global-unset-key (kbd "<f1>"))  ; unset f1
(global-unset-key (kbd "<f2>"))

(global-set-key (kbd "<f1>") 'hydra-all/body)
;; (global-set-key (kbd "s-s") 'hydra-all/body)
(global-set-key (kbd "M-g 1") 'hydra-jump-to-directory/body)
(global-set-key (kbd "M-g 2") 'hydra-jump-to-files/body)
;; (global-set-key (kbd "M-g b") 'hydra-bm/body)

(global-set-key (kbd "<f2>") 'major-mode-hydra)

;; 확실한 이지 가이드가 된다.

;;;; Header

;; define everything here
(require 'hydra)
(require 'pretty-hydra)
(require 'major-mode-hydra)

;; (require 'all-the-icons)
;; ;; with-faicon function allows an icon in hydra title. Requires following requires and aliases. To omit don't include 'with-faicon' in appearance-title
;; ;; define an icon function with all-the-icons-faicon
;; ;; to use filecon, etc, define same function with icon set
;; (defun with-faicon (icon str &rest height v-adjust)
;;     (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))
;; (defun with-fileicon (icon str &rest height v-adjust)
;;     (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

;;;; Hydra-All > hydra-jumps

(pretty-hydra-define hydra-jumps
  (:color amaranth :exit t :quit-key "q")
  ("Jump visually"
   (("j" avy-goto-word-1 "to word" :exit t)
    ("l" avy-goto-line "to line" :exit t)
    ("c" avy-goto-char "to char" :exit t)
    ("r" avy-resume "resume" :exit t))
   "Jump via minibuffer"
   (("i" consult-imenu "imenu" :exit t)
    ("o" consult-outline "outline" :exit t))
   "Jump & go"
   (("u" ash/avy-goto-url "open url" :exit t))
   "Misc"
   (("=" hydra-all/body "back" :exit t))))

;;;; Hydra-All > hydra-structural

;;;;; hydra-structural : smartparens

(when (locate-library "smartparens")
  (pretty-hydra-define hydra-structural
    (:color amaranth :quit-key "q")
    ("Change"
     (("i" sp-change-inner "change inner" :exit t)
      ("k" sp-kill-sexp "kill sexp")
      ("]" sp-slurp-hybrid-sexp "slurp")
      ("/" sp-swap-enclusing-sexp "swap enclusing"))
     "Movement"
     (("b" sp-beginning-of-sexp "beginning of sexp")
      ("e" sp-end-of-sexp "end of sexp")
      ("d" sp-down-sexp "down sexp")
      ("e" sp-up-sexp "up sexp"))
     "Formatting"
     (("r" sp-rewrap-sexp "rewrap"))
     "Misc"
     (("=" hydra-all/body "back" :exit t)))))

;;;;; hydra-structural : puni

(when (locate-library "puni")
  (pretty-hydra-define hydra-structural
    (:color amaranth :quit-key "q")
    ("Change"
     (("," puni-slurp-forward "slurp-forward")
      ("." puni-barf-forward "barf-forward")
      ("]" puni-slurp-forward "slurp-backward")
      ("[" puni-barf-forward "barf-backward")
      ("." puni-splice "splice")
      ("?" puni-convolute "convolute"))
     "Movement"
     (("a" puni-beginning-of-sexp "beginning of sexp")
      ("e" puni-end-of-sexp "end of sexp")
      (")" puni-syntactic-forward-punc "down sexp")
      ("(" puni-syntactic-backward-punc "up sexp"))
     "Formatting"
     (("z" puni-squeeze "squeeze/unwrap"))
     "Misc"
     (("=" hydra-all/body "back" :exit t)))))

;;;; Hydra-All > hydra-multiple-cursors

(pretty-hydra-define hydra-multiple-cursors
  (:color amaranth :quit-key "q")
  ("Mark via region"
   (("l" mc/edit-lines "edit lines" :exit t)
    ("s" mc/mark-all-in-region-regexp "mark all in region re" :exit t))
   "Mark"
   (("a" mc/mark-all-like-this "mark all" :exit t)
    ("d" mc/mark-all-dwim "mark dwim" :exit t))
   "Mark incrementally"
   (("n" mc/mark-next-like-this "mark next like this")
    ("N" mc/skip-to-next-like-this "skip to next like this")
    ("M-n" mc/unmark-next-like-this "unmark next like this")
    ("p" mc/mark-previous-like-this "mark previous like this")
    ("P" mc/skip-to-previous-like-this "skip to previous like this")
    ("M-p" mc/unmark-previous-like-this "unmark previous like this")
    ("L" mc/mark-next-lines "mark next lines"))
   "Insert"
   (("0" mc/insert-numbers "insert numbers" :exit t)
    ("A" mc/insert-letters "insert letters" :exit t))
   "Misc"
   (("=" hydra-all/body "back" :exit t))))

;;;; Hydra-All > hydra-expand

(pretty-hydra-define hydra-expand
  (:color amaranth :quit-key "q")
  ("Expand/Contract"
   (("e" er/expand-region "expand")
    ("c" er/contract-region "contract"))
   "Expand to..."
   (("d" er/mark-defun "defun")
    ("\"" er/mark-inside-quotes "quotes")
    ("'" er/mark-inside-quotes "quotes")
    ("p" er/mark-inside-pairs "pairs")
    ("." er/mark-method-call "call"))
   "Misc"
   (("=" hydra-all/body "back" :exit t))))

;;;; TODO Hydra-All > hydra-roam

;; (pretty-hydra-define hydra-roam ()
;;   ("Navigation"
;;    (("/" consult-org-roam-file-find "find" :exit t)
;;     ("n" org-roam-node-find "find more" :exit t)
;;     ("b" citar-create-note "create citar note" :exit t)
;;     ("?" org-roam-node-random "any random note" :exit t)
;;     ("r" ash/org-roam-node-random-no-dates "random note, no dates" :exit t)
;;     ("SPC" ash/org-roam-dailies-find-today "today" :exit t)
;;     ("T" org-roam-dailies-capture-today "capture today" :exit t)
;;     ("<" ash/org-roam-dailies-find-yesterday "yesterday" :exit t)
;;     (">" org-roam-dailies-find-tomorrow "tomorrow" :exit t)
;;     ("D" ash/org-roam-dailies-find-date "date" :exit t)
;;     )
;;    "Find in category"
;;    (
;;     ("a" ash/org-roam-node-find-areas :exit t)
;;     ("v" ash/org-roam-node-find-archives :exit t)
;;     ;; ("p" ash/org-roam-node-find-person :exit t)
;;     ("s" ash/org-roam-node-find-resources :exit t)
;;     ("p" ash/org-roam-node-find-projects :exit t)
;;     ("z" ash/org-roam-node-find-zettels :exit t)
;;     ("G" my/org-roam-get-all-tags :exit t)
;;     )
;;    )
;;   )

;;;; Hydra-All > hydra-denote

(pretty-hydra-define hydra-denote
  (:color amaranth :exit t :quit-key "q"
   :pre (progn (setq which-key-inhibit t))
   :post (progn (setq which-key-inhibit nil)))
  ("new"
   (("n" denote-create-note-using-signature "create-note-using-signature")
    ("t" denote-create-note-with-template "create-note-with-template")
    ("d" denote-create-note-using-date "create-note-using-date"))
   "link"
   (("i" denote-link "insert link")                  ;mnemonic "insert"
    ("c" denote-link-after-creating "link to new"))  ;mnemonic "create"
   "inspect & open"
   (("l" denote-link-find-file "open linked file")   ;mnemonic "link"
    ("b" denote-link-find-backlink "open backlink")  ;mnemonic "backlink"
    ;; ("s" consult-notes "search zettels")
    ;; ("s" consult-notes "search zettels")
    ("f" my/denote-find-file "denote-find-file")
    ("g" my/denote-grep "denote-grep"))
   "modify"
   (("r" denote-rename-file "rename")
    ("u" denote-rename-file-using-front-matter "rename using front-matter"))
   "Quit"
   (("q" nil "Quit" :color red :exit t))
   ))

;;;; Hydra-All > hydra-ekg

;; (pretty-hydra-define hydra-ekg ()
;;   ("Navigation"
;;    (
;;     ("e" ekg-show-notes-for-today "show today" :exit t)
;;     ("t" ekg-show-notes-with-tag "show with-tag" :exit t)
;;     ("a" ekg-show-notes-with-all-tags "show with-all-tags" :exit t)
;;     ("r" ekg-show-notes-latest-captured "show latest-captured" :exit t)
;;     ("b" ekg-embedding-show-similar-to-current-buffer "show similar to current buffer" :exit t)
;;     ("s" ekg-embedding-search "embedding-search" :exit t))
;;    "Capture"
;;    (
;;     ("k" ekg-capture)
;;     ("u" ekg-capture-url)
;;     ("f" ekg-capture-file)
;;     ;; ("u" ash/capture-literature-note)
;;     )
;;    "Quit"
;;    (("q" nil "Quit" :color red :exit t))
;;    ))

;;;; Hydra-All > hydra-yas

(pretty-hydra-define hydra-yas ()
  ("Snippets"
   (("n" yas-new-snippet "new" :exit t)
    ("r" yas-reload-all "reload" :exit t)
    ("v" yas-visit-snippet-file "visit" :exit t))
   "Movement"
   (("f" yas-next-field "forward field" :exit nil)
    ("b" yas-prev-field "previous field" :exit nil)
    ("q" nil "Quit" :color red :exit t))
   ))

;;;; Hydra-All > hydra-flycmake

(pretty-hydra-define hydra-flymake ()
  ("Movement"
   (
    ("n" flymake-goto-next-error "next error")
    ("p" flymake-goto-prev-error "previous error")
    ("d" flymake-goto-diagnostic "diagnostic")
    ("<" flycheck-previous-error "previous flycheck error")
    (">" flycheck-next-error "next flycheck error")
    ("l" flycheck-list-errors "list")
    ("." consult-flymake)
    ("," consult-flycheck)
    )
   "Display"
   (("." flymake-show-diagnostic "show diagnostic")
    ("B" flymake-show-diagnostics-buffer "diagnostics buffers"))
   "Misc"
   (("=" hydra-all/body "back" :exit t))))

;;;; Hydra-All > hydra-mail

;; notmuch is too specialized to be set up here, it varies from machine to
;; machine. At some point I should break it down into the general &
;; specialized parts.
(defun ash/inbox ()
  (interactive)
  (notmuch-search "tag:inbox" t))
(pretty-hydra-define hydra-mail ()
  ("Search"
   (("s" notmuch-search "search" :exit t)
    ("h" consult-notmuch "incremental search" :exit t))
   "Application"
   (("n" notmuch-hello "notmuch" :exit t)
    ("i" ash/inbox "inbox" :exit t)
    ("c" notmuch-mua-new-mail "compose" :exit t))
   "Misc"
   (("=" hydra-all/body "back" :exit t))))

;;;; Hydra-All > hydra-org-main

(pretty-hydra-define hydra-org-main ()
  ("Misc"
   (("a" org-agenda "agenda" :exit t)
    ("c" org-capture "capture" :exit t))
   "Links"
   (("s" org-store-link "store" :exit t))))

;;;; Hydra-All > hydra-find

(pretty-hydra-define hydra-find ()
  ("In-Buffer"
   (("i" consult-imenu "imenu" :exit t)
    ("m" consult-mark "mark rings" :exit t)
    ("l" consult-line-multi "line-multi" :exit t)
    ("e" consult-flymake "errors" :exit t)
    ;; ("e" consult-flycheck "errors" :exit t)
    ("g" consult-goto-line "line" :exit t))
   "Other"
   (("r" consult-ripgrep "grep" :exit t)
    ("b" consult-bookmark "bookmark" :exit t)
    ("R" consult-register "register" :exit t)
    ("C" consult-complex-command "complex command" :exit t))))

;;;; Hydra-All > hydra-toggles

;; (defvar hydra-toggles--title (with-faicon "toggle-on" "Toggles"))
(pretty-hydra-define hydra-toggles
  (:color amaranth :quit-key "<espace>") ;; :title hydra-toggles--title)
  ;; (pretty-hydra-define hydra-toggles ()
  ("Basic"
   (("n" linum-mode "line number" :toggle t)
    ("w" whitespace-mode "whitespace" :toggle t)
    ("W" whitespace-cleanup-mode "whitespace cleanup" :toggle t)
    ("r" rainbow-mode "rainbow" :toggle t)
    ("L" page-break-lines-mode "page break lines" :toggle t))
   "Highlight"
   (("o" symbol-overlay-mode "symbol" :toggle t)
    ("l" hl-line-mode "line" :toggle t)
    ("x" highlight-sexp-mode "sexp" :toggle t)
    ("t" hl-todo-mode "todo" :toggle t))
   "Writing"
   (
    ("s" flyspell-mode "flyspell" :toggle t)
    ("j" jinx-mode "flyspell" :toggle t)
    ("f" flymake-mode "flymake" :toggle t))
   "Emacs"
   (("D" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
    ("X" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit))
    ("q" nil "Quit" :color red :exit t)
    )
   ))

;;;; Hydra-All > hydra-window

(defvar hydra-window--title (concat (nerd-icons-faicon "nf-fa-windows" :height 1.0)
                                    " hydra-window :"))

(pretty-hydra-define hydra-window (:foreign-keys warn :title hydra-window--title :quit-key "<escape>")
  ("Actions"
   (("TAB" other-window "switch")
    ("x" ace-delete-window "delete")
    ("m" ace-delete-other-windows "maximize")
    ("s" ace-swap-window "swap")
    ("a" ace-select-window "select"))

   "Resize"
   (("h" move-border-left "←")
    ("j" move-border-down "↓")
    ("k" move-border-up "↑")
    ("l" move-border-right "→")
    ("n" balance-windows "balance")
    ("f" toggle-frame-fullscreen "toggle fullscreen"))

   "Split"
   (("b" split-window-right "horizontally")
    ("B" split-window-horizontally-instead "horizontally instead")
    ("v" split-window-below "vertically")
    ("V" split-window-vertically-instead "vertically instead"))

   "Zoom"
   (("+" zoom-in "in")
    ("=" zoom-in)
    ("-" zoom-out "out")
    ("0" jp-zoom-default "reset")

    ("q" nil "Quit" :color red :exit t)
    ("<f1>" nil "Quit" :color red :exit t)
    )))

;;;; Hydra-All > hydra-jump-to-files

(defhydra hydra-jump-to-files
  (:color amaranth :exit t :quit-key "q")
  "Jump to my org files"

  ("i" (find-file (my/org-inbox-file)) "Inbox")
  ("t" (find-file (my/org-tasks-file)) "Tasks")
  ("b" (find-file (my/org-blog-file)) "Blog")
  ("r" (find-file (my/org-reading-file)) "Reading")
  ("l" (find-file (my/org-links-file)) "Links")
  ("d" (find-file (my/org-drill-file)) "Drill")
  ("m" (find-file (my/org-mobile-file)) "Mobile")
  ("Q" (find-file (my/org-quote-file)) "Quote")
  ("c" (find-file (my/org-contacts-file)) "Contacts")
  ("k" (find-file (my/org-kdc-file)) "KDC")
  ("g" (find-file (my/org-glossary-file)) "Glossary")
  ("G" (find-file (my/org-tags-file) "Tags"))

  ;; ("p" (find-file org-projectile-file) "Project")
  ("z" (find-file "~/.zshrc") "zshrc")
  ;; ("e" (find-file "~/.spacemacs.d/elfeed.org") "elfeed.org")
  ("q" nil "Quit" :color red  :exit t))

;;;; Hydra-All > hydra-jump-to-directory

(defhydra hydra-jump-to-directory
  (:color amaranth :exit t :quit-key "q")
  "Jump to directory"

  ("b" (find-file (concat user-project-directory "blog") "blog"))
  ("n" (find-file (concat user-project-directory "notes") "notes"))
  ("c" (find-file "~/nosync/clone-notes/") "clone-notes")
  ("C" (find-file "~/sync/markdown/cheat") "cheat")
  ("m" (find-file "~/sync/man") "man")
  ("o" (find-file (concat org-directory) "org"))
  ("s" (find-file "~/.doom.d/snippets/") "snippets")
  ("p" (find-file "~/sync/markdown/prompt") "Prompt")
  ("P" (find-file "~/Pictures") "Pictures")

  ("v" (find-file "~/Videos") "Videos")
  ("d" (find-file "~/Documents") "Documents")
  ("D" (find-file "~/Downloads") "Downloads")
  ("u" (find-file "~/Public") "Public")
  ("t" (find-file "~/Templates") "Templates")
  ("q" nil "Quit" :color red :exit t))

;;;; Hydra-All > hydra-bm

;; (pretty-hydra-define hydra-bm ()
;;   ("Misc"
;;    (("n" bm-next "bm-next")
;;     ("p" bm-previous "bm-previous")
;;     ("t" bm-toggle "bm-toggle")
;;     ("q" nil "Quit" :color red :exit t))
;;    "Show"
;;    (("s" bm-show "bm-show" :exit t)
;;     ("S" bm-show-all "bm-show-all" :exit t)
;;     )))

;;;; Hydra-All > Key 'F1' or 's-s'

(pretty-hydra-define hydra-all
  (:quit-key "q" :title "All")
  ( "Apps" (
            ("SPC" gptel-menu "gptel-menu" :exit t)
            ("i" magit-status "magit" :exit t)
            ;; ("m" major-mode-hydra "Major-mode" :exit t)
            ("d" hydra-denote/body "Denote" :exit t)
            ;; ("r" hydra-roam/body "Roam" :exit t)
            ;; ("e" hydra-ekg/body "eKg" :exit t)
            ("o" hydra-org-main/body "Org" :exit t)
            ("n" hydra-mail/body "notmuch mail" :exit t)
            ;; ("!" ash/el-secretario-daily-review "secretary" :exit t)
            )
    "Edit" (
            ("m" hydra-multiple-cursors/body "multiple-cursors" :exit t)
            ("s" hydra-structural/body  "structural" :exit t)
            ("p" hydra-expand/body "expand-region" :exit t)
            ("y" hydra-yas/body "snippets" :exit t))
    "Move" (
            ("b" consult-bookmark "bookmark" :exit t)
            ("J" hydra-jumps/body "jumps" :exit t)
            ("E" hydra-flymake/body "errors" :exit t)
            ("g" deadgrep "grep" :exit t)
            )
    "Misc" (
            ("w" hydra-window/body "window" :exit t)
            ("f" hydra-find/body "find" :exit t)
            ("T" hydra-toggles/body "toggles" :exit t)
            ("q" nil "Quit" :color red :exit t)
            ("<f1>" nil "Quit" :color red :exit t)
            )
    ))

;;; Major-Mode-Hydra : 'F2' or 'M-c'

;;;; Major-Mode-Hydra > Load

;; Before hydra because we use pretty-hydra-define in the hydra confg.
(setq major-mode-hydra-invisible-quit-key "q")

;; (pretty-hydra-define hydra-window (:foreign-keys warn :title hydra-window--title :quit-key "q")

;;;; Major-Mode-Hydra > org-mode

(progn
  (defvar hydra-org-mode-title
    (concat (nerd-icons-sucicon "nf-custom-orgmode") " Org-mode"))

  (major-mode-hydra-define org-mode
    (:title hydra-org-mode-title ;; "Org-mode"
     :color amaranth :separator "-" :quit-key "<escape>")
    (
     "Agenda" (
               ("c" org-capture "org-capture")
               ("a" org-agenda-file-to-front "agenda-file-to-front")
               ("A" org-remove-file "org-remove-file")
               ("t" org-time-stamp "+ date")
               ("T" (lambda () (interactive) (org-time-stamp '(4))) "+ date/time")
               ("d" org-deadline "+ deadline")
               ("s" org-schedule "schedule")
               ;; ("d" my/denote-find-file "denote-find")
               ("f" +default/find-in-notes "+default/find-in-notes")
               ("F" +default/org-notes-search "+default/org-notes-search")
               )
     "Movement" (
                 ("u" org-up-element "up" :exit nil)
                 ("j" org-forward-heading-same-level "forward heading" :exit nil)
                 ("k" org-backward-heading-same-level "backward heading" :exit nil)
                 ("n" org-next-visible-heading "next heading" :exit nil)
                 ("p" org-previous-visible-heading "prev heading" :exit nil)
                 ("M-n" org-next-link "next link" :exit nil)
                 ("M-p" org-previous-link "prev link" :exit nil)
                 ("M-j" org-next-block "next block" :exit nil)
                 ("M-k" org-previous-block "prev block" :exit nil)
                 ("g" org-mark-ring-goto "pop mark" :exit t))
     "Subtrees" (
                 (">" org-demote-subtree "demote" :exit nil)
                 ("<" org-promote-subtree "promote" :exit nil)
                 ("N" org-narrow-to-subtree "narrow")
                 ("r" org-refile "refile")
                 ("K" org-cut-subtree "kill")
                 ("." org-tree-to-indirect-buffer "indirect buffer")
                 ("I" org-id-get-create "create id"))
     "Inserting" (
                  ("ib" org-cite-insert "org-site-insert" :exit t)
                  ("B" citar-insert-citation "insert citation")
                  ("e" org-expiry-insert-expiry "expiry property")
                  ;; ("8" org-insert-heading-respect-content "insert heading")
                  ("9" bh/insert-inactive-timestamp "in-act timestamp" :exit nil)
                  ("C-n" next-line "next-line" :exit nil)
                  ("C-p" previous-line "previous-line" :exit nil)
                  ;; ("y" ash/org-paste-link "yank link" :exit t)
                  )
     ;; "Denote" (("-" denote-show-backlinks-buffer "Backlinks" :toggle t)
     ;;           ("i" denote "add link"))
     "Babel" (("M-g" avy-jump-org-block "Goto ")
              ("M-o" avy-org-babel-execute-src-block "Block ")
              ("M-h" org-babel-execute-subtree "Section")
              ("M-b" org-babel-execute-buffer "Buffer")
              ("M-t" org-babel-tangle "to Default")
              ("M-f" org-babel-tangle-file "choose File")
              ("M-T" org-babel-detangle "from File")
              ;; ("M-e" avy-org-babel-edit-src-block "Edit Block ")
              ;; ("M-s" org-babel-pop-to-session-maybe "Session REPL")
              ;; ("M-v" ha-org-babel-tangle-visit-file "Visit Tangled")
              )
     "Clock" (
              ;; ("P" org-pomodoro "Start pomodoro")
              ;; ("Q" ash/org-pomodoro-til-meeting "Start pomodoro til half hour")
              ("<f2>" org-clock-goto "org-clock-goto" :color blue :exit t)
              ("M-c" nil "Quit" :color red :exit t)
              ("q" nil "Quit" :color red :exit t))
     )
    )
  )

;;;; Major-Mode-Hydra > emacs-lisp-mode

(major-mode-hydra-define emacs-lisp-mode
  (:title "Emacs-Lisp-mode" :color blue :separator "-" :quit-key "<escape>")
  ("Eval" (
           ("b" eval-buffer "eval buffer")
           (";" eval-expression "eval expression")
           ("d" eval-defun "eval defun")
           ("D" edebug-defun "edebug defun")
           ("e" eval-last-sexp "eval last sexp")
           ("<f2>" eval-last-sexp "eval last sexp")
           ("E" edebug-eval-last-sexp "edebug last sexp")
           ("l" ielm "ielm"))
   "Test" (
           ("t" ert "prompt")
           ("T" (ert t) "all")
           ("F" (ert :failed) "failed"))
   "Doc" (
          ("f" describe-function "function")
          ("v" describe-variable "variable")
          ("i" info-lookup-symbol "info lookup")
          ("q" nil "Quit" :color red :exit t)
          ("M-c" nil "Quit" :color red :exit t)
          ))
  )

;;;; DONT Major-Mode-Hydra > python-mode

;; /home/junghan/sync/man/dotsamples/vanilla/hamacs-evil/ha-programming-python.org

;; (major-mode-hydra-define python-mode
;;   (:title "Python-mode" :color pink :separator "-" :quit-key "<escape>")
;;   (
;;    "Basic"
;;    (
;;     ("'" python-shell-switch-to-shell)
;;     ("m" python-mark-defun)
;;     ("v" python-check)
;;     ("s" python-describe-at-point)
;;     ("R" run-python "run-python-repl")
;;     )
;;    ;; "Eval/Repl"
;;    ;; (
;;    ;;  ("ee" python-shell-send-region "send-region")
;;    ;;  ("ed" python-shell-send-defun "send-defun")
;;    ;;  ("eb" python-shell-send-buffer)
;;    ;;  ("ef" python-shell-send-file)
;;    ;;  ("er" python-shell-send-region)
;;    ;;  ("ep" python-shell-send-string)
;;    ;;  ("es" python-shell-send-statement)
;;    ;;  ("<f2>" python-shell-send-statement)
;;    ;;  )
;;    ;; "Navigate"
;;    ;; (
;;    ;;  ("nh" python-nav-backward-statement)
;;    ;;  ("nj" python-nav-forward-block)
;;    ;;  ("nk" python-nav-backward-block)
;;    ;;  ("nl" python-nav-forward-statement)
;;    ;;  ("n[" python-nav-backward-up-list)
;;    ;;  ("n]" python-nav-up-list)
;;    ;;  ("na" python-nav-beginning-of-block)
;;    ;;  ("ne" python-nav-end-of-block)
;;    ;;  ("nm" python-nav-if-name-main)
;;    ;;  )
;;    ;; "Imports"
;;    ;; (
;;    ;;  ("ii" pyimport-remove-unused)
;;    ;;  ("iu" pyimport-insert-missing)
;;    ;;  )
;;    "Help/Misc"
;;    (
;;     ("hh" consult-history "history" :exit t)
;;     ("hp" python-eldoc-at-point)
;;     ("q" nil "Quit" :color red :exit t)
;;     )
;;    )
;;   )

;;;; Major-Mode-Hydra > eshell-mode

(major-mode-hydra-define eshell-mode
  (:title "Eshell-mode" :color pink :separator "=" :quit-key "<escape>")
  ("Movement" (
               ("h" consult-history "history" :exit t)
               ("q" nil "Quit" :color red :exit t)
               ("M-c" nil "Quit" :color red :exit t)
               )))

;;;; TODO Major-Mode-Hydra > clojure-mode

(major-mode-hydra-define clojure-mode
  (:title "clojure-mode" :color blue :separator "-" :quit-key "<escape>")
  ("Eval" (
           (";" #'cider-eval-defun-to-comment "cider-eval-defun-to-comment")
           ("(" #'cider-eval-list-at-point "cider-eval-list-at-point")
           ("b" #'cider-eval-buffer "cider-eval-buffer")
           ("e" #'cider-eval-last-sexp "cider-eval-last-sexp")
           ("<f2>" #'cider-eval-last-sexp "cider-eval-last-sexp")
           ("f" #'cider-eval-defun-at-point "cider-eval-defun-at-point")
           ("i" #'cider-interrupt "cider-interrupt")
           ("r" #'cider-eval-region "cider-eval-region"))
   "ETC" (
          ("q" nil "Quit" :color red :exit t)
          ("M-c" nil "Quit" :color red :exit t)
          ))
  )

;;;; Major-Mode-Hydra > Elfeed

(progn
  (defvar ha-elfeeds-title
    (concat (nerd-icons-faicon "nf-fa-rss")  " Feed Reader"))

  (major-mode-hydra-define elfeed-search-mode
    (:title ha-elfeeds-title)
    ("Feeds"
     (("U" (elfeed-search-fetch 4) "Refresh Feeds")
      ("u" elfeed-update "Update Screen")
      ("f" elfeed-search-live-filter "Filter")
      ("F" elfeed-search-clear-filter "Clear Filter")
      ("X" elfeed-unjam "Unjam"))
     "Entry"
     (("r" elfeed-search-untag-all-unread "Mark read" :color pink)
      ("R" ha-elfeed-tag-unread "Mark read/refresh")
      ("y" elfeed-search-yank "Copy URL"))))

  (major-mode-hydra-define elfeed-show-mode
    (:title ha-elfeeds-title)
    ("Entry"
     (("n" elfeed-show-next "Next Entry" :color pink)
      ("p" elfeed-show-prev "Previous Entry" :color pink))
     "Read"
     (("b" elfeed-show-visit "Show in EWW")
      ("B" elfeed-show-visit-gui "Show in Browser")
      ("y" elfeed-show-yank "Copy URL"))
     "Navigation"
     (("o" link-hint-open-link "Open Link")
      ("q" bury-buffer "Close")
      ("Q" delete-window "Close Window"))))

  (defun ha-elfeed-tag-unread ()
    (interactive)
    (elfeed-search-untag-all 'unread)
    (elfeed-search-update))
  )


;;; provide

(provide 'hydrakeys)

;;; end-of
