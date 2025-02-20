;;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611

;; This file is not part of GNU Emacs.

;; License: GPLv3

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.
;;
;; See the GNU General Public License for more details. You should have received
;; a copy of the GNU General Public License along with this program. If not, see
;; <https://www.gnu.org/licenses/>.
;;
;;  ____
;; / ___| _ __   __ _  ___ ___ _ __ ___   __ _  ___ ___
;; \___ \| '_ \ / _` |/ __/ _ \ '_ ` _ \ / _` |/ __/ __|
;;  ___) | |_) | (_| | (_|  __/ | | | | | (_| | (__\__ \
;; |____/| .__/ \__,_|\___\___|_| |_| |_|\__,_|\___|___/
;;       |_|
;;
;;; Commentary:

;;  While any text editor can save your files, only Emacs can save your soul
;; ~/sync/org/roam/configs/spacemacs.org

;; fix Emacs 30.x on Android ELPA gpg problem
;; $ gpg --homedir ~/.emacs.d/elpa/gnupg --receive-keys 066DAFCB81E42C40

;; ❶ :: U+2776 ==> 더원싱 태그로 활용
;; ㉽ :: U+327D
;; ㉼ :: U+327C

;;; Pre-Init and Load

;; (setq debug-on-error t)

;;;; Load paths

(setq user-dotemacs-dir "~/dotemacs/") ;; dotspacemacs-directory
(setq emacs-type 'spacemacs)

;; ;; ;; Recommended to have this at the top
;; ;; (setq treesit-extra-load-path `(,(concat user-emacs-directory "tree-sitter-module/dist/")
;;                                 ,(concat user-emacs-directory "tree-sitter")))

;; optimize: force "lisp"" and "site-lisp" at the head to reduce the startup time.
;; (add-to-list 'load-path (concat dotspacemacs-directory "lisp"))
;; (dolist (dir '("lisp" "ccmenu")) ; "site-lisp"
;;   (push (expand-file-name dir dotspacemacs-directory) load-path))

;;;; Frame Version PGTK

;; You should be able to use input methods since GtkIMContext is enabled by
;; default. If you don't like GtkIMContext, you can disable it by writing as
;; follows in ~/.emacs: pgtk-use-im-context disable gtk im modules for
;; emacs-pgtk, add "Emacs*UseXIM: false" to ~/.Xresources to disable xim
(if (eq window-system 'pgtk)
    (pgtk-use-im-context nil))

(when (boundp 'pgtk-use-im-context-on-new-connection)
  (setq pgtk-use-im-context-on-new-connection nil))

;; Emacs version 29 added a new frame parameter for "true" transparency, which
;; means that only the blackground is transparent while the text is not. started
;; to use new #emacs 29 alpha-background frame-parameters. It only works on
;; gnu/#linux at the moment and look beautiful :

(if (eq system-type 'gnu/linux)
    (setq default-frame-alist (push '(alpha-background . 93) default-frame-alist))
  (setq default-frame-alist (push '(alpha . (95 90)) default-frame-alist)))

(setq emacs-major-version-string (format "%s" emacs-major-version))

;; (unless (display-graphic-p) ; terminal
;;   (set-display-table-slot standard-display-table
;;                           'vertical-border
;;                           (make-glyph-code ?│)))

;;;; Emacs-startup-hook

(setq my/emacs-started nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)
            (setq my/emacs-started t)))

;;;; Custom Define 'emacs-type'

(setq-default root-path "/")

;; /home/junghan/sync/man/dotsamples/korean/injae-dotfiles/module/+emacs.el
(defvar *is-mac*     (eq system-type 'darwin))
(defvar *is-windows* (eq system-type 'windows-nt))
(defvar *is-cygwin*  (eq system-type 'cygwin))
(defvar *is-linux*   (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
(defvar *is-wsl*     (eq (string-match "Linux.*microsoft.*WSL2.*Linux" (shell-command-to-string "uname -a")) 0))
(defvar *is-unix*    (or *is-linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)))
(defvar *is-android*  (eq system-type 'android)) ; android native emacs

;; (defvar IS-TERMUX
;;   (and (eq system-type 'gnu/linux)
;;     (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a")))))

;; on anroid 는 모두 해당
(defvar IS-TERMUX
  (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a"))))

(when IS-TERMUX
  (setq root-path "/data/data/com.termux/files/"))

(setq my/slow-ssh
      (or
       (string= (getenv "IS_TRAMP") "true")))

(setq my/remote-server
      (or (string= (getenv "IS_REMOTE") "true")
          ;; (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a")))
          (string= (system-name) "server1")
          (string= (system-name) "server2")
          (string= (system-name) "server3"))) ; for test

(setenv "IS_EMACS" "true")

;;;; TODO Native Android Support

;; (when *is-android*
;;     (message "Loading Android Emacs\n")
;;     (setenv "PATH" (format "%s:%s" "/data/data/com.termux/files/usr/bin" (getenv "PATH")))
;;     (setenv "LD_LIBRARY_PATH" (format "%s:%s" "/data/data/com.termux/files/usr/lib" (getenv "LD_LIBRARY_PATH")))
;;     (push "/data/data/com.termux/files/usr/bin" exec-path))

;;; Spacemacs Layer

;;;; 'Start' dotspacemacs/layers

(defun dotspacemacs/layers ()

  ;; 'Base' Configurations
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs-base
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it. (default 'unused)
   dotspacemacs-enable-lazy-installation nil
   dotspacemacs-ask-for-lazy-installation t
   ;; dotspacemacs-configuration-layer-path `(,(expand-file-name "layers/" dotspacemacs-directory))
   dotspacemacs-directory-snippets-dir '(concat user-emacs-dir "snippets/"))

;;;; 'Layer' Declarations
  ;; Default Layer Configurations List of configuration layers to load.
  (setq-default
   dotspacemacs-configuration-layers
   '(
;;;;; Layer: jh-base

     (spacemacs-defaults :packages (not electric-indent-mode))

     helpful

     (shell :variables shell-default-shell 'vterm
            shell-default-term-shell (concat root-path "usr/bin/zsh")
            spacemacs-vterm-history-file-location "~/.zsh_history"
            shell-default-full-span nil ; default t
            shell-default-position 'bottom)

     ;; ibuffer :: SPC b I, C-x C-b
     ;; refresh :: g r, fliter group :: g j/k, ][, TAB/S-TAB, M-n/p
     (ibuffer :variables
              ;; ibuffer-old-time 8 ; buffer considered old after that many hours
              ibuffer-show-empty-filter-groups nil
              ibuffer-group-buffers-by 'projects)

;;;;; Layer: jh-completion

     (auto-completion :packages (hippie-exp yasnippet))

     (compleseus :variables
                 compleseus-engine 'vertico
                 compleseus-use-nerd-icons t
                 marginalia-align 'left)

;;;;; Layer: jh-visual

     (colors :packages (color-identifiers-mode rainbow-mode))

     imenu-list

     (spacemacs-modeline :packages (doom-modeline))

     (spacemacs-visual :packages (ansi-colors desktop display-fill-column-indicator popwin posframe zoom-frm all-the-icons))

     ;; (themes-megapack :packages (modus-themes doom-themes))

;;;;; Layer: jh-navigation

     (spacemacs-navigation :packages (not paradox)) ; symbol-overlay

     ;; Visual file manager - `SPC p t'
     ;; treemacs-no-png-images t removes file and directory icons
     ;; 2023-09-01 Delete treemacs-magit treemacs-persp treemacs-projectile conflict lsp-mode
     ;; 2024-01-31 enable :packages (not treemacs-magit treemacs-persp treemacs-projectile)
     (treemacs
      :packages (not winum)
      :variables
      treemacs-position 'left
      treemacs-width 45
      treemacs-imenu-scope 'current-project
      treemacs-indentation 1
      treemacs-space-between-root-nodes nil ; spacing in treemacs views
      treemacs-fringe-indicator-mode nil

      treemacs-use-all-the-icons-theme nil ; important
      treemacs-use-icons-dired nil ; important
      treemacs-lock-width t)

;;;;; Layer: jh-workspace

     ;; (tabs :variables tabs-icons nil)
     spacemacs-layouts
     spacemacs-purpose

     bm
     spacemacs-project

     (git :packages (not evil-collection forge)
          :variables
          ;; delta-plugin requires git-delta (apt install git-delta)
          ;; git-enable-magit-delta-plugin t
          ;; Magit git client full screen (q restores previous layout)
          git-magit-status-fullscreen t
          magit-diff-refine-hunk t ; show word-granularity differences in current diff hunk
          git-enable-magit-todos-plugin nil)

     ;; Highlight changes in buffers
     ;; SPC g . transient state for navigating changes
     ;; diff-hl, diff-mode, evil-unimpaired, smerge-mode, vc, browse-at-remote
     ;; version-control-diff-side 'left ; default 'right
     version-control

;;;;; Layer: jh-checker

     ;; Spell as you type with Flyspell package,
     ;; requires external command - ispell, hunspell, aspell
     ;; SPC S menu, SPC S s to check current word
     (spell-checking :packages (not auto-dictionary flyspell-popup flyspell-correct-popup)
                     :variables
                     enable-flyspell-auto-completion nil
                     spell-checking-enable-auto-dictionary nil
                     spell-checking-enable-by-default nil)

     ;; (syntax-checking :packages (not flycheck-pos-tip popwin) ; popwin
     ;;                  :variables
     ;;                  syntax-checking-enable-by-default nil ; default t - prog-mode
     ;;                  flycheck-help-echo-function nil
     ;;                  flycheck-display-errors-function nil
     ;;                  ;; syntax-checking-enable-tooltip nil ; default t
     ;;                  ;; syntax-checking-auto-hide-tooltips 3 ;
     ;;                  ;; syntax-checking-use-standard-error-navigation t ; default nil
     ;;                  )

;;;;; Layer: jh-evil

     ;; 2023-11-03 evil-lisp-state evil-cleverparens ; depends on smartparens
     (spacemacs-evil
      :packages
      (evil-anzu evil-args evil-goggles evil-collection evil-escape evil-exchange evil-iedit-state evil-indent-plus evil-lion evil-nerd-commenter evil-matchit evil-numbers evil-surround evil-textobj-line evil-unimpaired evil-visual-mark-mode evil-visualstar evil-tutor eldoc hs-minor-mode)
      :variables
      ;; wdired wgrep - 넣지 말자. 스페이스맥스 권장 키 이용
      spacemacs-evil-collection-allowed-list ;; evil-collection-mode-list
      '(
        eww dired quickrun ediff (buff-menu "buff-menu") vterm magit-todos
        (magit magit-repos magit-submodule) forge

        ;; additional
        outline xref tar-mode thread
        atomic-chrome arc-mode
        calc bookmark calendar debug
        disk-usage dictionary
        eldoc flymake eglot emms ement

        tab-bar man
        dashboard shortdoc info
        devdocs
        corfu
        youtube-dl
        rg ripgrep
        deadgrep
        leetcode
        (image image-mode)
        pass popup proced profiler (process-menu simple)
        telega
        vlf vundo woman

        python
        ))

     ;; 이것 때문에 실수로 Replace 하는 경우가 생긴다.
     ;; (evil-snipe :variables evil-snipe-enable-alternate-f-and-t-behaviors t)

;;;;; Layer: jh-editing

     ;; `g r' menu in Emacs normal state
     ;; 2023-11-21 disable evil-mc that conflicts own keybindings C-n/C-p etc.
     ;; ~/sync/man/dotsamples/vanilla/minemacs/modules/me-multi-cursors.el
     (multiple-cursors :variables
                       multiple-cursors-backend 'mc
                       mc/cmds-to-run-once '(upcase-region))

     (spacemacs-editing :packages (not password-generator drag-stuff undo-tree smartparens)
                        :variables dotspacemacs-undo-system 'undo-fu)

     (spacemacs-editing-visual :packages (hide-comnt rainbow-delimiters volatile-highlights term-cursor writeroom-mode))

     copy-as-format

     (xclipboard :variables xclipboard-enable-cliphist t)

;;;;; Layer: jh-writing

     asciidoc ; e.g. docs.cider.mx editing

     (markdown :packages (markdown-mode edit-indirect markdown-toc gh-md))

     (translate :variables gt-langs '(en ko)
                translate-reference-buffer-read-only nil
                translate-enable-highlight t
                translate/paragraph-render 'buffer
                translate/word-render 'buffer ;; default 'posframe
                )

     plantuml

;;;;; Layer: jh-reading

     ;; spacemacs-language

     (eww :packages (not texfrag)
          :variables
          shr-max-image-proportion 0.6
          shr-width fill-column          ; check `prot-eww-readable'
          shr-max-width fill-column
          shr-use-fonts nil)

     ;; (pocket :variables
     ;;     pocket-reader-color-title nil
     ;;     pocket-reader-color-site t
     ;;     pocket-reader-site-column-max-width 16)

     ;; xkcd
     djvu

;;;;; Layer: jh-feed

     (elfeed
      :variables
      elfeed-enable-goodies nil
      elfeed-search-filter "" ; "@3-months-ago"
      rmh-elfeed-org-files (list (concat org-directory "/elfeed.org"))
      elfeed-enable-web-interface nil
      url-queue-timeout 30)

     ;; mu4e

     ;; fast, global-search and tag-based email system
     ;; (notmuch
     ;;  :variables
     ;;  notmuch-messages-deleted-tags '("+deleted" "-inbox" "-unread"))

;;;;; Layer: jh-coding

     ;; (common-lisp
     ;;  :packages (auto-highlight-symbol common-lisp-snippets
     ;;                                   evil slime))

     ;; (racket :packages (racket-mode))

     ;; (scheme :variables
     ;;         scheme-implementations '(mit)) ;; guile racket

     ;; eglot

     (emacs-lisp :packages (not emr flycheck-package flycheck-elsa))

     pandoc

     (dash :variables zeal-at-point-zeal-version "0.6.2")

     csv ; Tools to work with comma separate values e.g. data science data
     ;; graphviz ; graphviz - open-source graph declaration system

     (json :variables
           json-fmt-on-save nil
           json-backend nil) ; not 'lsp

     (yaml :variables yaml-enable-lsp nil)

     ;; linting, style checking, formatting supports
     (shell-scripts
      :packages (not fish-mode flycheck-bashate)
      :variables
      shell-scripts-format-on-save t ; with shfmt
      shell-scripts-backend nil ; 'lsp
      )

     (html :variables web-fmt-tool 'prettier)

     prettier ; + format-all or apheleia
     debug
     docker

     restclient

     semantic-web

     (clojure
      :packages (not evil-cleverparens)
      :variables
      clojure-backend 'lsp
      clojure-enable-kaocha-runner nil ; Kaocha test runner
      clojure-enable-sayid nil ; default nil
      clojure-enable-clj-refactor nil ; dependent on paredit
      )

     ;; haskell ; default dante

     ;; prodigy
     ;; protobuf
     ;; kubernetes
     ;; systemd

     ;; Language server protocol with minimal visual impact
     ;; https://practical.li/spacemacs/install-spacemacs/clojure-lsp/
     ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
     (lsp
      :packages (not helm-lsp lsp-ivy) ; lsp-treemacs lsp-origami
      :variables
      ;; lsp-idle-delay 0.2  ; smooth LSP features response
      lsp-headerline-breadcrumb-enable t ; spacemacs t
      lsp-headerline-breadcrumb-icons-enable nil
      lsp-ui-sideline-enable nil ; default t - nil for less distraction
      ;; lsp-ui-doc-enable nil ;; default t - disable all doc popups
      lsp-treemacs-error-list-current-project-only t
      )

     dap

     (python
      :packages (not helm-cscope
                     helm-pydoc
                     lsp-python-ms
                     anaconda-mode
                     company-anaconda
                     ;; lsp-pyright
                     ;; semantic
                     ;; cython-mode pip-requirements
                     ;; py-isort
                     ;; blacken
                     nose
                     yapfify
                     pippel)
      :variables
      python-backend 'lsp
      python-indent-guess-indent-offset-verbose nil
      python-lsp-server 'pyright ; 'pylsp
      lsp-pyright-langserver-command "basedpyright" ;; pipx install basedpyright
      python-indent-offset 4 ; use editorconfig
      ;; python-formatter 'lsp
      python-test-runner 'pytest
      python-poetry-activate nil
      ;; python-save-before-test nil
      ;; python-sort-imports-on-save nil ; use lsp
      pylookup-html-locations '("http://docs.python.org/ko/3.12")
      )

     ;; tmux ; X
     ;; (gtags :variables gtags-enable-by-default t) ; X
     ;; (command-log :packages (command-log-mode)) ; X never!

;;;;; Layer: jh-org

     (spacemacs-org :packages (default-org-config)) ; done

     (org :packages (not persp-mode emoji-cheat-sheet-plus)
          :variables
          org-want-todo-bindings t ; use evil-binding (t, T, M-t)
          org-enable-epub-support t
          org-enable-transclusion-support t
          org-enable-asciidoc-support t
          org-enable-github-support t ; gfm markdown support
          org-enable-hugo-support t ; use custom version
          org-enable-reveal-js-support t ; 2024-03-25 turn on < 2023-07-06 use ox-reveal instead
          org-todo-dependencies-strategy 'naive-auto
          org-enable-appear-support t

          ;; should be nil
          ;; org-appear-trigger 'manual ; for evil editing
          org-enable-org-journal-support t
          org-enable-org-contacts-support nil
          org-enable-modern-support nil
          org-enable-org-brain-support nil
          org-enable-verb-support nil
          org-enable-sticky-header nil ; use my-breadcrumbs
          org-enable-valign nil ; performance issue, should be nil
          org-enable-trello-support nil ; should be nil
          org-enable-roam-support nil
          org-enable-roam-protocol nil
          org-enable-roam-ui nil

          ;; org-directory "~/sync/org/" ; (file-truename "~/sync/org/")
          org-directory user-org-directory
          )

;;;;; Layer: jh-misc

     (spacemacs-misc :packages (not devdocs)) ;; use devdocs-browser for hangul

     ;; games

;;;;; Layer: jh-llm

     (llm-client :variables
                 llm-client-enable-ellama nil
                 llm-client-enable-gptel t)
     github-copilot
     openai

;;;;; end
     )
   )

;;;; 'Extra' Package Options

  ;; List of additional packages that will be installed without being wrapped
  ;; in a layer (generally the packages are installed only and should still be
  ;; loaded using load/require/use-package in the user-config section below in
  ;; this file). If you need some configuration for these packages, then
  ;; consider creating a layer. You can also put the configuration in
  ;; `dotspacemacs/user-config'. To use a local version of a package, use the
  ;; `:location' property: '(your-package :location "~/path/to/your-package/")
  ;; Also include the dependencies as they will not be resolved automatically.
  (setq-default
   dotspacemacs-additional-packages
   '(
;;;;; Packages: jh-base

     ;; (transient :min-version "0.7.4")
     dired-preview
     dired-narrow

     webpaste
     transpose-frame
     casual-suite

;;;;; Packages: jh-completion

     corfu
     cape
     tempel
     tempel-collection

     consult-gh
     (consult-omni :location (recipe :fetcher github :repo "armindarvish/consult-omni" :files (:defaults "sources/*.el")))

;;;;; Packages: jh-visual

     ;; minions
     nerd-icons
     nerd-icons-dired
     nerd-icons-corfu
     ;; treemacs-nerd-icons
     hl-todo
     fontaine
     list-unicode-display
     spacious-padding
     ;; (doom-themes :location (recipe :fetcher github :repo "junghan0611/doom-themes" :branch "ko" :files ("*.el" "extensions/*.el" "themes/*.el")))

;;;;; Packages: jh-workspace

     consult-git-log-grep
     (outli :location (recipe :fetcher github :repo "jdtsmith/outli"))
     tmr

;;;;; Packages: jh-feed


;;;;; Packages: jh-checker

     consult-flycheck
     consult-flyspell
     jinx

;;;;; Packages: jh-evil

     evil-string-inflection
     evil-traces
     ;; evil-owl

;;;;; Packages: jh-editing

     puni ; replace smartparens
     goto-last-change
     unfill
     deadgrep
     visual-fill-column
     separedit ; Edit comment/string/docstring/code block

     ;; pangu-spacing
     ;; (pangu-spacing :location (recipe :fetcher github :repo "junghan0611/pangu-spacing" :branch "ko"))

;;;;; Packages: jh-writing

     (txl :location (recipe :fetcher github :repo "junghan0611/txl.el" :branch "ko")) ;; tmalsburg
     immersive-translate
     palimpsest
     olivetti
     logos
     ;; mastodon

     ;; zotxt
     ;; zoxide
     ;; aas
     laas
     (ten :location (recipe :fetcher sourcehut :repo "nobiot/ten"))
     ;; typst-ts-mode

;;;;; Packages: jh-reading

     ;; wiki-summary
     ;; define-it
     ;; lexic
     ;; external-dict
     ;; define-word ; copy from spacemacs-language
     ;; (sdcv :location (recipe :fetcher github :repo "manateelazycat/sdcv"))
     ;; wordreference
     ;; powerthesaurus

     (guess-language :location (recipe
                                :fetcher github
                                :repo "junghan0611/guess-language.el"
                                :files ("*.el" "trigrams/*")))

     google-translate

     (hypothesis :location (recipe :fetcher github :repo "EFLS/hypothesis"))

;;;;; Packages: jh-coding

     jupyter

     ;; > tools
     apheleia
     ;; breadcrumb
     ;; dape

     ;; (eglot :min-version "1.17")
     ;; consult-eglot
     direnv
     ;; (copilot-chat :location (recipe :fetcher github :repo "chep/copilot-chat.el"
     ;;                                 :files ("*.el")))

     ;; conda

     ;; > treesit
     ;; (combobulate :location (recipe :fetcher github :repo "mickeynp/combobulate"))
     ;; evil-textobj-tree-sitter

     ;; > languages
     eros ; emacs-lisp
     (elisp-demos :location (recipe :fetcher github :repo "junghan0611/elisp-demos" :branch "ko"))

     ;; elixir-ts-mode ; elixir
     ;; exunit
     ;; ob-elixir

     bats-mode ; shell-scripts
     ;; awk-ts-mode

     ;; clojure-ts-mode

     ;; > python
     pipenv
     pytest

     ;; hy-mode
     ;; ob-hy

     ;; > document
     devdocs-browser ; with EWW

     ;; > practice
     exercism
     leetcode

     ;; > AI

     clojure-essential-ref-nov
     clay

;;;;; Packages: jh-org

     citar
     citar-embark
     org-drill
     org-rainbow-tags
     org-bookmarks
     ;; org-ql
     org-sliced-images

     (orgabilize :location (recipe :fetcher github :repo "akirak/orgabilize.el"))
     org-noter

     ;; wikinfo   ; wiki info-mode
     ;; wikinforg
     side-notes

     ob-mermaid
     ;; ob-d2
     ox-reveal

     org-fragtog
     cdlatex
     math-preview ; for org and markdown with mathjax
     math-symbol-lists

     ;; (org-node :location (recipe :fetcher github :repo "meedstrom/org-node"))

;;;;; Packages: jh-org-ext

     ;; consult-org-roam
     ;; citar-org-roam

     (org-glossary :location (recipe :fetcher github :repo "tecosaur/org-glossary"
                                     :files ("*.el" "*.org" "*.texi")))
     ;; math-symbol-lists
     ;; org-rich-yank
     ;; org-make-toc
     ;; (org-remark :location (recipe :fetcher github :repo "nobiot/org-remark")
     ;;     :toggle (not (or *is-windows* my/remote-server IS-TERMUX)))

;;;;; Packages: jh-pkm

     denote
     (consult-denote :location (recipe :fetcher github :repo "protesilaos/consult-denote"
                                       :files ("*.el" "*.org")))
     citar-denote
     denote-explore
     consult-notes

     ;; triples
     ;; (ekg :location (recipe :fetcher github :repo "ahyatt/ekg" :branch "develop" :files ("*.el" "*.org")))

     ;; (hyperbole :location (recipe :fetcher github :repo "rswgnu/hyperbole"))

;;;;; Packages: jh-misc

     pretty-hydra
     major-mode-hydra
     ;; (term-keys :location (recipe :fetcher github :repo "junghan0611/term-keys"))
     ;; xclip

     keycast

     ;; wakatime-mode
     ;; interaction-log
     ;; (screenshot :location (recipe :fetcher github :repo "tecosaur/screenshot"
     ;;                               :files ("*.el" "*.org")))

     ;; (atomic-chrome :toggle (not (or *is-windows* my/remote-server IS-TERMUX)))
     redacted
     disk-usage
     ;; ement
     ;; literate-calc-mode

     ;; engine-mode

;;;;; DONT Packages: jh-llm

     ;; (llm :location (recipe :fetcher github :repo "ahyatt/llm" :branch "main" :files ("*.el" "*.org")))
     ;; ellama

     ;; (khoj :toggle (not (or *is-windows* my/remote-server IS-TERMUX)))
     ;; chatgpt-shell
     ;; ob-chatgpt-shell
     ;; pcsv
     ;; dall-e-shell

;;;;; Packages: TODO waiting lists

     ;; musicbrainz ; music database api

     ;; etherpad ; 오픈소스 온라인 공동 편집 - 이맥스 연동
     ;; i-ching ; 주역 또는 역경은 점술 방법, 패턴 생성기 ?
     ;; smog ; A simple way to analyse the writing style, word use and readability of prose in Emacs.
     ;; quiet ; disconnect from the online world for a while

     quarto-mode ;; poly mode

     ;; revert-buffer-all
     ;; docsim ; search document with syntax

     ;; (champagne :location (recipe :fetcher github
     ;;                              :repo "positron-solutions/champagne"))
     ;; (ox-moderncv :location (recipe :fetcher github :repo "ohyecloudy/org-cv"))

     ;; (cal-korea-x :location (recipe :fetcher github :repo "cinsk/cal-korea-x"))
     ;; (typo :location (recipe :fetcher sourcehut :repo "pkal/typo")) ; ?

     ;; ox-zenn ;; ox-qmd
     )
   )

;;;; 'GUI' only layer and packages

  (when (display-graphic-p)

;;;;; Layers for gui only

    (add-to-list 'dotspacemacs-configuration-layers 'pdf)
    (add-to-list 'dotspacemacs-configuration-layers 'epub)

    ;; DONT Disable EAF
    ;; (add-to-list 'dotspacemacs-configuration-layers
    ;;              '(eaf :variables eaf-apps '(eaf-browser eaf-pdf-viewer)
    ;;                    ;; eaf-enable-debug t
    ;;                    eaf-pdf-synctex-path nil
    ;;                    eaf-pdf-dark-mode 'ignore
    ;;                    ;; browse-url-browser-function 'browse-url-firefox
    ;;                    ))

;;;;; Packages for gui only
    (add-to-list 'dotspacemacs-additional-packages 'saveplace-pdf-view)
    )

;;;; Excluded Packages

  (setq-default
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages'(
                                   undo-fu-session
                                   helm-make
                                   flycheck-pos-tip
                                   alchemist
                                   company
                                   auto-complete ac-ispell
                                   tern
                                   web-beautify
                                   emoji-cheat-sheet-plus ; dependent helm
                                   counsel-gtags
                                   fancy-battery
                                   fish-mode
                                   valign
                                   undo-tree
                                   volatile-highlights
                                   )
   dotspacemacs-install-packages 'used-only)

  ) ; defun dotspacemacs/layers

;;; Spacemacs Configuration

(defun dotspacemacs/init ()

;;;; Start and several functions

  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need to
   ;; compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;;
   ;; WARNING: pdumper does not work with Native Compilation, so it's disabled
   ;; regardless of the following setting when native compilation is in effect.
   ;;
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 15

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)
   ;; dotspacemacs-gc-cons '(256000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables. (default 'vim)
   ;; dotspacemacs-editing-style 'vim
   dotspacemacs-editing-style '(vim :variables
                                    vim-style-visual-feedback t ; default nil
                                    vim-style-remap-Y-to-y$ nil
                                    vim-style-retain-visual-state-on-shift t
                                    vim-style-visual-line-move-text nil
                                    vim-style-ex-substitute-global nil)

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official ;; 100 ; 'random
   ;; dotspacemacs-startup-banner (concat
   ;;                              dotspacemacs-directory "assets/splash/emacs.txt")

   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'auto

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   ;; dotspacemacs-startup-lists '(
   ;;                              (projects . 5)
   ;;                              ;; (agenda . 5)
   ;;                              (bookmarks . 5)
   ;;                              ;; (recents . 5)
   ;;                              )
   dotspacemacs-startup-lists nil

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers nil

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "all-the-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons nil

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'emacs-lisp-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent t

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable t

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   dotspacemacs-initial-scratch-message nil

;;;; Configuration

   dotspacemacs-themes '(;; (modus-operandi :package modus-themes)
                         modus-operandi-tinted
                         spacemacs-light
                         modus-vivendi-tinted
                         spacemacs-dark)

   dotspacemacs-mode-line-theme '(doom)
   ;; dotspacemacs-mode-line-theme '(spacemacs :separator zigzag :separator-scale 1.5)

   dotspacemacs-default-font '("Monoplex KR Nerd" :size 14.0
                               :width normal
                               :weight regular)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t

   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t ; t if evil-better-jumper layer

   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout t

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 5

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose t

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols nil

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling nil

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; dotspacemacs-line-numbers '(:relative t
   dotspacemacs-line-numbers '(:relative t
                                         :disabled-for-modes dired-mode
                                         text-mode ; for performance issue
                                         ;; org-mode
                                         doc-view-mode
                                         pdf-view-mode
                                         :size-limit-kb 1000)

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   ;; 2023-07-31 global 로 켜면 evil 과 충돌. 이거 끄고 글로벌 켜라
   dotspacemacs-smartparens-strict-mode nil ; vs. 'puni' + 'electric-pair'

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode nil ; vs. use 'puni'

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil ; conflict vterm ')'

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   ;; dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir "~/.cache/"

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%t@%S" ; "%a" / "%f"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Color highlight trailing whitespace in all prog-mode and text-mode derived
   ;; modes such as c++-mode, python-mode, emacs-lisp, html-mode, rst-mode etc.
   ;; (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil ; manually turn on - ws-butler-mode

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs. (default nil)
   dotspacemacs-pretty-docs t

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown. (default nil)
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil

   ) ; end-of setq-default
  ) ;; end-of dotspacemacs/init ()

;;; User Initialization

(defun dotspacemacs/user-init ()

;;;; User Initilization

  ;; Always prompt in minibuffer (no GUI)
  (setq use-dialog-box nil
        ;; Use y or n instead of yes or no
        use-short-answers t
        ;; Confirm before quitting
        confirm-kill-emacs 'y-or-n-p)

  ;; Don’t compact font caches during GC.
  ;; 이 설정 정말 중요하다. 특히 org-superstar 사용 시 필수!
  (setq inhibit-compacting-font-caches t)

;;;; native-comp-jit-compilation-deny-list from doomemacs

  (when (version< "28.0" emacs-version)
    (with-eval-after-load 'comp
      ;; HACK: On Emacs 30.0.92, `native-comp-jit-compilation-deny-list' was moved
      ;;   to comp-run. See emacsmirror/emacs@e6a955d24268. Doom forces straight
      ;;   to consult this variable when building packages.
      (require 'comp-run nil t)
      ;; HACK: Disable native-compilation for some troublesome packages
      (setq native-comp-jit-compilation-deny-list
            '("jupyter" "/with-editor\\.el\\'" "/vterm\\.el\\'"
              "/evil-collection-vterm\\.el\\'" "/emacs-jupyter.*\\.el\\'"
              "/seq-tests\\.el\\'"))
      ))

;;;; DONT Emacs Server

  ;; emacsclient -s ~/.cache/spacemacs-server -n
  ;; (setq server-name "spacemacs-server") ; default "server"
  ;; (setq server-name (concat "spacemacs-server-" emacs-major-version-string))

  (unless (display-graphic-p) ; terminal + termux
    (setq-default dotspacemacs-enable-server nil))

;;;; unset keys before layers are loaded

  (global-unset-key (kbd "M-c"))  ; unset capitalize-word

  (global-unset-key (kbd "<f1>"))
  (global-unset-key (kbd "<f2>"))
  (global-unset-key (kbd "<f3>"))
  (global-unset-key (kbd "<f4>"))
  (global-unset-key (kbd "<f5>"))
  (global-unset-key (kbd "<f7>"))
  (global-unset-key (kbd "<f8>"))
  (global-unset-key (kbd "<f10>"))
  (global-unset-key (kbd "<f12>"))

  ;; Disable deprecation warnings about =cl=. The =cl= library has been
  ;; deprecated, but lots of packages still use it. I can't control that, but I
  ;; can disable the warnings.
  (setq byte-compile-warnings '(cl-functions))

;;;; DONT Pinned 'stable' packages

  ;; (add-to-list 'package-pinned-packages '(evil . "nongnu") t)
  ;; (add-to-list 'package-pinned-packages '(consult . "gnu") t)
  ;; (add-to-list 'package-pinned-packages '(clojure-mode . "nongnu") t)
  ;; (add-to-list 'package-pinned-packages '(cider . "nongnu") t)
  ;; (add-to-list 'package-pinned-packages '(async . "gnu") t)
  ;; (add-to-list 'package-pinned-packages '(denote . "gnu") t)

  ;; fix git layer : (transient :min-version "0.5.3")
  ;; (add-to-list 'package-pinned-packages '(transient . "melpa") t) ; for magit
  ;; (add-to-list 'package-pinned-packages '(flycheck . "melpa") t) ; for magit

;;;; Load 'per-machine' configuration

  ;; Most of my per-environment config done via =customize= and is in .custom.el.
  ;; However, some config is more involved, such as packages I just want in one
  ;; environment and not the others.  To that end, let's load a file that can contain
  ;; those customizations.
  (let ((per-machine-filename (concat user-dotemacs-dir "/lisp/per-machine.el")))
    (when (file-exists-p per-machine-filename)
      (load-file per-machine-filename)))

;;;; Load 'user-keys'

  (let ((user-keys-filename (concat user-dotemacs-dir "user-keys.el")))
    (when (file-exists-p user-keys-filename)
      (load-file user-keys-filename)))

;;;; DONT Emacspeak

  ;; emacspeak 사용. 28 안정화 버전에서만 사용
  ;; (if (< emacs-major-version 29)
  ;;  )

  (defvar *run-emacspeak* nil) ; on / off

;;;; Load custom-file

  ;; 2023-12-04 debugging for termux
  ;; (setq async-debug t)

  (setq spacemacs-theme-comment-bg nil
        spacemacs-theme-org-bold t
        spacemacs-theme-org-height nil
        spacemacs-theme-org-highlight nil)

  ;; layers/+emacs/org/packages.el
  (setq spacemacs-space-doc-modificators
        '(org-indent-mode
          view-mode
          hide-line-numbers
          alternative-emphasis
          alternative-tags-look
          link-protocol
          org-block-line-face-remap
          org-kbd-face-remap
          resize-inline-images))

  (setq custom-file (concat dotspacemacs-directory "emacs-custom.el"))
  ;; (if (file-exists-p custom-file)
  ;;     (load-file custom-file)
  ;;   (load-file (concat dotspacemacs-directory "assets/emacs-custom-default.el")))

  ) ;; end-of init

;;; User Environment

(defun dotspacemacs/user-env ()
  (spacemacs/load-spacemacs-env))

;;; User Configuration

(defun dotspacemacs/user-config ()

;;;; DONT 'Fix' spacemacs's layer

  ;; (load (file-truename (concat dotspacemacs-directory "fixed.el")))

;;;; Basics
  ;; Ridiculous path view is vanilla emacs. change truename!
  ;; truename 을 원하지 않는다. 심볼링링크대로 쓰고 싶다면 nil
  (setq find-file-visit-truename nil)

  ;; (setq auto-save-list-file-prefix "~/spacemacs/.cache/auto-save/")
  (setq auto-save-interval 1500
        auto-save-timeout 180
        auto-save-visited-interval 90)

  (setq recentf-max-saved-items 200) ; default 20
  (unless recentf-mode (recentf-mode 1))

  ;; (setq sh-basic-offset tab-width)

  (setq
   ;; ====== Default behavior ======
   ;; Inhibit startup message
   inhibit-splash-screen t
   inhibit-startup-message t ; default nil
   ;; Do not ring
   ;; ring-bell-function 'ignore
   ;; Increase the large file threshold to 50 MiB
   large-file-warning-threshold (* 50 1024 1024)

   ;; Initial scratch message (will be overridden if "fortune" is installed)
   ;; initial-scratch-message ";; MinEmacs -- start here!"
   ;; Set initial buffer to fundamental-mode for faster load
   ;; initial-major-mode 'fundamental-mode

   ;; Filter duplicate entries in kill ring
   kill-do-not-save-duplicates t
   ;; Save existing clipboard text into the kill ring before replacing it.
   save-interprogram-paste-before-kill t

   ;; Save files only in sub-directories of current project
   ;; save-some-buffers-default-predicate 'save-some-buffers-root

   ;; Use single space between sentences
   sentence-end-double-space nil
   ;; Move stuff to trash
   delete-by-moving-to-trash t
   ;; trash-directory "~/.Trash"

   ;; Select help window for faster quit!
   help-window-select t

   ;; FIXME More info on completions
   completions-detailed t

   ;; Do not ask obvious questions, follow symlinks
   vc-follow-symlinks t

   ;; Kill the shell buffer after exit
   shell-kill-buffer-on-exit t

   ;; ====== Passwords and encryption ======

   ;; Default auth-sources to GPG
   ;; path for developer tokens (default ~/.authinfo)
   ;; Use XDG_CONFIG_HOME location or HOME
   auth-sources (list (concat (getenv "XDG_CONFIG_HOME") "/authinfo.gpg")
                      "~/.authinfo.gpg")
   ;; auth-sources '("~/.authinfo.gpg")
   ;; Enable password caching
   password-cache t
   ;; 10 minutes, default is 16 sec
   password-cache-expiry 600
   ;; Enable caching, do not keep asking about GPG key
   auth-source-do-cache t
   ;; All day, default is 2h (7200)
   auth-source-cache-expiry 86400

   ;; ====== Performances ======
   ;; Don’t compact font caches during GC
   inhibit-compacting-font-caches t
   ;; Increase single chunk bytes to read from subprocess (default 4096)
   read-process-output-max (if (eq system-type 'gnu/linux)
                               (condition-case nil
                                   ;; Android may raise permission-denied error
                                   (with-temp-buffer
                                     (insert-file-contents
                                      "/proc/sys/fs/pipe-max-size")
                                     (string-to-number (buffer-string)))
                                 ;; If an error occured, fallback to the default value
                                 (error read-process-output-max))
                             (* 1024 1024))

   ;; TODO 2023-06-19 왜 갑자기 클라이언트 프레임 사이즈가 이상하지?!
   ;; Do force frame size to be a multiple of char size
   frame-resize-pixelwise t

   ;; ;; Emacsclient does not use full frame size (NIL 필수!)
   frame-inhibit-implied-resize nil

   ;; Stretch cursor to the glyph width
   ;; make cursor the width of the character it is under
   ;; i.e. full width of a TAB
   x-stretch-cursor t
   ;; Show trailing whitespaces
   show-trailing-whitespace t
   ;; Resize window combinations proportionally
   window-combination-resize t
   ;; Enable time in the mode-line
   ;; display-time-string-forms '((propertize (concat 24-hours ":" minutes)))
   ;; No ugly button for widgets
   widget-image-enable nil
   ;; Show unprettified symbol under cursor (when in `prettify-symbols-mode')
   ;; prettify-symbols-unprettify-at-point t
   ;; Make tooltips last a bit longer (default 10s)
   tooltip-hide-delay 20
   ;; Use small frames to display tooltips instead of the default OS tooltips
   use-system-tooltips nil

   ;; ====== Undo ======
   ;; 10MB (default is 160kB)
   undo-limit 10000000
   ;; 50MB (default is 240kB)
   undo-strong-limit 50000000
   ;; 150MB (default is 24MB)
   undo-outer-limit 150000000

   ;; ====== Editing ======
   ;; Default behavior for `whitespace-cleanup'
   ;; whitespace-action '(cleanup auto-cleanup)
   ;; End files with newline
   require-final-newline t

   ;; Enable Drag-and-Drop of regions
   mouse-drag-copy-region t
   mouse-drag-and-drop-region t
   ;; Enable Drag-and-Drop of regions from Emacs to external programs
   mouse-drag-and-drop-region-cross-program t

   ;; ====== Scrolling ======
   ;; Do not adjust window-vscroll to view tall lines
   auto-window-vscroll nil
   ;; Keep the point in the same position while scrolling
   scroll-preserve-screen-position t
   ;; Do not move cursor to the center when scrolling
   scroll-conservatively 101
   ;; Scroll at a margin of one line
   scroll-margin 1
   ;; Better scrolling on Emacs29+, specially on a touchpad
   pixel-scroll-precision-use-momentum t

   column-number-mode t ; default nil

   ;; 복붙만 한다.
   ;; ;; ====== Compilation ======
   ;; ;; Scroll compilation buffer
   ;; compilation-scroll-output t ; 'first-error can be a good option
   ;; ;; Always kill current compilation process before starting a new one
   ;; compilation-always-kill t
   ;; ;; Skip visited messages on compilation motion commands
   ;; compilation-skip-visited t
   ;; ;; Keep it readable
   ;; compilation-window-height 12
   ) ; end-of-setq

  ;; Kill minibuffer when switching by mouse to another window
  ;; Taken from: https://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
  ;; (add-hook
  ;;  'mouse-leave-buffer-hook
  ;;  (defun +minibuffer--kill-on-mouse-h ()
  ;;    "Kill the minibuffer when switching to window with mouse."
  ;;    (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
  ;;      (abort-recursive-edit))))

  ;; Scroll pixel by pixel, in Emacs29+ there is a more pricise mode way to scroll
  (pixel-scroll-precision-mode 1)

  ;; Files with known long lines
  ;; SPC f l to open files literally to disable most text processing
  ;; So long mode when Emacs thinks a file would affect performance
  (global-so-long-mode 1)

  ;; Easily navigate sillycased words
  (global-subword-mode 1)

  ;; Emacs text rendering optimizations
  ;; https://200ok.ch/posts/2020-09-29_comprehensive_guide_on_handling_long_lines_in_emacs.html
  ;; Only render text left to right
  (setq-default bidi-paragraph-direction 'left-to-right)

  ;; 라인 컬럼 보여주는 검은 세로선
  ;; (when (display-graphic-p) ; gui
  ;;   (global-display-fill-column-indicator-mode))

  ;; /spacemacs/core/libs/ido-vertical-mode.el
  ;; 찾아서 꺼줘야 한다. Spacemacs 에서 자동으로 켜놓았네.
  (ido-vertical-mode -1)

  ;; http://yummymelon.com/devnull/surprise-and-emacs-defaults.html
  ;;텍스트를 선택한 다음 그 위에 입력하면 해당 텍스트가 삭제되어야 합니다.
  ;;놀랍게도 기본 Emac 에서는 이 동작이 기본적으로 제공되지 않습니다. 명시적으로
  ;;활성화해야 합니다.
  (setq delete-selection-mode t) ; default nil
  ;; (setq magit-save-repository-buffers 'dontask) ; default t

  ;; ====== Recent files ======
  ;; Increase the maximum number of saved items
  ;; Ignore case when searching recentf files
  (setq recentf-case-fold-search t)
  ;; Exclude some files from being remembered by recentf
  (setq recentf-max-saved-items 200) ; default 20

  (setq recentf-exclude nil)
  ;; (add-to-list 'recentf-exclude (recentf-expand-file-name package-user-dir))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name spacemacs-cache-directory))
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
  (when custom-file
    (add-to-list 'recentf-exclude (recentf-expand-file-name custom-file)))
  (add-to-list 'recentf-exclude ".gz")
  (add-to-list 'recentf-exclude ".elc")

  ;; Show recursion depth in minibuffer (see `enable-recursive-minibuffers')
  (minibuffer-depth-indicate-mode 1)

  ;; default 120 emacs-29, 60 emacs-28
  (setq kill-ring-max 30) ; keep it small

  ;; automatically revert buffers for changed files
  (setq auto-revert-interval 5) ; default 5

  ;; Disable .# lock files
  (setq create-lockfiles nil)

  ;; Shr group: Simple HTML Renderer 를 의미한다. 여기 설정을 바꾸면 faces 를 수정할 수 있음
  (setq shr-use-fonts nil)

  ;; buffer size 를 표기 합니다.
  (setq size-indication-mode t)

  ;; turn off page-break-lines-mode for org-roam
  (global-page-break-lines-mode -1)
  ;; (global-visual-line-mode +1)

  ;; Line should be 80 characters wide, not 72
  (setq-default
   ;; ====== Buffer-local variables ======
   ;; Display long lines
   truncate-lines nil
   ;; Default fill column width
   fill-column 80 ;; default 70
   display-fill-column-indicator-column 86

   ;; Never mix, use only spaces
   indent-tabs-mode nil

   ;; Width for line numbers
   ;; display-line-numbers-width 4
   display-line-numbers-width-start t
   tab-width 4 ;; 스페이스맥스는 기본 2이다. 둘다 설정한다.
   evil-shift-width tab-width
   )

  ;; Save backup files to a dedicated directory.
  (setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory))))
  (setq delete-old-versions -1)

  ;; The default is nil, which means Emacs moves file to backup and then copy it back.
  ;; First seen on https://idiomdrottning.org/bad-emacs-defaults.
  (setq backup-by-copying t)

  ;; Make numeric backup versions unconditionally.
  (setq version-control t)
  (setq vc-make-backup-files t)
  (setq auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save-list" user-emacs-directory) t)))

  ;; Do not create lock files.
  (setq create-lockfiles nil)

  (setq native-comp-async-report-warnings-errors nil)

  ;; Use year/month/day
  (setq calendar-date-style 'iso)

  ;; Remember and restore the last cursor location of opened files
  (save-place-mode 1)

  ;; Disable the alarm bell (https://www.emacswiki.org/emacs/AlarmBell).
  (setq ring-bell-function 'ignore)

  ;; Use shorter "y" or "n" to confirm killing emacs.
  (setq confirm-kill-emacs 'y-or-n-p)

  ;; On Emacs without X, tool-bar-mode and scroll-bar-mode are not defined.

  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

  (unless (display-graphic-p) ; terminal
    (menu-bar-mode -1)          ; Disable the menu bar
    (blink-cursor-mode -1)

    (setq all-the-icons-color-icons nil)
    (xterm-mouse-mode -1) ; important
    (evil-goggles-mode -1)
    (setq fast-but-imprecise-scrolling nil)

    ;; (gpm-mouse-mode -1)
    ;; (mouse-wheel-mode -1)
    ;; (pixel-scroll-precision-mode -1)
    ;; (setq mouse-wheel-follow-mouse -1)
    ;; (setq
    ;;  mouse-drag-and-drop-region nil
    ;;  mouse-drag-and-drop-region-cross-program nil
    ;;  auto-window-vscroll nil
    ;;  fast-but-imprecise-scrolling nil
    ;;  scroll-preserve-screen-position nil
    ;;  pixel-scroll-precision-use-momentum nil ; default t
    ;;  )
    ;; (setq evil-motions nil)
    )

  (defun my/edit-mode ()
    ;; (hl-line-mode +1)
    (local-set-key (kbd "C-S-s") 'write-file)
    (setq cursor-type 'bar)
    (setq show-trailing-whitespace t))

  (defun my/text-mode ()
    (my/edit-mode)
    (visual-line-mode +1)               ; enable "word-wrap"
    (toggle-truncate-lines -1)
    (goto-address-mode +1)
    (flyspell-mode))

  (defun my/prog-mode ()
    (display-line-numbers-mode +1)
    (my/edit-mode)
    (show-paren-mode +1)
    (goto-address-prog-mode +1)
    (flyspell-prog-mode))

  ;; (add-hook 'text-mode-hook 'my/text-mode)
  ;; (add-hook 'view-mode-hook 'my/text-mode)
  ;; (add-hook 'prog-mode-hook 'my/prog-mode)
  ;; (add-hook 'conf-mode-hook 'my/prog-mode)

  ;; Removes the overlay properties which flyspell uses on incorrect words for mouse operations.
  ;; https://emacs.stackexchange.com/a/55708
  (defun make-flyspell-overlay-return-mouse-stuff (overlay)
    (overlay-put overlay 'help-echo nil)
    (overlay-put overlay 'keymap nil)
    (overlay-put overlay 'mouse-face nil))
  (advice-add 'make-flyspell-overlay :filter-return #'make-flyspell-overlay-return-mouse-stuff)

  ;; Delete the selected text first before editing.
  (delete-selection-mode +1)

  ;; Mouse middle-click yanks where the point is, not where the mouse is.
  (setq mouse-yank-at-point t)

  (global-set-key (kbd "C-x R") 'rename-visited-file)

;;;;; cape mode setup for completion-at-point

  ;; Default nil
  (setq-default completion-at-point-functions nil)

  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (defun cape-text-mode-setup ()
    (interactive)
    ;; (add-to-list 'completion-at-point-functions #'cape-dict)
    ;; (add-to-list 'completion-at-point-functions #'cape-file)
    ;; (add-to-list 'completion-at-point-functions #'cape-history)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev) ; top
    )

  ;; 2023-07-08 순서 때문에 따로 확실하게 점검한다.
  (defun cape-markdown-mode-setup ()
    (interactive)
    ;; (add-to-list 'completion-at-point-functions #'cape-dict)
    ;; (add-to-list 'completion-at-point-functions #'cape-file)
    ;; (add-to-list 'completion-at-point-functions #'cape-history)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev) ; top
    )

  (defun cape-org-mode-setup ()
    ;; (add-to-list 'completion-at-point-functions #'cape-elisp-block)
    ;; (add-to-list 'completion-at-point-functions #'cape-history)
    ;; (add-to-list 'completion-at-point-functions #'cape-dict)
    ;; (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    )

  (defun cape-prog-mode-setup ()
    ;; (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    ;; (add-to-list 'completion-at-point-functions #'cape-history)
    ;; (add-to-list 'completion-at-point-functions #'cape-keyword) ;; no.1
    )

  (add-hook 'markdown-mode-hook 'cape-markdown-mode-setup)
  (add-hook 'org-mode-hook 'cape-org-mode-setup)
  ;; (add-hook 'conf-mode-hook 'cape-prog-mode-setup)

  (add-hook 'prog-mode-hook 'cape-prog-mode-setup)
  ;; (remove-hook 'org-mode-hook #'org-eldoc-load) ; conflict org-glossary

  ;; In non-programming-buffers, we don't want `pcomplete-completions-at-point'
  ;; or 't' which seems to complete everything.
  ;; (defun ash/fix-completion-for-nonprog-buffers ()
  ;;   (setq completion-at-point-functions
  ;;         (-remove-item t (append (-remove-item #'pcomplete-completions-at-point completion-at-point-functions)
  ;;                                 '(cape-file cape-abbrev cape-rfc1345)))))
  ;; (add-hook 'org-mode-hook #'ash/fix-completion-for-nonprog-buffers)
  ;; (add-hook 'notmuch-message-mode-hook #'ash/fix-completion-for-nonprog-buffers)

;;;;; DONT vertico hangul

  ;; 2023-11-07 버벅이는 문제로 끈다.
  ;; (with-eval-after-load 'vertico
  ;;     (defun my/vertico-setup-then-remove-post-command-hook (&rest args)
  ;;       "vertico--setup 함수에서 추가하는 post-command-hook 을 제거한다.
  ;; 입력 조합으로 표현하는 한글 입력시 post-command-hook 이 입력되지
  ;; 않는다. 한글 증분 완성을 위해 timer 로 호출하기 때문에 제거한다"
  ;;       (remove-hook 'post-command-hook #'vertico--exhibit 'local))

  ;;     (defun my/vertico-exhibit-with-timer (&rest args)
  ;;       "타이머를 넣어 타이머 이벤트 발생시 vertico--exhibit 을 호출해
  ;;  미니버퍼 완성(completion) 후보 리스트를 갱신한다
  ;; post-command-hook 이 발동하지 않는 한글 입력시에도 한글 증분
  ;; 완성을 하기 위해 timer 를 사용한다"
  ;;       (let (timer)
  ;;         (unwind-protect
  ;;             (progn
  ;;               (setq timer (run-with-idle-timer
  ;;                            0.01
  ;;                            'repeat
  ;;                            (lambda ()
  ;;                              (with-selected-window (or (active-minibuffer-window)
  ;;                                                        (minibuffer-window))
  ;;                                (vertico--exhibit))
  ;;                              )))
  ;;               (apply args))
  ;;           (when timer (cancel-timer timer)))))

  ;;     (advice-add #'vertico--setup :after #'my/vertico-setup-then-remove-post-command-hook)
  ;;     (advice-add #'vertico--advice :around #'my/vertico-exhibit-with-timer)
  ;;     )

;;;;; CJK Word Wrap

  ;; Emacs 28 adds better word wrap / line break support for CJK.
  ;; (setq word-wrap-by-category t) ; default nil

  ;; Normally when word-wrap is on, Emacs only breaks lines after
  ;; whitespace characters.  When this option is turned on, Emacs also
  ;; breaks lines after characters that have the "|" category (defined in
  ;; characters.el).  This is useful for allowing breaking after CJK
  ;; characters and improves the word-wrapping for CJK text mixed with
  ;; Latin text.

  ;; 일반적으로 단어 줄 바꿈이 켜져 있으면 Emac 은 공백 문자 뒤에 오는 줄만 줄
  ;; 바꿈합니다. 이 옵션을 켜면 Emac 은 "|" 범주(characters.el 에 정의됨)가 있는 문자
  ;; 뒤의 줄도 줄 바꿈합니다. 이 옵션은 한중일 문자 뒤에 줄 바꿈을 허용하는 데
  ;; 유용하며 라틴 텍스트와 혼합된 한중일 텍스트의 단어 줄 바꿈을 개선합니다.

;;;;; hungry-delete-backward and forward

  ;; layers/+emacs/better-defaults/keybindings.el
  (defun spacemacs/backward-kill-word-or-region (&optional arg)
    "Calls `kill-region' when a region is active and
`backward-kill-word' otherwise. ARG is passed to
`backward-kill-word' if no region is active."
    (interactive "p")
    (if (region-active-p)
        ;; call interactively so kill-region handles rectangular selection
        ;; correctly (see https://github.com/syl20bnr/spacemacs/issues/3278)
        (call-interactively #'kill-region)
      (backward-kill-word arg)))

  ;; 익숙한 키 바인딩이라. 그냥 두자.
  (global-set-key (kbd "M-<backspace>") 'spacemacs/backward-kill-word-or-region)

  ;; [x] 모드와 상관 없이 backsapce 는 delete-backward-char 안된다. : conflict
  ;; 모드와 상관 없이 Delete 키는 delete-forward-char : default

  (with-eval-after-load 'hungry-delete
    (define-key hungry-delete-mode-map (kbd "S-<backspace>") 'hungry-delete-backward)
    (define-key hungry-delete-mode-map (kbd "S-<delete>") 'hungry-delete-forward)
    (define-key hungry-delete-mode-map (kbd "S-DEL") 'hungry-delete-forward)
    )

  ;; 기본 스타일 바인딩을 사용하자.
  (global-set-key (kbd "S-<backspace>") 'hungry-delete-backward) ; default bindings
  (global-set-key (kbd "S-<delete>") 'hungry-delete-forward)
  (global-set-key (kbd "S-DEL") 'hungry-delete-forward)

  ;; C 로 하려다가 기본이 S 더라. 기본으로 가자.
  ;; (global-set-key (kbd "C-<backspace>") 'hungry-delete-backward)
  ;; (global-set-key (kbd "C-<delete>") 'hungry-delete-forward)
  ;; (global-set-key (kbd "C-DEL") 'hungry-delete-forward)

  (global-hungry-delete-mode t)

;;;;; winner-mode

  (require 'winner)
  (setq winner-boring-buffers-regexp "\\*.*\\*")
  (winner-mode +1)
  ;; C-c <left>   winner-undo
  ;; C-c <right>  winner-redo
  (define-key evil-window-map "u" 'winner-undo)
  (define-key evil-window-map "U" 'winner-redo)
  ;; (define-key winner-mode-map (kbd "M-s-[") #'winner-undo)
  ;; (define-key winner-mode-map (kbd "M-s-]") #'winner-redo)

  (use-package whitespace
    :ensure nil
    :init
    (setq whitespace-style '(face tabs trailing ;; lines-tail
                                  empty  missing-newline-at-eof)
          whitespace-line-column 80)
    :hook (prog-mode . whitespace-mode)
    :diminish)

;;;;; trailing-whitespace and check large file

  ;; 나는 문서는 정리하는게 좋다.
  ;; (defun kimim/save-buffer-advice (orig-fun &rest arg)
  ;;   ;; (when (not (memq major-mode '(org-mode markdown-mode text-mode)))
  ;;   (when (memq major-mode '(org-mode))
  ;;     (delete-trailing-whitespace))
  ;;   (apply orig-fun arg))
  ;; (advice-add 'save-buffer :around #'kimim/save-buffer-advice)
  ;; (setq save-silently t) ;; default nil

  (defun my/describe-thing-in-popup ()
    (interactive)
    (let* ((thing (symbol-at-point))
           (help-xref-following t)
           (description (with-temp-buffer
                          (help-mode)
                          (help-xref-interned thing)
                          (buffer-string))))
      (popup-tip description
                 :point (point)
                 :around t
                 :height 30
                 :scroll-bar t
                 :margin t)))
  (spacemacs/set-leader-keys "hh" 'my/describe-thing-in-popup)

  ;; ((file location) (file location))
  ;;   1              2
  ;; (defvar kimim/last-edit-list nil)

  ;; (defun kimim/backward-last-edit ()
  ;;   (interactive)
  ;;   (let ((position (car kimim/last-edit-list)))
  ;;     (when position
  ;;       (print position)
  ;;       ;;(print kimim/last-edit-list)
  ;;       (find-file (car position))
  ;;       (goto-char (cdr position))
  ;;       (setq kimim/last-edit-list (cdr kimim/last-edit-list)))))

  ;; ;; TODO shrink list if more items
  ;; (defun kimim/buffer-edit-hook (beg end len)
  ;;   (interactive)
  ;;   (let ((bfn (buffer-file-name)))
  ;;     ;; insert modification in current index
  ;;     ;; remove forward locations
  ;;     ;; if longer than 100, remove old locations
  ;;     (when bfn
  ;;       (progn
  ;;         (add-to-list 'kimim/last-edit-list (cons bfn end))))))

  ;; (add-hook 'after-change-functions 'kimim/buffer-edit-hook)

  ;; (global-set-key (kbd "M-`") 'kimim/backward-last-edit)
  ;; (spacemacs/set-leader-keys "jg" 'kimim/backward-last-edit)

;;;;; User Goto Functions

  ;; (defun goto-emacs-dotfiles.org ()
  ;;   "Open jh-emacs.org file."
  ;;   (interactive)
  ;;   (find-file (concat dotspacemacs-directory "jh-emacs.org")))

  (defun goto-pandoc-config ()
    "Open pandoc metadata file."
    (interactive)
    (find-file "~/.config/pandoc/metadata.yml"))

;;;;; show-paren-mode/electric-pair-mode and customize for org-mode

  ;; Turn off electric-indent-mode
  ;; 2023-11-10 puni + electric-pair 사용 중. 이걸 꺼야 org-block 에서 문제가 없다.
  ;; 2023-09-28 아니다. 켜 놓은 이유가 있을 것. elctric-pair 가 아니지 않는가?
  ;; 스페이스맥스에서 왜 이걸 켜 놓는 것인가?! 일단 끈다.
  ;; C-j 누르면 electric-newline-and-maybe-indent 수행. indent 가 안맞는다. 필요 없다.
  (electric-indent-mode -1) ; important!! 이렇게 따로 꺼야 한다.

  ;; Other Options
  ;; https://github.com/alphapapa/smart-tab-over

  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Matching.html
  ;; 괄호만 강조
  (setq show-paren-style 'parenthesis) ; default 'parenthesis
  ;; 괄호 입력 후 내용 입력시 괄호를 강조
  (setq show-paren-when-point-inside-paren t)
  ;; (setq show-paren-when-point-in-periphery t)

  ;; 괄호 강조를 즉시 보여준다
  (setq show-paren-delay 0) ; 0.125
  (show-paren-mode t)

  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Matching.html
  ;; 괄호, 구분자(delimiter) 자동 쌍 맞추기
  (setq electric-pair-pairs '((?\{ . ?\})
                              (?\( . ?\))
                              (?\[ . ?\])
                              (?\" . ?\")))

  ;; from Crafted-Emacs - crafted-org-config.el
  ;; Disable auto-pairing of "<" "[" "(" in org-mode with electric-pair-mode
  (defun my/org-enhance-electric-pair-inhibit-predicate ()
    "Disable auto-pairing of \"<\" or \"[\" in `org-mode' when using `electric-pair-mode'."
    (when (and electric-pair-mode (eql major-mode #'org-mode))
      (setq-local electric-pair-inhibit-predicate
                  `(lambda (c)
                     (if (or (char-equal c ?<)
                             (char-equal c ?\[ )
                             (char-equal c ?\( )
                             )
                         t (,electric-pair-inhibit-predicate c))))))

  ;; Add hook to both electric-pair-mode-hook and org-mode-hook
  ;; This ensures org-mode buffers don't behave weirdly,
  ;; no matter when electric-pair-mode is activated.
  (add-hook 'electric-pair-mode-hook #'my/org-enhance-electric-pair-inhibit-predicate)
  (add-hook 'org-mode-hook #'my/org-enhance-electric-pair-inhibit-predicate)

;;;;; evil + hangul

  ;; 노멀로 빠지면 무조건 영어로 변경
  (defun my/turn-off-input-method (&rest _)
    (if current-input-method
        (when (derived-mode-p 'prog-mode) ;; only prog-mode
          (deactivate-input-method))))

  (advice-add 'evil-normal-state :before #'my/turn-off-input-method)
  (mapc (lambda (mode)
          (let ((keymap (intern (format "evil-%s-state-map" mode))))
            (define-key (symbol-value keymap) [?\S- ]
                        #'(lambda () (interactive)
                            (message
                             (format "Input method is disabled in %s state." evil-state))))))
        '(motion normal visual))

;;;;; bookmark

  (with-eval-after-load 'bookmark
    (setq bookmark-default-file "~/emacs-bookmarks.el")
    (setq bookmark-use-annotations nil)
    (setq bookmark-automatically-show-annotations t)
    ;; (setq bookmark-fringe-mark t) ; 29.1 default 'bookmark-mark
    (add-hook 'bookmark-bmenu-mode-hook #'hl-line-mode)
    )

;;;;; evil-collection

  (with-eval-after-load 'vterm (evil-collection-vterm-setup))

;;;;; pulse (built-in)

  ;; add visual pulse when changing focus, like beacon but built-in
  ;; from from https://karthinks.com/software/batteries-included-with-emacs/
  (defun pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))
  (dolist (command '(scroll-up-command
                     scroll-down-command
                     recenter-top-bottom
                     other-window))
    (advice-add command :after #'pulse-line))

;;;;; consult custom

  (with-eval-after-load 'consult
    ;; C-c h, C-r
    (define-key minibuffer-local-map (kbd "M-r") 'consult-history)

    (setq consult-project-function (lambda (_) (projectile-project-root)))
    ;; (define-key projectile-command-map (kbd "b") 'consult-project-buffer)
    )

  (setq diary-file (concat user-dotemacs-dir "var/diary"))
  (setq org-agenda-include-diary t)


;;;; jh-base


;;;;; transient

  (use-package transient :ensure t)

;;;;; which-key

  (setq which-key-sort-order 'which-key-key-order-alpha) ; minemacs

;;;;; calendar

  (use-package calendar
    :ensure nil
    :config
    ;; (setq org-agenda-start-on-weekday nil)
    (add-hook 'calendar-today-visible-hook 'calendar-mark-today)
    (setq calendar-date-style 'iso ;; YYYY/MM/DD
          calendar-mark-holidays-flag t
          calendar-week-start-day 1 ;; 0 Sunday, 1 Monday
          calendar-mark-diary-entries-flag nil
          calendar-latitude user-calendar-latitude
          calendar-longitude user-calendar-longitude
          calendar-location-name user-calendar-location-name
          calendar-time-display-form
          '(24-hours ":" minutes
                     (if time-zone " (") time-zone (if time-zone ")")))
    )

;;;;; time

  (use-package time
    :ensure nil
    :after calendar
    :init
    ;; (setq display-time-format " |🅆%U📅%Y-%m-%d⏲%H:%M| ")
    ;; (setq display-time-format " |%m/%d|%H:%M| ")
    (setq display-time-format "|%a %e %b, %H:%M|")
    ;; Covered by `display-time-format'
    ;; (setq display-time-24hr-format t)
    ;; (setq display-time-day-and-date t)
    (setq display-time-interval 30) ; default 60
    (setq display-time-default-load-average nil)

    ;; NOTE 2022-09-21: For all those, I have implemented my own solution
    ;; that also shows the number of new items, although it depends on
    ;; notmuch: the `notmuch-indicator' package.
    (setq display-time-mail-directory nil)
    (setq display-time-mail-function nil)
    (setq display-time-use-mail-icon nil)
    (setq display-time-mail-string nil)
    (setq display-time-mail-face nil)

    ;; World clock
    (setq zoneinfo-style-world-list
          '(("America/Los_Angeles" "Los Angeles")
            ("America/Chicago" "Chicago")
            ("Brazil/Acre" "Rio Branco")
            ("America/New_York" "New York")
            ("Brazil/East" "Brasília")
            ("Europe/Lisbon" "Lisbon")
            ("Europe/Brussels" "Brussels")
            ("Europe/Athens" "Athens")
            ("Asia/Tbilisi" "Tbilisi")
            ("Asia/Yekaterinburg" "Yekaterinburg")
            ("Asia/Shanghai" "Shanghai")
            ("Asia/Seoul" "Seoul")
            ("Asia/Vladivostok" "Vladivostok")))

    ;; All of the following variables are for Emacs 28
    (setq world-clock-list t)
    (setq world-clock-time-format "%R %z  %A %d %B")
    (setq world-clock-buffer-name "*world-clock*") ; Placement handled by `display-buffer-alist'
    (setq world-clock-timer-enable t)
    (setq world-clock-timer-second 60)
    )

;;;;; abbrev

  (with-eval-after-load 'abbrev
    (setq abbrev-file-name (concat user-dotemacs-dir "var/abbrev_defs"))
    ;; (setq abbrev-file-name "~/sync/org/var/abbrev_defs")

    (read-abbrev-file abbrev-file-name)
    (setq save-abbrevs t)
    (setq-default abbrev-mode t))

;;;;; fortune

  ;; not work on termux
  (use-package fortune
    :ensure nil
    :if (not (or my/remote-server IS-TERMUX))
    :init
    (setq fortune-always-compile nil)
    (setq fortune-dir (concat root-path "usr/share/games/fortunes/advice"))
    (setq fortune-file (concat root-path "usr/share/games/fortunes/advice"))
    )

;;;; jh-completion

;;;;; jh-completion > vertico

  (with-eval-after-load 'vertico
    ;; 2023-12-02
    (define-key vertico-map (kbd"C-<return>") 'vertico-quick-exit)
    ;; vertico-directory
    (define-key vertico-map (kbd "RET")   'vertico-directory-enter)
    (define-key vertico-map (kbd "DEL")   'vertico-directory-delete-char)
    (define-key vertico-map (kbd "M-DEL") 'vertico-directory-delete-word)

    ;; 2023-05-23 org-roam-node-find
    (define-key vertico-map (kbd "C-n") #'spacemacs/next-candidate-preview)
    (define-key vertico-map (kbd "C-p") #'spacemacs/previous-candidate-preview)

    (define-key vertico-map (kbd "M-j") #'vertico-next)
    (define-key vertico-map (kbd "M-k") #'vertico-previous)
    (define-key vertico-map (kbd "M-v") #'toggle-input-method)
    (define-key vertico-map (kbd "M-g") #'toggle-input-method)
    ;; (define-key vertico-map (kbd "M-8") #'tempel-insert)
    (define-key vertico-map (kbd "M-*") #'tempel-insert)

    (unless IS-TERMUX
      (require 'vertico-buffer)
      ;; (setq vertico-resize 'grow-only)
      ;; (setq vertico-count 10)
      (setq vertico-buffer-display-action
            `(display-buffer-in-side-window
              (window-height . ,(+ 3 vertico-count))
              (side . top)))
      (vertico-mode +1)
      (vertico-buffer-mode +1)
      )

    (when IS-TERMUX
      (setq vertico-resize nil)
      (setq vertico-count 7))

    (unless (display-graphic-p) ; terminal
      (define-key vertico-map (kbd "M-<return>") #'vertico-exit-input))
    )

;;;;; jh-completion > tempel

  ;; Template-based in-buffer completion (tempel.el)
  ;; NOTE 2023-01-19: Check the `templates'
  (use-package tempel
    :demand t
    :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
           ("M-*" . tempel-insert))
    :config
    ;; (setq tempel-trigger-prefix "<") ; conflits with evil-shift
    (setq tempel-path (expand-file-name "var/tempel-templates.eld" user-dotemacs-dir))
    ;; Use concrete keys because of org mode
    ;; "M-RET" #'tempel-done
    ;; "M-{" #'tempel-previous
    ;; "M-}" #'tempel-next
    ;; "M-<up>" #'tempel-previous
    ;; "M-<down>" #'tempel-next

    ;; 2023-10-19 disable my custom
    (define-key tempel-map (kbd "RET") #'tempel-done)
    (define-key tempel-map (kbd "M-n") #'tempel-next)
    (define-key tempel-map (kbd "M-p") #'tempel-previous)

    (use-package tempel-collection)
    )

;;;;; jh-completion > corfu

  ;; TAB-and-Go customizations
  ;; https://github.com/minad/corfu?tab=readme-ov-file#tab-and-go-completion
  (use-package corfu
    :demand t
    :ensure t
    :custom
    (corfu-cycle t)  ;; Enable cycling for `corfu-next/previous'
    ;; (corfu-auto-prefix 4) ; default 3
    (corfu-auto t) ;; default nil
    (corfu-on-exact-match nil)
    ;; (corfu-min-width 35)
    ;; (corfu-max-width 80)
    ;; (corfu-preselect 'prompt) ;; Always preselect the prompt
    :bind
    (:map corfu-map ("M-." . corfu-move-to-minibuffer))
    :init
    ;; (setq completion-cycle-threshold nil) ; default nil
    (setq tab-always-indent t)

    (use-package corfu-echo
      :hook (corfu-mode . corfu-echo-mode))
    (use-package corfu-history
      :hook (corfu-mode . corfu-history-mode))

    (with-eval-after-load 'eldoc
      (eldoc-add-command #'corfu-insert))

    ;; from minemacs
    ;; HACK: Prevent the annoting completion error when no `ispell' dictionary is set, prefer `cape-dict'
    (when (eq emacs-major-version 30)
      (setq text-mode-ispell-word-completion nil))

    (global-corfu-mode)
    )

;;;;; jh-completion > cape

  (use-package cape
    :after corfu
    :demand t
    :init
    ;; /gopar-dotfiles-youtuber/README.org:1371
    (setq cape-dabbrev-min-length 4) ; default 4
    (setq cape-dabbrev-check-other-buffers 'some)
    (defun corfu-enable-in-minibuffer ()
      "Enable Corfu in the minibuffer if `completion-at-point' is bound."
      (when (where-is-internal #'completion-at-point (list (current-local-map)))
        (setq-local corfu-auto nil) ;; Enable/disable auto completion
        ;; Disable automatic echo and popup
        (setq-local corfu-echo-delay nil)
        (corfu-mode 1)))
    (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
    ;; Bind dedicated completion commands
    ;; Alternative prefix keys: C-c p, M-p, M-+, ...
    :bind (("M-p" . completion-at-point) ;; capf
           ("M-P t" . complete-tag)        ;; etags
           ("M-P d" . cape-dabbrev)        ;; or dabbrev-completion
           ("M-P h" . cape-history)
           ("M-P f" . cape-file)
           ("M-P k" . cape-keyword)
           ("M-P s" . cape-elisp-symbol)
           ("M-P e" . cape-elisp-block)
           ("M-P a" . cape-abbrev)
           ("M-P l" . cape-line)
           ("M-P w" . cape-dict)
           ("M-P :" . cape-emoji)
           )
    )


;;;;; jh-completion > my/compleseus-search-dir

  (with-eval-after-load 'consult
    (defun my/compleseus-search-dir ()
      "Search current folder with no initial input"
      (interactive)
      (spacemacs/compleseus-search nil default-directory))

    (defun my/compleseus-search-auto-hidden ()
      "Search folder with hiddens files"
      (interactive)
      (let*
          ((initial-directory (read-directory-name "Start from directory: "))
           (consult-ripgrep-args
            (concat "rg "
                    "--null "
                    "-. "  ;; for dotfiles e.g. .spacemacs.el
                    "--line-buffered "
                    "--color=never "
                    "--line-number "
                    "--smart-case "
                    "--no-heading "
                    "--max-columns=1000 "
                    "--max-columns-preview "
                    "--with-filename "
                    (shell-quote-argument initial-directory))))
        (consult-ripgrep)))
    )

;;;;; DONT jh-completion > consult

  ;; (with-eval-after-load 'consult
  ;;   (consult-customize
  ;;    consult-theme
  ;;    :preview-key '("M-." "C-SPC"
  ;;                   :debounce 3.0 any)

  ;;    ;; For `consult-line' change the prompt and specify multiple preview
  ;;    ;; keybindings. Note that you should bind <S-up> and <S-down> in the
  ;;    ;; `minibuffer-local-completion-map' or `vertico-map' to the commands which
  ;;    ;; select the previous or next candidate.
  ;;    consult-buffer
  ;;    consult-ripgrep
  ;;    consult-git-grep
  ;;    consult-grep
  ;;    consult-bookmark
  ;;    consult-yank-pop ; needed

  ;;    ;; ADD Belows
  ;;    consult-line ; :prompt "Consult-line: "
  ;;    consult-recent-file
  ;;    consult-xref
  ;;    consult-org-heading
  ;;    consult-outline ; 2023-05-23

  ;;    spacemacs/consult-line
  ;;    ;; my/custom-switch-to-buffer

  ;;    spacemacs/compleseus-switch-to-buffer
  ;;    spacemacs/compleseus-search-dir
  ;;    spacemacs/compleseus-search-auto
  ;;    spacemacs/compleseus-find-file ; 2023-05-14 추가
  ;;    spacemacs/embark-preview ; 2023-05-23
  ;;    spacemacs/compleseus-search-default
  ;;    :preview-key '("M-." "C-SPC"
  ;;                   :debounce 0.3 "<up>" "<down>" "C-n" "C-p"
  ;;                   ))
  ;;   )

;;;; jh-visual

;;;;; jh-visual > popup

;;;;;; jh-visual > popup > popwin

  (with-eval-after-load 'popwin
    ;; (spacemacs/set-leader-keys ";" 'popwin:messages)
    (spacemacs/set-leader-keys ":" 'spacemacs/last-popwin)
    (spacemacs/set-leader-keys ";" 'popwin:close-popup-window)

    ;; don't use spacemacs default value but manage it ourselves
    ;; (setq popwin:special-display-config nil)

    ;; 기존 창에 레이아웃 건들지 않고 새로운 버퍼로 등장했다가 사라지는가? dedicated t
    ;; popper 와 연동이 되는가?
    ;; popper 로 제거하거나 q 를 누를 때까지 stick 하고 있는가?
    ;; visual line mode 가 되거나. 라인 정렬이 깔끔한가
    ;;   (push '("*Go-Translate*"       :dedicated t :position bottom :stick t :noselect t :height 0.4) popwin:special-display-config)
    ;;   (push '(helpful-mode           :dedicated t :position bottom :stick nil :noselect t :height 0.4) popwin:special-display-config)
    ;;   (push '(help-mode              :dedicated t :position bottom :stick nil :noselect t :height 0.4) popwin:special-display-config)
    ;;   (push '("*wordreference*"      :dedicated t :position bottom :stick t :noselect t :height 0.4) popwin:special-display-config)
    ;;   (push '("*SDCV*"               :dedicated t :position bottom :stick nil :noselect t :height 0.4) popwin:special-display-config)
    ;;   (push '(ekg-notes-mode         :dedicated t :position right   :stick nil :noselect nil :width 90) popwin:special-display-config)
    ;;   (push '("*EKG Edit.*\\*"       :dedicated t :position right   :stick t   :noselect nil :width 90) popwin:special-display-config)

    ;; TODO 검증 할 것
    ;; (push '(telega-chat-mode :dedicated t :position right :stick t :noselect t :width 60) popwin:special-display-config)
    ;; (push '(telega-root-mode :dedicated t :position bottom :stick t :noselect t :height 0.4) popwin:special-display-config)
    ;; (push '("*Dogears List*" :dedicated t :position bottom :stick t :noselect t :height 0.4) popwin:special-display-config)

    ;; (push '("\\*-vterm-\\*" :dedicated t :position right :stick t :noselect nil :width 70) popwin:special-display-config)
    ;; (push '(dired-mode :dedicated t :position left :stick t :noselect nil :width 40) popwin:special-display-config)
    ;; (push '("*Keyboard layout*" :dedicated t :position bottom :stick t :noselect t :height 13) popwin:special-display-config)

    (push '("*eldoc*"              :dedicated t :position right  :stick t :noselect t :width 84) popwin:special-display-config)
    (push '("*devdocs-.*\\*"       :dedicated t :position right  :stick t :noselect t :width 84) popwin:special-display-config)
    (push '(compilation-mode       :dedicated t :position left   :stick t :noselect t :width 60) popwin:special-display-config)
    (push '(flymake-diagnostics-buffer-mode :dedicated t :position bottom :stick t :noselect t :width 0.3 :height 0.3) popwin:special-display-config)
    (push '("^\\*EGLOT" :dedicated t :position bottom :stick t :noselect t :height 0.4) popwin:special-display-config)
    (push '("^\\*ChatGPT\\*" :dedicated t :position right :stick t :noselect nil :width 84) popwin:special-display-config)
    (push '("*eww*" :dedicated t :position right :stick t :noselect t :width 84) popwin:special-display-config)
    (push '("^\\*eldoc for" :dedicated t :position right :stick t :noselect t :width 84) popwin:special-display-config)
    (push '("^\\*Backtrace\\*" :dedicated t :position bottom :stick t :noselect t :height 0.4) popwin:special-display-config)
    ;; (push '("*lsp-documentation*" :dedicated t :position right :stick t :noselect t :width 0.3) popwin:special-display-config)
    ;; (push '("*evil-owl*" :dedicated t :position right :stick t :noselect t :width 0.3) popwin:special-display-config)
    (push '("^\\*Flycheck.+\\*$" :regexp t :dedicated t :position bottom :height 0.3 :stick t :noselect t) popwin:special-display-config)
    )

  ;; /garyo-dotfiles-ekg/emacs-config.org:2160
  ;; (add-to-list 'display-buffer-alist
  ;;              '("\\*Calendar*"
  ;;                (display-buffer-at-bottom)))

  ;; (add-to-list 'display-buffer-alist
  ;;              `("\\*\\(Output\\|Register Preview\\).*"
  ;;                (display-buffer-reuse-mode-window display-buffer-at-bottom)))

  ;; (add-to-list 'display-buffer-alist
  ;;              `("\\*\\(Calendar\\|Bookmark Annotation\\|Buffer List\\).*"
  ;;                (display-buffer-reuse-mode-window display-buffer-below-selected)
  ;;                (window-height . fit-window-to-buffer)))

  ;; (add-to-list 'display-buffer-alist
  ;;              ;; bottom side window
  ;;              `("\\*Org Select\\*" ; the `org-capture' key selection
  ;;                (display-buffer-in-side-window)
  ;;                (dedicated . t)
  ;;                (side . bottom)
  ;;                (slot . 0)
  ;;                (window-parameters . ((mode-line-format . none)))))
  ;; (add-to-list 'display-buffer-alist
  ;;              `("\\*Embark Actions\\*"
  ;;                (display-buffer-reuse-mode-window display-buffer-at-bottom)
  ;;                (window-height . fit-window-to-buffer)
  ;;                (window-parameters . ((no-other-window . t)
  ;;                                      (mode-line-format . none)))))

;;;;;; DONT jh-visual > popup > popper

  ;; (use-package popper
  ;;   :config
  ;;   ;; (setq popper-echo-dispatch-keys '("a" "s" "d" "f" "g" "h" "j" "k" "l"))
  ;;   (setq popper-echo-dispatch-keys '(?q ?w ?e ?r ?t ?y ?u ?i ?o ?p))
  ;;   ;; (setq popper-mode-line '(:eval (propertize "POP" 'face `(:inverse-video t))))

  ;;   (setq popper-display-control nil) ; use popwin and display-buffer-alist
  ;;   (setq popper-reference-buffers
  ;;         '("\\*Messages\\*"
  ;;           ;; "Output\\*$"
  ;;           "*cider-error*"
  ;;           ;; "*cider-doc*"
  ;;           ;; "^\\*eldoc for"
  ;;           "\\*Async-native-compile-log\\*" ; JH
  ;;           "^\\*EGLOT" ; JH
  ;;           "^\\*Flycheck.+\\*$" ; JH
  ;;           ;; treemacs-mode ; JH
  ;;           "*Go-Translate*" ; JH
  ;;           "*wordreference*" ; JH
  ;;           "*tmr-tabulated-view*" ; JH
  ;;           "*SDCV*" ; JH
  ;;           "*Dogears List*" ; JH
  ;;           "^\\*Backtrace\\*"
  ;;           "*Hammy Log*"
  ;;           "*eww*"
  ;;           "*lsp-documentation*"
  ;;           "*devdocs-.*\\*"
  ;;           ;; "^\\*EKG Capture"
  ;;           ekg-notes-mode
  ;;           "^\\*Ibuffer\\*" ibuffer-mode
  ;;           "^\\*denote-backlinks to "
  ;;           "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Org Note\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
  ;;           help-mode
  ;;           telega-chat-mode
  ;;           helpful-mode
  ;;           compilation-mode
  ;;           process-menu-mode
  ;;           special-mode
  ;;           ;; eww-mode
  ;;           ;; "*Emacs Log*"
  ;;           ;; "*command-log*" ; JH
  ;;           ;; org-agenda-mode ; JH
  ;;           flymake-diagnostics-buffer-mode))
  ;;   (add-to-list
  ;;    'popper-reference-buffers
  ;;    '(("^\\*Warnings\\*$" . hide)
  ;;      ("^\\*Compile-Log\\*$" . hide)
  ;;      "^\\*Matlab Help.*\\*$"
  ;;      "^\\*Messages\\*$"
  ;;      ("*typst-ts-compilation*" . hide)
  ;;      ("^\\*dash-docs-errors\\*$" . hide)
  ;;      "^\\*evil-registers\\*"
  ;;      "^\\*Apropos"
  ;;      "^Calc:"
  ;;      "^\\*eldoc\\*"
  ;;      "^\\*TeX errors\\*"
  ;;      "^\\*ielm\\*"
  ;;      "^\\*TeX Help\\*"
  ;;      "^\\*ChatGPT\\*"
  ;;      "^\\*Gemini\\*"
  ;;      "^\\*gptel-quick\\*"
  ;;      "^\\*define-it:"
  ;;      "\\*Shell Command Output\\*"
  ;;      "\\*marginal notes\\*"
  ;;      ("\\*Async Shell Command\\*" . hide)
  ;;      "\\*Completions\\*"
  ;;      "[Oo]utput\\*"))

  ;;   ;; (global-set-key (kbd "C-`") 'popper-toggle)
  ;;   ;; (global-set-key (kbd "C-~") 'popper-kill-latest-popup)
  ;;   ;; (global-set-key (kbd "M-`") 'popper-cycle)
  ;;   ;; (global-set-key (kbd "C-M-`") 'popper-toggle-type)
  ;;   (popper-mode +1)
  ;;   (popper-echo-mode +1)
  ;;   )

  ;; (spacemacs/set-leader-keys ";" 'popper-toggle)
  ;; (spacemacs/set-leader-keys ":" 'popper-kill-latest-popup)

;;;;; jh-visual > utils
;;;;;; jh-visual > utils > hl-todo

  (use-package hl-todo
    :defer 2
    :config
    ;; (message "global-hl-todo-mode")
    (global-hl-todo-mode)
    )

;;;;;; DONT jh-visual > utils > breadcrumb

  ;; (use-package breadcrumb
  ;;   :ensure t
  ;;   :init
  ;;   ;; (add-hook 'prog-mode-hook 'breadcrumb-local-mode) ; conflict with lsp-mode
  ;;   (add-hook 'emacs-lisp-mode-hook 'breadcrumb-local-mode)
  ;;   (add-hook 'markdown-mode-hook 'breadcrumb-local-mode)
  ;;   :custom
  ;;   (breadcrumb-project-max-length 0.1)
  ;;   (breadcrumb-imenu-max-length 0.2)
  ;;   )

  (with-eval-after-load 'popup
    :config
    (define-key popup-menu-keymap (kbd "C-j") 'popup-next)
    (define-key popup-menu-keymap (kbd "C-k") 'popup-previous)
    (define-key popup-menu-keymap (kbd "C-n") 'popup-next)
    (define-key popup-menu-keymap (kbd "C-p") 'popup-previous))

;;;;; jh-visual > icons

  (use-package nerd-icons :demand t)

  ;; (use-package corfu-terminal
  ;;     :config
  ;;     (setq corfu-terminal-position-right-margin 1)
  ;;     (unless (display-graphic-p)
  ;;         (corfu-terminal-mode +1)))

  ;; (use-package kind-icon
  ;;     :after corfu
  ;;     :ensure t
  ;;     :custom
  ;;     (kind-icon-blend-background t)
  ;;     (kind-icon-default-face 'corfu-default) ; only needed with blend-background
  ;;     :config
  ;;     (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  ;;     (unless (display-graphic-p) ; terminal
  ;;         (setq kind-icon-use-icons nil)))

  (use-package nerd-icons-corfu
    :if window-system
    :after corfu nerd-icons
    :init
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
    ;; :config
    ;; ;; Optionally:
    ;; (setq nerd-icons-corfu-mapping
    ;;     '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
    ;;          (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
    ;;          ;; ...
    ;;          (t :style "cod" :icon "code" :face font-lock-warning-face)))
    ;; Remember to add an entry for `t', the library uses that as default.
    )

  (use-package nerd-icons-dired
    :if window-system
    :after nerd-icons
    :hook (dired-mode . nerd-icons-dired-mode)
    )

  (with-eval-after-load 'imenu-list
    (setq imenu-list-focus-after-activation nil)
    (setq imenu-list-auto-resize nil)
    (setq imenu-list-position 'left)
    (setq imenu-list-size 45) ; default 0.3
    (setq imenu-list-idle-update-delay 1.0) ; default 0.5
    ;; (setq-default imenu-list-mode-line-format nil)
    ;; (remove-hook 'imenu-list-major-mode-hook #'imenu-list--set-mode-line)
    (add-hook 'imenu-list-major-mode-hook #'spacemacs/toggle-truncate-lines-on))

;;;;; jh-visual > modeline

;;;;;; jh-visual > modeline > doom-modeline

  (with-eval-after-load 'doom-modeline
    ;; (doom-modeline-def-modeline 'main
    ;;   '(eldoc bar persp-name workspace-name window-number modals input-method matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
    ;;   '(compilation objed-state misc-info battery grip irc gnus github debug repl lsp minor-modes indent-info buffer-encoding major-mode process vcs check time))
    ;; (setq doom-modeline-time nil)
    (setq doom-modeline-time-icon nil)
    (setq doom-modeline-minor-modes nil)
    (setq doom-modeline-battery nil)
    ;; (setq doom-modeline-bar-width 10) ; = fringe-mode
    (setq Info-breadcrumbs-in-mode-line-mode nil)
    (setq doom-modeline-height 35)

    (setq doom-modeline-icon (display-graphic-p))
    ;; (setq doom-modeline-modal-icon nil)
    (setq doom-modeline-major-mode-icon nil)
    (setq doom-modeline-enable-word-count nil)
    (setq doom-modeline-window-width-limit (- fill-column 5))

    (setq doom-modeline-repl t)
    (setq doom-modeline-lsp t)
    (setq doom-modeline-github t)
    (setq doom-modeline-indent-info t)
    (setq doom-modeline-hud t)
    (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
    )

;;;;; jh-visual > fonts
;;;;;; jh-visual > fonts > base

  (progn
    (defvar show-keyboard-layout nil
      "If non nil, show keyboard layout in special buffer.")

    (setq default-input-method "korean-hangul")
    (setq default-transient-input-method "TeX")
    (set-language-environment "Korean")
    (set-keyboard-coding-system 'utf-8)
    (setq locale-coding-system  'utf-8)
    (prefer-coding-system 'utf-8)
    (set-charset-priority 'unicode)
    (set-default-coding-systems 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (setq-default buffer-file-coding-system 'utf-8-unix)
    ;; (unless (spacemacs/system-is-mswindows)
    ;;   (set-selection-coding-system 'utf-8))
    (set-selection-coding-system 'utf-8)
    (setq coding-system-for-read 'utf-8)
    (setq coding-system-for-write 'utf-8)

    ;; Treat clipboard input as UTF-8 string first; compound text next, etc.
    (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

    ;; 날짜 표시를 영어로한다. org mode 에서 time stamp 날짜에 영향을 준다.
    (setq system-time-locale "C")
    ;; (setenv "LANG" "en_US.UTF-8")
    ;; (setenv "LC_ALL" "en_US.UTF-8")

    (setq input-method-verbose-flag nil
          input-method-highlight-flag nil)

    ;; (global-set-key (kbd "<Alt_R>") 'toggle-input-method)
    (global-set-key (kbd "<S-SPC>") 'toggle-input-method)
    (global-set-key (kbd "<Hangul>") 'toggle-input-method)
    (global-set-key (kbd "<menu>") 'toggle-input-method) ;; caps lock as <menu>
    (add-hook 'context-menu-mode-hook '(lambda () (define-key context-menu-mode-map (kbd "<menu>") #'toggle-input-method)))
    ;; (global-unset-key (kbd "S-SPC"))

    )

;;;;;; jh-visual > fonts > fontaine

  ;; Read the manual: <https://protesilaos.com/emacs/fontaine>

  ;; +------------+------------+
  ;; | 일이삼사오 | 일이삼사오 |
  ;; +------------+------------+
  ;; | ABCDEFGHIJ | ABCDEFGHIJ |
  ;; +------------+------------+
  ;; | 1234567890 | 1234567890 |
  ;; +------------+------------+
  ;; | 일이삼사오 | 일이삼사오 |
  ;; | abcdefghij | abcdefghij |
  ;; +------------+------------+

  ;; terminal-mode is nil
  ;; A narrow focus package for naming font configurations and then selecting them.
  (use-package fontaine
    :if window-system
    :demand t
    :init
    ;; This is defined in Emacs C code: it belongs to font settings.
    (setq x-underline-at-descent-line t)
    ;; And this is for Emacs28.
    (setq-default text-scale-remap-header-line t)

    ;; Weights :: Thin ExtraLight Light Regular Medium SemiBold Bold ExtraBold Heavy
    ;; Slopes :: Upright Oblique Italic
    ;; Width :: Normal Extended

    :config
    (unless *is-android*
      (setq fontaine-presets
            ;; 80 120, 136, 151, 180, 211
            '(
              (birdview
               :default-height 80)
              (small
               :default-height 120)
              (regular
               :default-height 140)
              (medium
               :default-height 151)
              (large
               :default-height 180)
              (presentation
               :default-height 211
               ;; :fixed-pitch-family "Sarasa Mono Slab K"
               ;; :fixed-pitch-serif-family "Sarasa Mono Slab K"
               :default-width extended
               :bold-weight extrabold)
              (t
               ;; Following Prot’s example, keeping these for for didactic purposes.
               :line-spacing 2
               ;; :default-family "Sarasa Mono K"
               ;; :default-height 136
               :default-family "Monoplex KR Nerd"
               :default-height 140
               :default-weight regular
               :fixed-pitch-family nil ;; "Monoplex KR Nerd"
               :fixed-pitch-weight nil
               :fixed-pitch-height nil
               ;; :fixed-pitch-serif-family "Sarasa Mono Slab K" ; nil falls back to :default-family
               :fixed-piath-serif-family nil
               :fixed-pitch-serif-weight nil
               :fixed-pitch-serif-height nil
               :variable-pitch-family "Pretendard Variable"
               ;; :variable-pitch-height 140
               ;; "IBM Plex Sans KR"
               ;; "Noto Sans KR"
               ;; :variable-pitch-family nil
               ;; :variable-pitch-weight nil
               :bold-family nil
               :bold-weight bold
               ;; :bold-width extended
               :italic-family nil
               :italic-slant italic)))

      ;; Set last preset or fall back to desired style from `fontaine-presets'.
      ;; (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
      ;; (fontaine-set-preset 'regular)
      ;; (set-fontset-font t 'hangul (font-spec :family (face-attribute 'default :family))) ; t or nil ?

      ;; store current preset
      (defun my/fontaine-store-preset ()
        (interactive)
        (fontaine-store-latest-preset)
        ;; (message "my/fontaine-store-preset")
        )

      ;; load @ start-up
      (defun my/fontaine-load-preset ()
        (interactive)
        ;; The other side of `fontaine-restore-latest-preset'.
        ;; (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)
        (fontaine-set-preset 'regular) ; regular
        ;; 1) go default spacemacs themes
        (set-fontset-font "fontset-default" 'hangul (font-spec :family (face-attribute 'default :family)))
        )
      (add-hook 'spacemacs-post-user-config-hook #'my/fontaine-load-preset 90)

      ;; load @ theme change
      (defun my/fontaine-apply-current-preset ()
        (interactive)
        (fontaine-apply-current-preset)
        (set-fontset-font "fontset-default" 'hangul (font-spec :family (face-attribute 'default :family))) ; default face
        ;; (custom-set-faces
        ;;  `(ekg-notes-mode-title ((t (:inherit outline-7 :weight semibold :height 1.3))))
        ;;  `(ekg-title ((t (:inherit outline-0 :weight semibold :height 1.0 :underline t))))
        ;;  `(denote-faces-link ((t (:inherit link :weight bold :slant italic))))
        ;;  ;; `(org-special-keyword ((t (:inherit org-drawer :weight semibold))))
        ;;  )
        )
      (add-hook 'spacemacs-post-theme-change-hook 'my/fontaine-apply-current-preset)
      )
    )

;;;;;; jh-visual > fonts > emoji

  ;; emoji
  (defun my/emoji-set-font ()
    (interactive)
    (when (display-graphic-p) ; gui
      ;; (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji") nil 'prepend)
      (set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji") nil)
      (set-fontset-font t 'emoji (font-spec :family "Noto Emoji") nil 'prepend) ; Top
      )
    (unless (display-graphic-p) ; terminal
      (set-fontset-font "fontset-default" 'emoji (font-spec :family "Noto Emoji") nil 'prepend) ; default face
      )

    (set-fontset-font t 'symbol (font-spec :family "Symbola") nil 'prepend)
    (set-fontset-font t 'symbol (font-spec :family "Noto Sans Symbols 2") nil 'prepend)
    (set-fontset-font t 'symbol (font-spec :family "Noto Sans Symbols") nil 'prepend)
    )

  (unless IS-TERMUX (add-hook 'spacemacs-post-user-config-hook #'my/emoji-set-font))

;;;;;; jh-visual > fonts > global-mode-string

  ;; (defun my/load-global-mode-string ()
  ;;   (interactive)
  ;;   (when (not (bound-and-true-p display-time-mode))
  ;;     (display-time-mode t))
  ;;   (setq global-mode-string (remove 'display-time-string global-mode-string)))

  ;; (unless IS-TERMUX
  ;;   ;; (add-hook 'spacemacs-post-user-config-hook #'my/load-global-mode-string 90)
  ;;   (my/load-global-mode-string))

  (global-set-key (kbd "<S-SPC>") 'toggle-input-method)
  (global-set-key (kbd "<Hangul>") 'toggle-input-method)

  (spacemacs/set-leader-keys "ie" #'emoji-search)
  (spacemacs/set-leader-keys "iE" #'emoji-list)

  (progn
    (defun my/text-font-scale-up () (interactive) (text-scale-increase 1))
    (defun my/text-font-scale-down () (interactive) (text-scale-decrease 1))

    (global-set-key (kbd "C-+") 'my/text-font-scale-up)
    (global-set-key (kbd "C-_") 'my/text-font-scale-down)

    (spacemacs/set-leader-keys "z+" 'my/text-font-scale-up)
    (spacemacs/set-leader-keys "z_" 'my/text-font-scale-down))


;;;;;; jh-visual-> fonts > list-unicode-display

  (use-package list-unicode-display :defer 10)

;;;;; jh-visual > themes

;;;;;; jh-visual > themes > load-theme

  (defadvice load-theme (before clear-previous-themes activate)
    "Clear existing theme settings instead of layering them"
    (mapc #'disable-theme custom-enabled-themes))

  (setq pos-tip-internal-border-width 6
        pos-tip-border-width 1)

;;;;;; DONT jh-visual > themes > treemacs-nerd-icons

  ;; (use-package treemacs-nerd-icons
  ;;   :after treemacs
  ;;   :init
  ;;   (require 'treemacs-nerd-icons)
  ;;   :config
  ;;   (treemacs-load-theme "nerd-icons"))

;;;;;; jh-visual > themes > modus-themes - built-in

  (progn
    (setq modus-themes-bold-constructs t
          modus-themes-subtle-line-numbers t
          modus-themes-mode-line '(borderless)
          modus-themes-syntax '(green-strings yellow-comments)
          modus-themes-paren-match '(bold intense) ; underline
          modus-themes-region '(bg-only no-extend)
          modus-themes-org-blocks 'gray-background)

    (setq modus-themes-headings
          (quote ((0 . (background overline 1.2)) ; variable-pitch
                  (1 . (background overline 1.2)) ; variable-pitch
                  (2 . (overline rainbow 1.1))
                  (3 . (overline 1.05))
                  (t . (monochrome)))))
    )

;;;;;; DONT jit-lock-defer

  ;; NOTE: setting this to `0' like it was recommended in the article above seems
  ;; to cause fontification to happen in real time, which can be pretty slow in
  ;; large buffers. Giving it a delay seems to be better.
  ;; (setq jit-lock-defer-time 0.25) ;; better
  ;; (setq jit-lock-defer-time 0) ;; Important

;;;;; jh-visual > spacious-padding

  (use-package spacious-padding
    :defer 1
    :hook (server-after-make-frame . spacious-padding-mode)
    :init
    (setq spacious-padding-subtle-mode-line
          '( :mode-line-active spacious-padding-subtle-mode-line-active
             :mode-line-inactive spacious-padding-subtle-mode-line-inactive))
    (setq spacious-padding-widths
          '(:header-line-width 4
                               :mode-line-width 4 ; 6
                               :tab-width 4 ; sync mode-line-width for keycast-tab-bar
                               :internal-border-width 20 ; 15
                               :right-divider-width 30 ; 30
                               :scroll-bar-width 8
                               :fringe-width 8
                               ))
    :config
    (spacious-padding-mode +1)
    )

;;;; jh-workspace

;;;;; jh-workspace > navigation > winum

  (with-eval-after-load 'winum
    (defun my/winum-assign-custom ()
      (cond
       ;; 0 treemacs, 9 minibuffer, 8 imenu-list
       ((equal (buffer-name) "*Ilist*") 8)
       ((equal (buffer-name) "*Flycheck errors*") 7)
       ((equal (buffer-name) "*Calculator*") 6))
      )

    (set-face-attribute 'winum-face nil :weight 'bold)
    (add-to-list 'winum-assign-functions #'my/winum-assign-custom)

    ;; (define-key winum-keymap
    ;;             [remap winum-select-window-7] #'spacemacs/neotree-smart-focus)
    ;; (define-key winum-keymap
    ;;             [remap winum-select-window-8] #'spacemacs/imenu-list-smart-focus)
    ;; (define-key winum-keymap
    ;;             [remap winum-select-window-9] #'spacemacs/switch-to-minibuffer-window)

    (setq winum-scope                      'frame-local
          winum-auto-assign-0-to-minibuffer t
          winum-reverse-frame-list          nil)
    (winum-mode 1))

;;;;; jh-workspace > navigation > ace-window

  (with-eval-after-load 'ace-window
    (setq aw-scope 'frame
          aw-minibuffer-flag t)
    ;; (ace-window-display-mode 1)
    (global-set-key (kbd "M-g a") 'ace-swap-window))

;;;;; jh-workspace > navigation > goto-last-change

  ;; goto-chg 패키지에서 아래 함수 제공
  ;; evil-goto-last-change-reverse (g ,)
  ;; evil-goto-last-change (g ;)

  (require 'goto-last-change)

;;;###autoload
  (defun my/goto-last-change ()
    (interactive)
    (outline-show-all) ; 전체를 펼치고 찾아라!
    (goto-last-change))
  (global-set-key (kbd "C-x ,") 'my/goto-last-change)

;;;;; jh-workspace - dired

  (with-eval-after-load 'dired
    (setq dired-ls-F-marks-symlinks t ; -F marks links with @
          dired-recursive-copies 'always
          dired-dwim-target t)
    (setq dired-auto-revert-buffer t)
    (setq dired-listing-switches
          "-AGFhlv --group-directories-first --time-style=long-iso")
    (setq dired-kill-when-opening-new-dired-buffer t)
    (setq dired-make-directory-clickable t) ; Emacs 29.1
    (setq dired-free-space nil) ; Emacs 29.1
    (setq dired-mouse-drag-files t) ; Emacs 29.1
    (setq dired-guess-shell-alist-user ; those are the suggestions for ! and & in Dired
          '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open")
            ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open")
	        (".*" "xdg-open")))

    ;; (setq dired-recursive-deletes 'always)
    (setq copy-directory-create-symlink t)
    (setq dired-hide-details-hide-symlink-targets nil) ; default t

    ;; In Emacs 29 there is a binding for `repeat-mode' which let you
    ;; repeat C-x C-j just by following it up with j.  For me, this is a
    ;; problem as j calls `dired-goto-file', which I often use.
    ;; (define-key dired-jump-map (kbd "j") nil)

    (add-hook 'dired-mode-hook 'dired-hide-details-mode)
    (add-hook 'dired-mode-hook
              (lambda ()
                (interactive)
                (setq-local truncate-lines t) ; Do not wrap lines
                ;; (visual-line-mode -1)
                (hl-line-mode 1)))

    ;; (define-key image-mode-map (kbd "k") 'image-kill-buffer)
    ;; (define-key image-mode-map (kbd "<right>") 'image-next-file)
    ;; (define-key image-mode-map (kbd "<left>") 'image-previous-file)
    (define-key dired-mode-map (kbd "C-<return>") 'image-dired-dired-display-external)

    (define-key dired-mode-map (kbd "C-+") #'dired-create-empty-file)
    (setq dired-isearch-filenames 'dwim)
    (setq dired-create-destination-dirs 'ask) ; Emacs 27

    (require 'wdired)
    (setq wdired-allow-to-change-permissions t)
    (setq wdired-create-parent-directories t)
    (add-hook 'wdired-mode-hook 'evil-normal-state)
    (evil-define-key 'normal wdired-mode-map (kbd "^") 'evil-first-non-blank)

    (defun kimim/dired-get-org-link ()
      "get a link from dired for org"
      (interactive)
      (let ((filename (dired-get-filename)))
        (kill-new (concat
                   "[["
                   (concat "~/" (file-relative-name filename "~"))
                   "]["
                   (file-name-nondirectory filename)
                   "]]"))))

    (use-package dired-narrow :commands dired-narrow)

    (evil-define-key 'normal dired-mode-map
      (kbd "C-c C-e") 'wdired-change-to-wdired-mode
      (kbd "C-c l") 'kimim/dired-get-org-link
      (kbd "C-x /") 'dired-narrow-regexp
      (kbd ".") 'consult-line
      (kbd "J") 'spacemacs/compleseus-find-file
      (kbd "f") 'evil-avy-goto-line-below ;; 2024-01-25 useful
      (kbd "h") 'dired-up-directory
      (kbd "l") 'dired-find-file
      (kbd "S-SPC") 'dired-toggle-marks
      ;; <normal-state> RET            dired-find-file
      ;; <normal-state> S-<return>     dired-find-file-other-window
      )

    (spacemacs/set-leader-keys-for-major-mode 'dired-mode
      "h" 'dired-hide-details-mode
      "/" 'dired-narrow-regexp
      "o" 'dired-omit-mode)
    ;; (global-set-key (kbd "C-x /") #'dired-narrow-regexp)
    )

;;;;; jh-workspace > Workspace > outli

  (use-package outli
    :init
    (setq outli-speed-commands nil)
    :bind (:map outli-mode-map ; convenience key to get back to containing heading
                ;; ("C-c g" . (lambda () (interactive) (outline-back-to-heading)))
                ("C-c C-n" . 'outline-next-heading)
                ("C-c C-p" . 'outline-previous-heading))
    ;; if you want load-theme to update outli faces:
    :config
    ;; (add-to-list 'outli-heading-config '(tex-mode "%%" ?% t))
    (add-to-list 'outli-heading-config '(js2-mode "//" ?\/ t))
    (add-to-list 'outli-heading-config '(js-ts-mode "//" ?\/ t))
    (add-to-list 'outli-heading-config '(typescript-mode "//" ?\/ t))
    (add-to-list 'outli-heading-config '(typescript-ts-mode "//" ?\/ t))
    (add-to-list 'outli-heading-config '(python-mode "##" ?# t))
    (add-to-list 'outli-heading-config '(python-ts-mode "##" ?# t))
    (add-to-list 'outli-heading-config '(awk-mode "##" ?# t))
    (add-to-list 'outli-heading-config '(awk-ts-mode "##" ?# t))
    (add-to-list 'outli-heading-config '(elixir-mode "##" ?# t))
    (add-to-list 'outli-heading-config '(elixir-ts-mode "##" ?# t))
    (add-to-list 'outli-heading-config '(sh-mode "##" ?# t))
    (add-to-list 'outli-heading-config '(bash-ts-mode "##" ?# t))

    (add-to-list 'outli-heading-config '(clojure-mode ";;" ?\; t))
    (add-to-list 'outli-heading-config '(clojure-ts-mode ";;" ?\; t))
    (add-to-list 'outli-heading-config '(clojurescript-mode ";;" ?\; t))

    (add-hook 'prog-mode-hook 'outli-mode) ; not markdown-mode!
    ;; (add-hook 'org-mode-hook 'outli-mode)
    )

  ;; Tab for Heading : outline-mode and org-mode
  ;; search to narrow with heading and tag base on built-in outline-mode



;;;;; jh-workspace > Workspace > tmr

  (use-package tmr
    :after embark
    :config
    (unless IS-TERMUX
      (setq tmr-sound-file "/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga"))
    (setq tmr-notification-urgency 'normal
          tmr-description-list 'tmr-description-history)

    (defvar tmr-action-map
      (let ((map (make-sparse-keymap)))
        (define-key map "k" #'tmr-remove)
        (define-key map "r" #'tmr-remove)
        (define-key map "R" #'tmr-remove-finished)
        (define-key map "c" #'tmr-clone)
        (define-key map "e" #'tmr-edit-description)
        (define-key map "s" #'tmr-reschedule)
        map))
    ;; (define-key global-map (kbd "M-g M-t") 'tmr-action-map)

    (with-eval-after-load 'embark
      (add-to-list 'embark-keymap-alist '(tmr-timer . tmr-action-map))
      (cl-loop
       for cmd the key-bindings of tmr-action-map
       if (commandp cmd) do
       (add-to-list 'embark-post-action-hooks (list cmd 'embark--restart))))
    )

;;;;; jh-workspace > project > magit / git-commit

  ;; 사용자 설정 파일 git / github
  ;;
  ;; https://develop.spacemacs.org/layers/+source-control/version-control/README.html
  ;; https://develop.spacemacs.org/layers/+source-control/git/README.html
  ;; Git Delta guide - https://dandavison.github.io/delta/
  ;; ---------------------------------------

  ;; ---------------------------------------
  ;; Spacemacs as $EDITOR (or $GIT_EDITOR) for commit messages
  ;; for `git commit` on command line
  ;; (global-git-commit-mode t)
  ;; ---------------------------------------

  ;; ---------------------------------------
  ;; Set locations of all your Git repositories
  ;; with a number to define how many sub-directories to search
  ;; `SPC g L' - list all Git repositories in the defined paths,
  (setq magit-repository-directories
        '(
          ("~/spacemacs/" . 0)
          ("~/.spacemacs.d/" . 0)
          ("~/git/" . 2)
          ;; ("~/sync/code/" . 2)
          ))

  ;; Number of topics shown, open and closed values
  ;; - negative number toggles view of closed topics
  ;; using `SPC SPC forge-toggle-closed-visibility'
  ;; - set closed to 0 to never show closed issues
  ;; (setq  forge-topic-list-limit '(100 . 0))
  (setq  forge-topic-list-limit '(100 . -10))

  ;; GitHub user and organization accounts owned
  ;; used by @ c f  to create a fork
  (setq forge-owned-accounts
        '(("junghan0611" "junghanacs")))

  (with-eval-after-load 'magit
    ;; (require 'forge)

    ;; TODO work?
    (define-key magit-status-mode-map "gb"  'tab-next)
    (define-key magit-status-mode-map "gB"  'tab-previous)

    ;; Enforce git commit conventions.
    ;; See: http://chris.beams.io/posts/git-commit
    (require 'git-commit)
    (setq git-commit-summary-max-length 72) ; defaults to Github's max commit message length
    ;; (setq git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
    ;; (evil-set-initial-state 'git-commit-mode 'insert)
    (global-git-commit-mode 1)
    )

  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "M-g M-l") 'git-link) ;; 'SPC g l l'
  (spacemacs/set-leader-keys "g/" 'consult-git-grep) ; replace helm-git-grep

  ;; (use-package consult-git-log-grep)
  ;; (spacemacs/set-leader-keys "g*" 'consult-git-log-grep)


;;;;; jh-workspace > project > magit / consult-git-log-grep

  (use-package consult-git-log-grep
    :after magit
    :defer t
    :custom (consult-git-log-grep-open-function #'magit-show-commit)
    :bind (("C-c K" . consult-git-log-grep)))

;;;; jh-checker

;;;;; DONT jh-checker > flymake

  ;; (use-package flymake
  ;;     :ensure nil
  ;;     :bind (:map flymake-mode-map
  ;;                 ("M-p" . flymake-goto-prev-error)
  ;;                 ("M-n" . flymake-goto-next-error)
  ;;                 ;; ("M-g D"   . flymake-show-buffer-diagnostics)
  ;;                 ;; ("M-g P" . flymake-show-project-diagnostics)
  ;;                 :repeat-map flymake-repeatmap
  ;;                 ("p" . flymake-goto-prev-error)
  ;;                 ("n" . flymake-goto-next-error)
  ;;                 :map flymake-diagnostics-buffer-mode-map
  ;;                 ("?" . flymake-show-diagnostic-here)
  ;;                 :map flymake-project-diagnostics-mode-map
  ;;                 ("?" . flymake-show-diagnostic-here))
  ;;     :init
  ;;     ;; left - flycheck , right - flymake and git-gutter-fringe
  ;;     ;; (setq flymake-fringe-indicator-position 'right-fringe)
  ;;     ;; (evil-define-key '(normal) flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  ;;     ;; (evil-define-key '(normal) flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
  ;;     (defun flymake-show-diagnostic-here (pos &optional other-window)
  ;;       "Show the full diagnostic of this error.
  ;; Used to see multiline flymake errors"
  ;;       (interactive (list (point) t))
  ;;       (let* ((id (or (tabulated-list-get-id pos)
  ;;                      (user-error "Nothing at point")))
  ;;              (text (flymake-diagnostic-text (plist-get id :diagnostic))))
  ;;         (message text)))
  ;;     (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake))

;;;;; jh-checker > flycheck

  ;; If flycheck idle change delay is too short, then it overwrites the helpful
  ;; messages about how to call elisp functions, etc.
  (use-package consult-flycheck
    :after flycheck
    :bind (:map flycheck-command-map
                ("!" . consult-flycheck))
    :config
    ;; Wait before complaining so we don't step on useful help messages.
    (setq-default flycheck-idle-change-delay 1.0) ; default 0.5
    (global-set-key (kbd "M-g f") 'consult-flycheck))

;;;;; jh-checker > flyspell

  (with-eval-after-load 'flyspell
    (require 'ispell)
    (setq-default ispell-program-name "hunspell")
    (setq ispell-really-hunspell t)

    ;; 김아더 WordNet -- 테스트 완료
    (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
    (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
    (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))

    ;; −B Report run-together words with missing blanks as spelling errors.
    ;; −C Consider run-together words as valid compounds.
    (setq ispell-local-dictionary-alist
          '(("ko"
             "[가-힣]" "[^가-힣]"
             "[0-9a-zA-Z]" nil
             ("-d" "ko_KR")
             nil utf-8)))
    (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)
    (setq flyspell-default-dictionary "ko")
    (setq ispell-dictionary "ko")
    (setq ispell-personal-dictionary "~/.hunspell_personal")

    ;; delay for korean default
    ;; (setq flyspell-delay 1.0)

    (require 'flyspell-correct)
    (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper)
    (define-key flyspell-mode-map (kbd "C-M-i") nil) ;  for completion-at-point
    ;; evil key-bindings with [, ]
    (define-key ctl-x-x-map "s" #'flyspell-mode) ; C-x x s
    )

  (use-package consult-flyspell
    :config
    (setq consult-flyspell-always-check-buffer t)
    (spacemacs/set-leader-keys "Sf" 'consult-flyspell))

;;;;; jh-checker > jinx

  ;; 한글만 검사
  (use-package jinx
    :if (not (or my/remote-server IS-TERMUX))
    :init
    (spacemacs/declare-prefix "S"  "spelling")
    (spacemacs/set-leader-keys "Sj" 'jinx-correct)
    (spacemacs/set-leader-keys "SJ" 'jinx-mode)
    :config
    ;; (dolist (hook '(text-mode-hook conf-mode-hook)) ; prog-mode-hook
    ;;   (add-hook hook #'jinx-mode))
    ;; (add-hook 'prog-mode-hook #'jinx-mode) ; 주석
    ;; 1) 영어 제외 : 한글만 검사
    ;; 2) 한글 영어 선택하도록 제공
    (setq jinx-languages "ko")
    ;; (setq jinx-exclude-regexps
    ;;       '((t "[A-Za-z]" "[']")))
    (setq jinx-exclude-regexps
          '((emacs-lisp-mode "Package-Requires:.*$")
            (t "[A-Za-z]" "[']" "[A-Z]+\\>" "-+\\>" "\\w*?[0-9]\\w*\\>" "[a-z]+://\\S-+" "<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?" "\\(?:Local Variables\\|End\\):\\s-*$" "jinx-\\(?:languages\\|local-words\\):\\s-+.*$")))

    ;; M-$점 앞의 철자가 틀린 단어에 대한 수정을 트리거합니다.
    ;; C-u M-$전체 버퍼에 대한 수정을 트리거합니다.
    (keymap-global-set "M-$" #'jinx-correct)
    (keymap-global-set "C-M-$" #'jinx-languages)

    ;; /tecosaur-dot-doom/config.org
    (push 'org-inline-src-block
          (alist-get 'org-mode jinx-exclude-faces))
    )

;;;; jh-writing

;;;;; DONT jh-writing > hangul

  ;; (use-package pangu-spacing
  ;;   :ensure t
  ;;   :init
  ;;   (progn ;; replacing `chinese-two-byte' by `japanese'
  ;;     (setq pangu-spacing-include-regexp
  ;;           (rx (or (and (or (group-n 3 (any "，！？；：「」（）、"))
  ;;                            (group-n 1 (or
  ;;                                        (in "가-힣")
  ;;                                        (category korean-hangul-two-byte)
  ;;                                        ;; (category chinse-two-byte)
  ;;                                        ;; (category japanese-hiragana-two-byte)
  ;;                                        ;; (category japanese-katakana-two-byte)
  ;;                                        )))
  ;;                        (group-n 2 (in "a-zA-Z="))) ; 0-9 삭제, = 추가
  ;;                   (and (group-n 1 (in "a-zA-Z="))
  ;;                        (or (group-n 3 (any "，！？；：「」（）、"))
  ;;                            (group-n 2 (or
  ;;                                        (in "가-힣")
  ;;                                        (category korean-hangul-two-byte)
  ;;                                        ;; (category chinse-two-byte)
  ;;                                        ;; (category japanese-hiragana-two-byte)
  ;;                                        ;; (category japanese-katakana-two-byte)
  ;;                                        )))))))

  ;;     ;;     ;; modules/input/japanese/README.org
  ;;     ;;     ;; replacing `chinese-two-byte' by `japanese'
  ;;     ;;     ;; (setq pangu-spacing-chinese-before-english-regexp
  ;;     ;;     ;;       "\\(?1:\\cj\\)\\(?2:[0-9A-Za-z]\\)"
  ;;     ;;     ;;       pangu-spacing-chinese-after-english-regexp
  ;;     ;;     ;;       "\\(?1:[0-9A-Za-z]\\)\\(?2:\\cj\\)"
  ;;     ;;     ;;       ;; Always insert `real' space in text-mode including org-mode.
  ;;     ;;     ;;       )

  ;;     ;; Always insert `real' space in text-mode including org-mode.
  ;;     (setq pangu-spacing-real-insert-separtor t)
  ;;     ;; (global-pangu-spacing-mode 1) ; never
  ;;     ;; markdown-mode 추가하지마라!
  ;;     ;; (add-hook 'org-mode-hook 'pangu-spacing-mode)
  ;;     )
  ;;   )

;;;;; jh-writing > editing

  (use-package unfill
    :bind
    (([remap fill-paragraph] . unfill-toggle)
     :map org-mode-map ("M-q" . unfill-toggle)))

  (use-package undo-fu
    :config
    ;; C-r 은 isearch-backward 가 기본
    (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
    (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)

    ;; (evil-define-key 'normal 'global (kbd "C-r") #'undo-fu-only-redo)
    ;; (evil-define-key 'normal 'global "u" #'undo-fu-only-undo)

    ;; Undo-fu customization options
    ;; Undoing with a selection will use undo within that region.
    (setq undo-fu-allow-undo-in-region t)
    ;; Use the `undo-fu-disable-checkpoint' command instead of Ctrl-G `keyboard-quit' for non-linear behavior.
    (setq undo-fu-ignore-keyboard-quit t)
    ;; By default while in insert all changes are one big blob. Be more granular
    (setq evil-want-fine-undo t)

    (setq evil-undo-system 'undo-fu)
    (evil-set-undo-system 'undo-fu)
    )

  (use-package visual-fill-column
    :commands visual-fill-column-mode
    :hook ((eww-after-render . visual-fill-column-mode)
           (eww-after-render . visual-line-mode)
           ;; (notmuch-show-mode . visual-fill-column-mode)
           )
    :config
    (setq-default visual-fill-column-center-text t
                  visual-fill-column-width 80)
    )


;;;;; jh-writing > search and replace

;;;;;; jh-writing > search and replace > deadgrep

  (use-package deadgrep
    :after consult
    :custom
    (deadgrep-project-root-function 'projectile-project-root)
    :defer 10)

;;;;;; jh-writing > search and replace > keybindings

  (progn
    ;; (spacemacs/set-leader-keys "jd" 'xref-find-definitions)
    ;; (spacemacs/set-leader-keys "jD" 'xref-pop-marker-stack)
    ;; (spacemacs/set-leader-keys "j?" 'xref-find-references)

    ;; (global-set-key (kbd "M-s g") 'consult-grep)
    ;; (global-set-key (kbd "M-s r") 'consult-ripgrep)

    ;; TODO /kristoffer-dotfiles/site-lisp/toki-file.el:14

    (defun my/consult-find () (interactive) (consult-find "."))
    (defun my/consult-fd () (interactive) (consult-fd "."))

    (global-set-key (kbd "M-s F") 'my/consult-fd)

    (spacemacs/set-leader-keys "ff" 'spacemacs/compleseus-find-file) ; default
    (spacemacs/set-leader-keys "fF" 'my/consult-fd)
    (spacemacs/set-leader-keys "f M-f" 'my/consult-find)

    (global-set-key (kbd "M-s r") 'deadgrep)

    (spacemacs/set-leader-keys "sa" nil)
    (spacemacs/set-leader-keys "sr" nil)
    (spacemacs/set-leader-keys "sS" nil)
    (spacemacs/set-leader-keys "sP" nil)
    (spacemacs/set-leader-keys "sw" nil)
    (spacemacs/set-leader-keys "st" nil)

    (spacemacs/set-leader-keys "sr" 'deadgrep)
    (spacemacs/set-leader-keys "sg" 'consult-grep)
    (spacemacs/set-leader-keys "sk" 'consult-keep-lines)
    (spacemacs/set-leader-keys "ss" 'consult-line)
    (spacemacs/set-leader-keys "sd" #'my/compleseus-search-dir)
    (spacemacs/set-leader-keys "sD" #'spacemacs/compleseus-search-dir)
    (spacemacs/set-leader-keys "sF" 'my/compleseus-search-auto-hidden)
    ) ;; end-of progn

;;;;; jh-writing > evil

;;;;;; jh-writing > evil > evil

  (with-eval-after-load 'evil
    ;; (setq evil-want-C-i-jump t) ; use C-i / C-o  evil-jump-backward/forward
    ;; C-h is backspace in insert state
    (setq evil-want-C-h-delete t)
    (setq evil-want-C-w-delete t) ; default t
    (setq evil-want-C-u-scroll t) ; default nil

    ;;  /home/junghan/sync/man/dotsamples/vanilla/mpereira-dotfiles-evil-clojure/configuration.org
    ;; FIXME: this correctly causes '*' to match on whole symbols (e.g., on a
    ;; Clojure file pressing '*' on 'foo.bar' matches the whole thing, instead of
    ;; just 'foo' or 'bar', BUT, it won't match 'foo.bar' in something like
    ;; '(foo.bar/baz)', which I don't like.
    ;; (setq-default evil-symbol-word-search t)
    ;; (setq evil-jumps-cross-buffers nil)
    (setq evil-want-Y-yank-to-eol t)

    ;; 'Important' Prevent the cursor from moving beyond the end of line.
    ;; Don't move the block cursor when toggling insert mode
    (setq evil-move-cursor-back nil) ; nil is better - default t
    (setq evil-move-beyond-eol nil) ; default nil

    ;; Don't put overwritten text in the kill ring
    (setq evil-kill-on-visual-paste nil) ; better

    ;; Don't create a kill entry on every visual movement.
    ;; More details: https://emacs.stackexchange.com/a/15054:
    (fset 'evil-visual-update-x-selection 'ignore)

    (setq evil-insert-state-cursor '(bar "#F86155")) ;; better look
    ;; (setq evil-normal-state-cursor '(box "orange")
    ;;     evil-insert-state-cursor '(box "dodger blue")
    ;;     evil-emacs-state-cursor  '(box "purple"))

    ;; Prevent evil-motion-state from shadowing previous/next sexp
    (with-eval-after-load 'evil-maps
      (define-key evil-motion-state-map "L" nil)
      (define-key evil-motion-state-map "M" nil)

      ;; Replace Emacs Tabs key bindings with Workspace key bindings
      ;; replace "." search with consul-line in Evil normal state
      ;; use default "/" evil search
      (evil-global-set-key 'normal "." 'consult-line)

      (evil-global-set-key 'insert (kbd "C-k") 'kill-line)

      ;; o :: ace-link-info 이거면 충분하다.
      ;; [[file:~/spacemacs/doc/DOCUMENTATION.org::*Binding keys][Binding keys]]
      (define-key evil-insert-state-map (kbd "C-]") 'forward-char) ; very useful

      ;; =C-w= 'insert 'evil-delete-backward-word
      ;; =C-w= 'visual 'evil-window-map
      (define-key evil-normal-state-map (kbd "C-a") 'evil-beginning-of-line)
      (define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line-or-visual-line)
      (define-key evil-insert-state-map (kbd "C-a") 'evil-beginning-of-line)
      (define-key evil-insert-state-map (kbd "C-e") 'evil-end-of-line-or-visual-line))

    (with-eval-after-load 'evil-escape
      ;; evil-escape - switch between insert and normal state
      ;; fd 는 ㄹㅇ일 때 적용이 안되니 ,.을 입력 시 escape 하도록 바꿈.
      ;; unordered 로 해보니 minor-mode 를 열기도 해서 아예 논란이 없도록 바꿈.
      (setq-default evil-escape-key-sequence ",.")
      (setq-default evil-escape-unordered-key-sequence nil)
      (setq-default evil-escape-delay 1.0) ;; 0.5, default 0.1
      ;; (setq-default evil-escape-inhibit-functions nil)
      (evil-escape-mode)
      ;; (add-to-list 'evil-escape-excluded-major-modes 'code-review-mode)
      )
    ) ;; end-of evil

;;;;;; jh-writing > evil > evil-visualstar

  (global-evil-visualstar-mode t)
  (setq evil-visualstar/persistent t) ; need

;;;;;; jh-writing > evil > evil-string-inflection

  ;; superword-mode 관련 sub-word, superword-mode, substring,
  ;; string-inflaction, underscore 등 언더바 문제부터 해서 걸려있는 부분들이
  ;; 많이 있다. evil-symbol-word-search 도 여기와 연관이 되어 있다. 알고
  ;; 가야한다.

  (with-eval-after-load 'evil-string-inflection
    (define-key evil-normal-state-map "gR" 'evil-operator-string-inflection))


;;;;;; jh-writing > evil > evil-owl

  ;; gl ${operator}

;;;;;; jh-writing > evil > evil-surround

  ;; @call-function
  ;; visual mode S- or gS-
  ;; normal mode ys- or yS-
  ;; change surround cs-
  ;; delete surround ds-
  ;; @select area
  ;; call-functionu- - ;현재부터 단어 끝까지
  ;; {call-function}-i- ;현재 단어
  ;; {call-function}-s- ;현재 줄
  ;; @wrap function
  ;; {select-area}-w
  ;; ${target}( 바꾸고싶은거 ), ${change}(바뀔거)
  ;; 감싸기:     => y-s-i-w-${change}( "(", "{", "[")
  ;; 전부 감싸기 => y-s-s-${change}
  ;; 바꾸기: => c-s-${target}( "(", "{", "["), ${change}
  ;; 벗기기: => d-s-${target}( "(", "{", "[")

;;;;;; jh-writing > evil > evil-goggles

  ;; vim-style-visual-feedback t

;;;;;; jh-writing > evil > evil-traces

  ;; move: m +{n}, delete: +{n},+{n}d, join: .,+{n}j glboal: g/{target}/{change}
  (use-package evil-traces :after evil
    :if (not (or my/remote-server IS-TERMUX))
    :config (evil-traces-use-diff-faces)
    (evil-traces-mode))

;;;;;; DONT jh-writing > evil > evil-owl

  ;; (use-package evil-owl
  ;;   :if (not (or my/remote-server IS-TERMUX))
  ;;   :config
  ;;   (setq evil-owl-idle-delay 0.5)
  ;;   (setq evil-owl-max-string-length 500)

  ;;   ;; (when (display-graphic-p) ; gui
  ;;   ;;   (setq evil-owl-display-method 'posframe
  ;;   ;;         evil-owl-extra-posframe-args '(:width 50 :height 20)
  ;;   ;;         evil-owl-max-string-length 50))
  ;;   (evil-owl-mode))

;;;;; jh-writing > Structural Editing : puni

  (use-package puni
    :diminish ""
    :hook ((puni-mode  . electric-pair-mode)
           (prog-mode  . puni-mode))
    :init
    ;; The default `puni-mode-map' respects "Emacs conventions".  We don't, so
    ;; it's better to simply clear and rewrite it.
    (setcdr puni-mode-map nil)

    ;; (require 'lib-puni)

    (bind-keys
     :map puni-mode-map

     ;; ("M-<backspace>" . puni-splice)
     ("M-<delete>" . puni-splice) ; sp-unwrap-sexp

     ("C-<right>"  .  puni-slurp-forward)
     ("C-<left>" . puni-barf-forward)

     ("C-M-<left>" .  puni-slurp-backward)
     ("C-M-<right>" . puni-barf-backward)

     ("C-M-<delete>" . puni-splice-killing-forward)
     ("C-M-<backspace>" . puni-splice-killing-backward)

     ("C-M-a" . beginning-of-defun) ; default
     ("C-M-e" . end-of-defun)
     ("M-]" . forward-sexp) ; default
     ("M-[" . backward-sexp)

     ("C-M-f" . puni-forward-sexp)
     ("C-M-b" . puni-backward-sexp)

     ("C-M-p" . puni-beginning-of-sexp)
     ("C-M-n" . puni-end-of-sexp)

     ;; C-M-d down-sexp
     ("C-M-t" . transpose-sexp)
     ("C-M-?" . puni-convolute)

     ("C-M-k" . kill-sexp)
     ("C-M-K"   . backward-kill-sexp)
     ;; ("C-" . puni-backward-kill-word)

     ("M-)" . puni-syntactic-forward-punct)
     ("M-(" . puni-syntactic-backward-punct)

     ("C-c DEL" . puni-force-delete)
     ;; ("C-M-d" . puni-forward-delete-char)
     ;; ("C-M-k" . puni-kill-line)
     ;; ("C-M-K" . puni-backward-kill-line)
     ;;  ("C-M-w" . puni-kill-region)

     ;; ([remap puni-backward-kill-word] . backward-kill-word)
     ("C-M-z" . puni-squeeze) ; unwrap

     ("C-c {" . puni-wrap-curly)
     ("C-c (" . puni-wrap-round)
     ("C-c [" . puni-wrap-square)
     )
    ) ;; end-of puni


;;;;; jh-writing > Markup

;;;;;; jh-writing > Markup > markdown

  (with-eval-after-load 'markdown-mode
    ;; Make markdown-mode behave a bit more like org w.r.t. code blocks i.e.
    ;; use proper syntax highlighting
    (setq markdown-hide-urls nil) ; must
    (setq markdown-fontify-code-blocks-natively t)
    (setq markdown-display-remote-images t)
    (setq markdown-list-item-bullets '("◦" "-" "•" "–"))

    (setq markdown-command
          (concat
           "pandoc"
           ;; " --from=markdown --to=html"
           ;; " --standalone --mathjax --highlight-style=pygments"
           ;; " --css=~/.pandoc/pandoc.css"
           ;; " --quiet"
           ;; " --number-sections"
           ;; " --lua-filter=~/dotfiles/pandoc/cutsection.lua"
           ;; " --lua-filter=~/dotfiles/pandoc/cuthead.lua"
           ;; " --lua-filter=~/dotfiles/pandoc/date.lua"
           ;; " --metadata-file=~/dotfiles/pandoc/metadata.yml"
           ;; " --metadata=reference-section-title:References"
           ;; " --citeproc"
           ;; " --bibliography=~/Dropbox/Work/bibfile.bib"
           ))

    (advice-add
     'markdown-fontify-list-items :override
     (lambda (last)
       (when (markdown-match-list-items last)
         (when (not (markdown-code-block-at-point-p (match-beginning 2)))
           (let* ((indent (length (match-string-no-properties 1)))
                  (level (/ indent markdown-list-indent-width))
                  ;; level = 0, 1, 2, ...
                  (bullet (nth (mod level (length markdown-list-item-bullets))
                               markdown-list-item-bullets)))
             (add-text-properties
              (match-beginning 2) (match-end 2) '(face markdown-list-face))
             (cond
              ;; Unordered lists
              ((string-match-p "[\\*\\+-]" (match-string 2))
               (add-text-properties
                (match-beginning 2) (match-end 2) `(display ,bullet)))
              ;; Definition lists
              ((string-equal ":" (match-string 2))
               (let ((display-string
                      (char-to-string (markdown--first-displayable
                                       markdown-definition-display-char))))
                 (add-text-properties (match-beginning 2) (match-end 2)
                                      `(display ,display-string)))))))
         t)))

    (add-hook 'markdown-mode-hook #'visual-line-mode)
    (add-hook 'markdown-mode-hook #'spacemacs/toggle-auto-fill-mode-on)

    (add-hook
     'markdown-mode-hook
     (lambda ()
       "Beautify Markdown em-dash and checkbox Symbol"
       (push '("---" . "—") prettify-symbols-alist)
       (push '("->" . "⟶" ) prettify-symbols-alist)
       (push '("=>" . "⟹") prettify-symbols-alist)
       (prettify-symbols-mode)))

    ;; Plain text (text-mode)
    (add-to-list 'auto-mode-alist '("\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)\\'" . text-mode))


    (define-key markdown-mode-map (kbd "<f3>") 'markdown-toggle-markup-hiding)
    (define-key markdown-mode-map (kbd "<f4>") 'markdown-toggle-inline-images)

    (evil-define-key '(insert) markdown-mode-map (kbd "C-u") 'undo-fu-only-undo)
    (evil-define-key '(insert) markdown-mode-map (kbd "C-r") 'undo-fu-only-redo)

    (evil-define-key '(insert normal visual) markdown-mode-map (kbd "C-<up>") 'markdown-backward-paragraph) ;; same as org-mode
    (evil-define-key '(insert normal visual) markdown-mode-map (kbd "C-<down>") 'markdown-forward-paragraph)

    (evil-define-key '(normal visual) markdown-mode-map (kbd "C-n") 'markdown-outline-next)
    (evil-define-key '(normal visual) markdown-mode-map (kbd "C-p") 'markdown-outline-previous)
    (evil-define-key '(insert) markdown-mode-map (kbd "C-n") 'next-line)
    (evil-define-key '(insert) markdown-mode-map (kbd "C-p") 'previous-line)

    (evil-define-key '(normal visual) markdown-mode-map (kbd "C-j") 'markdown-outline-next-same-level)
    (evil-define-key '(normal visual) markdown-mode-map (kbd "C-k") 'markdown-outline-previous-same-level)
    (evil-define-key '(normal visual) markdown-mode-map (kbd "C-S-p") 'markdown-up-heading)
    (evil-define-key '(normal visual) markdown-mode-map "zu" 'markdown-up-heading) ;; same with evil-collection outline
    ) ; end-of markdown-mode

;;;;;; jh-writing > Markup > typst

  ;; (use-package typst-ts-mode
  ;;     :defer t
  ;;     :custom
  ;;     (typst-ts-mode-watch-options "--open")
  ;;     :config
  ;;     (with-eval-after-load 'consult
  ;;         (setq
  ;;             consult-imenu-config
  ;;             (append consult-imenu-config
  ;;                 '((typst-ts-mode :topLevel "Headings" :types
  ;;                       ((?h "Headings" typst-ts-markup-header-face)
  ;;                           (?f "Functions" font-lock-function-name-face))))))))

;;;;; DONT jh-writing > dictionary

  ;; (progn
  ;;   (use-package dictionary
  ;;     :config
  ;;     (setq dictionary-server "localhost")
  ;;     ;; (setq dictionary-server "dict.org")
  ;;     (setq dictionary-default-popup-strategy "lev" ; read doc string
  ;;           dictionary-create-buttons nil
  ;;           dictionary-use-single-buffer t))

  ;;   (use-package define-it
  ;;     ;; :if (not (or my/remote-server IS-TERMUX))
  ;;     :defer 12
  ;;     :config
  ;;     (setq
  ;;      define-it-show-google-translate nil
  ;;      define-it-show-header nil)
  ;;     ;; it doesn't pop to the buffer automatically, when definition is fetched
  ;;     (defun define-it--find-buffer (x)
  ;;       (let ((buf (format define-it--buffer-name-format define-it--current-word)))
  ;;         (pop-to-buffer buf)))
  ;;     (advice-add 'define-it--in-buffer :after #'define-it--find-buffer)
  ;;     (add-to-list
  ;;      'display-buffer-alist
  ;;      '("\\*define-it:"
  ;;        (display-buffer-reuse-window
  ;;         display-buffer-in-direction)
  ;;        (direction . right)
  ;;        (window . root)
  ;;        (window-width . 0.25))))

  ;;   (use-package lexic :if (not (or my/remote-server IS-TERMUX)) :defer 10)

  ;;   (use-package external-dict :if (not (or my/remote-server IS-TERMUX)) :defer 14)

  ;;   (use-package define-word :if (not (or my/remote-server IS-TERMUX)))

  ;;   (use-package sdcv
  ;;     :defer 6
  ;;     :if (not (or my/remote-server IS-TERMUX))
  ;;     :config
  ;;     (require 'posframe)
  ;;     (face-spec-set 'sdcv-tooltip-face
  ;;                    '((((background light))
  ;;                       :foreground "#000000" :background "#ffffff" :weight semi-light :height 0.9)
  ;;                      (t
  ;;                       :foreground "#ffffff" :background "#000000" :weight semi-light :height 0.9))
  ;;                    'face-override-spec)

  ;;     (add-hook 'sdcv-mode-hook #'visual-line-mode)
  ;;     (setq sdcv-tooltip-timeout 10)
  ;;     (setq sdcv-env-lang "ko_KR.UTF-8")
  ;;     (setq sdcv-tooltip-border-width 2)
  ;;     (setq sdcv-say-word-p t)               ; say word after translation
  ;;     (setq sdcv-dictionary-data-dir (file-truename "~/.stardict/dic/"))
  ;;     (setq sdcv-fail-notify-string "*Not Found*")

  ;;     (setq sdcv-dictionary-simple-list    ;; setup dictionary list for simple search
  ;;           '(
  ;;             "Korean Dic"
  ;;             "quick_english-korean" ; 영한 사전
  ;;             "Kor-Eng Dictionary" ; 한영 사전 (quick)
  ;;             "Merrian Webster 10th dictionary"
  ;;             "Hanja(Korean Hanzi) Dic"
  ;;             ))

  ;;     (setq sdcv-dictionary-complete-list     ;; setup dictionary list for complete search
  ;;           '(
  ;;             "Korean Dic"
  ;;             "quick_english-korean"
  ;;             "Kor-Eng Dictionary"
  ;;             "Hanja(Korean Hanzi) Dic"
  ;;             "Merrian Webster 10th dictionary"
  ;;             "Webster's Revised Unabridged Dictionary (1913)"
  ;;             ;; "Moby Thesaurus II"
  ;;             )))

  ;;   (use-package wordreference
  ;;     :defer 8
  ;;     :if (not (or my/remote-server IS-TERMUX))
  ;;     :hook (wordreference-mode . visual-line-mode)
  ;;     :commands (wordreference-search
  ;;                my/wr-enko
  ;;                my/wr-koen)
  ;;     :config
  ;;     (defun my/wr-koen ()
  ;;       (interactive)
  ;;       (let ((wordreference-source-lang "ko")
  ;;             (wordreference-target-lang "en"))
  ;;         (wordreference-search)))
  ;;     (defun my/wr-enko ()
  ;;       (interactive)
  ;;       (let ((wordreference-source-lang "en")
  ;;             (wordreference-target-lang "ko"))
  ;;         (wordreference-search))))

  ;;   (use-package powerthesaurus
  ;;     :after hydra
  ;;     :if (not (or my/remote-server IS-TERMUX))
  ;;     :defer 10)

  ;;   (use-package wiki-summary :defer 12)
  ;;   )
  ;; end-of progn dictionary


;;;;; jh-writing > translation

;;;;;; txl and guess-language

  (use-package guess-language
    :ensure t
    :config
    (setq guess-language-langcodes
          '((en . ("en" "English" "🇬🇧" "English"))
            (ko . ("ko" "Korean" "🇰🇷" "Korean"))))

    (setq guess-language-languages '(ko en))
    (setq guess-language-min-paragraph-length 35)

    (require 'txl)
    (setq txl-languages '(EN-US . KO))
    (setq txl-deepl-api-url "https://api-free.deepl.com/v2/translate") ;; free
    (setq txl-deepl-api-key user-deepl-api-key)
    )

;;;;;; jh-writing > translation > immersive-translate

  (use-package immersive-translate
    :init
    ;; (setq immersive-translate-auto-idle 2.0) ; default 0.5
    ;; wget git.io/trans; chmod +x trans; sudo mv trans /usr/local/bin
    ;; ko         Korean                         한국어
    (setq immersive-translate-backend 'trans)
    (setq immersive-translate-trans-target-language "ko")
    :config
    (add-hook 'elfeed-show-mode-hook #'immersive-translate-setup)
    (add-hook 'nov-mode-hook #'immersive-translate-setup)
    (add-hook 'Info-mode-hook #'immersive-translate-setup)
    (add-hook 'help-mode-hook #'immersive-translate-setup)
    (add-hook 'helpful-mode-hook #'immersive-translate-setup)
    )

;;;;; jh-writing > writing-tools

;;;;;; jh-writing > writing-tools > separedit

  (progn
    (use-package separedit
      :ensure t
      ;; Key binding for modes you want edit
      ;; or simply bind ?global-map? for all.
      :bind (:map prog-mode-map
                  ("C-c '" . separedit)
                  :map minibuffer-local-map
                  ("C-c '" . separedit)
                  :map help-mode-map
                  ("C-c '" . separedit))
      :init
      ;; Default major-mode for edit buffer
      ;; can also be other mode e.g. ?org-mode?.
      ;; (setq separedit-default-mode 'markdown-mode)

      ;; (setq separedit-write-file-when-execute-save t)
      ;; (setq separedit-remove-trailing-spaces-in-comment t)
      (setq separedit-preserve-string-indentation t)
      (setq separedit-continue-fill-column t))

;;;;;; jh-writing > writing-tools > palimpsest

    ;; M-x palimpsest-move-region-to-bottom
    ;; M-x palimpsest-move-region-to-top
    ;; M-x palimpsest-move-region-to-trash
    (use-package palimpsest
      :after org
      :defer 6
      :hook (org-mode . palimpsest-mode))

;;;;;; jh-writing > writing-tools > olivetti

    (use-package olivetti
      :custom
      ;; (olivetti-body-width 0.7) ; nil
      (olivetti-minimum-body-width 90) ; for compatibility fill-column 80
      (olivetti-recall-visual-line-mode-entry-state t))

;;;;;; jh-writing > writing-tools > org-noter

    (use-package org-noter :defer 4)

;;;;;; jh-writing > writing-tools > logos

    (use-package logos
      :after olivetti
      :init
      ;; If you want to use outlines instead of page breaks (the ^L):
      (setq logos-outlines-are-pages t)
      ;; This is the default value for the outlines:
      (setq logos-outline-regexp-alist
            `((emacs-lisp-mode . "^;;;+ ")
              (org-mode . "^\\*+ +")
              (markdown-mode . "^\\#+ +")
              (t . ,(if (boundp 'outline-regexp) outline-regexp logos--page-delimiter))))

      ;; These apply when `logos-focus-mode' is enabled.  Their value is
      ;; buffer-local.
      (setq-default logos-hide-cursor nil)
      (setq-default logos-hide-mode-line nil)
      (setq-default logos-hide-buffer-boundaries t)
      (setq-default logos-hide-fringe t)
      (setq-default logos-variable-pitch nil) ; see my `fontaine' configurations
      (setq-default logos-buffer-read-only nil)
      (setq-default logos-scroll-lock nil)
      (setq-default logos-olivetti t)

      :config
      ;; I don't need to do `with-eval-after-load' for the `modus-themes' as
      ;; I always load them before other relevant potentially packages.
      ;; (add-hook 'modus-themes-after-load-theme-hook #'logos-update-fringe-in-buffers)

      (let ((map global-map))
        (define-key map [remap narrow-to-region] #'logos-narrow-dwim)
        (define-key map [remap forward-page] #'logos-forward-page-dwim)
        (define-key map [remap backward-page] #'logos-backward-page-dwim)
        (define-key map (kbd "M-]") #'logos-forward-page-dwim)
        (define-key map (kbd "M-[") #'logos-backward-page-dwim)
        )
      ;; place point at the top when changing pages, but not in `prog-mode'
      (defun prot/logos--recenter-top ()
        "Use `recenter' to reposition the view at the top."
        (unless (derived-mode-p 'prog-mode)
          (recenter 1))) ; Use 0 for the absolute top
      (add-hook 'logos-page-motion-hook #'prot/logos--recenter-top)

      ;; Also consider adding keys to `logos-focus-mode-map'.  They will take
      ;; effect when `logos-focus-mode' is enabled.
      ;; Make EWW look like the rest of Emacs
      (setq shr-max-width fill-column)
      (setq shr-use-fonts nil)
      )
    ) ;; end-of progn



;;;;;; jh-writing > writing-tools > ten

  (use-package ten
    :after consult
    :defer 2
    ;; :bind (("M-c t" . complete-tag)
    ;;        ("C-c M-." . my/goto-etags))
    ;; :hook ((text-mode eww-mode nov-mode Info-mode) . ten-font-lock-mode)
    :init
    (setq ten-file-extensions '("org" "md" "txt"))
    (setq ten-exclude-regexps '("/\\."))
    ;; I am listing two specific dictionary files in the `test/`
    ;; subdirectory as an example below. You can list the
    ;; `~/src/ten/test/' directory to let Ten to search files recursively
    ;; in the directory and subdirectories in it. There are about 5,000
    ;; terms in total and I don't experience any perfomance issue on my
    ;; old Lenovo Thinkpad laptop. Ten looks for files with an extension
    ;; listed in `ten-file-extensions' and excludes files and those in
    ;; directories that match the list of regexps `ten-exclude-regexps'.
    ;; (setq ten-files-and-directories
    ;;       '( "~/sync/emacs/git/default/ten/test/Glossary-philosophy.txt"
    ;;          "~/sync/emacs/git/default/ten/test/Glossary-of-graph-theory.txt"))
    ;; The dictionary file (only one at a time can be active through
    ;; `etags', but you can switch between more than one of them if you
    ;; need to. The switching experience is not intuitive and it's a TODO
    ;; to improve it.)
    ;; (setq ten-tags-file-default "~/sync/emacs/git/default/ten/ten-TAGS")
    :config
    (require 'consult-ten)
    (add-to-list 'consult-buffer-sources 'consult-ten-glossary 'append) ; g
    )

;;;;; jh-writing > keybindings

  (global-set-key (kbd "M-g 0") 'txl-translate-insert)

  ;; (spacemacs/defer-until-after-user-config  ; otherwise, spacemacs-default layer would override the binding
  ;;  (lambda ()                               ; and set it to `duplicate-line-or-region', and it's pretty useles for me
  ;;    (spacemacs/set-leader-keys "xwi" #'define-it-at-point)
  ;;    (spacemacs/set-leader-keys "xwb" #'wiktionary-bro-dwim)
  ;;    (spacemacs/set-leader-keys "xww" #'define-word-at-point)
  ;;    (spacemacs/set-leader-keys "xwl" #'lexic-search)
  ;;    (spacemacs/set-leader-keys "xwe" #'external-dict-dwim)
  ;;    (spacemacs/set-leader-keys "xwm" #'mw-thesaurus-lookup-dwim)
  ;;    (spacemacs/set-leader-keys "xwp" #'powerthesaurus-transient)
  ;;    (spacemacs/set-leader-keys "xwd" #'dictionary-search)
  ;;    (spacemacs/set-leader-keys "xws" #'sdcv-search-pointer)
  ;;    (spacemacs/set-leader-keys "xwk" #'my/wr-enko)
  ;;    ))

;;;; jh-reading

;;;;; jh-reading > eww

  ;; ha-evil
  (with-eval-after-load 'eww
    (setq eww-browse-url-new-window-is-tab nil
          shr-use-colors nil
          shr-use-fonts nil     ; I go back and forth on this one
          ;; shr-discard-aria-hidden t
          shr-bullet "• "
          shr-inhibit-images t  ; Gotta see the images?
          ;; shr-blocked-images '(svg)
          ;; shr-folding-mode nil
          url-privacy-level '(email))

    ;; This function allows Imenu to offer HTML headings in EWW buffers,
    ;; helpful for navigating long, technical documents.
    ;; https://github.com/alphapapa/unpackaged.el
    (defun spacemacs/imenu-eww-headings ()
      "Return alist of HTML headings in current EWW buffer for Imenu.
Suitable for `imenu-create-index-function'."
      (let ((faces '(shr-h1 shr-h2 shr-h3 shr-h4 shr-h5 shr-h6 shr-heading)))
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (cl-loop for next-pos = (next-single-property-change (point) 'face)
                     while next-pos
                     do (goto-char next-pos)
                     for face = (get-text-property (point) 'face)
                     when (cl-typecase face
                            (list (cl-intersection face faces))
                            (symbol (member face faces)))
                     collect (cons (buffer-substring (point-at-bol) (point-at-eol)) (point))
                     and do (forward-line 1))))))

    (spacemacs/set-leader-keys "aweb" 'eww-list-bookmarks)
    (spacemacs/set-leader-keys-for-major-mode 'eww-mode "y" 'eww-copy-page-url)

    (spacemacs|add-toggle eww-as-default-browser
      :documentation "Eww as default browser."
      :status (equal browse-url-browser-function 'eww-browse-url)
      :on (setq browse-url-browser-function 'eww-browse-url)
      ;; should have a var to store the original one
      :off (setq browse-url-browser-function 'browse-url-default-browser)
      :evil-leader "t e")

    ;; https://github.com/alphapapa/unpackaged.el
    (add-hook 'eww-mode-hook (lambda () (setq-local imenu-create-index-function #'spacemacs/imenu-eww-headings)))
    )

;;;;; jh-reading > info

  ;; Info 모드 Node 이동
  ;; (evil-define-key '(motion normal visual) Info-mode-map
  ;;   "^" 'Info-up
  ;;   "C-n" 'Info-prev
  ;;   "C-p" 'Info-next
  ;;   "M-n" 'Info-forward-node
  ;;   "M-p" 'Info-backward-node
  ;;   )

  ;; use -- (kbd "C-x x v") 'view-text-file-as-info-manual
  (add-hook 'Info-mode-hook
            (lambda () (define-key Info-mode-map (kbd "M-[") 'Info-history-back)
              (define-key Info-mode-map (kbd "M-]") 'Info-history-forward)))

;;;;; jh-reading > Hypothesis

  ;; M-x hypothesis-to-org downloads the 200 newest notations and inserts
  ;; them into a temporary org-mode buffer. M-x hypothesis-to-archive
  ;; imports notations into hypothesis-archive. It will import up to 200
  ;; notations but will only import notations made after the last import.
  (use-package hypothesis
    :ensure t
    :commands hypothesis-to-org hypothesis-to-archive
    :config
    (setq hypothesis-username user-hypothesis-username)
    (setq hypothesis-token user-hypothesis-token)
    (setq hypothesis-quote-prefix "#+begin_example")
    (setq hypothesis-quote-sufix "#+end_example")
    )

  (with-eval-after-load 'hypothesis
    (setq hypothesis-archive (my/org-links-file)))

;;;;; jh-reading > saveplace-pdf-view (GUI only)

  (use-package saveplace-pdf-view
    :if window-system
    :hook (pdf-view-mode . (lambda ()
                             (require 'saveplace-pdf-view)
                             (save-place-mode t))))

;;;;; jh-reading > elfeed

  (with-eval-after-load 'elfeed
    (setq elfeed-search-filter "@6-months-ago") ;;  "@1-month-ago +unread"
    (add-hook 'elfeed-search-mode-hook #'elfeed-update))

;;;;; jh-reading > google-translate

  (use-package google-translate
    :commands (spacemacs/set-google-translate-languages)
    :init
    ;; fix search fail ',ttk'
    ;; (see https://github.com/atykhonov/google-translate/issues/52#issuecomment-727920888)
    (with-eval-after-load 'google-translate-tk
      (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130)))
    (progn
      (autoload 'google-translate-translate "google-translate-core-ui" "google-translate-translate" nil nil)
      (autoload 'popup-tip "popup" "popup-tip" nil nil)

      (defun spacemacs/set-google-translate-languages (&optional override-p)
        "Set source language for google translate.
For instance pass En as source for English."
        (interactive "P")
        (autoload 'google-translate-read-args "google-translate-default-ui")
        (let* ((langs (google-translate-read-args override-p nil))
               (source-language (car langs))
               (target-language (cadr langs)))
          (setq google-translate-default-source-language source-language)
          (setq google-translate-default-target-language target-language)
          (message
           (format "Set google translate source language to %s and target to %s"
                   source-language target-language))))

      (defun spacemacs/set-google-translate-target-language ()
        "Set the target language for google translate."
        (interactive)
        (spacemacs/set-google-translate-languages nil))

      (defun google-translate-to-korean (&optional str)
        "Translate given string automatically without language selection prompt."
        (let ((lang (cond
                     ((string-match "[가-힣]" str)
                      "ko")
                     ((or (string-match "[ァ-ヶー]" str)
                          (string-match "[ぁ-んー]" str)
                          ;; (string-match "[亜-瑤]" str)
                          )
                      "ja")
                     ((string-match "[一-龥]" str)
                      "zh-CN")
                     (t
                      "en"))))
          (google-translate-translate lang
                                      (if (string= "ko" lang) "en" "ko")
                                      str)))

      (defun korean/popup-translation (&optional str)
        "Display Google translation in tooltip."
        (interactive)
        (let* ((str (cond ((stringp str) str)
                          (current-prefix-arg
                           (read-string "Google Translate: "))
                          ((use-region-p)
                           (buffer-substring (region-beginning) (region-end)))
                          (t
                           (save-excursion
                             (let (s)
                               (forward-char 1)
                               (backward-sentence)
                               (setq s (point))
                               (forward-sentence)
                               (buffer-substring s (point)))))))
               (translated-str (save-window-excursion
                                 (funcall 'google-translate-to-korean
                                          (replace-regexp-in-string "^\\s-+" str))
                                 (switch-to-buffer "*Google Translate*")
                                 (buffer-string))))
          (if (region-active-p)
              (run-at-time 0.1 nil 'deactivate-mark))
          (kill-buffer "*Google Translate*")
          (popup-tip translated-str
                     :point (point)
                     :around t
                     ;; :height 30
                     :scroll-bar t
                     :margin t)))

      (setq google-translate-show-phonetic t)
      (setq google-translate-default-source-language "auto"
            google-translate-default-target-language "ko")
      )
    )

;;;;; DONT jh-reading > mastodon

  ;; (use-package mastodon
  ;;   :ensure t
  ;;   :init
  ;;   (require 'mastodon-toot)
  ;;   (setq mastodon-tl--horiz-bar "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  ;;   (setq mastodon-tl--highlight-current-toot t
  ;;         mastodon-tl--tag-timeline-tags t
  ;;         mastodon-tl--show-avatars t)
  ;;   :config
  ;;   ;; The default emojis take two characters for me
  ;;   (setq mastodon-tl--symbols
  ;;         '((reply "" . "R")
  ;;           (boost "" . "B")
  ;;           (favourite "" . "F")
  ;;           (bookmark "" . "K")
  ;;           (media "" . "[media]")
  ;;           (verified "" . "V")
  ;;           (locked "" . "[locked]")
  ;;           (private "" . "[followers]")
  ;;           (direct "" . "[direct]")
  ;;           (edited "" . "[edited]")))
  ;;   (mastodon-discover) ; context-mode
  ;;   ;; (add-hook 'mastodon-toot-mode-hook
  ;;   ;;           (lambda ()
  ;;   ;;             ;; (auto-fill-mode -1)
  ;;   ;;             (jinx-mode 1)))
  ;;   )

;;;; jh-coding


;;;;; jh-coding > Tunes
;;;;;; IDE Layout with Side Windows

  ;; popwin 에 이미 들어가 있다. 이 주제로 좀 고민이 필요하다.

  ;; show shell/compilation buffer in side window
  ;; (add-to-list 'display-buffer-alist
  ;;     '("\\*\\(?:shell\\|compilation\\)\\*"
  ;;          (display-buffer-reuse-window display-buffer-in-side-window)
  ;;          (side . bottom)
  ;;          (dedicated . t)
  ;;          (reusable-frames . visible)
  ;;          (window-height . 0.5)))

  (setq compilation-scroll-output t) ; 'first-error can be a good option
  (add-hook 'compilation-mode-hook 'goto-address-mode)

;;;;;; Make Script Files Executable Automatically

  ;; Make script files (with shebang like #!/bin/bash, #!/bin/sh) executable automatically. See this blog post from Emacs Redux.
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)

;;;;;; prog-mode-hooks

  ;; Color the string of whatever color code they are holding
  (add-hook 'prog-mode-hook 'rainbow-mode) ; 2023-11-23 on
  (add-hook 'prog-mode-hook 'prettify-symbols-mode)

  ;; (ws-butler-keep-whitespace-before-point nil)
  ;; (ws-butler-global-exempt-modes '(special-mode comint-mode term-mode eshell-mode diff-mode markdown-mode))
  (add-hook 'prog-mode-hook 'ws-butler-mode)

;;;;; jh-coding > lsp-mode with corfu

  (with-eval-after-load 'lsp-mode
    (setq lsp-completion-provider :none) ;; we use Corfu!

    ;; https://github.com/minad/corfu/wiki#basic-example-configuration-with-orderless
    ;; (defun my/lsp-mode-setup-completion ()
    ;;   (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
    ;;         '(flex))) ;; Configure flex
    (defun my/lsp-mode-setup-completion ()
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless))) ;; Configure orderless

    (add-hook 'lsp-mode-hook #'lsp-completion-mode) ;; important
    (add-hook 'lsp-completion-mode-hook #'my/lsp-mode-setup-completion)
    )

;;;;; DONT jh-coding > language server > eglot

  ;; (use-package consult-eglot
  ;;   :after consult
  ;;   :bind (:map eglot-mode-map
  ;;               ("C-c d" . eldoc)
  ;;               ("C-c a" . eglot-code-actions)
  ;;               ("C-c r" . eglot-rename)
  ;;               ("C-c s" . consult-eglot-symbols)))
  ;; :config
  ;; Provide `consult-lsp' functionality from `consult-eglot', useful
  ;; for packages which relay on `consult-lsp' (like `dirvish-subtree').
  ;; (defalias 'consult-lsp-file-symbols #'consult-eglot-symbols)
  ;; (define-key eglot-mode-map (kbd "C-c e c") #'consult-eglot-symbols)

;;;;; DONT jh-coding > debug > dape

  ;; C-x C-a
  ;; (use-package dape
  ;;   :defer 5
  ;;   :after transient eglot
  ;;   ;; :commands +dape-transient
  ;;   :custom
  ;;   (dape-inline-variables t))

  ;; (dape-adapter-dir (concat minemacs-local-dir "dape/"))
  ;; :config
  ;; (transient-define-prefix +dape-transient ()
  ;;     "Transient for dape."
  ;;     [["Stepping"
  ;;          ("n"  "Next" dape-next :transient t)
  ;;          ("s"  "Step in" dape-step-in :transient t)
  ;;          ("o"  "Step out" dape-step-out :transient t)
  ;;          ("c"  "Continue" dape-continue :transient t)
  ;;          ("r"  "Restart" dape-restart :transient t)]
  ;;         ["Breakpoints"
  ;;             ("bb" "Toggle" dape-breakpoint-toggle :transient t)
  ;;             ("be" "Expression" dape-breakpoint-expression :transient t)
  ;;             ("bd" "Remove at pt" dape-remove-breakpoint-at-point :transient t)
  ;;             ("bD" "Remove all" dape-breakpoint-face :transient t)
  ;;             ("bl" "Log" dape-breakpoint-log :transient t)]
  ;;         ["Info"
  ;;             ("ii" "Info" dape-info :transient t)
  ;;             ("im" "Memory" dape-read-memory :transient t)
  ;;             ("is" "Select Stack" dape-select-stack :transient t)
  ;;             ("R"  "Repl" dape-repl :transient t)]
  ;;         ["Quit"
  ;;             ("qq" "Quit" dape-quit :transient nil)
  ;;             ("qk" "Kill" dape-kill :transient nil)]])

  ;; (spacemacs/set-leader-keys
  ;;     "dd" #'dape
  ;;     "dn" #'dape-next
  ;;     "ds" #'dape-step-in
  ;;     "do" #'dape-step-out
  ;;     "dc" #'dape-continue
  ;;     "dr" #'dape-restart
  ;;     "dp" #'dape-pause
  ;;     "db" #'dape-breakpoint-toggle
  ;;     "de" #'dape-breakpoint-expression
  ;;     "dr" #'dape-remove-breakpoint-at-point
  ;;     "dR" #'dape-breakpoint-remove-all
  ;;     "dt" #'+dape-transient
  ;;     "dq" #'dape-kill
  ;;     "dQ" #'dape-quit)

;;;;; jh-coding > aggreesive-indent

  (when (locate-library "aggressive-indent")
    (require 'aggressive-indent)
    (add-hook 'emacs-lisp-mode-hook #'spacemacs/toggle-aggressive-indent-on)
    (add-hook 'css-mode-hook #'spacemacs/toggle-aggressive-indent-on)
    )

;;;;; jh-coding > apheleia for formatting

;;;###autoload
  (defun my/format-buffer ()
    "Format a buffer."
    (interactive)
    (cond
     ((eq major-mode 'emacs-lisp-mode)
      (indent-region (point-min) (point-max)))
     ((eq major-mode 'ledger-mode)
      (ledger-mode-clean-buffer))
     (t (call-interactively 'apheleia-format-buffer))))

  (use-package apheleia
    :after evil
    :commands (apheleia-format-buffer my/format-buffer)
    :config
    ;; (add-hook 'markdown-ts-mode-hook 'apheleia-mode)

    (add-hook 'yaml-ts-mode-hook 'apheleia-mode)
    )

;;;;; jh-coding > environment

  (use-package direnv
    :demand t
    :if (not (or *is-windows* my/remote-server IS-TERMUX))
    :config
    (direnv-mode))

;;;;; jh-coding > languages

;;;;;; common-lisp

  (setq inferior-lisp-program "sbcl")

;;;;;; emacs-lisp

  (use-package eros :config (eros-mode 1))

  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    ;; Emacs' built-in elisp files use a hybrid tab->space indentation scheme
                                    ;; with a tab width of 8. Any smaller and the indentation will be
                                    ;; unreadable. Since Emacs' lisp indenter doesn't respect this variable it's
                                    ;; safe to ignore this setting otherwise.
                                    ;; (setq-local tab-width 8)
                                    (setq-local comment-column 0)
                                    (evil-define-key '(normal visual) emacs-lisp-mode-map (kbd "<tab>") 'evil-jump-item)
                                    (define-key emacs-lisp-mode-map (kbd "M-[") 'backward-sexp)
                                    (define-key emacs-lisp-mode-map (kbd "M-]") 'forward-sexp)))

  (use-package bats-mode :defer 3)

  (use-package elisp-demos
    :after helpful
    :config
    (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;;;;;; jh-coding > languages > python

  ;; (remove-hook 'python-mode-local-vars-hook 'spacemacs//python-setup-backend)
  ;; (remove-hook 'python-mode-hook 'spacemacs//python-default)

  ;; (progn
  ;;   (defun my/python-setup-anaconda-complete ()
  ;;     (add-hook 'completion-at-point-functions #'anaconda-mode-complete nil t)
  ;;     )
  ;;   (add-hook 'python-mode-hook 'my/python-setup-anaconda-complete))

  ;; pytest.ini
  ;; [pytest]
  ;; markers =
  ;;     task: A concept exercise task.

  ;; Temporary
  ;; (defun spacemacs//bind-python-testing-keys ()
  ;;   "Bind the keys for testing in Python."
  ;;   (spacemacs/declare-prefix-for-mode 'python-ts-mode "mt" "test")
  ;;   (spacemacs/set-leader-keys-for-major-mode 'python-mode
  ;;     "tA" 'spacemacs/python-test-pdb-all
  ;;     "ta" 'spacemacs/python-test-all
  ;;     "tB" 'spacemacs/python-test-pdb-module
  ;;     "tb" 'spacemacs/python-test-module
  ;;     "tl" 'spacemacs/python-test-last
  ;;     "tf" 'spacemacs/python-test-last-failed
  ;;     "tF" 'spacemacs/python-test-pdb-last-failed
  ;;     "tT" 'spacemacs/python-test-pdb-one
  ;;     "tt" 'spacemacs/python-test-one
  ;;     "tM" 'spacemacs/python-test-pdb-module
  ;;     "tm" 'spacemacs/python-test-module
  ;;     "tS" 'spacemacs/python-test-pdb-suite
  ;;     "ts" 'spacemacs/python-test-suite))

  ;; (use-package pytest
  ;;   :init (spacemacs//bind-python-testing-keys)
  ;;   :commands (pytest-one
  ;;              pytest-pdb-one
  ;;              pytest-all
  ;;              pytest-pdb-all
  ;;              pytest-last-failed
  ;;              pytest-pdb-last-failed
  ;;              pytest-module
  ;;              pytest-pdb-module)
  ;;   :config
  ;;   (add-to-list 'pytest-project-root-files "setup.cfg")
  ;;   (add-to-list 'pytest-project-root-files "pytest.ini"))

;;;;;; DONT jh-coding > languages > python | hy-mode

  ;; pipx install hy
  ;; pipx install hyuga
  ;; (use-package hy-mode
  ;;   :mode "\\.hy\\'"
  ;;   :if (not (or *is-windows* my/remote-server IS-TERMUX))
  ;;   :config
  ;;   (add-hook 'hy-mode-hook #'aggressive-indent-mode))

  ;; (use-package ob-hy
  ;;   :if (not (or *is-windows* my/remote-server IS-TERMUX))
  ;;   :config
  ;;   (org-babel-do-load-languages 'org-babel-load-languages
  ;;                                (append org-babel-load-languages '((hy . t)))))

;;;;;; DONT jh-coding > languages > elixir

  ;; (use-package elixir-ts-mode
  ;;   :after treesit eglot
  ;;   :hook
  ;;   (elixir-ts-mode . eglot-ensure)
  ;;   (elixir-ts-mode
  ;;    .
  ;;    (lambda ()
  ;;      (message "=> Turn on elixir-ts-mode")
  ;;      (push '(">=" . ?\u2265) prettify-symbols-alist)
  ;;      (push '("<=" . ?\u2264) prettify-symbols-alist)
  ;;      (push '("!=" . ?\u2260) prettify-symbols-alist)
  ;;      (push '("==" . ?\u2A75) prettify-symbols-alist)
  ;;      (push '("=~" . ?\u2245) prettify-symbols-alist)
  ;;      (push '("<-" . ?\u2190) prettify-symbols-alist)
  ;;      (push '("->" . ?\u2192) prettify-symbols-alist)
  ;;      (push '("<-" . ?\u2190) prettify-symbols-alist)
  ;;      (push '("|>" . ?\u25B7) prettify-symbols-alist)))
  ;;   )

  ;; (use-package exunit)

  ;; (use-package ob-elixir
  ;;   :config
  ;;   (org-babel-do-load-languages 'org-babel-load-languages
  ;;                                (append org-babel-load-languages '((elixir . t)))))

;;;;; jh-coding > document

  ;; 2024-01-31 Python 1.14, NumPy 1.23 pandas 1.5.0, Elixir 1.13

  (use-package devdocs-browser
    :defer 2
    :bind (("M-s-," . devdocs-browser-open) ;; M-s-/ yas-next-field
           ("M-s-." . devdocs-browser-open-in))
    :config
    (add-to-list 'devdocs-browser-major-mode-docs-alist '(js2-mode "javascript" "node"))
    (add-to-list 'devdocs-browser-major-mode-docs-alist '(python-mode "Python" "NumPy" "pandas"))
    (add-to-list 'devdocs-browser-major-mode-docs-alist '(python-ts-mode "Python" "NumPy" "pandas"))
    (add-to-list 'devdocs-browser-major-mode-docs-alist '(elixir-ts-mode "Elixir"))
    ;; (add-to-list 'devdocs-browser-major-mode-docs-alist '(rjsx-mode "react" "javascript" "node"))
    ;; (add-to-list 'devdocs-browser-major-mode-docs-alist '(typescript-ts-mode "typescript"))
    ;; (add-to-list 'devdocs-browser-major-mode-docs-alist '(js-ts-mode "javascript" "node"))
    )

;;;;; DONT jh-coding > treesitter > combobulate

  ;; `M-x combobulate' (default: `C-c o o') to start using Combobulate
  ;; (setq c-ts-mode-indent-offset 4)
  ;; (use-package treesit
  ;;   :ensure nil
  ;;   :preface
  ;;   (defun mp-setup-install-grammars ()
  ;;     "Install Tree-sitter grammars if they are absent."
  ;;     (interactive)
  ;;     (dolist (grammar
  ;;              '((css "https://github.com/tree-sitter/tree-sitter-css")
  ;;                (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
  ;;                (python "https://github.com/tree-sitter/tree-sitter-python")
  ;;                (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
  ;;                (yaml "https://github.com/ikatyang/tree-sitter-yaml")
  ;;                (heex "https://github.com/phoenixframework/tree-sitter-heex")
  ;;                (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
  ;;                (json "https://github.com/tree-sitter/tree-sitter-json")
  ;;                (clojure "https://github.com/sogaiu/tree-sitter-clojure")
  ;;                ))
  ;;       (add-to-list 'treesit-language-source-alist grammar)
  ;;       ;; Only install `grammar' if we don't already have it
  ;;       ;; installed. However, if you want to *update* a grammar then
  ;;       ;; this obviously prevents that from happening.
  ;;       (unless (treesit-language-available-p (car grammar))
  ;;         (treesit-install-language-grammar (car grammar)))))

  ;;   ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;;   ;; distinct from their ordinary counterparts.
  ;;   ;;
  ;;   ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;;   ;; that this does *not* extend to hooks! Make sure you migrate them
  ;;   ;; also
  ;;   (dolist (mapping '(
  ;;                      (python-mode . python-ts-mode)
  ;;                      (css-mode . css-ts-mode)
  ;;                      (typescript-mode . tsx-ts-mode)
  ;;                      (json-mode . json-ts-mode)
  ;;                      (js-mode . js-ts-mode)
  ;;                      (css-mode . css-ts-mode)
  ;;                      (elixir-mode . elixir-ts-mode)
  ;;                      (yaml-mode . yaml-ts-mode)))
  ;;     (add-to-list 'major-mode-remap-alist mapping))
  ;;   :config
  ;;   (mp-setup-install-grammars)
  ;;   ;; Do not forget to customize Combobulate to your liking:
  ;;   ;;  M-x customize-group RET combobulate RET
  ;;   (use-package combobulate
  ;;     :ensure t
  ;;     :preface
  ;;     ;; You can customize Combobulate's key prefix here.
  ;;     ;; Note that you may have to restart Emacs for this to take effect!
  ;;     (setq combobulate-key-prefix "C-c o")

  ;;     ;; Optional, but recommended.
  ;;     ;; You can manually enable Combobulate with `M-x
  ;;     ;; combobulate-mode'.
  ;;     :hook ((python-ts-mode . combobulate-mode)
  ;;            (js-ts-mode . combobulate-mode)
  ;;            (css-ts-mode . combobulate-mode)
  ;;            (yaml-ts-mode . combobulate-mode)
  ;;            (json-ts-mode . combobulate-mode)
  ;;            (typescript-ts-mode . combobulate-mode)
  ;;            (tsx-ts-mode . combobulate-mode))
  ;;     )
  ;;   )

;;;;; DONT jh-coding > treesitter > evil-textobj-tree-sitter

  ;; (use-package evil-textobj-tree-sitter
  ;;   :ensure t
  ;;   :after evil
  ;;   :config
  ;;   ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
  ;;   (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  ;;   ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
  ;;   (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))

  ;;   ;; You can also bind multiple items and we will match the first one we can find
  ;;   (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer"))))

;;;;; jh-coding > practices

  (use-package exercism
    :ensure t
    :if (not (or *is-windows* my/remote-server IS-TERMUX))
    :custom (exercism-display-tests-after-run t)
    :commands exercism
    :config

    (defun ert/eval-and-run-all-tests-in-buffer ()
      "Deletes all loaded tests from the runtime, evaluates the current buffer and runs all loaded tests with ert."
      (interactive)
      (ert-delete-all-tests)
      (eval-buffer)
      (ert 't))
    )

  (use-package leetcode
    :if (not (or *is-windows* my/remote-server IS-TERMUX))
    :defer 5
    :init
    (setq leetcode-prefer-language "python")
    (setq leetcode-prefer-sql "mysql")
    (setq leetcode-save-solutions t)
    (setq leetcode-directory "~/leetcode")
    (setq leetcode-show-problem-by-slug t)
    ;; :config
    ;; (add-hook 'leetcode-solution-mode-hook
    ;;           (lambda() (flycheck-mode -1)))
    )

;;;;; jh-coding > clojure and cider

  (with-eval-after-load 'cider
    (if (package-installed-p 'corfu)
        (evil-define-key 'insert cider-repl-mode-map
          (kbd "C-j") 'corfu-next
          (kbd "C-k") 'corfu-previous))

    ;; copy from corgi
    (add-to-list 'auto-mode-alist '("\\.endl$" . clojure-mode))
    (add-to-list 'magic-mode-alist '("^#![^\n]*/\\(clj\\|clojure\\|bb\\|lumo\\)" . clojure-mode))

    ;; Because of CIDER's insistence to send forms to all linked REPLs, we
    ;; *have* to be able to switch cljc buffer to clj/cljs mode without
    ;; cider complaining.
    ;; (setq clojure-verify-major-mode nil) ; 나중에 해보고
    ;; (setq clojure-indent-style 'align-arguments)

    ;; Vertically align s-expressions
    ;; https://github.com/clojure-emacs/clojure-mode#vertical-alignment
    ;; (setq clojure-align-forms-automatically t)

    ;; manually use on lsp mode
    ;; (remove-hook 'clojure-mode-hook 'spacemacs//clojure-setup-backend)
    (defun my/cider-repl-prompt (namespace)
      "Return a prompt string that mentions NAMESPACE."
      (format "%s🦄 " (cider-abbreviate-ns namespace)))

    ;; NOTE 2022-11-21: for the linter (clj-kondo), refer to the Flymake
    ;; NOTE 2022-11-23: This is not final.  I will iterate on it over
    ;; time as I become more familiar with the requirements.
    (setq cider-repl-result-prefix ";; => "
          cider-eval-result-prefix ""
          cider-connection-message-fn t ; cute, but no!
          cider-repl-prompt-function #'my/cider-repl-prompt
          ;; cider-use-overlays nil ; echo area is fine
          )

    ;; NOTE: formatting with LSP.
    ;; "F" #'cider-format-buffer

    (setq
     cider-prompt-for-symbol nil
     cider-repl-display-help-banner t ;; enable help banner
     ;; cider-print-fn 'puget                   ;; pretty printing with sorted keys / set values
     clojure-align-forms-automatically t
     ;; clojure-toplevel-inside-comment-form t
     ;; cider-result-overlay-position 'at-point   ; results shown right after expression
     ;; cider-overlays-use-font-lock t
     cider-repl-buffer-size-limit 100          ; limit lines shown in REPL buffer
     nrepl-use-ssh-fallback-for-remote-hosts t ; connect via ssh to remote hosts
     cider-preferred-build-tool 'clojure-cli
     )
    )

  (use-package clojure-essential-ref-nov
    :defer 8
    :bind (:map cider-mode-map
                ("M-9" . clojure-essential-ref)
                :map cider-repl-mode-map
                ("M-9" . clojure-essential-ref))
    :init
    (setq clojure-essential-ref-default-browse-fn #'clojure-essential-ref-nov-browse)
    (setq clojure-essential-ref-nov-epub-path "~/git/default/clj-essential-ref-v31.epub")
    )

  (use-package clay :after cider :config (require 'clay))

  (with-eval-after-load 'clojure-mode
    (define-key clojure-mode-map (kbd "<tab>") #'evil-jump-item)
    (define-key clojure-mode-map (kbd "TAB") #'evil-jump-item))

;;;;; jh-coding > quarto

  (use-package  quarto-mode
    :mode (("\\.[qQ][mM][dD]" . poly-quarto-mode)))

;;;;; DONT jh-coding > python conda

  ;; (use-package conda
  ;;   :defer t
  ;;   :config
  ;;   (require 'conda)
  ;;   ;; if you want interactive shell support, include:
  ;;   (conda-env-initialize-interactive-shells)
  ;;   ;; if you want eshell support, include:
  ;;   ;; (conda-env-initialize-eshell)
  ;;   ;; if you want auto-activation (see below for details), include:
  ;;   (conda-env-autoactivate-mode t)
  ;;   ;; if you want to automatically activate a conda environment on the opening of a file:
  ;;   (add-to-list 'find-file-hook (lambda () (when (bound-and-true-p conda-project-env-path)
  ;;                                         (conda-env-activate-for-buffer))))
  ;;   )

;;;; jh-org

;;;;; jh-org > packages

;;;;;;  DONT jupyter / ob-jupyter

  ;; (setq org-confirm-babel-evaluate nil)
  ;; (use-package jupyter
  ;;   :config
  ;;   (require 'ob-jupyter)
  ;;   ;; (org-babel-jupyter-override-src-block "python")
  ;;   (org-babel-do-load-languages 'org-babel-load-languages
  ;;                                (append org-babel-load-languages '((jupyter . t)))))
  ;; (load-file (concat user-dotemacs-dir "lisp/my-python-jupyter.el"))

;;;;;; jh-org > packages > remember

  (use-package remember
    :ensure nil
    :defer 2
    :commands remember
    :config
    (setq remember-data-file (my/org-remember-file))
    (setq remember-notes-initial-major-mode 'org-mode
          remember-notes-auto-save-visited-file-name t))

;;;;;; jh-org > packages > ob-mermaid

  (use-package ob-mermaid
    :after org
    :config
    (org-babel-do-load-languages 'org-babel-load-languages
                                 (append org-babel-load-languages '((mermaid . t)))))

;;;;;; DONT jh-org > packages > ob-d2

  ;; (use-package ob-d2
  ;;   :after org
  ;;   :config
  ;;   (org-babel-do-load-languages 'org-babel-load-languages
  ;;                                (append org-babel-load-languages '((d2 . t)))))

;;;;;; jh-org > packages > orgabilize

  (use-package orgabilize
    :ensure t
    :defer 5
    :config
    (setq orgabilize-org-archive-directory (concat user-org-directory "docs/")))

;;;;;; jh-org > packages > citar

  (use-package citar
    :config
    :hook (org-mode . citar-capf-setup)
    :config
    (setq citar-bibliography config-bibfiles)
    (setq org-cite-global-bibliography config-bibfiles)

    ;; use #+cite_export: csl apa.csl
    (setq org-cite-csl-styles-dir (concat user-org-directory ".csl"))
    (setq citar-citeproc-csl-styles-dir (concat user-org-directory ".csl"))
    (setq citar-citeproc-csl-style "apa.csl") ; ieee.csl
    (setq citar-notes-paths (list (concat org-directory "bib/")))
    (setq org-cite-global-bibliography config-bibfiles)
    (setq org-cite-insert-processor 'citar)
    (setq org-cite-follow-processor 'citar)
    (setq org-cite-activate-processor 'citar)
    (setq citar-symbol-separator " ")

    (setq citar-format-reference-function 'citar-format-reference)

    ;; Managing Bibliographies
    (with-eval-after-load 'bibtex
      (add-hook 'bibtex-mode-hook 'display-line-numbers-mode)
      (setq bibtex-dialect 'biblatex)
      (setq bibtex-align-at-equal-sign t)
      (setq bibtex-text-indentation 20))

    (with-eval-after-load 'savehist
      (add-to-list 'savehist-additional-variables 'citar-history))

    (use-package citar-embark :after embark :config (citar-embark-mode 1))
    ) ;; end-of citar

;;;;;; jh-org > packages > org-glossary

  (use-package org-glossary
    :after org
    :defer 2
    :init
    (setq org-glossary-idle-update-period 1.0) ; 0.5
    (setq org-glossary-autodetect-in-headings t) ; 2024-06-13 new
    ;; :hook (org-mode . org-glossary-mode)
    :config
    (setq org-glossary-collection-root (concat org-directory "dict/"))
    ;; (setq org-glossary-global-terms "global")

    (define-key org-mode-map (kbd "C-}") 'org-glossary-insert-term-reference)
    (define-key org-mode-map (kbd "C-{") 'org-glossary-create-definition)
    (define-key org-mode-map (kbd "C-\"") 'org-glossary-create-definition)
    ;; (setq org-glossary-automatic nil) ;; disable auto-export
    )

;;;;;; jh-org > packages > math-preview

;;;###autoloads
  (defun auto/math-preview-all ()
    "Auto update clock table."
    (interactive)
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (goto-char 0)
        (unless (string-equal (cadar (org-collect-keywords '("NO_MATH_PREVIEW"))) "t")
          (when (re-search-forward "\\$\\|\\\\[([]\\|^[ \t]*\\\\begin{[A-Za-z0-9*]+}" (point-max) t)
            (math-preview-all))))))

  (use-package math-preview
    :after org
    :defer 5
    :commands math-preview-all math-preview-clear-all
    ;; :hook (find-file . (lambda ()
    ;;                        (when (eq major-mode 'org-mode)
    ;;                            (auto/math-preview-all))))
    :config
    ;; (setq math-preview-scale 1.1)
    ;; (setq math-preview-raise 0.2)
    ;; (setq math-preview-margin '(1 . 0))
    (add-to-list 'org-options-keywords "NO_MATH_PREVIEW:"))

;;;;;; jh-org > packages > org-drill

  (use-package org-drill
    :after org
    :defer 5
    :config
    ;; save buffers after drill sessions without prompt.
    (setq org-drill-save-buffers-after-drill-sessions-p nil)
    ;; reduce from the default 30 to make it to become a habit.
    (setq org-drill-maximum-items-per-session 10)
    )


;;;;;; jh-org > packages > org-rainbow-tags

  (use-package org-rainbow-tags
    :after org
    :init
    (setq org-rainbow-tags-hash-start-index 0)
    (setq org-rainbow-tags-extra-face-attributes
          '(:inverse-video t :box nil :weight 'bold))
    ;; :hook (org-mode . org-rainbow-tags-mode)
    )

;;;;;; DONT jh-org > packages > org-ql

  ;; ;; isamert-dotfiles-imdb/emacs/init.el
  ;; (use-package org-ql
  ;;   :after org
  ;;   :defer t
  ;;   ;; Load org-ql-search prematurely to be able to use org-ql blocks in
  ;;   ;; org-mode
  ;;   :init
  ;;   (with-eval-after-load 'org
  ;;     (require 'org-ql-search)))

  ;; ;; Here are some utility functions that I use in org-ql dynamic blocks:

  ;; (defun sort-by-num-prop (prop x y)
  ;;   (< (string-to-number (or (org-element-property prop y) "0"))
  ;;      (string-to-number (or (org-element-property prop x) "0"))))

  ;; (defun sort-by-prop (prop x y)
  ;;   (string< (or (org-element-property prop y) "")
  ;;            (or (org-element-property prop x) "")))

  ;; ;; You have to use ~:sort (lambda ...)~ syntax in org-ql dynamic
  ;; ;; blocks if you want to supply a function for the ~:sort~
  ;; ;; parameter. You can't use a function that returns a lambda, hence
  ;; ;; the functions defined above should be used like this:

  ;; ;; #+begin: org-ql :query ... :sort (lambda (x y) (sort-by-num-prop :RATING x y))
  ;; ;; #+end

  ;; ;; Here are some predefined searches:
  ;; (defun my/org-ql-current-week-tasks ()
  ;;   (interactive)
  ;;   (let ((week-end (im-date "next monday - 1 day")))
  ;;     (org-ql-search org-agenda-files
  ;;       `(and (or (scheduled :to ,week-end)
  ;;                 (deadline :to ,week-end))
  ;;             (not (done)))
  ;;       :title (format "W%s" (format-time-string "%U"))
  ;;       :super-groups '((:auto-category))
  ;;       :sort '(date))))

;;;;;; jh-org > packages > org-bookmarks with link

  (use-package org-bookmarks
    :defer t
    :after org
    :commands (org-bookmarks)
    :init (setq org-bookmarks-file (my/org-links-file))
    ;; :config
    ;; (org-bookmarks-add-org-capture-template t)
    ;; (org-bookmarks-add-org-capture-template)
    )

;;;;;; jh-org > packages > org-sliced-images

  ;; for smooth scroll of images in or mode
  (use-package org-sliced-images
    :after org
    :config (org-sliced-images-mode))

;;;;;; jh-org > packages > org-attach

  (use-package org-attach
    :ensure nil
    :after org
    :commands (org-attach-follow org-attach-complete-link)
    :init
    (org-link-set-parameters "attachment"
                             :follow #'org-attach-follow
                             :complete #'org-attach-complete-link)
    :config
    (setq-default org-attach-id-dir (expand-file-name ".attach/" org-directory)) ; doom default
    (setq org-attach-archive-delete 'nil ; doom nil
          org-attach-method 'cp ; doom 'cp
          org-attach-store-link-p 'attached) ; doom 'attached
    )

;;;;;; jh-org > packages > side-notes

  (use-package side-notes
    :init
    (add-hook 'side-notes-hook #'visual-line-mode) ; Good
    )

;;;;;; jh-org > packages > latex-preview

  ;; LaTeX previews
;;;;;;; org-fragtog

  (use-package org-fragtog
    :after org
    :defer 3
    :custom
    ;; (org-startup-with-latex-preview t) ; nil
    (org-fragtog-preview-delay 0.2)
    :hook
    (org-mode . org-fragtog-mode)
    ;; :config
    ;; (setq org-format-latex-options
    ;;  (plist-put org-format-latex-options :scale 2)
    ;;  (plist-put org-format-latex-options :foreground 'auto)
    ;;  (plist-put org-format-latex-options :background 'auto))
    )

;;;;;;; cdlatex

  (use-package cdlatex
    :bind
    (("C-'" . (lambda () (interactive)
                (cdlatex-ensure-math)
                (cdlatex-math-symbol))))
    :init
    (setq cdlatex-math-symbol-prefix ?\;))
  :config
  (define-minor-mode org-math-mode
    "Some config to write math on `org-mode'."
    :lighter "org-math-mode"
    (org-fragtog-mode 1)
    (org-cdlatex-mode 1)
    (lauremacs-cdlatex-add-math-symbols))

  (defun lauremacs-cdlatex-add-math-symbols ()
    (add-multiple-into-list
     'cdlatex-math-symbol-alist-comb
     '(
       (?.  "\\cdot"   "\\dots")
       (?\; "\\;")
       (?C  ""         "\\mathbb{C}"   "\\arccos")
       (?N  "\\nabla"  "\\mathbb{N}"   "\\exp")
       (?Q  "\\Theta"  "\\mathbb{Q}")
       (?R  "\\Re"     "\\mathbb{R}")
       (?Z  ""         "\\mathbb{Z}")
       )))

;;;;;;; math-symbol-lists

  (use-package math-symbol-lists :after org)

;;;;;;; laas

  (use-package laas
    :hook ((Latex-mode . laas-mode)
           (org-mode . laas-mode)))

;;;;;; jh-org > packages > ox-reveal

  (use-package ox-reveal)

;;;;; jh-org > customize

  (with-eval-after-load 'org
    (message "Org Reloading...")

    ;; load org-funcs.el
    (load-file (concat user-dotemacs-dir "lisp/org-funcs.el"))

    ;; load org-config.el
    (load-file (concat user-dotemacs-dir "lisp/org-config.el"))
    )

;;;;; jh-org > packages > org-journal

  (progn
    (require 'org-journal)

    (defun my-old-carryover (old_carryover)
      (save-excursion
        (let ((matcher (cdr (org-make-tags-matcher org-journal-carryover-items))))
          (dolist (entry (reverse old_carryover))
            (save-restriction
              (narrow-to-region (car entry) (cadr entry))
              (goto-char (point-min))
              (org-scan-tags '(lambda ()
                                (org-todo "DONT")
                                (org-set-tags ":ARCHIVE:"))
                             matcher org--matcher-tags-todo-only))))))

    (setq org-journal-carryover-items  "TODO=\"TODO\"|TODO=\"NEXT\"")
    (setq org-journal-handle-old-carryover-fn 'my-old-carryover)

    (setq org-journal-dir (concat user-org-directory "journal"))
    (setq org-journal-file-format "%Y%m%dT000000--%Y-%m-%d__journal_week%W.org")
    (setq org-journal-date-format "%Y-%m-%d %a") ; Week%W:

    ;; (setq org-journal-date-prefix "#+title: ")
    ;; (setq org-journal-time-prefix "** ") ; default **
    ;; (setq org-journal-time-format "%R ") ; "[%<%Y-%m-%d %a %H:%M>]" ; default "%R "

    (setq org-journal-enable-agenda-integration t) ; default nil
    (setq org-journal-file-type 'weekly) ; default 'daily

    (setq org-journal-tag-alist '(("meet" . ?m) ("dev" . ?d) ("idea" . ?i) ("emacs" . ?e) ("discuss" . ?c) ("1on1" . ?o))) ; default nil
    )

;;;; jh-misc


;;;;; jh-misc > keycast

  (require 'keycast)
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (add-hook 'pre-command-hook 'keycast--update t)
      (remove-hook 'pre-command-hook 'keycast--update)))
  (add-to-list 'global-mode-string '(" " keycast-mode-line " "))

  ;; (setq keycast-mode-line-format "%10s%k%c%r")
  (dolist (input '(self-insert-command
                   org-self-insert-command
                   ))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '(mouse-event-p
                   mouse-movement-p
                   mwheel-scroll
                   handle-select-window
                   mouse-set-point mouse-drag-region
                   dired-next-line ; j
                   dired-previous-line ; k
                   next-line
                   previous-line
                   evil-next-line ; j
                   evil-previous-line ; k
                   evil-forward-char ; l
                   evil-backward-char ; h
                   pixel-scroll-interpolate-up ; <prior> page-up
                   pixel-scroll-interpolate-down ; <next> page-down

                   toggle-input-method
                   block-toggle-input-method
                   evil-formal-state
                   evil-force-normal-state

                   ;; 2023-10-02 Added for clojure-dev
                   lsp-ui-doc--handle-mouse-movement
                   ignore-preserving-kill-region
                   ;; pdf-view-text-region
                   ;; pdf-view-mouse-set-region
                   ;; mouse-set-region
                   ))
    (add-to-list 'keycast-substitute-alist `(,event nil)))

  (add-hook 'spacemacs-post-user-config-hook (lambda ()
                                               ;; (display-time-mode +1)
                                               (doom-modeline-mode +1)
                                               (keycast-mode +1)
                                               ))

;;;;; jh-misc > redacted

  (use-package redacted
    :if (not (or my/remote-server IS-TERMUX))
    :defer (spacemacs/defer)
    :commands (redacted-mode))

;;;;; DONT jh-misc > terminal : term-keys / xclip

  ;; (use-package term-keys
  ;;   :unless window-system
  ;;   :demand
  ;;   :config
  ;;   (term-keys-mode t)
  ;;   )

  ;; (use-package xclip
  ;;   :if (not (or my/remote-server IS-TERMUX))
  ;;   :config
  ;;   (unless (display-graphic-p)
  ;;     (xclip-mode 1)))


;;;;; jh-misc > wakatime-mode

  ;; $ python3 -c "$(wget -q -O - https://raw.githubusercontent.com/wakatime/vim-wakatime/master/scripts/install_cli.py)"
  ;; (use-package wakatime-mode
  ;;     :if (and (or
  ;;                  (string= (system-name) "jhnuc")
  ;;                  (string= (system-name) "junghan-laptop")
  ;;                  )
  ;;             (not my/slow-ssh)
  ;;             (not my/remote-server))
  ;;     :init
  ;;     (add-hook 'prog-mode-hook 'wakatime-mode)
  ;;     (add-hook 'org-mode-hook 'wakatime-mode)
  ;;     (add-hook 'markdown-mode-hook 'wakatime-mode)
  ;;     :defer 5
  ;;     :config
  ;;     (advice-add 'wakatime-init :after (lambda () (setq wakatime-cli-path (expand-file-name "~/.wakatime/wakatime-cli"))))

  ;;     ;; wakatime-api-key  "your-api-key" in permachine.el
  ;;     (defun spacemacs/wakatime-dashboard ()
  ;;         (interactive)
  ;;         (browse-url "https://wakatime.com/dashboard"))
  ;;     (global-wakatime-mode)
  ;;     )


;;;;; DONT jh-misc > engine-mode

  ;; (use-package engine-mode
  ;;   :init
  ;;   (setq engine/browser-function 'eww-browse-url)
  ;;   :config
  ;;   (defengine google
  ;;     "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
  ;;     :keybinding "g")

  ;;   (defengine google-images
  ;;     "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s"
  ;;     :browser 'browse-url-default-browser
  ;;     :keybinding "i")

  ;;   (defengine github
  ;;     "https://github.com/search?ref=simplesearch&q=%s"
  ;;     :browser 'browse-url-default-browser
  ;;     :keybinding "h")

  ;;   (defengine wikipedia
  ;;     "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
  ;;     :keybinding "w")

  ;;   (defengine wiktionary
  ;;     "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s"
  ;;     :keybinding "d")

  ;;   (defengine youtube
  ;;     "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
  ;;     :browser 'browse-url-default-browser
  ;;     :keybinding "v")
  ;;   (add-hook 'spacemacs-post-user-config-hook #'engine-mode))

;;;;; jh-misc > plantuml

  (use-package plantuml-mode
    :commands plantuml-download-jar
    :config
    (setq plantuml-jar-path (concat user-emacs-directory ".cache/plantuml.jar")
          org-plantuml-jar-path plantuml-jar-path))


;;;; jh-pkm


;;;;; jh-pkm > denote/citar/consult-notes

;;;;;; jh-pkm > denote

;;;;;;; jh-pkm > denote > denote

  (use-package denote
    :after org
    :commands (denote denote-create-note denote-insert-link denote-show-backlinks-buffer denote-link-ol-store)
    :init
    (setq denote-directory user-org-directory)
    (require 'denote-silo-extras)
    ;; (require 'denote-journal-extras)
    (require 'denote-org-extras)
    (setq denote-file-type 'org)
    (setq denote-sort-components '(signature title keywords identifier))
    (setq denote-backlinks-show-context t)
    (setq denote-sort-keywords t)
    (setq denote-infer-keywords t)

    (setq denote-excluded-directories-regexp "screenshot")
    ;; (setq denote-rename-buffer-format "Denote: %t (%k)")
    ;; (setq denote-rename-buffer-format "[D]%t") ; prot
    ;; (setq denote-rename-buffer-format "%s %t") ; default

    (setq denote-org-front-matter
          ":PROPERTIES:
:CREATED: %2$s
:END:
#+title:      %1$s
#+filetags:   %3$s
#+date:       %2$s
#+identifier: %4$s
#+export_file_name: %4$s.md
#+description:
#+HUGO_CATEGORIES: Noname
#+filetags:   :fleeting:

# #+hugo_front_matter_key_replace: title>today
# #+begin_src yaml :front_matter_extra t
# title: \"타이틀\"
# #+end_src

#+hugo: more

* Related-Notes
#+print_bibliography:
\n")

    ;; (setq denote-modules '(project xref ffap)) ; Enable integration with Emacs modules
    (setq denote-prompts '(subdirectory title keywords)) ; These are the minimum viable prompts for notes
    (setq denote-date-prompt-use-org-read-date t) ; And `org-read-date' is an amazing bit of tech

    :config
    ;; More functionality
    (setq denote-org-store-link-to-heading nil ; default t
          denote-rename-confirmations nil ; default '(rewrite-front-matter modify-file-name)
          denote-save-buffers t) ; default nil
    (add-hook 'org-mode-hook
              (lambda ()
                ;; (setq denote-rename-buffer-backlinks-indicator " @")
                (setq denote-rename-buffer-format "[D] %t%b")
                (denote-rename-buffer-mode +1)))


    (progn ;; vedangs tips
      (setq denote-silo-extras-directories
            (list (expand-file-name denote-directory)))

      (unless IS-TERMUX
        (add-to-list
         'denote-silo-extras-directories
         (expand-file-name "~/git/jh-blogookpub/org"))
        (add-to-list
         'denote-silo-extras-directories
         (expand-file-name "~/sync/markdown/books"))
        (add-to-list
         'denote-silo-extras-directories (expand-file-name "~/sync/winmacs/org")))

      ;; I use Yasnippet to expand these into a better template.
      (add-to-list 'denote-templates '(reference-note . "reference"))
      (add-to-list 'denote-templates '(morning . "morningpage"))
      (add-to-list 'denote-templates '(emotion . "emotion"))
      (add-to-list 'denote-templates '(insight . "insight"))
      (add-to-list 'denote-templates '(weekly_intentions . "weekint"))
      (add-to-list 'denote-templates '(weekly_report . "weekrpt"))

      (setq
       denote-dired-directories-include-subdirectories t
       denote-dired-directories denote-silo-extras-directories)

      ;; Also check the commands `denote-link-after-creating',
      ;; `denote-link-or-create'.  You may want to bind them to keys as well.

      ;; If you want to have Denote commands available via a right click
      ;; context menu, use the following and then enable
      ;; `context-menu-mode'.
      ;; (add-hook 'context-menu-functions #'denote-context-menu)
      ) ;; end-of progn from vedang's custom

    ;; denote-link-backlinks buffer
    (setq denote-link-backlinks-display-buffer-action
          '((display-buffer-reuse-window display-buffer-in-side-window)
            (side . right)
            (slot . 99)
            (window-width . 0.3)
            (dedicated . t)
            (preserve-size . (t . t))))

;;;;;;; jh-pkm > denote > consult-denote

    (use-package consult-denote
      :bind
      (("C-c 0 h" . consult-org-heading)
       ("C-c 0 f" . consult-denote-find)
       ("C-c 0 g" . consult-denote-grep)
       ("C-x b" . consult-buffer))
      :config (consult-denote-mode t))

;;;;;;; jh-pkm > denote > consult-notes

    (use-package consult-notes
      :defer 2
      :commands (consult-notes consult-notes-search-in-all-notes)
      :init
      (setq consult-notes-denote-display-id t)
      (setq consult-notes-denote-dir t)
      (setq consult-notes-denote-title-margin 2) ; 24
      :config
      ;; (unless IS-TERMUX
      ;;   (setq consult-notes-file-dir-sources
      ;;         '(("Clone-notes"  ?c  "~/nosync/clone-notes/"))))

      ;; '(("Denote"  ?d  "~/org/denotes")
      ;;  ("Fleeting"  ?f  "~/org/denotes/fleeting")
      ;;  ("Literature"  ?l  "~/org/denotes/literature")
      ;;  ("Permanent"  ?p  "~/org/denotes/permanent")
      ;;  ("Personal"  ?e  "~/org/denotes/personal")
      ;;  ("Journal"  ?j  "~/org/denotes/journal")
      ;;  ("Hub"  ?h  "~/org/denotes/hubs"))

      ;; 1) denote
      (progn
        (defun my/consult-notes-denote--display-keywords (keywords)
          (format "%30s" (if keywords (concat "#" (mapconcat 'identity keywords " ")) ""))) ; default 18

        (defun my/consult-notes-denote--display-dir (dirs)
          (format "%10s" (concat "/" dirs))) ; default 18

        (setq consult-notes-denote-display-keywords-function #'my/consult-notes-denote--display-keywords)
        (setq consult-notes-denote-display-dir-function #'my/consult-notes-denote--display-dir)

        (consult-notes-denote-mode 1)

        (defun +consult-notes--unbound-org-roam ()
          (fmakunbound 'consult-notes-org-roam-mode)
          (fmakunbound 'consult-notes-org-roam-find-node-relation))
        (+consult-notes--unbound-org-roam)
        )

      ;; 2) heading
      ;; (setq consult-notes-org-headings-files
      ;;       (list
      ;;        (my/org-inbox-file)
      ;;        (my/org-life-file)
      ;;        (my/org-tasks-file)
      ;;        ;; (my/org-diary-file)
      ;;        (my/org-drill-file)
      ;;        (my/org-quote-file)
      ;;        (my/org-mobile-file)
      ;;        (my/org-contacts-file)
      ;;        (my/org-links-file)))
      ;; (consult-notes-org-headings-mode 1)
      )

;;;;;;; jh-pkm > denote > denote-explore

    ;; 읽어볼 것 https://github.com/pprevos/denote-explore
    (use-package denote-explore)

;;;;;;; jh-pkm > denote > citar-denote

    (use-package citar-denote
      :demand t ;; Ensure minor mode is loaded
      :commands
      (citar-create-note citar-open-notes citar-denote-open citar-denote-add-citekey)
      :init
      (require 'bibtex)
      (require 'citar)
      :hook (find-file . citar-denote-mode)
      :custom
      ;; (citar-open-always-create-notes t)
      ;; (citar-denote-signature t)
      (citar-denote-file-type 'org)
      (citar-denote-subdir t)
      (citar-denote-keyword "bib")
      (citar-denote-title-format "author-year-title") ; default title
      (citar-denote-use-bib-keywords nil)
      (citar-denote-title-format-authors 1)
      (citar-denote-title-format-andstr "and"))

;;;;;;; end-of denote
    ) ;; end-of denote

;;;;;; jh-pkm > denote > custom modules

  (with-eval-after-load 'denote
    (message "Load: custom denote")
    ;; no dependency on org-roam, use default's org-id
    (load-file (concat user-dotemacs-dir "lisp/denote-funcs.el"))
    (load-file (concat user-dotemacs-dir "lisp/denote-config.el"))
    (load-file (concat user-dotemacs-dir "lisp/denote-hugo.el"))
    )

;;;;; jh-pkm > ekg/triples/llm

  ;; (use-package triples)
  ;; (use-package llm)

  ;; (use-package ekg
  ;;   :ensure t
  ;;   :init
  ;;   (setq ekg-db-file (concat user-org-directory "ekg/ekg.db"))
  ;;   :commands (ekg-dispatch ekg-capture ekg-capture-url ekg-show-notes-with-all-tags)
  ;;   :bind (("C-c n u" . ekg-show-notes-with-all-tags)
  ;;          ("C-c n U" . ekg-capture)
  ;;          (:map ekg-notes-mode-map
  ;;                (("<return>" . ekg-notes-open)
  ;;                 ("C-c C-o" . org-open-at-point))))
  ;;   :config
  ;;   (require 'ekg-auto-save)
  ;;   (require 'ekg-embedding)
  ;;   (ekg-embedding-generate-on-save)
  ;;   (require 'ekg-llm)

  ;;   (setq llm-warn-on-nonfree nil)
  ;;   (require 'llm-openai)  ;; The specific provider you are using must be loaded.
  ;;   ;; (require 'llm-gemini)

  ;;   (let ((my-provider (make-llm-openai :key my-openai-api-key)))
  ;;     (setq ekg-llm-provider my-provider
  ;;           ekg-embedding-provider my-provider))

  ;;   ;; (let ((my-provider (make-llm-gemini :key my-gemini-api-key)))
  ;;   ;;   (setq ekg-llm-provider my-provider
  ;;   ;;     ekg-embedding-provider my-provider))

  ;;   ;; (defun ash/capture-literature-note ()
  ;;   ;;   (interactive)
  ;;   ;;   (ekg-capture-url (ash/get-current-url) (ash/get-current-title)))

  ;;   ;; org-mode 를 고집할 필요가 있나?!
  ;;   ;; (setq ekg-capture-default-mode 'markdown-mode) ; default 'org-mode

  ;;   (setq ekg-metadata-separator-text "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  ;;   (setq ekg-display-note-template "%n(id)%n(tagged)%n(titled)%n(text 50)%n(other)")
  ;;   (setq ekg-notes-display-images nil)
  ;;   (setq ekg-inline-custom-tag-completion-symbols
  ;;         '((?@ . "person")
  ;;           (?! . "idea")
  ;;           ;; (?$ . "term")
  ;;           ;; (?% . "doc")
  ;;           ;; (?\& . "project")
  ;;           ))

  ;;   (unless IS-TERMUX
  ;;     ;; gleek-dotfiles-ekg/core/lang/core-org.el:802
  ;;     (defun +ekg-logseq-sync(&rest args)
  ;;       (require 'ekg-logseq)
  ;;       ;; (setq ekg-logseq-dir (concat +ekg-directory "logseq/"))
  ;;       (setq ekg-logseq-dir "~/sync/logseq/logseqfiles/")
  ;;       (ekg-logseq-sync))
  ;;     ;; (add-hook 'ekg-note-save-hook '+ekg-logseq-sync)
  ;;     )

  ;;   ;; /ahyatt-dotfiles/.emacs.d/emacs.org:1098
  ;;   (defun ash/log-to-ekg (text &optional org-mode)
  ;;     "Log TEXT as a note to EKG's date, appending if possible."
  ;;     (let ((notes (ekg-get-notes-with-tags (list (ekg-tag-for-date) "log"))))
  ;;       (if notes
  ;;           (progn
  ;;             (message "ash/log-to-ekg...")
  ;;             (setf (ekg-note-text (car notes)) (concat (ekg-note-text (car notes)) "\n" text))
  ;;             (ekg-save-note (car notes)))
  ;;         (ekg-save-note (ekg-note-create :text text :mode (if org-mode 'org-mode 'text-mode)
  ;;                                         :tags `(,(ekg-tag-for-date) "log"))))))

  ;;   ;; load-transient
  ;;   (setup-ekg-transients) ; only run this once all ekg funcs are loaded
  ;;   )
  ;; end-of ekg

;;;; jh-llm

;;;;; copilot copilot-chat

;;;; llmclient: github copilot

  (use-package copilot
    :commands (copilot-login copilot-diagnose)
    :init
    ;; Sometimes the copilot agent doesn't start. Restarting fixes the issue.
    (setq copilot-indent-offset-warning-disable t
          copilot-max-char 10000) ; default 100000
    :bind (:map copilot-completion-map
                ("C-g" . 'copilot-clear-overlay)
                ("M-p" . 'copilot-previous-completion)
                ("M-n" . 'copilot-next-completion)
                ("<tab>" . 'copilot-accept-completion) ; vscode
                ("TAB" . 'copilot-accept-completion) ; vscode
                ("M-f" . 'copilot-accept-completion-by-word)
                ("M-<return>" . 'copilot-accept-completion-by-line)
                ("M-]" . 'copilot-next-completion) ; vscode
                ("M-[" . 'copilot-next-completion) ; vscode
                ;; ("C-'" . 'copilot-accept-completion)
                ;; ("C-;" . 'copilot-accept-completion)
                )
    ;; :hook ((prog-mode . copilot-mode))
    ;; (org-mode . copilot-mode)
    ;; (markdown-mode . copilot-mode)
    )

;;;; llmclient: github copilot-chat

  ;; (use-package copilot-chat
  ;;     :after request
  ;;     :config
  ;;     (setq copilot-chat-backend 'request)
  ;;     (setq copilot-chat-frontend 'markdown)
  ;;     ;; From https://github.com/chep/copilot-chat.el/issues/24
  ;;     (defun meain/copilot-chat-display (prefix)
  ;;       "Opens the Copilot chat window, adding the current buffer to the context.
  ;; Called with a PREFIX, resets the context buffer list before opening"
  ;;       (interactive "P")

  ;;       (require 'copilot-chat)
  ;;       (let ((buf (current-buffer)))

  ;;         ;; Explicit reset before doing anything, avoid it resetting later on
  ;;         ;; target-fn and ignoring the added buffers
  ;;         (unless (copilot-chat--ready-p)
  ;;           (copilot-chat-reset))

  ;;         (when prefix (copilot-chat--clear-buffers))

  ;;         (copilot-chat--add-buffer buf)
  ;;         (copilot-chat-display)))
  ;;     )

;;;;; openai layer

  (setq openai-key user-openai-api-key) ;; (getenv "OPENAI_API_KEY")
  ;; Also, most requests require setting a user, which is done via:
  (setq openai-user "user")

;;;;; llm-client layer

  (with-eval-after-load 'gptel
    (setq gptel-default-mode 'org-mode)
    (setq gptel-api-key user-openai-api-key)
    (setq gptel-model "gpt-4o") ; "gpt-4o-mini"

    ;; (setq-default gptel-backend
    ;;               ;; :key can be a function that returns the API key.
    ;;               (gptel-make-gemini
    ;;                   "gemini-1.5-pro-latest"
    ;;                 :key (auth-source-pick-first-password :host "gemini")
    ;;                 :stream t))
    )

  ;; (setq gptel-api-key
  ;;     (auth-source-pick-first-password :host "api.openai.com")))

;;;; DONT Flyspell and Jinx-

  ;; (add-hook 'text-mode-hook #'flyspell-mode) ; hangul
  ;; (add-hook 'prog-mode-hook #'flyspell-prog-mode)

  ;; (add-hook 'prog-mode-hook #'jinx-mode) ; english

;;;; Load shared dotfiles

  (load-file (concat user-dotemacs-dir "lisp/uniconfig.el"))
  (load-file (concat user-dotemacs-dir "lisp/keys.el"))
  (load-file (concat user-dotemacs-dir "lisp/hydrakeys.el"))

;;;; Load spacemacs-keys

  (load-file (concat dotspacemacs-directory "spacemacs-keys.el"))

;;;; ccmenu

  ;; (when (display-graphic-p) ;; gui
  ;;   (require 'ccmenu))

;;;; DONT EAF

  ;; (when (locate-library "eaf")
  ;;   (require 'eaf-demo)
  ;;   (require 'eaf-browser)
  ;;   )

;;;; end-of user-config
  ) ;; end-of user-config

;;; End-Of File
