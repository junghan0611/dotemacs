;; -*- no-byte-compile: t; -*-

;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;;(package! this-package
;;  :recipe (:host github :repo "username/repo"
;;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")

;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

;;; doom-unpin-packages

;;; consult - consult-omni consult-gh

;; (package! consult-omni :recipe (:host github :repo "armindarvish/consult-omni" :files (:defaults "sources/*.el")))

(package! consult :pin "03fa8f6b7482eab1dee85d32ab604f0b7312cf82") ; 2.0 stable

;; from agzam
(package! consult-omni :recipe
  (:host github :repo "armindarvish/consult-omni" :branch "develop" :files (:defaults "sources/*.el")) :pin "017ba1b4e13c5515b88f588d22f0bfea83fb9b3e")

(package! consult-gh :recipe (:host github :repo "armindarvish/consult-gh" :files ("*")))

;;; DONT NEVER use built-in on emacs 30

;; (when (eq emacs-major-version 30)
;;  (package! eldoc :built-in t) ; 2025-02-25 with flycheck
;;  )

;;; doom-disabled-packages

(disable-packages!
 ;; yasnippet-capf ; too much information
 ;; lsp-mode
 ;; consult-lsp
 diredfl ; conflict denote
 dirvish
 code-review
 ;; nose ; python module
 ;; lsp-python-ms
 flyspell-lazy
 flymake-popon
 vundo
 undo-fu-session
 elfeed-goodies
 solaire-mode
 ace-window
 flycheck-popup-tip) ; conflict

;; (package! paredit :disable t) ; clojure module
;; (package! flycheck-plantuml :disable t)

(package! emojify :disable t) ; from mastodon

;; (package! corfu-popupinfo :disable t)

;; (package! evil-snipe :disable t)
;; (package! evil-mc :disable t)

;; Disable tty module
(package! evil-terminal-cursor-changer :disable t) ; conflict on kitty
(package! kkp :disable t) ; conflict on term-keys

;;; DONT org-mode for latex-preview-auto-mode

;; cd doom-emacs-dir
;; rm -Rf eln-cache
;; cd .local/straight
;; rm -Rf build-30.0.93/org or build-29.4.50/org
;; rm -Rf repos/org
;; doom sync -u

;; (package! org :recipe
;;   (:host nil :repo "https://git.tecosaur.net/mirrors/org-mode.git" :remote "mirror" :fork
;;    (:host nil :repo "https://git.tecosaur.net/tec/org-mode.git" :branch "dev" :remote "tecosaur")
;;    :files
;;    (:defaults "etc")
;;    :build t :pre-build
;;    (with-temp-file "org-version.el"
;;      (require 'lisp-mnt)
;;      (let
;;          ((version
;;            (with-temp-buffer
;;              (insert-file-contents "lisp/org.el")
;;              (lm-header "version")))
;;           (git-version
;;            (string-trim
;;             (with-temp-buffer
;;               (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
;;               (buffer-string)))))
;;        (insert
;;         (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
;;         (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
;;         "(provide 'org-version)\n"))))
;;   :pin nil)
;; (unpin! org)

;;; additional packages

;;;; :os tty

(package! term-keys :recipe (:host github :repo "junghan0611/term-keys"))

;;;; :ui - visual

;; (package! pulsar)
(package! transpose-frame)
(package! hydra)
(package! pretty-hydra)
(package! major-mode-hydra)

(package! ef-themes)
(package! modus-themes)
(package! doric-themes)

(package! show-font)
;; (package! hammy)

(package! info+)
(package! list-unicode-display)
(package! spacious-padding)

(unpin! doom-themes)
;; https://github.com/junghan0611/doom-themes/commit/9baa0ae4cef9a2b46eea0101964ac0d04c1c31c1
(package! doom-themes :recipe (:host github :repo "junghan0611/doom-themes" :branch "ko"))

(package! keycast)
(package! outli :recipe (:host github :repo "jdtsmith/outli" :files ("*.el")))
(package! fontaine) ; break custom.el
(package! golden-ratio)
(package! mode-minder :recipe (:host github :repo "jdtsmith/mode-minder"))
(package! breadcrumb) ; with eglot
(package! celestial-mode-line)
(package! lin)
;; (package! nerd-icons-dired)
;; (package! nerd-icons-completion) ; 2025-03-26 disable conflict with what?!

(package! dired-preview)

(package! dired+)
;; (package! bookmark+)

;;;; :editor

(package! copy-as-format)
(package! expand-region)
(package! string-inflection)
(package! evil-matchit)
(package! evil-owl) ;; register
(package! tempel)
(package! tempel-collection)
(package! imenu-list :recipe (:host github :repo "junghan0611/imenu-list" :branch "master"))

;; (package! titlecase)
(package! deadgrep)
(package! rg) ; ripgrep
(package! affe)
(package! fzf)
(package! ace-link)
(package! unfill)
(package! translate-mode)
(package! separedit :recipe (:host github :repo "twlz0ne/separedit.el"))
(package! goto-last-change)

;;;; :lang org-mode

(package! org-headline-card :recipe (:host github :repo "yibie/org-headline-card")) ; plantuml
(package! org-todoist :recipe (:host github :repo "lillenne/org-todoist" :branch "main")) ;; (package! todoist)
(package! orgbox)
(package! org-side-tree)

(package! org-fragtog) ;; interactive toggling of inline latex formulas
(package! org-appear)
(package! orgabilize :recipe (:host github :repo "akirak/orgabilize.el"))

(package! org-cv :recipe (:host github :repo "ohyecloudy/org-cv"))

(package! org-glossary :recipe (:host github :repo "tecosaur/org-glossary" :files ("*.el" "*.org" "*.texi")))
(package! autocorrect :recipe (:host github :repo "tecosaur/autocorrect" :files ("*.el" "*.org")))
(package! org-pandoc-import :recipe (:host github :repo "tecosaur/org-pandoc-import" :files ("*.el" "*.org" "filters" "preprocessors")))
(package! org-web-tools)
(package! org-index) ;; 색인 지원
(package! corg :recipe (:host github :repo "isamert/corg.el"))

(package! org-download)
(package! ox-epub)

(package! org-rainbow-tags)
(package! org-drill)

(package! org-rich-yank)
(package! ox-reveal)
(package! org-transclusion)
(package! org-remark)
(package! org-bookmarks)

(package! ox-leanpub) ;; https://github.com/junghan0611/ox-leanpub

(package! ob-mermaid)
;; (package! mermaid-mode)

(package! org-ql)
(package! org-kanban)

(package! org-sliced-images :recipe (:host github :repo "ElleNajt/org-sliced-images"))
;; (package! image-slicing :recipe (:host github :repo "ginqi7/image-slicing"))

(package! parse-csv :recipe (:host github :repo "junghan0611/el-csv")) ; for om-dash
(package! om-dash :recipe (:host github :repo "gavv/om-dash" :files ("*.el" "*.org"))) ; org-based dashboards

(package! org-modern-indent :recipe (:host github :repo "jdtsmith/org-modern-indent"))

;; (package! org-bookmark-heading)
;; (package! d2-mode)
;; (package! ob-d2 :recipe (:host github :repo "dmacvicar/ob-d2"))

;; (package! org-linenote) ; require lsp-mode
;; (package! org-linenote :recipe (:local-repo "local/org-linenote"))
(package! org-linenote :recipe (:host github :repo "junghan0611/org-linenote" :branch "main")) ; eglot
;; (package! language-detection) ; html2org

;;;; :tools

;;;;; :tools biblio

;(package! org-ref :recipe (:files (:defaults "citeproc" (:exclude "*ivy*" "*helm*"))))
;
;;;; :tools writing

(package! hypothesis :recipe (:host github :repo "EFLS/hypothesis"))
(package! side-notes)
(package! redacted)
(package! centered-cursor-mode)

(package! google-translate)
(package! jinx)
(package! logos)
(package! olivetti)
(package! palimpsest)
(package! immersive-translate)
(package! focus)

;; (package! quarto-mode :recipe (:host github :repo "quarto-dev/quarto-emacs" )) ; require polymode
;; (package! quarto-mode :pin "a7b974f7d22ef939eaed8b9919434bcf20b1438f")
(package! ox-quarto :recipe (:host github :repo "jrgant/ox-quarto"))

(package! math-preview)

(package! guess-language :recipe (:host github :repo "junghan0611/guess-language.el" :branch "master" :files ("*.el" "trigrams/*")))
;; (package! txl :recipe (:host github :repo "junghan0611/txl.el" :branch "ko"))
(package! txl :recipe (:local-repo "~/emacs/git/junghan0611/txl.el/"))
;; (package! html2org :recipe (:local-repo "~/emacs/git/default/html2org/"))
;; (package! flymake-vale :recipe (:host github :repo "tpeacock19/flymake-vale"))

;;;; :pkm

;;;;; :pkm denote

(package! denote)
(package! denote-org)
(package! denote-silo)
(package! denote-sequence)
(package! denote-markdown)
;; (package! denote-journal)

(package! denote-explore)
(package! denote-search)

(package! denote-regexp)

(package! consult-notes)

(package! consult-denote)

(package! citar-denote)
(package! citar-org-mode :recipe (:host github :repo "pprevos/citar-org-mode"))
(package! tmr) ; timer

;; (package! ekg)

(package! binder)
(package! astute :recipe (:host github :repo "rnkn/astute"))

(package! ten :recipe (:host sourcehut :repo "nobiot/ten")) ;; https://git.sr.ht/~nobiot/ten
;; (package! chu :recipe (:host sourcehut :repo "nobiot/chu")) ;; https://git.sr.ht/~nobiot/chu
;; (package! obsidian)

;; (package! org-fc
;;   :recipe (:host github
;;            ;; :repo "l3kn/org-fc"
;;            :repo "cashpw/org-fc"
;;            :branch "feat/classes"
;;            :files (:defaults "awk" "demo.org")))
;;;; modules/tools llm

(unpin! gptel)
(package! gptel :recipe (:host github :repo "junghan0611/gptel" :branch "ko"))
(package! mcp)

;; (package! gptel-prompt :recipe (:host github :repo "jwiegley/gptel-prompts"))
;; (package! gptel-rag :recipe (:host github :repo "jwiegley/gptel-rag"))

;; (package! gptel :recipe (:host github :repo "karthink/gptel" :branch "feature-tool-use"))
;; (package! gptel-quick :recipe (:host github :repo "karthink/gptel-quick"))

;;;; extra packages

(package! khoj)

;; (package! minuet) ;; code completion using LLM
(package! aider)

;; (package! aidermacs)
(package! aidermacs :recipe (:host github :repo "MatthewZMD/aidermacs" :files ("*.el")))
(package! emigo :recipe (:host github :repo "MatthewZMD/emigo"))

(package! llm)
(package! semext :recipe (:host github :repo "ahyatt/semext"))

;; (package! kagi)
;; (package! chatgpt-shell)
;; (package! ob-chatgpt-shell)
(package! pcsv)

;; gptel plugins
;; (package! elysium :recipe (:host github :repo "lanceberge/elysium" :branch "main" :files ("*.el")))  ; star 236 using smerge
(package! elysium)
;; (package! evedel :recipe (:host github :repo "daedsidog/evedel")) ; star 87
;; (package! yap :recipe (:host github :repo "meain/yap")) ; star 13

(package! copilot :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))) ; github copilot
(package! copilot-chat :recipe (:host github :repo "chep/copilot-chat.el" :files ("*.el"))) ; github copilot

(package! whisper :recipe (:host github :repo "natrys/whisper.el"))

(package! gpt-babel :recipe (:host github :repo "ElleNajt/gpt-babel" :branch "main" :files ("*.el")))

;; (package! codeium :recipe (:host github :repo "Exafunction/codeium.el"))
;; (package! ellama)
;; (package! openai :recipe (:host github :repo "emacs-openai/openai" :files ("*.el")))
;; (package! dall-e :recipe (:host github :repo "emacs-openai/dall-e"))
;; (package! elisa :recipe (:host github :repo "s-kostyaev/elisa" :files ("*.el" "*.org")))

;; ~/sync/man/dotsamples/doom/lemon-dot-doom/modules/cae/ai/config.el
;; (package! greader)

;; git@github.com:ziova/wolfram.el.git
;; (package! wolfram :recipe (:host github :repo "ziova/wolfram.el"))
;; (package! org-ai)

;;;; :lang

;;;;; TODO latex snippets

;; (package! aas)
(package! laas)
(package! math-symbol-lists)

;;;; Coding

(package! elisp-autofmt)
(package! sideline-blame)
(package! git-messenger)

;; (package! eglot-booster :recipe (:type git :host github :repo "jdtsmith/eglot-booster"))

(package! auto-highlight-symbol)
(package! symbol-overlay)

(when (modulep! :lang clojure)
  (package! clojure-mode-extra-font-locking) ;; better looks
  (package! kaocha-runner) ; Koacha test runner in Emacs
  (package! vega-view)
  (package! clj-deps-new)
  (package! clojure-essential-ref-nov)
  (package! clay))

(package! exercism)
(package! leetcode)

(package! bats-mode)

(unpin! jupyter)
(package! jupyter :recipe (:host github :repo "junghan0611/emacs-jupyter" :branch "ko"))

(when (modulep! :lang python)
  (package! pydoc)
  (package! code-cells))

(package! evil-textobj-tree-sitter)

;; Use the latest available packages for Clojure
;; - cider, clojure-mode
;; (unpin! (:lang clojure))

;; (unpin! conda)
;; (package! conda)

(package! devdocs-browser)
(package! aggressive-indent)

;; (package! bats-mode) ; shell-scripts

(package! hy-mode :recipe (:host github :repo "jethack23/hy-mode"))
(package! ob-hy)

;; (package! geiser)
;; (package! geiser-mit :recipe (:host github :repo "emacsmirror/geiser-mit"))

;;;;; DONT python

;; (package! uv-mode :recipe (:host github :repo "z80dev/uv-mode"))
;; (package! uv-menu :recipe (:host github :repo "pizzatorque/uv-menu"))

;;;;; DONT treesit

;; (package! treesit-auto)
;; (package! clojure-ts-mode :recipe (:host github :repo "clojure-emacs/clojure-ts-mode"))

;;;; Git

(package! git-link :recipe (:host github :repo "sshaw/git-link"))

(package! git-cliff)
(package! gist)
(package! consult-git-log-grep)
(package! magit-todos)
(package! magit-blame-color-by-age :recipe (:host github :repo "jdtsmith/magit-blame-color-by-age"))

;;;; Reading

(package! tp)
(package! mastodon)
;; (package! adoc-mode)

(package! yeetube :recipe (:host github :repo "Boruch-Baum/emacs-yeetube.el"))

(package! elfeed-tube-mpv)
;; (package! elfeed-webkit) ; not working on ubuntu

(package! browser-hist :recipe (:host github :repo "agzam/browser-hist.el"))

(package! subed :recipe (:host github :repo "sachac/subed" :files ("subed/*.el")))
;; (package! dwim-shell-command)
;; (package! bm) ; visible bookmark

(package! hnreader :recipe (:host github :repo "agzam/emacs-hnreader" :branch "major-mode"))
(package! consult-hn :recipe (:host github :repo "agzam/consult-hn"))
(package! reddigg :recipe (:host github :repo "agzam/emacs-reddigg" :branch "major-mode"))

;; filter marked text out
(package! org-marked-text-overview :recipe (:host github :repo "lijigang/org-marked-text-overview"))

(package! docsim)

;;(package! org-books :recipe (:host github :repo "junghan0611/org-books" :branch "ko"))
(package! org-books :recipe (:local-repo "~/emacs/git/junghan0611/org-books"))

;; (package! org-zettel-ref-mode :recipe (:host github :repo "junghan0611/org-zettel-ref-mode" :branch "ko"))
(package! org-zettel-ref-mode :recipe (:local-repo "~/emacs/git/junghan0611/org-zettel-ref-mode/"))

;; (package! org-supertag :recipe (:host github :repo "yibie/org-supertag")) ; require epc
(package! org-supertag :recipe (:local-repo "~/emacs/git/junghan0611/org-supertag/"))

;;;; Workspace

(package! tabgo)

;;;; Misc

(package! command-log-mode)
(package! atomic-chrome)
(package! empv) ;; TODO mpv frontend
(package! djvu)
(package! calibredb :recipe (:files (:defaults (:exclude "*ivy*" "*helm*"))))
(package! nov)
(package! osm) ; OpenStreetMaps
(package! gif-screencast)
(package! lorem-ipsum)
(package! go-translate)
(package! youtube-sub-extractor)
;; (package! jira)

;; (package! ready-player)
;; Very large files mode loads large files in chunks to open ridiculously large files.
;; (package! vlf :recipe (:host github :repo "m00natic/vlfi" :files ("*.el"))
;;   :pin "cc02f2533782d6b9b628cec7e2dcf25b2d05a27c" :disable t)

;; (package! zoxide)
;; (package! telega) ; telegram

;;;; Transient

(package! ccmenu :recipe (:host github :repo "junghan0611/ccmenu"))
;; (package! ccmenu :recipe (:local-repo "local/ccmenu"))

;; (package! casual-suite :recipe (:host github :repo "kickingvegas/casual-suite"))
(package! casual)
(package! recent-rgrep :recipe (:host github :repo "kickingvegas/recent-rgrep"))

(package! p-search :recipe (:host github :repo "zkry/p-search"))

(package! git-grep-transient)

(package! transient-posframe)

;;;; Forked PKGs

(unless IS-TERMUX
  ;; This is an emacs package which supports OWL Manchester Notation https://www.w3.org/TR/owl2-manchester-syntax/
  ;; (package! omn-mode)
  ;; (package! elot :recipe (:host github :repo "junghan0611/elot"  :branch "ko" :files("elot-package/*")))
  ;; "johanwk/elot"
  ;; (package! elot :recipe (:local-repo "local/elot" :branch "ko" :files("elot-package/*")))

  ;; (package! pylookup :recipe (:host github :repo "junghan0611/pylookup"))
  (package! pylookup :recipe (:local-repo "local/pylookup")))

  ;; (package! paw :recipe (:local-repo "local/paw" :branch "ko" :files ("*"))))

  ;; (package! paw :recipe (:host github :repo "junghan0611/paw" :branch "ko" :files ("*")))


;; (package! trekker
;;   :recipe (:host github :repo "junghan0611/trekker" :branch "ko" :files("*.md" "*.el" "*.py")))

;;; Applications
;;;; Calculator

(package! literate-calc-mode)
(package! calctex :recipe (:host github
                           :repo "johnbcoughlin/calctex"
                           :files ("*.el" "calctex/*.el" "calctex-contrib/*.el" "org-calctex/*.el" "vendor")))

;;;; TODO waiting

;; (package! default-text-scale :recipe (:host github :repo "purcell/default-text-scale"))
;; (package! moc :recipe (:host github :repo "positron-solutions/moc"))
;; (package! dslide :recipe (:host github :repo "positron-solutions/dslide"))
;; (package! anddo :recipe (:host github :repo "junghan0611/anddo.el"))

;; A method for blocking access to emacs commands based on time.
;; https://git.sr.ht/~swflint/time-block-command
;; randomly ask yourself a question to collect productivity data. The primary
;; entry point is the macro define-asker. It may be called as follows
;; https://git.sr.ht/~swflint/random-ask

;;;; choi

;; (package! google-this)
(package! webpaste)
;; (package! password-store-menu)

;;;;; misc

(package! fireplace)
(package! snow)
;; (package! oneko-macs :recipe (:host github :repo "ElleNajt/oneko-macs")) ; sudo apt-get install oneko
;; (package! selectric-mode)

(package! wiki-summary :recipe (:host github :repo "rnkn/wiki-summary.el"))

;;;; DONT Emacs Application Framework (EAF)

;; (progn
;;   (defun +eaf-install-deps-for-app(app-dir)
;;     "Install deps from dependencies.json."
;;     (let* ((deps-dict (with-temp-buffer
;;                         (insert-file-contents
;;                          (expand-file-name "dependencies.json" app-dir))
;;                         (json-parse-string (buffer-string))))
;;            (pip-deps (gethash (if IS-LINUX "linux" "darwin")
;;                               (or (gethash "pip" deps-dict)
;;                                   (make-hash-table))))
;;            (vue-install (gethash "vue_install" deps-dict))
;;            (npm-install (gethash "npm_install" deps-dict))
;;            (npm-rebuild (gethash "npm_rebuild" deps-dict)))
;;       (when pip-deps
;;         (dolist (pkg (append pip-deps nil))
;;           (message "%s" (shell-command-to-string (format "pip install %s" pkg)))))
;;       (when vue-install
;;         (let ((default-directory app-dir))
;;           (message "%s" (shell-command-to-string "npm install"))
;;           (message "%s" (shell-command-to-string "npm run build"))))
;;       (when npm-install
;;         (let ((default-directory app-dir))
;;           (message "%s" (shell-command-to-string "npm install"))))
;;       (when npm-rebuild
;;         (let ((default-directory app-dir))
;;           (message "%s" (shell-command-to-string "npm rebuild"))))))

;;   (package! eaf
;;     :recipe (:host github :repo "emacs-eaf/emacs-application-framework"
;;              :files ("*")
;;              :post-build
;;              (shell-command "/usr/bin/python install-eaf.py --install-core-deps"))) ;; use builtin python

;;   (package! eaf-browser
;;     :recipe (:host github :repo "emacs-eaf/eaf-browser"
;;              :files ("*")
;;              :post-build
;;              (+eaf-install-deps-for-app
;;               (concat straight-base-dir "/straight/" straight-build-dir "/eaf-browser"))))

;;   (package! eaf-pdf-viewer
;;     :recipe (:host github :repo "emacs-eaf/eaf-pdf-viewer"
;;              :files ("*")
;;              :post-build
;;              (+eaf-install-deps-for-app
;;               (concat straight-base-dir "/straight/" straight-build-dir "/eaf-pdf-viewer"))))

;;   (package! eaf-mind-elixir
;;     :recipe (:host github :repo "emacs-eaf/eaf-mind-elixir"
;;              :files ("*")
;;              :post-build
;;              (+eaf-install-deps-for-app
;;               (concat straight-base-dir "/straight/" straight-build-dir "/eaf-mind-elixir"))))
;;   )

;;; end-of file
