;;; lisp/uniconfig.el -*- lexical-binding: t; -*-

;;; Commentary:

;; ❶ :: U+2776 ==> 더원싱 태그로 활용
;; ㉽ :: U+327D
;; ㉼ :: U+327C

;; 이맥스 배포판에 호환하는 통합 설정
;; 목적 : 둠 스페이스맥스 등 관계 없이 패키지가 있다면
;; 여기 설정에 따라서 동작하도록 함.

;; 2024-04-20 일단 둠 이맥스 설정을 싹 비우는게 목표

;;; Configs

;;;; backtrace-mode-hook

(add-hook 'backtrace-mode-hook 'display-line-numbers-mode)
(add-hook 'backtrace-mode-hook 'visual-line-mode)

;;;; dabbrev

(progn
  (require 'dabbrev)
  (setq dabbrev-abbrev-char-regexp "[가-힣A-Za-z-_]")
  (setq dabbrev-ignored-buffer-regexps
        '("\\` "
          "\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"
          "\\(?:\\(?:[EG]?\\|GR\\)TAGS\\|e?tags\\|GPATH\\)\\(<[0-9]+>\\)?"))
  (setq dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
  (setq dabbrev-upcase-means-case-search nil)) ; default t
;; (setq dabbrev-check-all-buffers t) ;; default t

(with-eval-after-load 'cape
  ;; /gopar-dotfiles-youtuber/README.org:1371
  (setq cape-dabbrev-min-length 6) ; default 4
  ;; (setq cape-dabbrev-check-other-buffers 'some)
  )

;;;; visual-line-mode

;; /home/junghan/sync/man/dotsamples/vanilla/localauthor-dotfiles-zk/init.el:119
;; (with-current-buffer "*Messages*"
;;   (visual-line-mode))
;; (with-current-buffer "*scratch*"
;;   (visual-line-mode))

(add-hook 'compilation-mode-hook 'visual-line-mode)
;; (add-hook 'fundamental-mode-hook 'visual-line-mode)

;;;; Xref

;; Denote 23.9. Speed up backlinks’ buffer creation?
;; Prefer ripgrep, then ugrep, and fall back to regular grep.

(setq xref-search-program
      (cond
       ((or (executable-find "ripgrep")
            (executable-find "rg"))
        'ripgrep)
       ((executable-find "ugrep")
        'ugrep)
       (t
        'grep)))

;; check below
;; (setq xref-file-name-display 'project-relative)
;; Use completing-read interface instead of definitions buffer (needs xref 1.1.0)
;; (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
;; (setq xref-show-definitions-function #'consult-xref) ;; default xref-show-definitions-buffer

;;;; Hangul Korean

(setq default-input-method "korean-hangul")
(set-language-environment "Korean")
;; (setq default-transient-input-method "TeX") ; C-u set-input-method
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)

(set-selection-coding-system 'utf-8) ;; important
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(set-clipboard-coding-system 'utf-8)

(setq-default line-spacing 3) ; use fontaine

;; (setenv "LANG" "en_US.UTF-8")
;; (setenv "LC_ALL" "en_US.UTF-8")
(setenv "LANG" "ko_KR.UTF-8")
(setenv "LC_ALL" "ko_KR.UTF-8")

;; 날짜 표시를 영어로한다. org mode에서 time stamp 날짜에 영향을 준다.
(setq system-time-locale "C")

(setq
 input-method-verbose-flag nil
 input-method-highlight-flag nil)

;; (global-set-key (kbd "<Alt_R>") 'toggle-input-method)
(global-set-key (kbd "<S-SPC>") 'toggle-input-method)
(global-set-key (kbd "<Hangul>") 'toggle-input-method)
(global-set-key (kbd "<menu>") 'toggle-input-method) ;; caps lock as <menu>
(add-hook 'context-menu-mode-hook '(lambda () (define-key context-menu-mode-map (kbd "<menu>") #'toggle-input-method)))
;; (global-unset-key (kbd "S-SPC"))

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

;;;; Emoji and Fonts with Hangul

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

(progn
  (defun my/set-fonts-scale (
                             ;; default-font-name
                             ;; default-font-height
                             ;; cjk-font-name
                             ;; cjk-font-scale
                             unicode-font-name
                             unicode-font-scale
                             emoji-font-name
                             emoji-font-scale
                             )
    "Helper function to set the default, CJK and Emoji fonts."
    ;; Set the default font
    ;; (when (member default-font-name (font-family-list))
    ;;   (set-face-attribute 'default nil
    ;;                       :family default-font-name
    ;;                       :height default-font-height)
    ;;   (set-frame-font default-font-name nil t))

    ;; Set the CJK font in the default fontset.
    ;; (when (member cjk-font-name (font-family-list))
    ;;   (dolist (script (list 'han 'kana 'cjk-misc))
    ;;     (set-fontset-font t script cjk-font-name)))

    (when (member unicode-font-name (font-family-list))
      (set-fontset-font t 'unicode unicode-font-name nil 'prepend)
      (set-fontset-font t 'mathematical unicode-font-name nil 'prepend)
      (set-fontset-font t 'symbol unicode-font-name nil 'prepend)
      )

    ;; Set the Emoji font in the default fontset.
    (when (member emoji-font-name (font-family-list))
      (set-fontset-font t 'emoji emoji-font-name nil 'prepend))

    ;; Rescale the CJK and emoji fonts.
    (setq face-font-rescale-alist
          `(;; (,(format ".*%s.*" cjk-font-name) . ,cjk-font-scale)
            (,(format ".*%s.*" unicode-font-name) . ,unicode-font-scale)
            (,(format ".*%s.*" emoji-font-name) . ,emoji-font-scale)
            )))

;;;###autoload
  (defun my/emoji-set-font ()
    (interactive)

    (set-fontset-font "fontset-default" 'hangul (font-spec :family (face-attribute 'default :family)))

    (when (display-graphic-p) ; gui
      ;; (set-fontset-font t 'unicode (font-spec :family "Symbola") nil 'prepend) ;; 2024-09-16 테스트 -- 𝑀＜1
      ;; (set-fontset-font t 'mathematical (font-spec :family "Symbola") nil 'prepend) ; best
      ;; ;; https://fonts.google.com/noto/specimen/Noto+Sans+Math
      ;; ;; (set-fontset-font t 'mathematical (font-spec :family "Noto Sans Math") nil 'prepend) ;; 2024-10-26 테스트 DONT
      ;; ;; (set-fontset-font t 'mathematical "DejaVu Math TeX Gyre" nil 'prepend) ; DONT for test
      ;; (set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji") nil)
      ;; (set-fontset-font t 'emoji (font-spec :family "Noto Emoji") nil 'prepend) ; Top - my choice

      ;; Noto Emoji, Noto Color Emoji,
      ;; Different computers might need different scaling factors with the same fonts.
      (my/set-fonts-scale
       "Symbola" 0.90 ; unicode
       "Noto Color Emoji" 0.95)
      )

    (unless (display-graphic-p) ; terminal
      ;; (set-fontset-font t 'unicode (font-spec :family "Symbola") nil 'prepend) ;; 2024-09-16 테스트 -- 𝑀＜1
      (set-fontset-font "fontset-default" 'emoji (font-spec :family "Noto Emoji") nil 'prepend) ; default face
      )

    ;; (set-fontset-font t 'symbol (font-spec :family "Symbola") nil 'prepend)
    ;; (set-fontset-font t 'symbol (font-spec :family "Noto Sans Symbols 2") nil 'prepend)
    ;; (set-fontset-font t 'symbol (font-spec :family "Noto Sans Symbols") nil 'prepend)
    )

  (unless IS-TERMUX
    (my/emoji-set-font)
    (add-hook 'after-setting-font-hook #'my/emoji-set-font))
  )


;;;; DONT jit-lock-defer-time

;; NOTE: setting this to `0' like it was recommended in the article above seems
;; to cause fontification to happen in real time, which can be pretty slow in
;; large buffers. Giving it a delay seems to be better.
;; (setq jit-lock-defer-time 0.05) ;; better
;; (setq jit-lock-defer-time 0) ; important

;; My guess for how big this number should be for my setup. Call
;; `cae-set-jit-lock-chunk-size-to-optimal' on a few different files to get an
;; idea.
;; (setq jit-lock-chunk-size 2500) ; default 1500

;;; functions

;;;; Corfu and electric-Pair and Jump In/Out Parens

;; Linux GUI : <tab> TAB
;; Linux Terminal : TAB
;; Linux GUI : S-<iso-lefttab>
;; Linux Terminal : <backtab>

(progn
;;;###autoload
  (defun jump-out-of-pair ()
    (interactive)
    (let ((found (search-forward-regexp "[])}\"'`*=]" nil t)))
      (when found
        (cond
         ((or (looking-back "\\*\\*" 2)
              (looking-back "``" 2)
              (looking-back "\"\"" 2) ; 2023-10-02 added
              (looking-back "''" 2) (looking-back "==" 2))
          (forward-char))
         (t
          (forward-char 0))))))
  ;; 절대 하지 말것! (global-set-key [remap indent-for-tab-command] #'jump-out-of-pair)

;;;###autoload
  (defun jump-backward-pair ()
    (interactive)
    (let ((found (search-backward-regexp "[])}\"'`*=]" nil t)))
      (when found
        (cond
         ((or (looking-back "\\*\\*" 2)
              (looking-back "``" 2)
              (looking-back "\"\"" 2) ; 2023-10-02 added
              (looking-back "''" 2) (looking-back "==" 2))
          (backward-char))
         (t
          (backward-char 0)))))))



;;;; my/backward-kill-word-or-region

;; no kill-ring propagate

;;;###autoload
(defun my/delete-backward-word (arg)
  "Like `backward-kill-word', but doesn't affect the kill-ring."
  (interactive "p")
  (let ((kill-ring nil) (kill-ring-yank-pointer nil))
    (ignore-errors (backward-kill-word arg))))

;;;###autoload
(defun my/backward-delete-word-or-region (&optional arg)
  "Calls `delete-region' when a region is active and doesn't affect the kill-ring"
  (interactive "p")
  (if (region-active-p)
      (call-interactively #'delete-region)
    (my/delete-backward-word arg)))

;;;; my/write-window-setup

;; Writing Window Setup on Dired
;; 괜찮다. 화면 버퍼 구성이 여러모로 집중하기 좋다.
(defun my/write-window-setup ()
  (interactive)
  (split-window-right)
  (windmove-right)
  (split-window-below)
  (windmove-left)
  (find-file "*draft.org" t)
  (windmove-right)
  (find-file "*notes.txt" t) ; txt
  (windmove-left))
(with-eval-after-load 'dired
  (define-key dired-mode-map [f3] #'my/write-window-setup))


;;;; my/convert-hangul-jamo-to-syllable

(progn
  (require 'ucs-normalize)

  (defun my/convert-hangul-jamo-to-syllable ()
    "Convert conjoining jamo to precomposed syllables in the current buffer."
    (interactive)
    (let* ((choseong "[\u1100-\u115F\uA960-\uA97C]")
           (jungseong "[\u1160-\u11A7\uD7B0-\uD7C6]")
           (jongseong "[\u11A8-\u11FF\uD7CB-\uD7FB]?")
           (pattern (concat choseong jungseong jongseong)))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward pattern nil t)
          (let ((start (match-beginning 0))
                (end (match-end 0)))
            (ucs-normalize-NFC-region start end))))))

  ;; 버퍼 전체에 적용하려면 다음 함수를 호출하세요:
  ;; (my/convert-hangul-jamo-to-syllable)

  (defun my/process-files-by-extension (directory extension process-func)
    "주어진 디렉토리에서 특정 확장자를 가진 파일들에 대해 처리 함수를 적용합니다."
    (interactive
     (list (read-directory-name "처리할 디렉토리: ")
           (read-string "파일 확장자 (예: txt): ")
           (intern (completing-read "처리 함수: " obarray 'functionp t))))
    (dolist (file (directory-files-recursively directory (concat "\\." extension "$")))
      (with-current-buffer (find-file-noselect file)
        (funcall process-func)
        (save-buffer)
        (kill-buffer)))))


;;;; my/replace-latex-delimiters-with-dollar

(defun my/replace-latex-delimiters-with-dollar ()
  "현재 버퍼에서 \\[와 \\]를 $로 바꿉니다."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\\[" nil t)
    (replace-match "$$" nil nil))
  (goto-char (point-min))
  (while (search-forward "\\]" nil t)
    (replace-match "$$" nil nil)))

;;; kmacro

;;;; mirror-buffer
(defalias 'ash/mirror-buffer
  (kmacro "C-x 1 C-x 3 C-x o"))
;; (general-define-key "s-'" 'ash/mirror-buffer)

;;; Utils
;;;; text-font-scale-up

(defun my/text-font-scale-up ()
  (interactive)
  (text-scale-increase 1))
(defun my/text-font-scale-down ()
  (interactive)
  (text-scale-decrease 1))

;; TODO Evil Keymaap
;; (global-set-key (kbd "C-=") 'my/text-font-scale-up)
;; (global-set-key (kbd "C--") 'my/text-font-scale-down)
;;;; Make Script Files Executable Automatically

;; Make script files (with shebang like #!/bin/bash, #!/bin/sh) executable automatically. See this blog post from Emacs Redux.
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;;; clear-kill-ring

(defun clear-kill-ring ()
  "Clear the results on the kill ring."
  (interactive) (setq kill-ring nil))
(global-set-key (kbd "M-Y") 'clear-kill-ring)

;;;; disable enable easy ui

(defun disable-easy-ui ()
  (interactive)
  (setq use-dialog-box nil)

  ;; (blink-cursor-mode nil)

  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'tooltip-mode)
    (tooltip-mode -1))
  (menu-bar-mode -1))

;; On Emacs without X, tool-bar-mode and scroll-bar-mode are not defined.
(defun enable-easy-ui ()
  (interactive)
  (setq tool-bar-style 'image)
  (setq use-dialog-box t) ; default t
  (blink-cursor-mode t)
  (when (fboundp 'tooltip-mode)
    (tooltip-mode 1))
  (menu-bar-mode -1)
  (menu-bar-mode 1))
;; (when (fboundp 'tool-bar-mode)
;;   (tool-bar-mode 1))


;;;; goto-addr

;; Actionable URLs in Emacs buffers via [[http://xenodium.com/#actionable-urls-in-emacs-buffers][Álvaro Ramírez]].

(progn
  (require 'goto-addr)
  ;; :hook
  ;; ((compilation-mode . goto-address-mode)
  ;;  (prog-mode . goto-address-prog-mode)
  ;;  (eshell-mode . goto-address-mode)
  ;;  (shell-mode . goto-address-mode))
  ;; :bind (:map goto-address-highlight-keymap ("C-c C-o" . goto-address-at-point))
  ;; :config
  (global-goto-address-mode +1))


;;;; Eldoc

(progn
  (require 'eldoc)
  (setq eldoc-idle-delay 0)
  (setq eldoc-echo-area-use-multiline-p nil) ;  important - default 'truncate-sym-name-if-fit
  (setq eldoc-echo-area-display-truncation-message nil)
  (setq eldoc-echo-area-prefer-doc-buffer t)) ; default nil - alway show echo-area

;; eldoc-display-functions '(eldoc-display-in-echo-area eldoc-display-in-buffer)


;;;###autoload
(defun eldoc-toggle ()
  "Toggle eldoc's documentation buffer."
  (interactive)
  (let ((buffer (eldoc-doc-buffer)))
    (if-let (w (and buffer (get-buffer-window buffer)))
        (delete-window w)
      (eldoc-doc-buffer t))))

;;;; Run commands in a popup frame

;; i3config : bindsym $mod+$wm.binding.orgcapture exec --no-startup-id emacsclient -e '(progn (select-frame-set-input-focus (selected-frame)) (prot-window-popup-org-capture))'
;; myai.sh : emacsclient -e '(progn (select-frame-set-input-focus (selected-frame)) (ad/ai-from-anywhere))'

;;;;; prot's emacs-command-popup-frame

;; https://protesilaos.com/codelog/2024-09-19-emacs-command-popup-frame-emacsclient/

;; The emacsclient call depends on the daemon or `server-mode' (I use the latter)
;; The emacsclient calls that need ot be bound to system-wide keys
;; emacsclient -e '(prot-window-popup-org-capture)'
;; emacsclient -e '(prot-window-popup-tmr)'

(defun prot-window-delete-popup-frame (&rest _)
  "Kill selected selected frame if it has parameter `prot-window-popup-frame'.
Use this function via a hook."
  (when (frame-parameter nil 'prot-window-popup-frame)
    (delete-frame)))

(defmacro prot-window-define-with-popup-frame (command)
  "Define interactive function which calls COMMAND in a new frame.
Make the new frame have the `prot-window-popup-frame' parameter."
  `(defun ,(intern (format "prot-window-popup-%s" command)) ()
     ,(format "Run `%s' in a popup frame with `prot-window-popup-frame' parameter.
Also see `prot-window-delete-popup-frame'." command)
     (interactive)
     (let ((frame (make-frame '((prot-window-popup-frame . t)))))
       (select-frame frame)
       (switch-to-buffer " prot-window-hidden-buffer-for-popup-frame")
       (condition-case nil
           (call-interactively ',command)
         ((quit error user-error)
          (delete-frame frame))))))

(declare-function org-capture "org-capture" (&optional goto keys))
(defvar org-capture-after-finalize-hook)

;;;###autoload (autoload 'prot-window-popup-org-capture "prot-window")
(prot-window-define-with-popup-frame org-capture)

(add-hook 'org-capture-after-finalize-hook #'prot-window-delete-popup-frame)

(when (locate-library "tmr")
  (require 'tmr)
  (declare-function tmr "tmr" (time &optional description acknowledgep))
  (defvar tmr-timer-created-functions)

;;;###autoload (autoload 'prot-window-popup-tmr "prot-window")
  (prot-window-define-with-popup-frame tmr)
  (add-hook 'tmr-timer-created-functions #'prot-window-delete-popup-frame))

;;;;; gptel: ai-from-anywhare

;; https://www.armindarvish.com/post/use_emacs_as_a_chatgpt_client/
(when (locate-library "gptel")

;;;###autoload
  (defun my/ai-from-anywhere ()
    (interactive)
    (let* ((screen-width (display-pixel-width))
           (screen-height (display-pixel-height))
           (frame-width (/ screen-width 3))
           (frame-height screen-height)
           (frame-left (- screen-width frame-width))
           (frame-top 0)
           (chat-frame (make-frame `((window-system . x) ; macOS use "ns"
                                     (top . ,frame-top)
                                     (left . ,frame-left)
                                     (width . (text-pixels . ,frame-width))
                                     (heigth . (text-pixels . ,frame-height))
                                     (minibuffer . t)))))

      (select-frame chat-frame))

    (gptel "My:AI Chat" gptel-api-key nil)
    (switch-to-buffer "My:AI Chat")
    (delete-other-windows))
  )


;;;;; use gptel citations utf8

(defun my/url-decode-to-korean (url)
  "URL을 디코딩하고 한글로 표시합니다."
  (decode-coding-string (url-unhex-string url t) 'utf-8))

;; (when (locate-library "gptel")
;;   (with-eval-after-load 'gptel
;;     (require 'url)
;;     (fmakunbound 'gptel--perplexity-parse-citations)
;;     (defun gptel--perplexity-parse-citations (citations)
;;       (let ((counter 0))
;;         (concat "\n\nCitations:\n"
;;                 (mapconcat (lambda (url)
;;                              (setq counter (1+ counter))
;;                              (format "- [%d] %s" counter
;;                                      (decode-coding-string (url-unhex-string url t) 'utf-8)
;;                                      ;; (url-decode-to-korean url)
;;                                      ))
;;                            citations "\n"))))
;;     )
;;   )

;;; Packages with functions
;;;; consult - swiper style check wiki

;; https://github.com/minad/consult/wiki
;; (progn
;;   (defcustom my/consult-ripgrep-or-line-limit 300000
;;     "Buffer size threshold for `my/consult-ripgrep-or-line'.
;; When the number of characters in a buffer exceeds this threshold,
;; `consult-ripgrep' will be used instead of `consult-line'."
;;     :type 'integer)

;;   (defun my/consult-ripgrep-or-line ()
;;     "Call `consult-line' for small buffers or `consult-ripgrep' for large files."
;;     (interactive)
;;     (if (or (not buffer-file-name)
;;             (buffer-narrowed-p)
;;             (ignore-errors
;;               (file-remote-p buffer-file-name))
;;             (jka-compr-get-compression-info buffer-file-name)
;;             (<= (buffer-size)
;;                 (/ my/consult-ripgrep-or-line-limit
;;                    (if (eq major-mode 'org-mode) 4 1))))
;;         (consult-line)
;;       (when (file-writable-p buffer-file-name)
;;         (save-buffer))
;;       (let ((consult-ripgrep-args
;;              (concat consult-ripgrep-args
;;                      ;; filter to desired filename
;;                      " -g "
;;                      (shell-quote-argument (file-name-nondirectory buffer-file-name))
;;                      " ")))
;;         (consult-ripgrep))))
;;   )


;;;; consult-denote and howmish

;; (let ((tab (make-hash-table :test 'equal)))
;;   (puthash "Foo" "foo.org" tab)
;;   (gethash (completing-read "Yes: "  tab) tab))

(when (locate-library "consult-denote")
  (progn
    ;; https://systemcrafters.net/live-streams/march-7-2025/
    ;; - Searching for notes in most-recent-edit order
    ;; - Searching for notes from today, yesterday, specific date
    ;; - Following search query links

    ;; (consult-denote-mode +1)

    (defun my/denote-howmish-find-file ()
      (declare (interactive-only t))
      (interactive)
      (let* ((sorted-files (sort (mapcar (lambda (file)
                                           (cons (file-attribute-modification-time
                                                  (file-attributes file))
                                                 file))
                                         (denote-directory-files))
                                 (lambda (left right)
                                   (not
                                    (time-less-p (car left)
                                                 (car right))))))
             (table (make-hash-table :test 'equal))
             (options
              (mapcar (lambda (file)
                        (puthash (denote-retrieve-title-or-filename (cdr file) 'org)
                                 (cdr file)
                                 table))
                      sorted-files))
             (result
              (consult--read table
                             :prompt "Note: "
                             :sort nil
                             :require-match t
                             :add-history (thing-at-point 'filename)
                             :state (consult--file-preview)
                             :category 'file
                             :history '(:input consult--find-history))))
        (when result
          (find-file (gethash result table)))))
    )
  )

;;;; DONT fontaine

;; ;; This is defined in Emacs C code: it belongs to font settings.
;; (setq x-underline-at-descent-line t)
;; ;; And this is for Emacs28.
;; (setq-default text-scale-remap-header-line t)

;; Weights :: Thin ExtraLight Light Regular Medium SemiBold Bold ExtraBold Heavy
;; Slopes :: Upright Oblique Italic
;; Width :: Normal Extended

;; (when (locate-library "fontaine")
;;   (when (display-graphic-p) ; gui
;;     ;; (setq fontaine-latest-state-file
;;     ;;       (locate-user-emacs-file "fontaine-latest-state.eld"))
;;     (setq fontaine-presets
;;           ;; 80 120, 136, 151, 180, 211 ; sarasa mono / term
;;           ;; 120, 140, 170, 190, 210, 230 ; monoplex kr nerd
;;           '(
;;             (small12 :default-height 120)
;;             (regular :default-height 140)
;;             (regular14 :default-height 140)
;;             (regular17 :default-height 170)
;;             (logosfocus :default-height 170)
;;             (large19 :default-height 190)
;;             (large21 :default-height 210)
;;             (present23
;;              :default-height 230
;;              ;; :fixed-pitch-family "Sarasa Term Slab K"
;;              ;; :fixed-pitch-serif-family "Sarasa Term Slab K"
;;              :bold-weight extrabold)
;;             (t
;;              ;; Following Prot’s example, keeping these for for didactic purposes.
;;              :line-spacing 3
;;              ;; :default-family "Sarasa Term K Nerd Font"
;;              ;; :default-height 151
;;              :default-family "Monoplex Nerd"
;;              :default-height 140
;;              :default-weight regular
;;              ;; :fixed-pitch-family "Sarasa Term K Nerd Font"
;;              ;; :fixed-pitch-height 151
;;              ;; :fixed-pitch-weight nil
;;              ;; :fixed-piath-serif-family nil
;;              ;; :fixed-pitch-serif-weight nil
;;              ;; :fixed-pitch-serif-height nil
;;              :variable-pitch-family "Pretendard Variable"
;;              ;; :variable-pitch-height 1.0
;;              ;; :variable-pitch-family nil
;;              ;; :variable-pitch-weight nil
;;              :bold-family nil
;;              :bold-weight bold
;;              ;; :bold-width extended
;;              :italic-family nil
;;              :italic-slant italic)))

;;     ;; (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular14))
;;     (fontaine-set-preset 'regular)
;;     (set-fontset-font "fontset-default" 'hangul (font-spec :family (face-attribute 'default :family))))
;;   ;; Persist the latest font preset when closing/starting Emacs and while switching between themes.
;;   ;; (fontaine-mode 1)
;;   )

;;;; Ten with etags

;; gavinok-dotfiles/init.el
;; Getting added in emacs 30 https://debbugs.gnu.org/cgi/bugreport.cgi?bug=67687
;; (use-package etags-regen
;;   :when (executable-find "etags")
;;   :custom (etags-regen-tags-file "/tmp/TAGS")
;;   :commands etags-regen-mode
;;   :bind (("C-c t" . complete-tag)
;;          ("C-c M-." . my/goto-etags))
;;   :init
;;   (defvar etags-regen-mode-map (make-sparse-keymap))
;;   (add-to-list 'minor-mode-map-alist (cons 'etags-regen-mode etags-regen-mode-map)))

;; (defun my/goto-etags ()
;;   (interactive)
;;   (let ((xref-backend-functions '(etags--xref-backend t)))
;;     (call-interactively 'xref-find-definitions)))

;; ;; eww-mode nov-mode -- conflict face 켜면 안된다.

(when (locate-library "ten")
  (require 'ten)
  (setq ten-glossary-file-extensions '("org" "md" "txt"))
  (setq ten-glossary-exclude-regexps '("/\\."))
  (setq ten-tags-file-default user-ten-tags-file)
  ;;   ;; :bind (("M-c t" . complete-tag)
  ;;   ;;        ("C-c M-." . my/goto-etags))
  ;;   ;; :hook ((org-mode Info-mode) . ten-font-lock-mode) ;; text-mode
  (with-eval-after-load 'consult
    (require 'consult-ten)
    (add-to-list 'consult-buffer-sources 'consult-ten-glossary 'append) ; g
    )
  )

;;;; markdown-mode

;; (when (locate-library "markdown-mode")
;;   (progn
;;     ;; https://www.reddit.com/r/emacs/comments/10h9jf0/beautify_markdown_on_emacs/

;;     (defvar nb/current-line '(0 . 0)
;;       "(start . end) of current line in current buffer")
;;     (make-variable-buffer-local 'nb/current-line)

;;     (defun nb/unhide-current-line (limit)
;;       "Font-lock function"
;;       (let ((start (max (point) (car nb/current-line)))
;;             (end (min limit (cdr nb/current-line))))
;;         (when (< start end)
;;           (remove-text-properties start end
;;                                   '(invisible t display "" composition ""))
;;           (goto-char limit)
;;           t)))

;;     (defun nb/refontify-on-linemove ()
;;       "Post-command-hook"
;;       (let* ((start (line-beginning-position))
;;              (end (line-beginning-position 2))
;;              (needs-update (not (equal start (car nb/current-line)))))
;;         (setq nb/current-line (cons start end))
;;         (when needs-update
;;           (font-lock-fontify-block 3))))

;;     (defun nb/markdown-unhighlight ()
;;       "Enable markdown concealling"
;;       (interactive)
;;       (markdown-toggle-markup-hiding 'toggle)
;;       (font-lock-add-keywords nil '((nb/unhide-current-line)) t)
;;       (add-hook 'post-command-hook #'nb/refontify-on-linemove nil t))

;;     (add-hook 'markdown-mode-hook #'nb/markdown-unhighlight)
;;     )
;;   )

;;;; org-rainbow-tags

(when (locate-library "org-rainbow-tags")
  (with-eval-after-load 'org
    (require 'org-rainbow-tags)
    (setq org-rainbow-tags-hash-start-index 0)
    (setq org-rainbow-tags-extra-face-attributes
          '(:inverse-video t :box nil :weight 'bold))
    (add-hook 'org-mode-hook #'org-rainbow-tags-mode)))

;;;; tab-width for org-mode and org-journal-mode

(when (locate-library "org-journal")
  (add-hook 'org-mode-hook (lambda () (setq-local tab-width 8)))
  (add-hook 'org-journal-mode-hook (lambda () (setq-local tab-width 8)))
  )

;;;; goto-last-change

(when (locate-library "goto-last-change")
  (defun my/goto-last-change ()
    (interactive)
    (outline-show-all) ; 전체를 펼치고 찾아라!
    (goto-last-change))
  (global-set-key (kbd "C-x ,") 'my/goto-last-change))

;;;; side-notes

(when (locate-library "side-notes")
  (require 'side-notes)
  (setq side-notes-display-alist '((side . right)
                                   (window-width . 84)))
  (add-hook 'side-notes-hook #'visual-line-mode)
  )

;;;; custom citar-org

(when (locate-library "citar")

  (with-eval-after-load 'citar

;;;;; citar-templates
    (setq citar-templates
          '((main
             .
             ;; [${urldate:10}]
             "'${dateadded:10} ${author editor:19} ${title:49}  ${date year issued:4} ${translator:7} ${=key= id:17}")  ; 2024-09-12 김정한
            (suffix
             . "#${datemodified:10} ${=type=:10} ${shorttitle:19} ${namea:16} ${url:19} ${tags keywords:*}") ; 2024-11-17 add url
            (preview
             .
             "${title}\n- ${shorttitle}\n- ${author} ${translator} ${namea}\n- ${abstract}\n- ${year issued date:4}") ; citar-copy-reference
            (note . "#+title: ${author translator:10}, ${title}")))

    ;; (note . "Notes on ${author:10 editor:%etal}, ${title}")

;;;;; my/citar-org-to-reading-list

    (progn
      (require 'ol-bibtex)

      ;; use embark with at-point
      ;; (setq citar-at-point-function 'embark-act) ; citar-dwim
      ;; add beref entry for bookends
      ;; (setq citar-additional-fields '("url"))

      (defun my/citar-org-to-reading-list (citekeys)
        "Insert bibliographic entry associated with the CITEKEYS."
        (interactive (list (citar-select-refs)))
        (dolist (citekey citekeys)
          (my/citar--org-to-reading-list
           citekey)))

      (defun my/citar--org-to-reading-list (citekey)
        "Insert the bibtex entry for CITEKEY at point."

        (let* ((key citekey)
               (reading-list-file (my/org-reading-file)))
          (when key
            (with-temp-buffer
              ;; BibTeX 엔트리를 버퍼에 삽입
              (citar--insert-bibtex citekey)
              ;; org-bibtex 형식으로 변환
              (org-bibtex-read)
              ;; 변환된 내용을 reading list 파일에 추가
              (with-current-buffer (find-file-noselect reading-list-file)
                (goto-char (point-max))
                (insert "\n")
                (org-bibtex-write)
                (insert "\n")
                (insert (format "[cite:@%s]" citekey))
                (insert "\n")

                (save-buffer)))
            (message "Entry added to reading list: %s" key))))
      )

;;;;; citar-indicator icons

    ;; (progn
    ;;   (defvar citar-indicator-files-icons
    ;;     (citar-indicator-create
    ;;      :symbol (nerd-icons-faicon
    ;;               "nf-fa-file_o"
    ;;               :face 'nerd-icons-green
    ;;               :v-adjust 0.01)
    ;;      :function #'citar-has-files
    ;;      :padding "  " ; need this because the default padding is too low for these icons
    ;;      :tag "has:files"))
    ;;   (defvar citar-indicator-links-icons
    ;;     (citar-indicator-create
    ;;      :symbol (nerd-icons-faicon
    ;;               "nf-fa-link"
    ;;               :face 'nerd-icons-orange
    ;;               :v-adjust 0.0)
    ;;      :function #'citar-has-links
    ;;      :padding "  "
    ;;      :tag "has:links"))
    ;;   (defvar citar-indicator-notes-icons
    ;;     (citar-indicator-create
    ;;      :symbol (nerd-icons-codicon
    ;;               "nf-cod-note"
    ;;               :face 'nerd-icons-blue
    ;;               :v-adjust 0.0)
    ;;      :function #'citar-has-notes
    ;;      :padding "    "
    ;;      :tag "has:notes"))
    ;;   (defvar citar-indicator-cited-icons
    ;;     (citar-indicator-create
    ;;      :symbol (nerd-icons-faicon
    ;;               "nf-fa-circle_o"
    ;;               :face 'nerd-icon-green
    ;;               :v-adjust 0.0)
    ;;      :function #'citar-is-cited
    ;;      :padding "  "
    ;;      :tag "is:cited"))
    ;;   (setq citar-indicators
    ;;         (list citar-indicator-files-icons
    ;;               citar-indicator-links-icons
    ;;               citar-indicator-notes-icons
    ;;               citar-indicator-cited-icons)))

    )
  )

;;;; ox-hugo

;; 2024-04-26 move here, Important
(with-eval-after-load 'org
  (require 'ox-hugo)
  ;; (setq org-hugo-base-dir (file-truename "~/git/blog/"))
  (setq org-hugo-base-dir user-hugo-notes-dir) ;; 2024-10-07 fix quartz

  (progn
    ;; Append and update time-stamps for
    ;; #+hugo_lastmod: Time-stamp: <>
    ;; org-hugo-auto-set-lastmod should be nil
    (require 'time-stamp)
    ;; (add-hook 'write-file-functions 'time-stamp)
    ;; M-x time-stamp
    ;; Update last modified date for ox-hugo export
    ;; (add-hook 'before-save-hook 'time-stamp)
    (setq time-stamp-active t
          time-stamp-start "#\\+hugo_lastmod:[ \t]*"
          time-stamp-end "$"
          time-stamp-format "\[%Y-%m-%d\]"))


  (setq org-hugo-front-matter-format 'yaml)

  ;; My blog is created using Hugo and ox-hugo. It generates better markdown than what you would get using org-md-export!
  ;; It works well out-of-the-box. However, extra configuration is required to embed video.
  ;; In ox-hugo, uses #+begin_video to generate the <video> HTML5 tag (details in ox-hugo/issues/274).
  ;; In Hugo config, set markup.goldmark.renderer.unsafe to true (details in discourse.gohugo.io).
  (add-to-list 'org-hugo-external-file-extensions-allowed-for-copying "webm")

  (setq org-hugo-section "notes") ; 2024-04-26 change
  (setq org-hugo-paired-shortcodes
        "mermaid callout cards details tabs") ; hint sidenote

  ;; https://ox-hugo.scripter.co/doc/formatting/
  ;; if org-hugo-use-code-for-kbd is non-nil
  ;; Requires CSS to render the <kbd> tag as something special.
  ;; eg: ~kbd~
  ;; (setq org-hugo-use-code-for-kbd t)

  ;; https://ox-hugo.scripter.co/doc/linking-numbered-elements/

  ;; org-export-dictionary 에 Figure, Table 에 한글 번역을 넣으면
  ;; 한글로 바뀌어 export 될 것이다.
  (setq org-hugo-link-desc-insert-type t)

  ;; 내보낼 때는 fill-column 끈다.
  (setq org-hugo-preserve-filling nil) ; important

  (setq org-hugo-allow-spaces-in-tags t) ; default t
  (setq org-hugo-prefer-hyphen-in-tags t) ; default t

  ;; Assume all static files are images for now otherwise this
  ;; defaults to /ox-hugo/mypicture.png which is ugly
  (setq org-hugo-default-static-subdirectory-for-externals "images") ; imgs
  ;; (setq org-hugo-default-static-subdirectory-for-externals "~/git/temp/notes.junghanacs.com/quartz/static/images") ; imgs

  ;; Override the default `org-hugo-export-creator-string' so that this
  ;; string is consistent in all ox-hugo tests.
  (setq org-hugo-export-creator-string "Emacs + Org-mode + ox-hugo")

  ;; In that normal example of the sidenote, ox-hugo trims the whitespace around
  ;; the sidenote block. That is configured by customizing the
  ;; org-hugo-special-block-type-properties variable:
  (progn
    (add-to-list 'org-hugo-special-block-type-properties '("mermaid" :raw t))
    (add-to-list 'org-hugo-special-block-type-properties '("callout" :raw t))
    (add-to-list 'org-hugo-special-block-type-properties '("cards" :raw t))
    (add-to-list 'org-hugo-special-block-type-properties '("details" :raw t)))
  ;; (add-to-list 'org-hugo-special-block-type-properties '("sidenote" . (:trim-pre t :trim-post t)))

  ;; If this property is set to an empty string, this heading will not be auto-inserted.
  ;; default value is 'References'
  ;; https://ox-hugo.scripter.co/doc/org-cite-citations/
  (plist-put org-hugo-citations-plist :bibliography-section-heading "References")

  (defun +org-export-remove-white-space (text _backend _info)
    "Remove zero width spaces from TEXT."
    (unless (org-export-derived-backend-p 'org)
      (replace-regexp-in-string " " "" text)))
  (add-to-list 'org-export-filter-final-output-functions #'+org-export-remove-white-space t))



;;;; insert unicode for notetaking

(with-eval-after-load 'org
  (defun my/insert-white-space ()
    (interactive)
    (insert " "))

  ;; "⊢" prove, "⊨" entail , "∉" notin
  ;;  『 』(겹낫표), ≪ ≫(겹화살괄호) / ｢ ｣(홑낫표) - https://wikidocs.net/79912
  ;; 0x002012	‒	FIGURE DASH
  ;; 0x002013	–	EN DASH
  ;; 0x002014	—	EM DASH
  ;; 0x002015	―	QUOTATION DASH
  ;; 0x002015	―	HORIZONTAL BAR
  (setq my/unicode-notetaking '( " " "§"
                                 "¶" "†" "‡" "№" "↔" "←" "→" "⊢" "⊨" "∉"
                                 "『겹낫표』"
                                 "≪겹화살괄호≫"
                                 "｢홑낫표｣"
                                 "― QUOTADASH"
                                 ))

  (defun my/insert-unicode-notetaking ()
    "Insert Unicode for NoteTaking."
    (interactive)
    (insert (completing-read "Select unicode: " my/unicode-notetaking)))

  (evil-define-key '(insert normal) text-mode-map (kbd "M-M") #'my/insert-unicode-notetaking)
  (evil-define-key '(insert normal) text-mode-map (kbd "M-m") #'my/insert-white-space))


;;;; TODO Org-Hugo Links

;; lambda-setup/lem-setup-org-extensions.el
;; (progn
;;   ;; New link type for Org-Hugo internal links
;;   (defun org-hugo-link-complete ()
;;     "Create link with Hugo ref shortcode"
;;     (concat "{{% ref " (file-relative-name (read-file-name "File: ")) " %}}"))

;;   (defun org-hugo-follow (link)
;;     (find-file (expand-file-name link)))

;;   (with-eval-after-load 'org
;;     (org-link-set-parameters "hugo"
;;                              :complete 'org-hugo-link-complete
;;                              :follow 'org-hugo-follow))
;;   )

;;; provide

(provide 'uniconfig)

;;; end-of configuration
