;;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-

;;;;; evil-evilified-state

;; /home/junghan/spacemacs/layers/+distributions/spacemacs-bootstrap/local/evil-evilified-state/evil-evilified-state.el
(defun evilified-state--evilify-event (map map-symbol evil-map event evil-value
                                           &optional processed pending-funcs)
  "Evilify EVENT in MAP and return a list of PROCESSED events."
  (if (and event (or evil-value pending-funcs))
      (let* ((kbd-event (kbd (single-key-description event)))
             (map-value (lookup-key map kbd-event))
             (evil-value (or evil-value
                             (lookup-key evil-map kbd-event)
                             (car (pop pending-funcs)))))
        (when evil-value
          (evil-define-key 'evilified map kbd-event evil-value))
        (when map-value
          (add-to-list 'pending-funcs (cons map-value event) 'append))
        (push event processed)
        (setq processed (evilified-state--evilify-event
                         map map-symbol evil-map
                         (evilified-state--find-new-event event) nil
                         processed pending-funcs))))
  processed)

;;;;; spacemacs-default

(spacemacs|define-transient-state buffer
  :title "Buffer Transient State"
  :doc "
 [_C-1_.._C-9_] goto nth window            [_n_/_<right>_]^^  next buffer       [_b_]   buffer list
 [_1_.._9_]     move buffer to nth window  [_N_/_p_/_<left>_] previous buffer   [_C-d_] bury buffer
 [_M-1_.._M-9_] swap buffer w/ nth window  [_d_]^^^^          kill buffer       [_o_]   other window
 ^^^^                                      [_z_]^^^^          recenter          [_q_]   quit"
  :bindings
  ("n" next-buffer)
  ("<right>" next-buffer)
  ("p" previous-buffer)
  ("N" previous-buffer)
  ("o" other-window)
  ("<left>" previous-buffer)
  ("b" (cond ((configuration-layer/layer-used-p 'helm)
              (helm-buffers-list))
             ((configuration-layer/layer-used-p 'ivy)
              (ivy-switch-buffer))
             ((configuration-layer/layer-used-p 'compleseus)
              (spacemacs/compleseus-switch-to-buffer))))
  ("d" spacemacs/kill-this-buffer)
  ("C-d" bury-buffer)
  ("z" recenter-top-bottom)
  ("q" nil :exit t)
  ("1" move-buffer-window-no-follow-1)
  ("2" move-buffer-window-no-follow-2)
  ("3" move-buffer-window-no-follow-3)
  ("4" move-buffer-window-no-follow-4)
  ("5" move-buffer-window-no-follow-5)
  ("6" move-buffer-window-no-follow-6)
  ("7" move-buffer-window-no-follow-7)
  ("8" move-buffer-window-no-follow-8)
  ("9" move-buffer-window-no-follow-9)
  ("M-1" swap-buffer-window-no-follow-1)
  ("M-2" swap-buffer-window-no-follow-2)
  ("M-3" swap-buffer-window-no-follow-3)
  ("M-4" swap-buffer-window-no-follow-4)
  ("M-5" swap-buffer-window-no-follow-5)
  ("M-6" swap-buffer-window-no-follow-6)
  ("M-7" swap-buffer-window-no-follow-7)
  ("M-8" swap-buffer-window-no-follow-8)
  ("M-9" swap-buffer-window-no-follow-9)
  ("C-1" spacemacs/winum-select-window-1)
  ("C-2" spacemacs/winum-select-window-2)
  ("C-3" spacemacs/winum-select-window-3)
  ("C-4" spacemacs/winum-select-window-4)
  ("C-5" spacemacs/winum-select-window-5)
  ("C-6" spacemacs/winum-select-window-6)
  ("C-7" spacemacs/winum-select-window-7)
  ("C-8" spacemacs/winum-select-window-8)
  ("C-9" spacemacs/winum-select-window-9))


;;;;; DONT compleseus layer - consult

;; (defvar consult--source-layout-persp-buffers
;;   `(
;;     :name     "Layout/Persp Buffer"
;;     :narrow   ?l
;;     :category buffer
;;     :face     consult-buffer
;;     :history  buffer-name-history
;;     :state    ,#'consult--buffer-state
;;     :default  t
;;     :items
;;     ,(lambda ()
;;        (consult--buffer-query
;;         :sort 'visibility
;;         :predicate #'persp-contain-buffer-p
;;         :as #'buffer-name)))
;;   "Per perpecstive buffer source.")

;; (defun spacemacs/compleseus-switch-to-buffer ()
;;   "`consult-buffer' with buffers provided by persp."
;;   (interactive)
;;   (consult-buffer
;;    '(consult--source-hidden-buffer
;;      consult--source-layout-persp-buffers
;;      consult--source-modified-buffers
;;      consult--source-recent-file
;;      consult--source-bookmark
;;      consult--source-project-buffer
;;      consult--source-project-recent-file)))

;;;;; compleseus

;;;; compleseus/search-dir

;;;###autoload
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

;;;; format-buffer

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
