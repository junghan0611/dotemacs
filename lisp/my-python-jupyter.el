;;; lisp/my-python-jupyter.el -*- lexical-binding: t; -*-

;; https://github.com/emacs-jupyter/jupyter/pull/573/files

;;;; jupyter code completion with corfu

;; dotsamples/vanilla/karthink-dotfiles-popper/lisp/setup-python.el
(with-eval-after-load 'jupyter

  ;; :bind (:map jupyter-repl-interaction-mode-map
  ;;             ("M-i"   . nil)
  ;;             ("C-h ." . jupyter-inspect-at-point))

  ;; Make jupyter's completion work with corfu-show-documentation
  (unless (fboundp 'company-mode)
    (defun company-doc-buffer (&optional string)
      (with-current-buffer (get-buffer-create "*jupyter help*")
        (erase-buffer)
        (markdown-mode)
        (when string
          (save-excursion
            (insert string)
            (visual-line-mode)))
        (current-buffer))))

  (defun jupyter-completion--company-doc-buffer (arg)
    "Send an inspect request for ARG to the kernel.
Use the `company-doc-buffer' to insert the results."
    (let* ((buf (company-doc-buffer))
           (prefix (car (jupyter-code-context 'inspect)))
           (sym (if (string-match ".*\\." prefix)
                    (concat (match-string 0 prefix) arg)
                  arg)))
      (jupyter-inspect sym (length sym) buf)
      (with-current-buffer buf
        (when (> (point-max) (point-min))
          (let ((inhibit-read-only t))
            (remove-text-properties
             (point-min) (point-max) '(read-only))
            (font-lock-mode 1)
            (goto-char (point-min))
            (current-buffer))))))
  )

;;;; ob-jupyter - ansi block

(with-eval-after-load 'ob-jupyter
  ;; :bind (:map jupyter-org-interaction-mode-map
  ;;             ("M-i"   . nil)
  ;;             ("C-h ." . jupyter-inspect-at-point))
  ;; Clean up ob-jupyter source block output
  ;; From Henrik Lissner
  (defun my/org-babel-jupyter-strip-ansi-escapes-block ()
    (when (string-match-p "^jupyter-"
                          (nth 0 (org-babel-get-src-block-info)))
      (unless (or
               ;; ...but not while Emacs is exporting an org buffer (where
               ;; `org-display-inline-images' can be awfully slow).
               (bound-and-true-p org-export-current-backend)
               ;; ...and not while tangling org buffers (which happens in a temp
               ;; buffer where `buffer-file-name' is nil).
               (string-match-p "^ \\*temp" (buffer-name)))
        (save-excursion
          (when-let* ((beg (org-babel-where-is-src-block-result))
                      (end (progn (goto-char beg)
                                  (forward-line)
                                  (org-babel-result-end))))
            (ansi-color-apply-on-region (min beg end) (max beg end)))))))

  (add-hook 'org-babel-after-execute-hook
            #'my/org-babel-jupyter-strip-ansi-escapes-block)

  (define-key jupyter-org-interaction-mode-map (kbd "C-c h") nil)
  (define-key jupyter-org-interaction-mode-map (kbd "<f2>") 'jupyter-org-hydra/body)

  (setq org-babel-jupyter-resource-directory (concat user-emacs-directory "ob-jupyter"))

  ;; (setq org-babel-default-header-args:jupyter-python
  ;;       '((:async . "yes") (:session . "py") (:kernel . "python3")
  ;;         ;; (:tangle . "jupyter-python/tangled.py")
  ;;         ;; (:exports . "both")
  ;;         ))
  )

;;;; jupyter - repl interaction

;; starterkit/snakemacs-python/main.el
;; sqrt-dotfiles-elfeed/Emacs.org

(progn

  (setq jupyter-repl-echo-eval-p t) ; default nil

  ;; When this minor mode is enabled you may evaluate code from the current
  ;; buffer using the associated REPL (see jupyter-repl-associate-buffer to
  ;; associate a REPL).

  (defun my/jupyter-refresh-kernelspecs ()
    "Refresh Jupyter kernelspecs"
    (interactive)
    (jupyter-available-kernelspecs t))

  (defun my/jupyter-refesh-langs ()
    "Refresh Jupyter languages"
    (interactive)
    (org-babel-jupyter-aliases-from-kernelspecs t))

  (setq my/jupyter-runtime-folder
        (expand-file-name "~/.local/share/jupyter/runtime"))

  (defun my/get-open-ports ()
    (mapcar
     #'string-to-number
     (split-string (shell-command-to-string
                    "ss -tulpnH | awk '{print $5}' | sed -e 's/.*://'")
                   "\n")))

  (defun my/list-jupyter-kernel-files ()
    (mapcar
     (lambda (file)
       (cons
        (car file)
        (cdr (assq 'shell_port (json-read-file (car file))))))
     (sort
      (directory-files-and-attributes my/jupyter-runtime-folder
                                      t ".*kernel.*json$")
      (lambda (x y) (not (time-less-p (nth 6 x) (nth 6 y)))))))

  (defun my/select-jupyter-kernel ()
    (let ((ports (my/get-open-ports))
          (files (my/list-jupyter-kernel-files)))
      (completing-read
       "Jupyter kernels: "
       (seq-filter (lambda (file) (member (cdr file) ports)) files))))

  ;; #+PROPERTY: header-args:python :session <path-to-kernel>
  (defun my/insert-jupyter-kernel ()
    "Insert a path to an active Jupyter kernel into the buffer"
    (interactive)
    (insert (my/select-jupyter-kernel)))

  (defun my/jupyter-connect-repl ()
    "Open an emacs-jupyter REPL, connected to a Jupyter kernel"
    (interactive)
    (jupyter-connect-repl (my/select-jupyter-kernel) nil nil nil t))

  (defun my/jupyter-qtconsole ()
    "Open Jupyter QtConsole, connected to a Jupyter kernel"
    (interactive)
    (start-process "jupyter-qtconsole" nil "setsid" "jupyter" "qtconsole" "--existing"
                   (file-name-nondirectory (my/select-jupyter-kernel))))

  (defun my/jupyter-cleanup-kernels ()
    (interactive)
    (let* ((ports (my/get-open-ports))
           (files (my/list-jupyter-kernel-files))
           (to-delete
            (seq-filter
             (lambda (file) (not (member (cdr file) ports))) files)))
      (when (and (length> to-delete 0)
                 (y-or-n-p
                  (format "Delete %d files?" (length to-delete))))
        (dolist (file to-delete)
          (delete-file (car file))))))
  )

;;; provide

(provide 'my-python-jupyter)
