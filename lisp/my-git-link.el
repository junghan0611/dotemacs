;;; git-link.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Junghan Kim
;;
;; Author: Junghan Kim <junghanacs@gmail.com>
;; Maintainer: Junghan Kim <junghanacs@gmail.com>
;; Created: March 12, 2025
;; Modified: March 12, 2025
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/junghan0611/git-link
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;; agzam-dot-doom/modules/custom/git/autoload/git-link.el
;;
;;; Code:

;;;###autoload
(defun git-link-main-branch (&optional browse?)
  (interactive "P")
  (require 'git-link)
  (let* ((git-link-default-branch (magit-main-branch)))
    (call-interactively #'git-link-kill)))

;;;###autoload
(defun git-link-blame ()
  (interactive)
  (cl-flet ((git-link--new* (x) (replace-regexp-in-string "/blob/" "/blame/" x)))
    (advice-add 'git-link--new :override #'git-link--new*)
    (let ((link (call-interactively 'git-link)))
      (advice-remove 'git-link--new #'git-link--new*)
      (git-link--new link))))

;;;###autoload
(defun git-link-kill (&optional browse?)
  "Copy URL to current file/revision/forge-topic"
  (interactive "P")
  (require 'git-link)
  (cl-letf (((symbol-function #'git-link--new) (lambda (link) link)))
    (let ((link (pcase major-mode
                  ((pred (lambda (x) (string-match-p "forge-topic" (symbol-name x))))
                   (git-link-forge-topic))

                  ((pred (lambda (x) (string-match-p "magit" (symbol-name x))))
                   (message (browse-at-remote-kill)))

                  ;; #'git-link fn detestably has been made to be exclusively called
                  ;; interactively, so I had to temporarily redefine #'git-link--new
                  ;; (above), ignore prefix arg and other parameters, in order to retrieve
                  ;; the link
                  (_ (let* ((current-prefix-arg nil)
                            (git-link-open-in-browser browse?)
                            (lnk (call-interactively #'git-link)))
                       (kill-new lnk)
                       (prin1 lnk))))))
      (if browse?
          (browse-url link)
        link))))

;;;###autoload
(defun git-link-forge-topic ()
  (interactive)
  (let ((url (forge-get-url (forge-current-topic))))
    (message url)
    (kill-new url)))

;;; provide

(provide 'my-git-link)

;;; git-link.el ends here
