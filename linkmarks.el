;;; linkmarks.el --- Org-mode link based bookmarks -*- lexical-binding: t; -*-
;; Copyright (C) 2018 Dustin Lacewell

;; Author: Dustin Lacewell <dlacewell@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24") (dash "0") (org "0"))
;; Keywords: bookmarks, org
;; URL: http://github.com/dustinlacewell/linkmarks

;;; Commentary:

;; This package lets you use org-mode links as bookmarks.

;;; Code:
(require 'cl-lib)
(require 'dash)
(require 'org)

;;;###autoload
(defgroup linkmarks nil
  "Configuration options for linkmarks."
  :group 'org)

;;;###autoload
(defcustom linkmarks-file (expand-file-name "~/org/bookmarks.org")
  "Define linkmarks file to store your bookmarks in."
  :group 'linkmarks
  :type 'string)

(cl-defmacro linkmarks--setup (&rest body)
  `(let ((org-outline-path-complete-in-steps nil)
         (org-refile-use-outline-path t)
         (org-agenda-files `(,linkmarks-file))
         (org-refile-targets `((,linkmarks-file :maxlevel . 99))))
     ,@body))

(cl-defun linkmarks--in-file ()
  (linkmarks--setup
   (with-temp-buffer
     (insert-file-contents linkmarks-file t)
     (org-mode)
     (goto-char (point-min))
     (outline-show-all)
     (cl-loop
      for target in (org-refile-get-targets)
      for element = (progn
                      (goto-char (nth 3 target))
                      (org-end-of-meta-data t)
                      (org-element-context))
      for type = (car element)
      for props = (cadr element)
      for begin = (plist-get props :begin)
      for end = (plist-get props :end)
      for content = (buffer-substring begin end)
      if (equal 'link type)
      collect (list (car target) content)))))

;;;###autoload
(cl-defun linkmarks-select (arg)
  (interactive "P")
  (if arg
      (linkmarks--setup
       (org-refile '(4)))
    (-let* ((targets (linkmarks--in-file))
            (choices (mapcar 'car targets))
            (choice (completing-read "Bookmark: " choices))
            ((_ link) (-first (lambda (i) (equal (car i) choice)) targets)))
      (org-link-open-from-string link))))

;;;###autoload
(defun linkmarks-capture ()
  (interactive)
  (let ((org-capture-templates '(("t" "Bookmark" entry (file linkmarks-file)
                                  "* %^{Title}\n[[%?]]\n  added: %U" :kill-buffer t))))
    (linkmarks--setup
     (org-capture))))

(provide 'linkmarks)

;;; linkmarks.el ends here
