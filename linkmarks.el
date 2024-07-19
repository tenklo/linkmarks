;;; linkmarks.el --- Org-mode link based bookmarks -*- lexical-binding: t; -*-
;; Copyright (C) 2018 Dustin Lacewell

;; Author: Dustin Lacewell <dlacewell@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24") (org "0"))
;; Keywords: bookmarks, org
;; URL: http://github.com/dustinlacewell/linkmarks

;;; Commentary:

;; This package lets you use org-mode links as bookmarks.

;;; Code:
(require 'cl-lib)
(require 'org)

;;;###autoload
(defgroup linkmarks nil
  "Configuration options for linkmarks."
  :group 'org)

;;;###autoload
(defcustom linkmarks-file "~/org/bookmarks.org"
  "Define linkmarks file to store your bookmarks in."
  :group 'linkmarks
  :type 'string)

(cl-defun linkmarks--in-file ()
  (cl-loop
   for target in (org-refile-get-targets)
   for element = (with-current-buffer (find-buffer-visiting (nth 1 target))
                   (goto-char (nth 3 target))
                   (org-end-of-meta-data t)
                   (org-element-context))
   for type = (car element)
   for props = (cadr element)
   for link = (plist-get props :raw-link)
   if (and (equal 'link type) link)
   collect (list (car target) link)))

;;;###autoload
(cl-defun linkmarks-select (arg)
  (interactive "P")
  (if arg
      (org-refile '(4))
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
