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
   collect (list (car target) link target)))

;;;###autoload
(cl-defun linkmarks-select (arg)
  (interactive "P")
  (let* ((targets (linkmarks--in-file))
         (choice (assoc (completing-read "Bookmark: " (mapcar 'car targets)) targets)))
    (if (not arg) (org-link-open-from-string (cadr choice))
      (let ((buffer (find-buffer-visiting (nth 1 (nth 2 choice))))
            (point (nth 3 (nth 2 choice))))
        (message "Jumping to Heading \"%s\" in buffer %s at %s" (car choice) buffer point)
        (pop-to-buffer buffer)
        (goto-char point)))))


(provide 'linkmarks)

;;; linkmarks.el ends here
