;;; ghost-mode.el --- A mode to manage Ghost blog
;;

;; Author: Javier Aguirre <hello@javaguirre.net>
;; Maintainer: Javier Aguirre <hello@javaguirre.net>
;; Created: 10 Feb 2016
;; Keywords: ghost

;; This file is not part of GNU Emacs.

;;; Commentary:

;; TODO Make It clickable
;; TODO GEt the post on click
;; TODO EDIT and save!
;; TODO Set buffer read only
;;
;; This is a minor mode to manage Ghost blogs

;;; Code:
;;
(require 'url)
(require 'json)

(defvar ghost-mode-url nil)
(defvar ghost-mode-bearer-token nil)

;;;###autoload
(defun ghost-mode-connection ()
  "Connection"
  (interactive)
  (let ((url-request-method "GET")
        (url-request-extra-headers
         `(("Authorization" . ,ghost-mode-bearer-token))))

        (url-retrieve ghost-mode-url 'ghost-mode-get-posts))
  )

(defun ghost-mode-get-posts (status)
    ""
    (switch-to-buffer-other-window (current-buffer))
    (search-forward "\n\n")
    (delete-region (point-min) (point))

    (let* ((json-object-type 'hash-table)
	  (body (json-read-from-string (buffer-string)))
	  (posts (gethash "posts" body)))
      (delete-region (point-min) (point-max))
      (insert "Ghost mode - Articles")
      (newline)

      (dotimes (i (length posts))
	(insert (gethash "title" (aref posts i)))
	(define-button-type 'link)
	(newline)
	(insert (gethash "id" (aref posts i)))
	(newline)
	(insert (gethash "slug" (aref posts i)))
	(newline)
	(insert (gethash "created_at" (aref posts i)))
	(newline)
	)))

(provide 'ghost-mode)
;;; ghost-mode.el ends here
