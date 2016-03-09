;;; ghost-mode.el --- A mode to manage Ghost blog
;;

;; Author: Javier Aguirre <hello@javaguirre.net>
;; Maintainer: Javier Aguirre <hello@javaguirre.net>
;; Created: 10 Feb 2016
;; Keywords: ghost

;; This file is not part of GNU Emacs.

;;; Commentary:

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
(defun ghost-mode-get-posts ()
  "Get posts from ghost."
  (interactive)
  (ghost-mode-connection "/posts" 'ghost-mode-get-posts-callback))

(defun ghost-mode-connection (endpoint callback &optional method data)
  "Connection"
  (let ((url-request-method "GET")
        (url-request-extra-headers
         `(("Authorization" . ,ghost-mode-bearer-token))))
    (url-retrieve (concat ghost-mode-url endpoint) callback)))

(defun ghost-mode-get-posts-callback (status)
    ""
    (switch-to-buffer-other-window (current-buffer))
    (search-forward "\n\n")
    (delete-region (point-min) (point))

    (let* ((json-object-type 'hash-table)
	  (body (json-read-from-string (buffer-string)))
	  (posts (gethash "posts" body)))

	(defun ghost-show-post (button)
	 "Show a post by id from BUTTON."
  	  (let (id (car (split-string (button-label button))))
	    (message (button-label button))
            (ghost-mode-connection (concat "/posts" id) 'ghost-mode-get-post-callback)))

      (define-button-type 'ghost-show-post-button
	'action 'ghost-show-post
	'follow-link t
	'help-echo "Show post")

      (delete-region (point-min) (point-max))

      (insert "Ghost mode - Posts\n\n")

      (dotimes (i (length posts))

	(insert-text-button (format "%d %s - %s\n\n"
          (gethash "id" (aref posts i))
          (gethash "created_at" (aref posts i))
          (gethash "title" (aref posts i)))
	  :type 'ghost-show-post-button)
      )))

(defun ghost-mode-get-post-callback (status)
    ""
    (switch-to-buffer (current-buffer))
    (search-forward "\n\n")
    (delete-region (point-min) (point))

    (let* ((json-object-type 'hash-table)
	  (body (json-read-from-string (buffer-string)))
	  (posts (gethash "posts" body)))

      (delete-region (point-min) (point-max))

      (insert (format "%s\n\n%s"
	(gethash "title" (aref posts 0))
	(gethash "markdown" (aref posts 0))))
      ))

(provide 'ghost-mode)
;;; ghost-mode.el ends here
