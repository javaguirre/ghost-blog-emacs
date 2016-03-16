;;; ghost-mode.el --- A mode to manage Ghost blog
;;

;; Author: Javier Aguirre <hello@javaguirre.net>
;; Maintainer: Javier Aguirre <hello@javaguirre.net>
;; Created: 10 Feb 2016
;; Keywords: ghost, blog

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
(defun ghost-mode-new-post ()
  "Create new post template."
  (interactive)

  (ghost-mode--use-ghost-post-buffer
   "---\n\ntitle: New title\nslug: /new-title\n\n---\n\nNew post"))
(defun ghost-mode-get-posts ()
  "Get posts from ghost."
  (interactive)
  (ghost-mode--connection "/posts" 'ghost-mode--get-posts-callback))

(defun ghost-mode--connection (endpoint callback &optional method data)
  "HTTP Connection with Ghost API using ENDPOINT, execute CALLBACK.  METHOD and DATA can be set."
  (let ((url-request-method "GET")
	(url-request-extra-headers
	 `(("Authorization" . ,ghost-mode-bearer-token))))
    (url-retrieve (concat ghost-mode-url endpoint) callback)))

(defun ghost-mode--get-posts-callback (status)
  "Process post list callback, receive HTTP response STATUS."
  (ghost-mode--go-to-body)

  (let ((posts (ghost-mode--get-response-posts)))
    (define-button-type 'ghost-show-post-button
      'action 'ghost-mode--show-post-action
      'follow-link t
      'help-echo "Show post")

    (delete-region (point-min) (point-max))

    (insert "Ghost mode - Posts\n\n")

    (dotimes (i (length posts))

      (insert-text-button (format "%d %s - %s\n\n"
				  (gethash "id" (aref posts i))
				  (gethash "created_at" (aref posts i))
				  (gethash "title" (aref posts i)))
			  :type 'ghost-show-post-button))))

(defun ghost-mode--get-post-callback (status)
  "Process post read callback, receive HTTP response STATUS."
  (ghost-mode--go-to-body)

  (let ((posts (ghost-mode--get-response-posts)))
    (ghost-mode--use-ghost-post-buffer
     (format "%s\n\n%s"
	     (gethash "title" (aref posts 0))
	     (gethash "markdown" (aref posts 0))))))

(defun ghost-mode--use-ghost-post-buffer (buffer-data)
  "Use ghost post buffer and insert BUFFER-DATA on It."
  (let ((post-buffer "ghost-mode post"))
    (get-buffer-create post-buffer)
    (switch-to-buffer post-buffer)
    (delete-region (point-min) (point-max))
    (insert buffer-data)

    (setq-default major-mode 'markdown-mode)))

(defun ghost-mode--show-post-action (button)
  "Show a post by id from BUTTON."
  (let* ((id (car (split-string (button-label button))))
	 (endpoint (concat "/posts/" id)))
    (ghost-mode--connection endpoint 'ghost-mode--get-post-callback)))

(defun ghost-mode--go-to-body ()
  "Go to HTTP response body."
  (switch-to-buffer (current-buffer))
  (search-forward "\n\n")
  (delete-region (point-min) (point)))

(defun ghost-mode--get-response-posts ()
  "Get posts from HTTP response body."
  (let ((body (ghost-mode--get-response-body)))
    (gethash "posts" body)))

(defun ghost-mode--get-response-body ()
  "Get HTTP response body json decoded."
  (let ((json-object-type 'hash-table))
    (json-read-from-string (buffer-string))))

(provide 'ghost-mode)
;;; ghost-mode.el ends here
