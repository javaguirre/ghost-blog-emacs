;;; ert.el --- Tests for Ghost mode

;;; Code:

(require 'ert)

;; Variables
(defvar ghost-mode--test-data
      "HTTP/1.1 200 OK
Server: cloudflare-nginx
Date: Wed, 16 Mar 2016 14:40:23 GMT
Content-Type: application/json; charset=utf-8
Transfer-Encoding: chunked
Connection: keep-alive
Cache-Control: no-cache, private, no-store, must-revalidate, max-stale=0, post-check=0, pre-check=0
Cf-Railgun: 8fa74abe46 3.53 0.074589 0030 3350
Last-Modified: Wed, 16 Mar 2016 14:40:23 GMT
Status: 200 OK
Vary: Accept-Encoding
X-Ghost-Cache-Status: From Backend - Not Cached
X-Powered-By: Express
CF-RAY: 2848f5c229e802d9-AMS
Content-Encoding: gzip

{\"posts\":[{\"id\":1,\"uuid\":\"\",\"title\":\"Title 1\",\"slug\":\"save-and-load-requests\",\"markdown\":\"Some months ago I started to use\",\"image\":\"/content/images/2016/03/Atom-Wallpaper.png\",\"featured\":false,\"page\":false,\"status\":\"published\",\"language\":\"en_US\",\"meta_title\":null,\"meta_description\":null,\"created_at\":\"2016-03-15T11:04:59.000Z\",\"created_by\":1,\"updated_at\":\"2016-03-16T08:28:46.000Z\",\"updated_by\":1,\"published_at\":\"2016-03-16T08:28:46.000Z\",\"published_by\":1,\"author\":1,\"url\":\"/2016/03/16/save-and-load-requests\"},{\"id\":2,\"uuid\":\"\",\"title\":\"Title 2\",\"image\":null,\"featured\":false,\"page\":false,\"status\":\"published\",\"language\":\"en_US\",\"meta_title\":null,\"meta_description\":null,\"created_at\":\"2016-01-17T12:15:21.000Z\",\"created_by\":1,\"updated_at\":\"2016-01-17T12:24:16.000Z\",\"updated_by\":1,\"published_at\":\"2016-01-17T12:23:41.000Z\",\"published_by\":1,\"author\":1,\"url\":\"/2016/01/17/ansible-role\"}],\"meta\":{\"pagination\":{\"page\":1,\"limit\":2,\"pages\":97,\"total\":193,\"next\":2,\"prev\":null}}}")

(defvar ghost-mode--test-body
      "{\"posts\":[{\"id\":1,\"uuid\":\"\",\"title\":\"Title 1\",\"slug\":\"save-and-load-requests\",\"markdown\":\"Some months ago I started to use\",\"image\":\"/content/images/2016/03/Atom-Wallpaper.png\",\"featured\":false,\"page\":false,\"status\":\"published\",\"language\":\"en_US\",\"meta_title\":null,\"meta_description\":null,\"created_at\":\"2016-03-15T11:04:59.000Z\",\"created_by\":1,\"updated_at\":\"2016-03-16T08:28:46.000Z\",\"updated_by\":1,\"published_at\":\"2016-03-16T08:28:46.000Z\",\"published_by\":1,\"author\":1,\"url\":\"/2016/03/16/save-and-load-requests\"},{\"id\":2,\"uuid\":\"\",\"title\":\"Title 2\",\"image\":null,\"featured\":false,\"page\":false,\"status\":\"published\",\"language\":\"en_US\",\"meta_title\":null,\"meta_description\":null,\"created_at\":\"2016-01-17T12:15:21.000Z\",\"created_by\":1,\"updated_at\":\"2016-01-17T12:24:16.000Z\",\"updated_by\":1,\"published_at\":\"2016-01-17T12:23:41.000Z\",\"published_by\":1,\"author\":1,\"url\":\"/2016/01/17/ansible-role\"}],\"meta\":{\"pagination\":{\"page\":1,\"limit\":2,\"pages\":97,\"total\":193,\"next\":2,\"prev\":null}}}")

(defvar ghost-mode--expected-metadata-string
      "---\n\ntitle:\nslug:\nstatus:\nimage:\nfeatured:\npage:\nlanguage:\nmeta_title:\nmeta_description:\n\n---\n\n")

;; Unittest
(defun ghost-mode-ert--last-message ()
  "Get last message in Messages."
  (with-current-buffer "*Messages*"
    (forward-line (- 1 0))
    (backward-char)
    (let ((end (point)))
      (forward-line 0)
      (buffer-substring-no-properties (point) end))))

(ert-deftest ghost-mode-new-post ()
  (ghost-mode-new-post)
  (should
   (equal "---\n\ntitle: New title\nslug: /new-title\n\n---\n\nNew post" (buffer-string))))

(ert-deftest ghost-mode-get-posts ()
  (let ((ghost-mode-url "http://javaguirre.net/ghost/api/v0.1")
	(ghost-mode-bearer-token ""))
    (ghost-mode-get-posts)
    (sit-for 5)
    (should
     (equal ghost-mode-http-authentication-warning-message (ghost-mode-ert--last-message)))))

(ert-deftest ghost-mode-get-post-list-endpoint ()
  (should
   (equal "/posts/?limit=10" (ghost-mode--get-post-list-endpoint))))

(ert-deftest ghost-mode-get-post-list-endpoint-limit-changed ()
  (let ((ghost-mode-post-list-limit 2))
    (should
     (equal "/posts/?limit=2" (ghost-mode--get-post-list-endpoint)))))

(ert-deftest ghost-mode-new-post-read-from-post-buffer ()
  (ghost-mode-new-post)

  (let* ((expected-hash (make-hash-table :test 'equal))
	 (post-hash (ghost-mode--read-from-post-buffer)))
    
    (puthash "title" "New title" expected-hash)
    (puthash "markdown" "New post" expected-hash)
    (puthash "slug" "/new-title" expected-hash)

    (should
     (equal (gethash "title" expected-hash) (gethash "title" post-hash)))
    (should
     (equal (gethash "markdown" expected-hash) (gethash "markdown" post-hash)))
    (should
     (equal (gethash "slug" expected-hash) (gethash "slug" post-hash)))))

(ert-deftest ghost-mode-get-http-status-code ()
  (with-temp-buffer
    (insert ghost-mode--test-data)
    (goto-char (point-min))

    (should
     (equal "200" (ghost-mode--get-http-status-code)))))

(ert-deftest ghost-mode-go-to-body ()
  (with-temp-buffer
    (insert ghost-mode--test-data)
    (goto-char (point-min))

    (ghost-mode--go-to-body)

    (should
     (equal ghost-mode--test-body (buffer-string)))))

(ert-deftest ghost-mode-get-metadata-as-string ()
  (let ((metadata (ghost-mode--get-metadata-as-string)))
    (should
     (equal ghost-mode--expected-metadata-string metadata))))

(provide 'ert)
;;; ert.el ends here
