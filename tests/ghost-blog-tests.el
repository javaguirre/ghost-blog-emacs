;;; ert.el --- Tests for Ghost mode
;;; Commentary:

;;; Code:

(require 'ert)

;; Variables
(defvar ghost-blog--test-data
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

(defvar ghost-blog--test-body
      "{\"posts\":[{\"id\":1,\"uuid\":\"\",\"title\":\"Title 1\",\"slug\":\"save-and-load-requests\",\"markdown\":\"Some months ago I started to use\",\"image\":\"/content/images/2016/03/Atom-Wallpaper.png\",\"featured\":false,\"page\":false,\"status\":\"published\",\"language\":\"en_US\",\"meta_title\":null,\"meta_description\":null,\"created_at\":\"2016-03-15T11:04:59.000Z\",\"created_by\":1,\"updated_at\":\"2016-03-16T08:28:46.000Z\",\"updated_by\":1,\"published_at\":\"2016-03-16T08:28:46.000Z\",\"published_by\":1,\"author\":1,\"url\":\"/2016/03/16/save-and-load-requests\"},{\"id\":2,\"uuid\":\"\",\"title\":\"Title 2\",\"image\":null,\"featured\":false,\"page\":false,\"status\":\"published\",\"language\":\"en_US\",\"meta_title\":null,\"meta_description\":null,\"created_at\":\"2016-01-17T12:15:21.000Z\",\"created_by\":1,\"updated_at\":\"2016-01-17T12:24:16.000Z\",\"updated_by\":1,\"published_at\":\"2016-01-17T12:23:41.000Z\",\"published_by\":1,\"author\":1,\"url\":\"/2016/01/17/ansible-role\"}],\"meta\":{\"pagination\":{\"page\":1,\"limit\":2,\"pages\":97,\"total\":193,\"next\":2,\"prev\":null}}}")

(defvar ghost-blog--expected-metadata-string
      "---\n\ntitle: \nslug: \nstatus: \nimage: \nfeatured: \npage: \nlanguage: \nmeta_title: \nmeta_description: \n\n---\n\n")

;; Unittest

;; Commands
(ert-deftest ghost-blog-new-post ()
  (ghost-blog-new-post)
  (should
   (equal "---\n\ntitle: New title\nslug: /new-title\n\n---\n\nNew post" (buffer-string))))

;; Metadata
(ert-deftest ghost-blog-get-metadata-as-string ()
  (let* ((test-data (make-hash-table :test 'equal))
	 (metadata (ghost-blog--get-metadata-as-string test-data)))
    (should
     (equal ghost-blog--expected-metadata-string metadata))))

(ert-deftest ghost-blog-is-metadata-valid-return-true ()
  (let ((test-metadata (make-hash-table :test 'equal)))
    (puthash "title" "New title" test-metadata)
    (puthash "markdown" "New post" test-metadata)

    (should
     (equal t (ghost-blog--is-metadata-valid test-metadata)))))

(ert-deftest ghost-blog-is-metadata-valid-return-false ()
  (let ((test-metadata (make-hash-table :test 'equal)))
    (puthash "title" "New title" test-metadata)

    (should
     (equal nil (ghost-blog--is-metadata-valid test-metadata)))))

;; Utils
(ert-deftest ghost-blog-new-post-read-from-post-buffer ()
  (ghost-blog-new-post)

  (let* ((expected-hash (make-hash-table :test 'equal))
	 (post-hash (ghost-blog--read-from-post-buffer)))
    
    (puthash "title" "New title" expected-hash)
    (puthash "markdown" "New post" expected-hash)
    (puthash "slug" "/new-title" expected-hash)

    (should
     (equal (gethash "title" expected-hash) (gethash "title" post-hash)))
    (should
     (equal (gethash "markdown" expected-hash) (gethash "markdown" post-hash)))
    (should
     (equal (gethash "slug" expected-hash) (gethash "slug" post-hash)))))

(ert-deftest ghost-blog-get-http-status-code ()
  (with-temp-buffer
    (insert ghost-blog--test-data)
    (goto-char (point-min))

    (should
     (equal "200" (ghost-blog--get-http-status-code)))))

(ert-deftest ghost-blog-go-to-body ()
  (with-temp-buffer
    (insert ghost-blog--test-data)
    (goto-char (point-min))

    (ghost-blog--go-to-body)

    (should
     (equal ghost-blog--test-body (buffer-string)))))

;; Endpoint
(ert-deftest ghost-blog-get-post-list-endpoint ()
  (should
   (equal "/posts/?limit=10" (ghost-blog--get-post-list-endpoint))))

(ert-deftest ghost-blog-get-post-list-endpoint-limit-changed ()
  (let ((ghost-blog-post-list-limit 2))
    (should
     (equal "/posts/?limit=2" (ghost-blog--get-post-list-endpoint)))))

;; Utils for ert
(defun ghost-blog-ert--last-message ()
  "Get last message in Messages."
  (with-current-buffer "*Messages*"
    (forward-line (- 1 0))
    (backward-char)
    (let ((end (point)))
      (forward-line 0)
      (buffer-substring-no-properties (point) end))))

(provide 'ert)
;;; ert.el ends here
