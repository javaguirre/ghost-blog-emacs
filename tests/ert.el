;;; ert.el --- Tests for Ghost mode

;;; Code:

(require 'ert)

(ert-deftest new-post ()
  (ghost-mode-new-post)
  (should
   (equal "---\n\ntitle: New title\nslug: /new-title\n\n---\n\nNew post" (buffer-string))))

(ert-deftest get-post-list-endpoint ()
  (should
   (equal "/posts/?limit=10" (ghost-mode--get-post-list-endpoint))))

(ert-deftest get-post-list-endpoint-limit-changed ()
  (setq ghost-mode-post-list-limit 2)
  (should
   (equal "/posts/?limit=2" (ghost-mode--get-post-list-endpoint))))

(provide 'ert)
;;; ert.el ends here
