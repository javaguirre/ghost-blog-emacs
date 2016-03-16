;;; ert.el --- Tests for Ghost mode

;;; Code:

(require 'ert)

(ert-deftest new-post ()
  (ghost-mode-new-post)
  (should
   (equal "---\n\ntitle: New title\nslug: /new-title\n\n---\n\nNew post" (buffer-string))))

(provide 'ert)
;;; ert.el ends here
