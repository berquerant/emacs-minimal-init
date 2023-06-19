;;; minimal-init-test.el --- Unittest -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'minimal-init)

(defun test-minimal-init-setup ()
  (minimal-init-setup)
  (should (eq minimal-init-setup-done t)))

(provide 'minimal-init-test)
;;; minimal-init-test.el ends here
