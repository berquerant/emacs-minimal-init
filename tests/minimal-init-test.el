;;; minimal-init-test.el --- Unittest -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'minimal-init)

(ert-deftest test-minimal-init-setup ()
  "`minimal-init-setup-done' should be non-nil after `minimal-init-setup'."
  (minimal-init-setup)
  (should (eq minimal-init-setup-done t))
  (should (eq isearch-lazy-count t))
  (when (boundp 'electric-pair-mode)
    (should (eq electric-pair-mode t))))

(provide 'minimal-init-test)
;;; minimal-init-test.el ends here
