;; -*- lexical-binding: t; -*-
(require 'asdf)
(require 'ert)

(defun asdf--run-tests ()
  (interactive)
  (if (featurep 'ert)
      (ert-run-tests-interactively "asdf--test")
    (message "cant run without ert.")))

(provide 'asdf-tests)
