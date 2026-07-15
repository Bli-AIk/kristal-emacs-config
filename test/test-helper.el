;;; test-helper.el --- FUMOS ERT helpers -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'cl-lib)
(require 'ert)

(defconst fumos-test-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

(defun fumos-test-wait-until (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil, for at most TIMEOUT seconds."
  (let ((deadline (+ (float-time) (or timeout 2.0))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (funcall predicate)))

(defmacro fumos-test-with-directory (binding &rest body)
  "Bind BINDING to a temporary directory while evaluating BODY."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,binding (make-temp-file "fumos-test-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,binding t))))

(provide 'test-helper)
;;; test-helper.el ends here
