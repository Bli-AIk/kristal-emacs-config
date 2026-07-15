;;; init.el --- Kristal project Emacs entry -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'cl-lib)
(require 'subr-x)

(let* ((config-dir (file-name-directory (or load-file-name buffer-file-name)))
       (lisp-dir (expand-file-name "lisp" config-dir))
       (vendor-dir (expand-file-name "vendor/fennel-mode" config-dir)))
  (add-to-list 'load-path vendor-dir)
  (add-to-list 'load-path lisp-dir)
  (cl-labels
      ((file-sha256
        (file)
        (with-temp-buffer
          (insert-file-contents-literally file)
          (secure-hash 'sha256 (current-buffer))))
       (loaded-source
        (symbol)
        (when-let* ((loaded (symbol-file symbol 'defun)))
          (cond
           ((string-suffix-p ".elc" loaded)
            (concat (file-name-sans-extension loaded) ".el"))
           ((string-suffix-p ".el" loaded) loaded))))
       (assert-pinned
        (feature symbol filename)
        (when (featurep feature)
          (let ((loaded (loaded-source symbol))
                (pinned (expand-file-name filename vendor-dir)))
            (unless (and loaded
                         (file-readable-p loaded)
                         (equal (file-sha256 loaded)
                                (file-sha256 pinned)))
              (error
               (concat
                "Loaded %s does not match the pinned FUMOS vendor; "
                "run doom sync and restart Emacs")
               feature))))))
    (assert-pinned 'fennel-mode 'fennel-mode "fennel-mode.el")
    (assert-pinned 'fennel-proto-repl 'fennel-proto-repl-mode
                   "fennel-proto-repl.el"))
  (require 'fennel-mode)
  (require 'fumos-project)
  (require 'fumos-eglot)
  (fumos-project-install))

(provide 'kristal-emacs-config)
;;; init.el ends here
