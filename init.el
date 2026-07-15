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
       (source-defvar-value
        (file symbol)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (unless (re-search-forward
                   (format "^(defvar[[:space:]]+%s[[:space:]]+"
                           (regexp-quote (symbol-name symbol)))
                   nil t)
            (error "Pinned vendor does not define %s" symbol))
          (goto-char (match-beginning 0))
          (let ((form (read (current-buffer))))
            (unless (and (eq (car-safe form) 'defvar)
                         (eq (nth 1 form) symbol)
                         (stringp (nth 2 form)))
              (error "Pinned vendor has an invalid %s definition" symbol))
            (nth 2 form))))
       (loaded-source
        (symbol)
       (when-let* ((loaded (symbol-file symbol 'defun)))
         (cond
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
              feature)))))
       (assert-pinned-protocol-runtime
        ()
        (when (featurep 'fennel-proto-repl)
          (let ((expected
                 (source-defvar-value
                  (expand-file-name "fennel-proto-repl.el" vendor-dir)
                  'fennel-proto-repl--protocol)))
            (unless (and (boundp 'fennel-proto-repl--protocol)
                         (equal fennel-proto-repl--protocol expected))
              (error
               (concat
                "Loaded fennel-proto-repl bytecode does not match the "
                "pinned FUMOS vendor; run doom sync and restart Emacs")))))))
    (assert-pinned 'fennel-mode 'fennel-mode "fennel-mode.el")
    (assert-pinned 'fennel-proto-repl 'fennel-proto-repl-mode
                   "fennel-proto-repl.el")
    (assert-pinned-protocol-runtime))
  (require 'fennel-mode)
  (require 'fennel-proto-repl)
  (unless (and (stringp fennel-proto-repl--protocol)
               (string-match-p ":version \\\"0\\.6\\.4\\\""
                               fennel-proto-repl--protocol))
    (error "FUMOS requires fennel-proto-repl 0.6.4"))
  (require 'fumos-project)
  (require 'fumos-eglot)
  (require 'fumos-instance)
  (require 'fumos-repl)
  (require 'fumos-eval)
  (fumos-project-install))

(provide 'kristal-emacs-config)
;;; init.el ends here
