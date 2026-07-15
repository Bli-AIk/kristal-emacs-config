;;; fumos-eglot.el --- Pinned fennel-ls integration -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'seq)
(require 'subr-x)
(require 'fumos-project)

(defconst fumos-fennel-ls-short-commit "0c21b003")

(defun fumos--xdg-data-home ()
  (let ((data-home (getenv "XDG_DATA_HOME"))
        (home (getenv "HOME")))
    (file-name-as-directory
     (cond
      ((and data-home (not (string-empty-p data-home)))
       (expand-file-name data-home))
      ((and home (not (string-empty-p home)))
       (expand-file-name ".local/share" home))
      (t
       (user-error "HOME and XDG_DATA_HOME are both empty"))))))

(defun fumos-fennel-ls-executable ()
  "Return the executable for the pinned fennel-ls build."
  (let ((binary
         (expand-file-name
          (format "kristal-emacs-config/fennel-ls/%s/bin/fennel-ls"
                  fumos-fennel-ls-short-commit)
          (fumos--xdg-data-home))))
    (unless (file-executable-p binary)
      (user-error
       "Pinned fennel-ls is missing; run .emacs/tools/install-fennel-ls.sh"))
    binary))

(defun fumos-fennel-ls-command (_interactive _project)
  "Return the pinned fennel-ls command for Eglot."
  (list (fumos-fennel-ls-executable)))

(defun fumos-register-fennel-eglot ()
  "Make the pinned FUMOS client the only fennel-mode Eglot entry."
  (when (boundp 'eglot-server-programs)
    (setq eglot-server-programs
          (cons (cons 'fennel-mode #'fumos-fennel-ls-command)
                (seq-remove (lambda (entry) (eq (car entry) 'fennel-mode))
                            eglot-server-programs)))))

(defun fumos-install-project-config ()
  "Install the FUMOS flsproject template without overwriting a file."
  (interactive)
  (let* ((root (or (fumos-project-root)
                   (user-error "Current directory is not a FUMOS project")))
         (target (expand-file-name "flsproject.fnl" root))
         (config-root
          (file-name-directory
           (directory-file-name
            (file-name-directory
             (or (locate-library "fumos-eglot")
                 (user-error "Cannot locate fumos-eglot.el"))))))
         (template (expand-file-name "templates/flsproject.fnl" config-root)))
    (when (file-exists-p target)
      (user-error "Refusing to overwrite %s" target))
    (copy-file template target nil)
    (find-file target)))

(with-eval-after-load 'eglot
  (fumos-register-fennel-eglot))

(provide 'fumos-eglot)
;;; fumos-eglot.el ends here
