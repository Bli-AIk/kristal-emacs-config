;;; fumos-project-test.el --- FUMOS project tests -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'test-helper)
(require 'fennel-mode)
(require 'fumos-project)
(require 'fennel-proto-repl)

(defmacro fumos-test-with-project (root &rest body)
  "Create a minimal FUMOS project, bind ROOT, and evaluate BODY."
  (declare (indent 1) (debug (symbolp body)))
  `(fumos-test-with-directory ,root
     (make-directory (expand-file-name "libraries/fumos" ,root) t)
     (make-directory (expand-file-name ".emacs" ,root) t)
     (with-temp-file (expand-file-name "mod.json" ,root)
       (insert "{\"id\":\"demo\",\"dev\":true}\n"))
     (with-temp-file (expand-file-name "libraries/fumos/lib.json" ,root)
       (insert "{\"id\":\"fumos\"}\n"))
     (with-temp-file (expand-file-name ".emacs/init.el" ,root)
       (insert "; project marker\n"))
     ,@body))

(defun fumos-test--run-clean-emacs (&rest arguments)
  "Run a clean batch Emacs with ARGUMENTS and return (STATUS . OUTPUT)."
  (with-temp-buffer
    (let ((status
           (apply #'call-process
                  (expand-file-name invocation-name invocation-directory)
                  nil t nil "-Q" "--batch" arguments)))
      (cons status (buffer-string)))))

(ert-deftest fumos-project-root-requires-mod-library-and-config ()
  (fumos-test-with-project root
    (make-directory (expand-file-name "scripts/battle" root) t)
    (should
     (equal (file-name-as-directory (file-truename root))
            (fumos-project-root (expand-file-name "scripts/battle" root))))
    (delete-file (expand-file-name "libraries/fumos/lib.json" root))
    (should-not (fumos-project-root root))))

(ert-deftest fumos-project-install-is-idempotent ()
  (let ((before (length fennel-mode-hook)))
    (fumos-project-install)
    (fumos-project-install)
    (should (= (1+ before) (length fennel-mode-hook)))
    (should (eq 'fennel-mode
                (cdr (assoc "\\.fnlm\\'" auto-mode-alist))))))

(ert-deftest fumos-mode-only-enables-inside-fumos-project ()
  (fumos-test-with-project root
    (let ((default-directory root))
      (with-temp-buffer
        (setq buffer-file-name (expand-file-name "mod.fnl" root))
        (fennel-mode)
        (fumos-project-activate)
        (should fumos-mode)
        (should (eq fumos-mode-map
                    (cdr (assq 'fumos-mode
                               minor-mode-overriding-map-alist))))
        (should (eq #'fumos-eval-last-sexp
                    (key-binding (kbd "C-x C-e") t)))))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/not-fumos.fnl")
      (fennel-mode)
      (fumos-project-activate)
      (should-not fumos-mode)
      (should-not (assq 'fumos-mode minor-mode-overriding-map-alist))
      (should-not
       (local-variable-p 'minor-mode-overriding-map-alist (current-buffer))))))

(ert-deftest fumos-first-open-selects-fennel-mode-and-activates-fumos ()
  (dolist (extension '("fnl" "fnlm"))
    (fumos-test-with-project root
      (let* ((entry (expand-file-name ".emacs/init.el" root))
             (source (expand-file-name (format "scripts/first.%s" extension)
                                       root))
             (loaded nil)
             (fumos-project--installed nil)
             (fennel-mode-hook
              (remove #'fumos-project-activate fennel-mode-hook))
             (loader
              (lambda (&rest _)
                (unless loaded
                  (setq loaded t)
                  (load entry nil 'nomessage)))))
        (with-temp-file entry
          (insert (format "(load %S nil 'nomessage)\n"
                          (expand-file-name "init.el" fumos-test-root))))
        (make-directory (file-name-directory source) t)
        (with-temp-file source (insert "(+ 1 2)\n"))
        (advice-add #'set-auto-mode :before loader)
        (unwind-protect
            (let ((buffer (find-file-noselect source)))
              (unwind-protect
                  (with-current-buffer buffer
                    (should (eq major-mode 'fennel-mode))
                    (should fumos-mode))
                (kill-buffer buffer)))
          (advice-remove #'set-auto-mode loader))))))

(ert-deftest fumos-project-entry-accepts-identical-preloaded-fennel ()
  (fumos-test-with-directory shadow
    (dolist (file '("fennel-mode.el" "fennel-proto-repl.el"))
      (copy-file (expand-file-name file
                                   (expand-file-name "vendor/fennel-mode"
                                                     fumos-test-root))
                 (expand-file-name file shadow)))
    (pcase-let ((`(,status . ,output)
                 (fumos-test--run-clean-emacs
                  "-L" shadow
                  "-l" (expand-file-name "fennel-mode.el" shadow)
                  "-l" (expand-file-name "fennel-proto-repl.el" shadow)
                  "-l" (expand-file-name "init.el" fumos-test-root)
                  "--eval" "(princ \"pinned-preload-ok\\n\")")))
      (should (equal 0 status))
      (should (string-match-p "pinned-preload-ok" output)))))

(ert-deftest fumos-project-entry-rejects-stale-preloaded-proto ()
  (fumos-test-with-directory shadow
    (dolist (file '("fennel-mode.el" "fennel-proto-repl.el"))
      (copy-file (expand-file-name file
                                   (expand-file-name "vendor/fennel-mode"
                                                     fumos-test-root))
                 (expand-file-name file shadow)))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "fennel-proto-repl.el" shadow))
      (goto-char (point-max))
      (insert "\n;; Simulated stale global package.\n")
      (write-region (point-min) (point-max)
                    (expand-file-name "fennel-proto-repl.el" shadow)
                    nil 'silent))
    (pcase-let ((`(,status . ,output)
                 (fumos-test--run-clean-emacs
                  "-L" shadow
                  "-l" (expand-file-name "fennel-mode.el" shadow)
                  "-l" (expand-file-name "fennel-proto-repl.el" shadow)
                  "-l" (expand-file-name "init.el" fumos-test-root))))
      (should-not (equal 0 status))
      (should (string-match-p "does not match the pinned FUMOS vendor"
                              output)))))

(ert-deftest fumos-project-entry-rejects-stale-preloaded-bytecode ()
  (fumos-test-with-directory shadow
    (dolist (file '("fennel-mode.el" "fennel-proto-repl.el"))
      (copy-file (expand-file-name file
                                   (expand-file-name "vendor/fennel-mode"
                                                     fumos-test-root))
                 (expand-file-name file shadow)))
    (let ((proto (expand-file-name "fennel-proto-repl.el" shadow))
          (load-path (cons shadow load-path)))
      (with-temp-buffer
        (insert-file-contents proto)
        (goto-char (point-min))
        (while (search-forward "0.6.4" nil t)
          (replace-match "0.6.3" t t))
        (goto-char (point-max))
        (insert "\n(defvar fumos-stale-bytecode-marker t)\n")
        (write-region (point-min) (point-max) proto nil 'silent))
      (should (byte-compile-file proto))
      (copy-file
       (expand-file-name "vendor/fennel-mode/fennel-proto-repl.el"
                         fumos-test-root)
       proto t))
    (pcase-let ((`(,status . ,output)
                 (fumos-test--run-clean-emacs
                  "-L" shadow
                  "-l" (expand-file-name "fennel-mode.el" shadow)
                  "-l" (expand-file-name "fennel-proto-repl.elc" shadow)
                  "-l" (expand-file-name "init.el" fumos-test-root))))
      (should-not (equal 0 status))
      (should (string-match-p
               "bytecode does not match the pinned FUMOS vendor"
               output)))))

(provide 'fumos-project-test)
;;; fumos-project-test.el ends here
