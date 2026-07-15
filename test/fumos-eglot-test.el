;;; fumos-eglot-test.el --- FUMOS Eglot tests -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'test-helper)
(require 'eglot)
(require 'fumos-eglot)

(defmacro fumos-eglot-test-with-project (root &rest body)
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

(ert-deftest fumos-fennel-ls-requires-pinned-binary ()
  (fumos-test-with-directory data
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "XDG_DATA_HOME" data)
      (should-error (fumos-fennel-ls-executable) :type 'user-error)
      (let ((binary
             (expand-file-name
              "kristal-emacs-config/fennel-ls/0c21b003/bin/fennel-ls"
              data)))
        (make-directory (file-name-directory binary) t)
        (with-temp-file binary (insert "#!/bin/sh\nexit 0\n"))
        (set-file-modes binary #o755)
        (should (equal binary (fumos-fennel-ls-executable)))))))

(ert-deftest fumos-fennel-ls-empty-xdg-data-home-uses-default ()
  (fumos-test-with-directory home
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "HOME" home)
      (setenv "XDG_DATA_HOME" "")
      (should
       (equal (file-name-as-directory
               (expand-file-name ".local/share" home))
              (fumos--xdg-data-home))))))

(ert-deftest fumos-fennel-ls-empty-home-without-xdg-fails-closed ()
  (dolist (home '(nil ""))
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "HOME" home)
      (setenv "XDG_DATA_HOME" "")
      (should-error (fumos--xdg-data-home) :type 'user-error))))

(ert-deftest fumos-install-project-config-refuses-overwrite ()
  (fumos-eglot-test-with-project root
    (let ((default-directory root))
      (with-temp-file (expand-file-name "flsproject.fnl" root)
        (insert "{:extra-globals \"custom\"}\n"))
      (should-error (fumos-install-project-config) :type 'user-error)
      (should
       (equal "{:extra-globals \"custom\"}\n"
              (with-temp-buffer
                (insert-file-contents (expand-file-name "flsproject.fnl" root))
                (buffer-string)))))))

(ert-deftest fumos-registers-only-fennel-eglot-entry ()
  (let ((eglot-server-programs
         '((lua-mode . ("lua-language-server"))
           (fennel-mode . ("old-fennel-ls")))))
    (fumos-register-fennel-eglot)
    (should (equal '("lua-language-server")
                   (cdr (assq 'lua-mode eglot-server-programs))))
    (should (eq #'fumos-fennel-ls-command
                (cdr (assq 'fennel-mode eglot-server-programs))))))

(provide 'fumos-eglot-test)
;;; fumos-eglot-test.el ends here
