;;; vendor-test.el --- Vendored dependency tests -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'test-helper)
(require 'subr-x)

(ert-deftest fumos-vendored-fennel-mode-is-pinned ()
  (let* ((vendor (expand-file-name "vendor/fennel-mode/" fumos-test-root))
         (upstream (expand-file-name "UPSTREAM" vendor)))
    (should (file-readable-p upstream))
    (should
     (equal
      (string-trim
       (with-temp-buffer
         (insert-file-contents upstream)
         (buffer-string)))
      (concat "https://git.sr.ht/~technomancy/fennel-mode\n"
              "bbc28a629405de628880d8fb485fce23ff7fab69")))
    (should (file-readable-p (expand-file-name "LICENSE" vendor)))
    (should (file-readable-p (expand-file-name "fennel-mode.el" vendor)))
    (should (file-readable-p (expand-file-name "fennel-proto-repl.el" vendor)))))

(ert-deftest fumos-vendored-proto-version-is-064 ()
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "vendor/fennel-mode/fennel-proto-repl.el"
                       fumos-test-root))
    (should (search-forward ";; Version: 0.6.4" nil t))
    (should (search-forward ":version \\\"0.6.4\\\"" nil t))))
