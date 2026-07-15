;;; fumos-eval-test.el --- FUMOS evaluation tests -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'test-helper)
(require 'fumos-repl-test)
(require 'fumos-eval)

(cl-defmacro fumos-test-with-fennel-file
    ((root file connection server) &rest body)
  "Create one real project FILE and ready CONNECTION for BODY."
  (declare (indent 1) (debug ((symbolp symbolp symbolp symbolp) body)))
  `(let* ((fumos-repl--connections (make-hash-table :test #'equal))
          (,root (fumos-test-make-project-root
                  (make-temp-file "fumos-eval-root-" t)))
          (,server
           (fumos-test-server-start (fumos-test-make-proto-handler 4242)))
          (,connection
           (fumos-repl-connect-instance
            (fumos-test-instance-for-server ,server 4242 ,root)))
          (,file (expand-file-name "scripts/foo.fnl" ,root))
          source-buffer)
     (unwind-protect
         (progn
           (should
            (fumos-test-wait-until
             (lambda () (eq 'ready
                            (fumos-connection-state ,connection)))))
           (should
            (fumos-test-wait-until
             (lambda ()
               (fumos-connection-macro-cache-valid ,connection))))
           (make-directory (file-name-directory ,file) t)
           (with-temp-file ,file)
           (setq source-buffer (find-file-noselect ,file))
           (with-current-buffer source-buffer
             (unless (eq major-mode 'fennel-mode) (fennel-mode))
             (setq-local fumos-repl--connection ,connection)
             (fumos-mode 1)
             (fumos-repl--link-buffer-to-connection
              ,connection source-buffer)
             ,@body))
       (when (buffer-live-p source-buffer)
         (with-current-buffer source-buffer
           (set-buffer-modified-p nil))
         (kill-buffer source-buffer))
       (fumos-repl-close ,connection)
       (fumos-test-server-stop ,server)
       (delete-directory ,root t))))

(defun fumos-test-read-eval-request (line)
  "Structurally parse one Fennel eval map LINE as an Elisp plist."
  (unless (and (stringp line)
               (> (length line) 1)
               (eq (aref line 0) ?\{)
               (eq (aref line (1- (length line))) ?\})
               (not (string-match-p "[\r\n]" line)))
    (ert-fail (format "Not one Fennel map line: %S" line)))
  (let* ((input (concat "(" (substring line 1 -1) ")"))
         (result (read-from-string input))
         (value (car result)))
    (unless (and (= (cdr result) (length input))
                 (proper-list-p value)
                 (cl-evenp (length value))
                 (integerp (plist-get value :id))
                 (stringp (plist-get value :eval)))
      (ert-fail (format "Malformed eval request: %S" line)))
    value))

(defun fumos-test-eval-request-p (line)
  "Return non-nil when LINE is a structurally valid eval request."
  (condition-case nil
      (progn (fumos-test-read-eval-request line) t)
    (error nil)))

(defun fumos-test-send-eval-values (server client line value)
  "Send one real accept/values/done chunk for request LINE."
  (let* ((request (fumos-test-read-eval-request line))
         (id (plist-get request :id)))
    (fumos-test-server-send
     server
     (format
      (concat "(:id %d :op \"accept\")\n"
              "(:id %d :op \"eval\" :values (%S))\n"
              "(:id %d :op \"done\")\n")
      id id value id)
     client)))

(defun fumos-test-send-eval-error (server client line &optional duplicate)
  "Send runtime error terminal and done for request LINE."
  (let* ((request (fumos-test-read-eval-request line))
         (id (plist-get request :id))
         (terminal
          (format
           (concat "(:id %d :op \"error\" :type \"runtime\" "
                   ":data \"boom\" :traceback \"trace\")\n")
           id)))
    (fumos-test-server-send
     server
     (concat (format "(:id %d :op \"accept\")\n" id)
             terminal
             (if duplicate terminal "")
             (format "(:id %d :op \"done\")\n" id))
     client)))

(defun fumos-test-install-eval-handler (server &optional value)
  "Install an automatic terminal handler on SERVER."
  (setf
   (fumos-test-server-handler server)
   (lambda (state client line)
     (when (fumos-test-eval-request-p line)
       (fumos-test-send-eval-values state client line (or value "3"))))))

(defun fumos-test-eval-lines-since (server count)
  "Return eval request lines received by SERVER after COUNT."
  (seq-filter #'fumos-test-eval-request-p
              (nthcdr count (fumos-test-server-lines server))))

(defun fumos-test-eval-settled-p (connection)
  "Return non-nil when CONNECTION owns no eval delivery resources."
  (let ((repl (fumos-connection-repl-buffer connection)))
    (and
     (or (not (buffer-live-p repl))
         (with-current-buffer repl
           (hash-table-empty-p fennel-proto-repl--message-callbacks)))
     (hash-table-empty-p
      (fumos-repl--callback-delivery-table connection))
     (null (fumos-connection-callback-timers connection))
     (null (fumos-connection-terminal-timers connection))
     (null (fumos-connection-terminal-deliveries connection))
     (null (fumos-connection-active-request-ids connection)))))

(defun fumos-test-assert-eval-settled (connection)
  "Wait for and assert complete eval cleanup on CONNECTION."
  (should
   (fumos-test-wait-until
    (lambda () (fumos-test-eval-settled-p connection)))))

(defun fumos-test-hook-contains-fumos-p (hook)
  "Return non-nil when current buffer's HOOK contains a FUMOS function."
  (seq-some
   (lambda (function)
     (string-match-p "fumos" (format "%S" function)))
   (ensure-list (symbol-value hook))))

(defun fumos-test-advice-contains-fumos-p (symbol)
  "Return non-nil when SYMBOL has advice whose identity contains FUMOS."
  (let (found)
    (advice-mapc
     (lambda (function _properties)
       (when (string-match-p "fumos" (format "%S" function))
         (setq found t)))
     symbol)
    found))

(ert-deftest fumos-eval-region-sends-utf8-byte-source-context-when-narrowed ()
  (fumos-test-with-fennel-file (root file connection server)
    (erase-buffer)
    (insert "\n\n\n\n\n\t中文😀(+ damage 1)\n")
    (let ((before (length (fumos-test-server-lines server))))
      (fumos-test-install-eval-handler server)
      (goto-char (point-min))
      (forward-line 5)
      (let ((line-begin (point)))
        (search-forward "😀")
        (let ((begin (point))
              (end (line-end-position)))
          (narrow-to-region (1+ line-begin) (point-max))
          (should (integerp (fumos-eval-region begin end)))))
      (should
       (fumos-test-wait-until
        (lambda ()
          (= 1 (length (fumos-test-eval-lines-since server before))))))
      (let ((request
             (fumos-test-read-eval-request
              (car (fumos-test-eval-lines-since server before)))))
        (should (equal "(+ damage 1)" (plist-get request :eval)))
        (should (equal "mods/demo/scripts/foo.fnl"
                       (plist-get request :file)))
        (should (= 6 (plist-get request :line)))
        (should (= 12 (plist-get request :column))))
      (fumos-test-assert-eval-settled connection))))

(ert-deftest fumos-eval-top-level-bounds-cover-fennel-forms ()
  (fumos-test-with-fennel-file (root file connection server)
    (erase-buffer)
    (insert (concat "(first)\n"
                    "(second (nested value))\n"
                    "[vector item]\n"
                    "{:key table-value}\n"
                    "top-atom\n"
                    "\"top string\"\n"))
    (let ((before (length (fumos-test-server-lines server)))
          (expected '("(second (nested value))"
                      "(second (nested value))"
                      "[vector item]"
                      "{:key table-value}"
                      "top-atom"
                      "\"top string\""))
          ids)
      (fumos-test-install-eval-handler server)
      (goto-char (point-min))
      (search-forward "(second")
      (goto-char (match-beginning 0))
      (push (fumos-eval-defun) ids)
      (search-forward "nested")
      (push (fumos-eval-defun) ids)
      (search-forward "vector")
      (push (fumos-eval-defun) ids)
      (search-forward "table-value")
      (push (fumos-eval-defun) ids)
      (search-forward "top-atom")
      (backward-char 3)
      (push (fumos-eval-defun) ids)
      (search-forward "top string")
      (backward-char 3)
      (push (fumos-eval-defun) ids)
      (should (seq-every-p #'integerp ids))
      (should
       (fumos-test-wait-until
        (lambda ()
          (= (length expected)
             (length (fumos-test-eval-lines-since server before))))))
      (should
       (equal expected
              (mapcar
               (lambda (line)
                 (plist-get (fumos-test-read-eval-request line) :eval))
               (fumos-test-eval-lines-since server before))))
      (fumos-test-assert-eval-settled connection))))

(ert-deftest fumos-eval-reader-bounds-preserve-fennel-hashfn ()
  (fumos-test-with-fennel-file (root file connection server)
    (erase-buffer)
    (insert "#(+ $1 $2)\n")
    (let ((before (length (fumos-test-server-lines server)))
          ids)
      (fumos-test-install-eval-handler server)
      ;; Point on the reader prefix and inside its body must both select the
      ;; complete hashfn, not the prefix or the otherwise-invalid $1 body.
      (goto-char (point-min))
      (push (fumos-eval-defun) ids)
      (search-forward "$1")
      (push (fumos-eval-defun) ids)
      ;; The standard last-expression commands share the same reader boundary.
      (search-forward ")")
      (push (fumos-eval-last-sexp) ids)
      (push (fumos-eval-print-last-sexp) ids)
      (should (seq-every-p #'integerp ids))
      (should
       (fumos-test-wait-until
        (lambda ()
          (= 4 (length (fumos-test-eval-lines-since server before))))))
      (should
       (equal (make-list 4 "#(+ $1 $2)")
              (mapcar
               (lambda (line)
                 (plist-get (fumos-test-read-eval-request line) :eval))
               (fumos-test-eval-lines-since server before))))
      (fumos-test-assert-eval-settled connection))))

(ert-deftest fumos-eval-top-level-bounds-handle-comments-narrowing-and-errors ()
  (fumos-test-with-fennel-file (root file connection server)
    (erase-buffer)
    (insert "; heading\n\n(next [inside])\n\n(last)\n; trailing\n")
    (let ((before (length (fumos-test-server-lines server)))
          (expected '("(next [inside])" "(last)" "(last)"
                      "(next [inside])")))
      (fumos-test-install-eval-handler server)
      (goto-char (point-min))
      (push (fumos-eval-defun) expected)
      (setq expected (cdr expected))
      (search-forward "\n\n(last)")
      (backward-char (length "(last)"))
      (push (fumos-eval-defun) expected)
      (setq expected (cdr expected))
      (goto-char (point-max))
      (push (fumos-eval-defun) expected)
      (setq expected (cdr expected))
      (goto-char (point-min))
      (search-forward "inside")
      (let ((inside (point)))
        (narrow-to-region (- inside 2) (1+ inside))
        (push (fumos-eval-defun) expected))
      (setq expected '("(next [inside])" "(last)" "(last)"
                       "(next [inside])"))
      (widen)
      (should
       (fumos-test-wait-until
        (lambda ()
          (= 4 (length (fumos-test-eval-lines-since server before))))))
      (should
       (equal expected
              (mapcar
               (lambda (line)
                 (plist-get (fumos-test-read-eval-request line) :eval))
               (fumos-test-eval-lines-since server before))))
      (fumos-test-assert-eval-settled connection)
      (let ((requests (length (fumos-test-server-lines server)))
            (callbacks
             (with-current-buffer (fumos-connection-repl-buffer connection)
               (hash-table-count fennel-proto-repl--message-callbacks)))
            (deliveries
             (hash-table-count
              (fumos-repl--callback-delivery-table connection))))
        (erase-buffer)
        (insert "(incomplete")
        (goto-char (point-max))
        (should-error (fumos-eval-defun) :type 'user-error)
        (erase-buffer)
        (insert "; comment only\n")
        (should-error
         (fumos-eval-region (point-min) (point-max)) :type 'user-error)
        (erase-buffer)
        (should-error
         (fumos-eval-region (point-min) (point-max)) :type 'user-error)
        (should (= requests (length (fumos-test-server-lines server))))
        (should
         (= callbacks
            (with-current-buffer (fumos-connection-repl-buffer connection)
              (hash-table-count fennel-proto-repl--message-callbacks))))
        (should
         (= deliveries
            (hash-table-count
             (fumos-repl--callback-delivery-table connection))))))))

(ert-deftest fumos-repl-eval-without-source-remains-standard-proto ()
  (fumos-test-with-ready-connection (connection server)
    (let ((before (length (fumos-test-server-lines server))))
      (fumos-test-install-eval-handler server)
      (with-current-buffer (fumos-connection-repl-buffer connection)
        (should
         (integerp
          (fumos-repl-send-eval "(+ 1 2)" nil (list :values #'ignore)))))
      (should
       (fumos-test-wait-until
        (lambda ()
          (= 1 (length (fumos-test-eval-lines-since server before))))))
      (let* ((line (car (fumos-test-eval-lines-since server before)))
             (request (fumos-test-read-eval-request line)))
        (should (equal (list :id (plist-get request :id)
                             :eval "(+ 1 2)")
                       request))
        (should-not (plist-member request :file)))
      (fumos-test-assert-eval-settled connection))))

(ert-deftest fumos-repl-eval-rejects-zero-id-and-rolls-back ()
  (fumos-test-with-ready-connection (connection server)
    (let ((before (length (fumos-test-server-lines server))))
      (with-current-buffer (fumos-connection-repl-buffer connection)
        ;; Zero is reserved for bootstrap and rejected by the game Session.
        (setq fennel-proto-repl--message-id 0)
        (should-error
         (fumos-repl-send-eval "42" nil (list :values #'ignore))
         :type 'user-error))
      (should (= before (length (fumos-test-server-lines server))))
      (fumos-test-assert-eval-settled connection))))

(ert-deftest fumos-repl-eval-quotes-code-and-source-as-one-frame ()
  (fumos-test-with-ready-connection (connection server)
    (let* ((before (length (fumos-test-server-lines server)))
           (code "(print \"quote\" \\\\)\n\r中文😀")
           (file "mods/demo/scripts/引号\\路径\"x\n\r.fnl")
           (source (list :file file :line 9 :column 17)))
      (fumos-test-install-eval-handler server "ok")
      (with-current-buffer (fumos-connection-repl-buffer connection)
        (should
         (integerp
          (fumos-repl-send-eval code source (list :values #'ignore)))))
      (should
       (fumos-test-wait-until
        (lambda ()
          (= 1 (length (fumos-test-eval-lines-since server before))))))
      (let* ((lines (fumos-test-eval-lines-since server before))
             (line (car lines))
             (request (fumos-test-read-eval-request line)))
        (should (= 1 (length lines)))
        (should-not (string-match-p "[\r\n]" line))
        (should (equal code (plist-get request :eval)))
        (should (equal file (plist-get request :file)))
        (should (= 9 (plist-get request :line)))
        (should (= 17 (plist-get request :column))))
      (fumos-test-assert-eval-settled connection))))

(ert-deftest fumos-repl-eval-formatter-and-send-nonlocal-exits-roll-back ()
  (fumos-test-with-fennel-file (root file connection server)
    (erase-buffer)
    (insert "42")
    (let ((original-copy-marker (symbol-function 'copy-marker))
          (original-table
           (symbol-function 'fumos-repl--callback-delivery-table)))
      (dolist (point '(assignment formatter send))
        (dolist (kind '(error quit))
          (let ((before (length (fumos-test-server-lines server)))
                markers condition assignment-failed)
            (cl-letf
                (((symbol-function 'copy-marker)
                  (lambda (&rest arguments)
                    (let ((marker (apply original-copy-marker arguments)))
                      (when (eq (marker-buffer marker) (current-buffer))
                        (push marker markers))
                      marker)))
                 ((symbol-function 'fumos-repl--callback-delivery-table)
                  (lambda (value)
                    (if (and (eq point 'assignment)
                             (not assignment-failed))
                        (progn
                          (setq assignment-failed t)
                          (signal kind '("assignment failure")))
                      (funcall original-table value))))
                 ((symbol-function 'fumos-repl--format-eval-request)
                  (if (eq point 'formatter)
                      (lambda (&rest _)
                        (signal kind '("formatter failure")))
                    (symbol-function 'fumos-repl--format-eval-request)))
                 ((symbol-function 'fennel-proto-repl--send-string)
                  (if (eq point 'send)
                      (lambda (&rest _)
                        (signal kind '("send failure")))
                    (symbol-function 'fennel-proto-repl--send-string))))
              (setq condition
                    (condition-case caught
                        (progn
                          (fumos-eval-region (point-min) (point-max))
                          (ert-fail "Injected eval failure did not propagate"))
                      ((error quit) caught))))
            (should (eq kind (car condition)))
            (should (= 1 (length markers)))
            (should-not (marker-buffer (car markers)))
            (should-not (marker-position (car markers)))
            (should (= before (length (fumos-test-server-lines server))))
            (fumos-test-assert-eval-settled connection)))))))

(ert-deftest fumos-repl-eval-rejects-over-8mib-after-utf8-encoding ()
  (fumos-test-with-ready-connection (connection server)
    (let* ((code (make-string 2796203 ?中))
           (before (length (fumos-test-server-lines server)))
           sent condition)
      (should (< (length code) 8388608))
      (should (> (string-bytes (encode-coding-string code 'utf-8-unix))
                 8388608))
      (with-current-buffer (fumos-connection-repl-buffer connection)
        (cl-letf (((symbol-function 'fennel-proto-repl--send-string)
                   (lambda (&rest _) (setq sent t))))
          (setq condition
                (condition-case caught
                    (progn
                      (fumos-repl-send-eval
                       code nil (list :values #'ignore))
                      (ert-fail "Oversized eval request was accepted"))
                  (user-error caught)))))
      (should condition)
      (should-not sent)
      (should (= before (length (fumos-test-server-lines server))))
      (fumos-test-assert-eval-settled connection))))

(ert-deftest fumos-eval-source-rejects-remote-outside-and-symlink-escape ()
  (fumos-test-with-fennel-file (root file connection server)
    (erase-buffer)
    (insert "(+ 1 2)")
    (let* ((instance (fumos-connection-instance connection))
           (canonical-root (fumos-instance-project-root instance))
           (outside (make-temp-file "fumos-eval-outside-" nil ".fnl"))
           (escape (expand-file-name "scripts/escape.fnl" root))
           (loop-a (expand-file-name "scripts/loop-a.fnl" root))
           (loop-b (expand-file-name "scripts/loop-b.fnl" root))
           (before (length (fumos-test-server-lines server))))
      (unwind-protect
          (progn
            (make-symbolic-link outside escape)
            (make-symbolic-link loop-b loop-a)
            (make-symbolic-link loop-a loop-b)
            (dolist (candidate
                     (list "/ssh:fumos@example.invalid:/tmp/remote.fnl"
                           outside escape loop-a))
              (setq buffer-file-name candidate)
              (let ((canonicalized nil)
                    (original-truename (symbol-function 'file-truename)))
                (if (file-remote-p candidate)
                    (cl-letf (((symbol-function 'file-truename)
                               (lambda (&rest arguments)
                                 (setq canonicalized t)
                                 (apply original-truename arguments))))
                      (should-error
                       (fumos-eval-region (point-min) (point-max))
                       :type 'user-error)
                      (should-not canonicalized))
                  (should-error
                   (fumos-eval-region (point-min) (point-max))
                   :type 'user-error)))
              (should (= before (length (fumos-test-server-lines server))))
              (fumos-test-assert-eval-settled connection))
            ;; The authenticated root is subject to the same pre-canonicalization
            ;; local-path gate as the source file.
            (setq buffer-file-name file)
            (setf (fumos-instance-project-root instance)
                  "/ssh:fumos@example.invalid:/tmp/project/")
            (let ((canonicalized nil))
              (cl-letf (((symbol-function 'file-truename)
                         (lambda (&rest _)
                           (setq canonicalized t)
                           (ert-fail "Remote project root was canonicalized"))))
                (should-error
                 (fumos-eval-region (point-min) (point-max))
                 :type 'user-error))
              (should-not canonicalized))
            (should (= before (length (fumos-test-server-lines server))))
            (fumos-test-assert-eval-settled connection))
        (setf (fumos-instance-project-root instance) canonical-root)
        (setq buffer-file-name file)
        (when (file-exists-p outside) (delete-file outside))))))

(ert-deftest fumos-eval-source-canonicalizes-in-project-symlink ()
  (fumos-test-with-fennel-file (root file connection server)
    (let* ((target (expand-file-name "scripts/real.fnl" root))
           (alias (expand-file-name "scripts/alias.fnl" root))
           (before (length (fumos-test-server-lines server))))
      (with-temp-file target (insert "(+ 40 2)\n"))
      (make-symbolic-link target alias)
      (setq buffer-file-name alias)
      (erase-buffer)
      (insert "(+ 40 2)")
      (fumos-test-install-eval-handler server "42")
      (should
       (integerp (fumos-eval-region (point-min) (point-max))))
      (should
       (fumos-test-wait-until
        (lambda ()
          (= 1 (length (fumos-test-eval-lines-since server before))))))
      (let* ((request
              (fumos-test-read-eval-request
               (car (fumos-test-eval-lines-since server before))))
             (virtual (plist-get request :file))
             (relative (string-remove-prefix "mods/demo/" virtual))
             (segments (split-string relative "/" nil)))
        (should (equal "mods/demo/scripts/real.fnl" virtual))
        (should (string-match-p "\\.fnlm?\\'" relative))
        (should-not (file-name-absolute-p relative))
        (should-not (string-search "\\" relative))
        (should-not (string-search "//" relative))
        (should
         (cl-every
          (lambda (segment) (not (member segment '("" "." ".."))))
          segments)))
      (fumos-test-assert-eval-settled connection))))

(ert-deftest fumos-eval-overlay-and-echo-survive-real-deferred-delivery ()
  (fumos-test-with-fennel-file (root file connection server)
    (erase-buffer)
    (insert "(game-call)\n(next-form)\n")
    (goto-char (point-min))
    (setf (fumos-test-server-handler server) nil)
    (let* ((source (current-buffer))
           (other (generate-new-buffer " *fumos-eval-other*"))
           (before (length (fumos-test-server-lines server)))
           (window-before (window-buffer (selected-window)))
           ids lines overlays messages
           (expected-position
            (save-excursion
              (goto-char (point-min))
              (search-forward ")")
              (point))))
      (unwind-protect
          (progn
            (cl-letf
                (((symbol-function 'fennel-proto-repl-send-message-sync)
                  (lambda (&rest _)
                    (ert-fail "Explicit eval used sync transport")))
                 ((symbol-function 'accept-process-output)
                  (lambda (&rest _)
                    (ert-fail "Explicit eval blocked for network output"))))
              (push (fumos-eval-defun-overlay) ids)
              (should (eq source (current-buffer)))
              (should (eq window-before (window-buffer (selected-window))))
              (push (fumos-eval-defun-async) ids)
              (should (eq source (current-buffer)))
              (should (eq window-before (window-buffer (selected-window))))
              (push (fumos-eval-defun) ids)
              (should (eq source (current-buffer)))
              (should (eq window-before (window-buffer (selected-window)))))
            (setq ids (nreverse ids))
            (should (seq-every-p #'integerp ids))
            (should (= 3 (length (delete-dups (copy-sequence ids)))))
            (should
             (fumos-test-wait-until
              (lambda ()
                (= 3
                   (length
                    (fumos-test-eval-lines-since server before))))))
            (setq lines (fumos-test-eval-lines-since server before))
            ;; Insert exactly where a marker incorrectly placed after the LF
            ;; would sit.  The result must remain attached to game-call.
            (goto-char (point-min))
            (forward-line 1)
            (insert "(inserted-before-next)\n")
            (with-current-buffer other
              (cl-letf
                  (((symbol-function 'eros-eval-overlay)
                    (lambda (text position)
                      (let ((overlay
                             (make-overlay position position
                                           (current-buffer))))
                        (overlay-put overlay 'fumos-result text)
                        (push overlay overlays))))
                   ((symbol-function 'message)
                    (lambda (format-string &rest arguments)
                      (when (stringp format-string)
                        (push (apply #'format format-string arguments)
                              messages)))))
                (cl-mapc
                 (lambda (line value)
                   (fumos-test-send-eval-values
                    server (fumos-test-server-client server) line value))
                 lines '("overlay-value" "async-value" "standard-value"))
                (should
                 (fumos-test-wait-until
                  (lambda ()
                    (and (fumos-test-eval-settled-p connection)
                         (= 1 (length overlays))
                         (seq-every-p
                          (lambda (value)
                            (member value messages))
                          '("overlay-value" "async-value"
                            "standard-value"))))))
                (should (eq other (current-buffer)))))
            (should (= 1 (length overlays)))
            (let ((overlay (car overlays)))
              (should (overlayp overlay))
              (should (eq source (overlay-buffer overlay)))
              (should (= expected-position (overlay-start overlay)))
              (should
               (equal "overlay-value"
                      (substring-no-properties
                       (overlay-get overlay 'fumos-result)))))
            (fumos-test-assert-eval-settled connection))
        (mapc #'delete-overlay overlays)
        (when (buffer-live-p other) (kill-buffer other))))))

(ert-deftest fumos-eval-error-and-disconnect-release-result-markers ()
  ;; Runtime error and duplicate terminal frames share one finalizer.
  (fumos-test-with-fennel-file (root file connection server)
    (erase-buffer)
    (insert "(runtime-failure)")
    (setf (fumos-test-server-handler server) nil)
    (let ((source (current-buffer))
          (before (length (fumos-test-server-lines server)))
          (original-copy-marker (symbol-function 'copy-marker))
          (original-set-marker (symbol-function 'set-marker))
          markers errors (finalize-count 0))
      (cl-letf
          (((symbol-function 'copy-marker)
            (lambda (&rest arguments)
              (let ((marker (apply original-copy-marker arguments)))
                (when (eq source (marker-buffer marker))
                  (push marker markers))
                marker)))
           ((symbol-function 'set-marker)
            (lambda (marker position &optional buffer)
              (when (and (null position) (memq marker markers))
                (cl-incf finalize-count))
              (funcall original-set-marker marker position buffer)))
           ((symbol-function 'fumos-repl--default-error-handler)
            (lambda (&rest arguments) (push arguments errors))))
        (should
         (integerp (fumos-eval-region (point-min) (point-max))))
        (should
         (fumos-test-wait-until
          (lambda ()
            (= 1 (length (fumos-test-eval-lines-since server before))))))
        (fumos-test-send-eval-error
         server (fumos-test-server-client server)
         (car (fumos-test-eval-lines-since server before)) t)
        (should
         (fumos-test-wait-until
          (lambda ()
            (and errors (fumos-test-eval-settled-p connection)))))
        (should (= 1 (length markers)))
        (should (= 1 (length errors)))
        (should (equal '("runtime" "boom" "trace") (car errors)))
        (should (= 1 finalize-count))
        (should-not (marker-buffer (car markers)))
        (should-not (marker-position (car markers))))))
  ;; A request without a protocol terminal is finalized by connection-lost.
  (fumos-test-with-fennel-file (root file connection server)
    (erase-buffer)
    (insert "(+ 1 2)")
    (goto-char (point-max))
    (setf (fumos-test-server-handler server) nil)
    (let ((source (current-buffer))
          (before (length (fumos-test-server-lines server)))
          (original-copy-marker (symbol-function 'copy-marker))
          (original-set-marker (symbol-function 'set-marker))
          markers errors (finalize-count 0))
      (cl-letf
          (((symbol-function 'copy-marker)
            (lambda (&rest arguments)
              (let ((marker (apply original-copy-marker arguments)))
                (when (eq source (marker-buffer marker))
                  (push marker markers))
                marker)))
           ((symbol-function 'set-marker)
            (lambda (marker position &optional buffer)
              (when (and (null position) (memq marker markers))
                (cl-incf finalize-count))
              (funcall original-set-marker marker position buffer)))
           ((symbol-function 'fumos-repl--default-error-handler)
            (lambda (&rest arguments) (push arguments errors))))
        (should (integerp (fumos-eval-print-last-sexp)))
        (should
         (fumos-test-wait-until
          (lambda ()
            (= 1 (length (fumos-test-eval-lines-since server before))))))
        (fumos-test-server-drop-client server)
        (should
         (fumos-test-wait-until
          (lambda ()
            (and errors (fumos-test-eval-settled-p connection)))))
        (should (= 1 (length markers)))
        (should (= 1 (length errors)))
        (should (equal "connection-lost" (caar errors)))
        (should (= 1 finalize-count))
        (should-not (marker-buffer (car markers)))
        (should-not (marker-position (car markers)))))))

(ert-deftest fumos-dead-source-real-callback-does-not-block-done-cleanup ()
  (fumos-test-with-fennel-file (root file connection server)
    (erase-buffer)
    (insert "(+ 1 2)")
    (setf (fumos-test-server-handler server) nil)
    (let ((source (current-buffer))
          (before (length (fumos-test-server-lines server)))
          (original-copy-marker (symbol-function 'copy-marker))
          (original-set-marker (symbol-function 'set-marker))
          marker (finalize-count 0))
      (cl-letf
          (((symbol-function 'copy-marker)
            (lambda (&rest arguments)
              (let ((value (apply original-copy-marker arguments)))
                (when (eq source (marker-buffer value))
                  (setq marker value))
                value)))
           ((symbol-function 'set-marker)
            (lambda (value position &optional buffer)
              (when (and (eq value marker) (null position))
                (cl-incf finalize-count))
              (funcall original-set-marker value position buffer))))
        (should
         (integerp (fumos-eval-region (point-min) (point-max))))
        (should
         (fumos-test-wait-until
          (lambda ()
            (= 1 (length (fumos-test-eval-lines-since server before))))))
        (kill-buffer source)
        (fumos-test-send-eval-values
         server (fumos-test-server-client server)
         (car (fumos-test-eval-lines-since server before)) "3")
        (should
         (fumos-test-wait-until
          (lambda () (fumos-test-eval-settled-p connection))))
        (should marker)
        (should (= 1 finalize-count))
        (should-not (marker-buffer marker))
        (should-not (marker-position marker))))))

(ert-deftest fumos-save-buffer-never-evaluates-compiles-or-reloads ()
  (fumos-test-with-fennel-file (root file connection server)
    (erase-buffer)
    (insert "(+ 1 2)\n")
    (set-buffer-modified-p t)
    (let* ((repl (fumos-connection-repl-buffer connection))
           (before (length (fumos-test-server-lines server)))
           (callbacks
            (with-current-buffer repl
              (hash-table-count fennel-proto-repl--message-callbacks)))
           (deliveries
            (hash-table-count
             (fumos-repl--callback-delivery-table connection)))
           (callback-timers
            (copy-sequence (fumos-connection-callback-timers connection)))
           (terminal-timers
            (copy-sequence (fumos-connection-terminal-timers connection))))
      (dolist (hook '(before-save-hook after-save-hook
                      write-file-functions write-contents-functions))
        (should-not (fumos-test-hook-contains-fumos-p hook)))
      (dolist (function '(save-buffer basic-save-buffer write-region))
        (should-not (fumos-test-advice-contains-fumos-p function)))
      (cl-letf (((symbol-function 'fumos-repl-send-eval)
                 (lambda (&rest _)
                   (ert-fail "save-buffer evaluated Fennel")))
                ((symbol-function 'fumos-reload-current-file)
                 (lambda (&rest _)
                   (ert-fail "save-buffer reloaded Fennel")))
                ((symbol-function 'fennel-proto-repl-send-message)
                 (lambda (&rest _)
                   (ert-fail "save-buffer used proto transport"))))
        (save-buffer)
        (accept-process-output nil 0.05))
      (should-not (buffer-modified-p))
      (should (equal "(+ 1 2)\n"
                     (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))))
      (should (= before (length (fumos-test-server-lines server))))
      (should
       (= callbacks
          (with-current-buffer repl
            (hash-table-count fennel-proto-repl--message-callbacks))))
      (should
       (= deliveries
          (hash-table-count
           (fumos-repl--callback-delivery-table connection))))
      (should (equal callback-timers
                     (fumos-connection-callback-timers connection)))
      (should (equal terminal-timers
                     (fumos-connection-terminal-timers connection)))
      (dolist (hook '(before-save-hook after-save-hook
                      write-file-functions write-contents-functions))
        (should-not (fumos-test-hook-contains-fumos-p hook)))
      (dolist (function '(save-buffer basic-save-buffer write-region))
        (should-not (fumos-test-advice-contains-fumos-p function))))))

(provide 'fumos-eval-test)
;;; fumos-eval-test.el ends here
