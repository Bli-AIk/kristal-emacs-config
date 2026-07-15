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

(defun fumos-test-send-eval-runtime-error
    (server client line message &optional type)
  "Send a real eval error MESSAGE and done for request LINE."
  (let* ((request (fumos-test-read-eval-request line))
         (id (plist-get request :id)))
    (fumos-test-server-send
     server
     (format
      (concat "(:id %d :op \"accept\")\n"
              "(:id %d :op \"error\" :type %S :data %S :traceback nil)\n"
              "(:id %d :op \"done\")\n")
      id id (or type "runtime") message id)
     client)))

(defun fumos-test-send-compile-error (server client line message)
  "Send a real compile error MESSAGE and done for command LINE."
  (let* ((request (fumos-test-read-command-request line))
         (id (plist-get request :id)))
    (fumos-test-server-send
     server
     (format
      (concat "(:id %d :op \"accept\")\n"
              "(:id %d :op \"error\" :type \"compile\" "
              ":data %S :traceback nil)\n"
              "(:id %d :op \"done\")\n")
      id id message id)
     client)))

(defun fumos-test-reload-eval-line-p (line)
  "Return non-nil when LINE is a semantic source reload request."
  (and (fumos-test-eval-request-p line)
       (string-match-p
        (regexp-quote "_G.Mod.libs.fumos.reload")
        (plist-get (fumos-test-read-eval-request line) :eval))))

(ert-deftest fumos-reload-current-file-uses-attached-canonical-source ()
  (fumos-test-with-fennel-file (root file connection server)
    (let ((paths '("mod.fnl"
                   "scripts/waves/spiral.fnl"
                   "fnl/combat/math.fnl"
                   "fnl/combat/macros.fnlm"))
          reload-lines)
      (setf
       (fumos-test-server-handler server)
       (lambda (state client line)
         (cond
          ((and (fumos-test-eval-request-p line)
                (string-match-p "macro-loaded" line))
           (fumos-test-send-eval-values state client line "nil"))
          ((fumos-test-reload-eval-line-p line)
           (push line reload-lines)
           (fumos-test-send-eval-values state client line "true")))))
      (dolist (relative paths)
        (let ((absolute (expand-file-name relative root)))
          (make-directory (file-name-directory absolute) t)
          (with-temp-file absolute (insert "; saved\n"))
          (setq buffer-file-name absolute)
          (set-buffer-modified-p nil)
          (let ((before (length reload-lines)))
            (should (integerp (fumos-reload-current-file)))
            (should
             (fumos-test-wait-until
              (lambda ()
                (and (= (1+ before) (length reload-lines))
                     (fumos-connection-macro-cache-valid connection)))))
            (let* ((request
                    (fumos-test-read-eval-request (car reload-lines)))
                   (code (plist-get request :eval))
                   (guard
                    (string-match
                     (regexp-quote
                      "(assert (= _G _G._G) \"FUMOS tooling requires unshadowed _G\")")
                     code))
                   (call
                    (string-match
                     (regexp-quote "(_G.Mod.libs.fumos.reload") code)))
              (should (integerp guard))
              (should (integerp call))
              (should (< guard call))
              (should
               (string-match-p
                (regexp-quote (format "{:path %S}" relative)) code))
              (should-not (string-match-p "([^_]Mod\\.libs" code))
              (should
               (equal (concat "mods/demo/" relative)
                      (plist-get request :file)))))))
      (should (= (length paths) (length reload-lines))))))

(ert-deftest fumos-reload-rejects-unsaved-unsupported-and-shadowed-global ()
  (fumos-test-with-fennel-file (root file connection server)
    (erase-buffer)
    (insert "(local Mod {})\n(local Kristal {})\n(local _G {})\n")
    (let* ((outside (make-temp-file "fumos-reload-outside-" nil ".fnl"))
           (escape (expand-file-name "scripts/escape.fnl" root))
           (original-file file)
           (before (length (fumos-test-server-lines server)))
           guard-line)
      (unwind-protect
          (progn
            (make-symbolic-link outside escape)
            (set-buffer-modified-p t)
            (cl-letf (((symbol-function 'save-buffer)
                       (lambda (&rest _)
                         (ert-fail "reload implicitly saved the source"))))
              (should-error (fumos-reload-current-file) :type 'user-error))
            (set-buffer-modified-p nil)
            (dolist (candidate
                     (list nil
                           outside
                           "/ssh:fumos@example.invalid:/tmp/source.fnl"
                           escape
                           (expand-file-name "scripts/not-fennel.lua" root)
                           (expand-file-name "scripts/not-supported.fnlm" root)))
              (setq buffer-file-name candidate)
              (should-error (fumos-reload-current-file) :type 'user-error))
            (should (= before (length (fumos-test-server-lines server))))
            (setq buffer-file-name original-file)
            (setf
             (fumos-test-server-handler server)
             (lambda (state client line)
               (when (fumos-test-reload-eval-line-p line)
                 (setq guard-line line)
                 (fumos-test-send-eval-runtime-error
                  state client line
                  "FUMOS tooling requires unshadowed _G"))))
            (should (integerp (fumos-reload-current-file)))
            (should (fumos-test-wait-until (lambda () guard-line)))
            (let* ((code
                    (plist-get
                     (fumos-test-read-eval-request guard-line) :eval))
                   (guard (string-match "(assert (= _G _G\\._G)" code))
                   (side-effect
                    (string-match "(_G\\.Mod\\.libs\\.fumos\\.reload" code)))
              (should (integerp guard))
              (should (integerp side-effect))
              (should (< guard side-effect))
              (should-not (string-match-p "(Kristal\\.quickReload" code))))
        (setq buffer-file-name original-file)
        (set-buffer-modified-p nil)
        (when (file-exists-p outside) (delete-file outside))))))

(ert-deftest fumos-reload-remote-source-fails-before-project-discovery ()
  (with-temp-buffer
    (setq buffer-file-name
          "/ssh:fumos@example.invalid:/tmp/scripts/remote.fnl"
          default-directory "/ssh:fumos@example.invalid:/tmp/")
    (set-buffer-modified-p nil)
    (let ((fumos-repl--connection nil))
      (cl-letf (((symbol-function 'fumos-project-root)
                 (lambda (&rest _)
                   (ert-fail "Remote reload attempted project discovery"))))
        (should-error (fumos-reload-current-file) :type 'user-error)))))

(ert-deftest fumos-reload-success-invalidates-macro-cache-and-refreshes ()
  (fumos-test-with-fennel-file (root file connection server)
    (erase-buffer)
    (insert "(+ 1 2)\n")
    (set-buffer-modified-p nil)
    (let ((baseline '(("old.macros" "old")))
          (mode 'source-success)
          (refreshes 0)
          initial-epoch)
      (setf (fumos-connection-macro-cache connection) baseline
            (fumos-connection-macro-cache-valid connection) t)
      (setq initial-epoch
            (fumos-connection-macro-refresh-epoch connection))
      (setf
       (fumos-test-server-handler server)
       (lambda (state client line)
         (cond
          ((and (fumos-test-eval-request-p line)
                (string-match-p "macro-loaded" line))
           (cl-incf refreshes)
           (fumos-test-send-eval-values
            state client line "[[\"fresh.macros\" \"new\"]]"))
          ((fumos-test-reload-eval-line-p line)
           (pcase mode
             ('source-success
              (fumos-test-send-eval-values state client line "true"))
             ('failure-report
              (fumos-test-server-send
               state
               (let* ((request (fumos-test-read-eval-request line))
                      (id (plist-get request :id)))
                 (format
                  (concat "(:id %d :op \"accept\")\n"
                          "(:id %d :op \"eval\" :values "
                          "(\"false\" \"report\"))\n"
                          "(:id %d :op \"done\")\n")
                  id id id))
               client))
             ('error
              (fumos-test-send-eval-runtime-error
               state client line "reload failed"))))
          ((fumos-test-command-request-p line)
           (fumos-test-send-command-values state client line "module")))))
      (should (integerp (fumos-reload-current-file)))
      (should
       (fumos-test-wait-until
        (lambda ()
          (and (= 1 refreshes)
               (fumos-connection-macro-cache-valid connection)
               (equal '(("fresh.macros" "new"))
                      (fumos-connection-macro-cache connection))))))
      (should (> (fumos-connection-macro-refresh-epoch connection)
                 initial-epoch))
      (setf (fumos-connection-macro-cache connection) baseline
            (fumos-connection-macro-cache-valid connection) t)
      (let ((before-epoch
             (fumos-connection-macro-refresh-epoch connection)))
        (setq mode 'failure-report)
        (should (integerp (fumos-reload-current-file)))
        (fumos-test-assert-eval-settled connection)
        (should (= before-epoch
                   (fumos-connection-macro-refresh-epoch connection)))
        (should (fumos-connection-macro-cache-valid connection))
        (should (equal baseline (fumos-connection-macro-cache connection))))
      (let ((before-epoch
             (fumos-connection-macro-refresh-epoch connection)))
        (setq mode 'error)
        (should (integerp (fumos-reload-current-file)))
        (fumos-test-assert-eval-settled connection)
        (should (= before-epoch
                   (fumos-connection-macro-refresh-epoch connection))))
      (setq mode 'module-success)
      (setf (fumos-connection-macro-cache connection) baseline
            (fumos-connection-macro-cache-valid connection) t)
      (let ((before refreshes))
        (should (integerp (fumos-reload-module "demo.module")))
        (should
         (fumos-test-wait-until
          (lambda ()
            (and (= (1+ before) refreshes)
                 (fumos-connection-macro-cache-valid connection))))))))
  ;; A transport loss is not a successful reload and starts no refresh.
  (fumos-test-with-fennel-file (root file connection server)
    (let ((refreshes 0)
          (epoch (fumos-connection-macro-refresh-epoch connection)))
      (set-buffer-modified-p nil)
      (setf
       (fumos-test-server-handler server)
       (lambda (state _client line)
         (cond
          ((string-match-p "macro-loaded" line) (cl-incf refreshes))
          ((fumos-test-reload-eval-line-p line)
           (fumos-test-server-drop-client state)))))
      (fumos-reload-current-file)
      (should
       (fumos-test-wait-until
        (lambda () (eq 'disconnected (fumos-connection-state connection)))))
      (should (= 0 refreshes))
      (should (> (fumos-connection-macro-refresh-epoch connection) epoch))
      (should-not (fumos-connection-macro-refresh-pending connection)))))

(ert-deftest fumos-compile-buffer-is-one-do-unit-with-no-trailing-eval ()
  (fumos-test-with-fennel-file (root file connection server)
    (erase-buffer)
    (insert (concat "(macro twice [x] `(+ ,x ,x))\n"
                    "(set _G.fumos_compile_first true)\n"
                    "(set _G.fumos_compile_second (twice 2))"))
    (let ((source (buffer-string))
          (before (length (fumos-test-server-lines server))))
      (setf
       (fumos-test-server-handler server)
       (lambda (state client line)
         (when (fumos-test-command-request-p line)
           (fumos-test-send-command-values
            state client line
            "fumos_compile_first = true\nfumos_compile_second = (2 + 2)"))))
      (should (integerp (fumos-compile-buffer)))
      (should
       (fumos-test-wait-until
        (lambda ()
          (and (= 1 (length (fumos-test-command-lines-since server before)))
               (fumos-test-eval-settled-p connection)))))
      (let* ((commands (fumos-test-command-lines-since server before))
             (request (fumos-test-read-command-request (car commands))))
        (should (= 1 (length commands)))
        (should (plist-member request :compile))
        (should (equal (concat "(do\n" source "\n)")
                       (plist-get request :compile))))
      (should-not (fumos-test-eval-lines-since server before))
      (when-let* ((buffer (get-buffer "*FUMOS Lua*")))
        (kill-buffer buffer)))))

(ert-deftest fumos-compile-buffer-real-result-shows-complete-lua ()
  (fumos-test-with-fennel-file (root file connection server)
    (erase-buffer)
    (insert "(do (set _G.first true) (set _G.second (+ 2 2)))")
    (let ((lua (concat "_G.first = true\n"
                       "_G.second = (2 + 2)\n"
                       "return _G.second")))
      (unwind-protect
          (progn
            (setf
             (fumos-test-server-handler server)
             (lambda (state client line)
               (when (fumos-test-command-request-p line)
                 (fumos-test-send-command-values state client line lua))))
            (fumos-compile-buffer)
            (should
             (fumos-test-wait-until
              (lambda ()
                (and (get-buffer "*FUMOS Lua*")
                     (fumos-test-eval-settled-p connection)))))
            (with-current-buffer "*FUMOS Lua*"
              (should (equal (concat lua "\n") (buffer-string)))
              (should buffer-read-only)
              (should (eq major-mode 'prog-mode))))
        (when-let* ((buffer (get-buffer "*FUMOS Lua*")))
          (kill-buffer buffer))))))

(ert-deftest fumos-compile-error-remaps-pinned-session-locus ()
  (fumos-test-with-fennel-file (root file connection server)
    (erase-buffer)
    (insert "(bad-first)\n(bad-second)\n\t中文(bad-third)\n")
    (let ((responses
           '("Error compiling expression: unknown:3:0: first"
             "Error compiling expression: unknown:4:0: second"
             "Error compiling expression: unknown:5:3: unicode"
             "Compile error: unknown:3:0: hostile-prefix"
             "Error compiling expression: unknown:2:0: wrapper"))
          received)
      (setf
       (fumos-test-server-handler server)
       (lambda (state client line)
         (when (fumos-test-command-request-p line)
           (fumos-test-send-compile-error
            state client line (pop responses)))))
      (cl-letf
          (((symbol-function 'fumos-repl--default-error-handler)
            (lambda (type message traceback)
              (should-not compilation-error-screen-columns)
              (push (list type message traceback) received))))
        (dolist (expected
                 '("Error compiling expression: mods/demo/scripts/foo.fnl:1:1: first"
                   "Error compiling expression: mods/demo/scripts/foo.fnl:2:1: second"
                   "Error compiling expression: mods/demo/scripts/foo.fnl:3:4: unicode"
                   "Compile error: unknown:3:0: hostile-prefix"
                   "Error compiling expression: unknown:2:0: wrapper"))
          (let ((before (length received)))
            (fumos-compile-buffer)
            (should
             (fumos-test-wait-until
              (lambda () (= (1+ before) (length received)))))
            (should (equal expected (cadar received)))))
        (let ((original-file buffer-file-name)
              (before (length received)))
          (setq buffer-file-name nil)
          (setf
           (fumos-test-server-handler server)
           (lambda (state client line)
             (when (fumos-test-command-request-p line)
               (fumos-test-send-compile-error
                state client line
                "Error compiling expression: unknown:3:0: no-source"))))
          (fumos-compile-buffer)
          (should
           (fumos-test-wait-until
            (lambda () (= (1+ before) (length received)))))
          (should
           (equal "Error compiling expression: unknown:3:0: no-source"
                  (cadar received)))
          (setq buffer-file-name original-file))
        (erase-buffer)
        (insert "; prefix\n\t中文(bad-offset)\n")
        (goto-char (point-min))
        (search-forward "(bad-offset)")
        (backward-char (length "(bad-offset)"))
        (let ((before (length received)))
          (setf
           (fumos-test-server-handler server)
           (lambda (state client line)
             (when (fumos-test-command-request-p line)
               (fumos-test-send-compile-error
                state client line
                "Error compiling expression: unknown:3:0: offset"))))
          (fumos-compile-defun)
          (should
           (fumos-test-wait-until
            (lambda () (= (1+ before) (length received)))))
          (should
           (equal
            "Error compiling expression: mods/demo/scripts/foo.fnl:2:4: offset"
            (cadar received))))
        (let* ((source (fumos-eval--source (point-min)))
               (callback
                (fumos-eval--compile-error-callback
                 connection (fumos-connection-process connection)
                 (1- (fumos-connection-generation connection))
                 source 1 1))
               stale)
          (cl-letf (((symbol-function 'fumos-repl--default-error-handler)
                     (lambda (_type message _traceback)
                       (setq stale message))))
            (funcall callback
                     "compile"
                     "Error compiling expression: unknown:3:0: stale"
                     nil))
          (should
           (equal "Error compiling expression: unknown:3:0: stale" stale)))))))

(ert-deftest fumos-compile-request-counts-wrapper-overhead-at-8mib ()
  (fumos-test-with-fennel-file (root file connection server)
    (let ((base "; quote=\" slash=\\ cr=\r\n; 中文\n")
          (repl (fumos-connection-repl-buffer connection))
          captured)
      (cl-labels
          ((wire
            (id source)
            (string-replace
             "\r" "\\r"
             (fennel-proto-repl--format-message
              id :compile (concat "(do\n" source "\n)") t)))
           (exact-source
            (id)
            (let* ((base-bytes (fumos-repl--utf8-bytes (wire id base)))
                   (padding (- fumos-repl--max-message-bytes base-bytes)))
              (should (> padding 0))
              (concat base (make-string padding ?x)))))
        (let* ((id (buffer-local-value
                    'fennel-proto-repl--message-id repl))
               (source (exact-source id)))
          (erase-buffer)
          (insert source)
          (should
           (= fumos-repl--max-message-bytes
              (fumos-repl--utf8-bytes (wire id source))))
          (cl-letf (((symbol-function 'fennel-proto-repl--send-string)
                     (lambda (_process frame) (setq captured frame))))
            (should (integerp (fumos-compile-buffer))))
          (should (= fumos-repl--max-message-bytes
                     (fumos-repl--utf8-bytes captured)))
          (should
           (equal (concat "(do\n" source "\n)")
                  (plist-get
                   (fumos-test-read-command-request captured) :compile)))
          (fumos-test-send-command-values
           server (fumos-test-server-client server) captured "return true")
          (fumos-test-assert-eval-settled connection))
        (let* ((id (buffer-local-value
                    'fennel-proto-repl--message-id repl))
               (source (concat (exact-source id) "x"))
               sent)
          (erase-buffer)
          (insert source)
          (should (> (fumos-repl--utf8-bytes (wire id source))
                     fumos-repl--max-message-bytes))
          (cl-letf (((symbol-function 'fennel-proto-repl--send-string)
                     (lambda (&rest _) (setq sent t))))
            (should-error (fumos-compile-buffer) :type 'user-error))
          (should-not sent)
          (fumos-test-assert-eval-settled connection)))
      (when-let* ((buffer (get-buffer "*FUMOS Lua*")))
        (kill-buffer buffer)))))

(ert-deftest fumos-compile-never-saves-or-runs-source ()
  (fumos-test-with-fennel-file (root file connection server)
    (erase-buffer)
    (insert "(set _G.must_not_run true)\n")
    (set-buffer-modified-p t)
    (let ((before (length (fumos-test-server-lines server))))
      (setf
       (fumos-test-server-handler server)
       (lambda (state client line)
         (when (fumos-test-command-request-p line)
           (fumos-test-send-command-values
            state client line "_G.must_not_run = true"))))
      (cl-letf (((symbol-function 'save-buffer)
                 (lambda (&rest _) (ert-fail "compile saved source")))
                ((symbol-function 'fumos-reload-current-file)
                 (lambda (&rest _) (ert-fail "compile reloaded source")))
                ((symbol-function 'fumos-repl-send-eval)
                 (lambda (&rest _) (ert-fail "compile evaluated source"))))
        (should (integerp (fumos-compile-buffer))))
      (should
       (fumos-test-wait-until
        (lambda () (fumos-test-eval-settled-p connection))))
      (should (buffer-modified-p))
      (should (equal "" (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))))
      (should (= 1 (length (fumos-test-command-lines-since server before))))
      (should-not (fumos-test-eval-lines-since server before))
      (cl-letf (((symbol-function 'fumos-repl-send-command)
                 (lambda (&rest _) (ert-fail "save compiled source"))))
        (save-buffer))
      (should-not (buffer-modified-p))
      (should
       (equal "(set _G.must_not_run true)\n"
              (with-temp-buffer
                (insert-file-contents file)
                (buffer-string))))
      (when-let* ((buffer (get-buffer "*FUMOS Lua*")))
        (kill-buffer buffer)))))

(defun fumos-test-process-attributes-at (start)
  "Return a minimal current-user process attribute set for START."
  `((euid . ,(user-uid)) (start . ,start)))

(defun fumos-test-game-connection (root token &optional pid)
  "Return a token-redacted game reload connection rooted at ROOT."
  (make-fumos-connection
   :instance
   (make-fumos-instance
    :project-root (file-name-as-directory (file-truename root))
    :mod-id "demo" :pid (or pid 4242) :token token)
   :state 'ready :generation 7 :game-reload-generation 0))

(ert-deftest fumos-game-reload-public-modes-use-guarded-global ()
  (fumos-test-with-ready-connection (connection server)
    (let ((start (current-time))
          (original-run-at-time (symbol-function 'run-at-time))
          lines timers)
      (setf
       (fumos-test-server-handler server)
       (lambda (state client line)
         (when (fumos-test-eval-request-p line)
           (push line lines)
           (fumos-test-send-eval-values state client line "true"))))
      (cl-letf
          (((symbol-function 'process-attributes)
            (lambda (_pid) (fumos-test-process-attributes-at start)))
           ((symbol-function 'run-at-time)
            (lambda (delay repeat callback &rest arguments)
              (if (and (equal delay 0.1) (equal repeat 0.1))
                  (let ((timer
                         (funcall original-run-at-time 3600 nil callback)))
                    (push timer timers)
                    timer)
                (apply original-run-at-time
                       delay repeat callback arguments)))))
        (dolist (case '((fumos-reload-game-preserve . "temp")
                        (fumos-reload-game-save . "save")
                        (fumos-reload-game-from-start . "none")))
          (let ((before (length lines)))
            (should
             (integerp
              (with-current-buffer
                  (fumos-connection-repl-buffer connection)
                (funcall (car case)))))
            (should
             (fumos-test-wait-until
              (lambda ()
                (and (= (1+ before) (length lines))
                     (fumos-test-eval-settled-p connection)))))
            (let* ((request (fumos-test-read-eval-request (car lines)))
                   (code (plist-get request :eval)))
              (should
               (equal
                (format
                 (concat "(do\n"
                         "  (assert (= _G _G._G) "
                         "\"FUMOS tooling requires unshadowed _G\")\n"
                         "  (_G.Kristal.quickReload %S))")
                 (cdr case))
                code))
              (should-not (string-match-p "([^_]Kristal\\." code)))
              (should (equal (cdr case)
                             (fumos-connection-pending-game-reload
                              connection)))
            (fumos-repl--cancel-game-reload-timer connection))))
      (should (= 3 (length lines)))
      (dolist (timer timers) (fumos-repl--cancel-timer timer)))))

(ert-deftest fumos-game-reload-rejects-concurrent-intent ()
  (fumos-test-with-ready-connection (connection server)
    (let ((start (current-time))
          (original-run-at-time (symbol-function 'run-at-time))
          first-timer)
      (setf
       (fumos-test-server-handler server)
       (lambda (state client line)
         (when (fumos-test-eval-request-p line)
           (fumos-test-send-eval-values state client line "true"))))
      (cl-letf
          (((symbol-function 'process-attributes)
            (lambda (_pid) (fumos-test-process-attributes-at start)))
           ((symbol-function 'run-at-time)
            (lambda (delay repeat callback &rest arguments)
              (if (and (equal delay 0.1) (equal repeat 0.1))
                  (setq first-timer
                        (funcall original-run-at-time 3600 nil callback))
                (apply original-run-at-time
                       delay repeat callback arguments)))))
        (with-current-buffer (fumos-connection-repl-buffer connection)
          (fumos-reload-game-preserve))
        (should first-timer)
        (let ((before (length (fumos-test-server-lines server)))
              (generation
               (fumos-connection-game-reload-generation connection)))
          (should-error
           (with-current-buffer (fumos-connection-repl-buffer connection)
             (fumos-reload-game-save))
           :type 'user-error)
          (should (= before (length (fumos-test-server-lines server))))
          (should (eq first-timer
                      (fumos-connection-game-reload-timer connection)))
          (should (= generation
                     (fumos-connection-game-reload-generation connection)))
          (should (equal "temp"
                         (fumos-connection-pending-game-reload connection)))))
      (fumos-repl--cancel-game-reload-timer connection))))

(ert-deftest fumos-game-reload-rejects-pid-reuse ()
  (let* ((root (make-temp-file "fumos-game-pid-root-" t))
         (token (make-string 64 ?a))
         (connection (fumos-test-game-connection root token))
         (captured-start (current-time))
         (current-start captured-start)
         callback connected)
    (unwind-protect
        (cl-letf
            (((symbol-function 'process-attributes)
              (lambda (_pid)
                (fumos-test-process-attributes-at current-start)))
             ((symbol-function 'run-at-time)
              (lambda (_delay _repeat function &rest _)
                (setq callback function)
                'game-timer))
             ((symbol-function 'timerp)
              (lambda (value) (eq value 'game-timer)))
             ((symbol-function 'cancel-timer) #'ignore)
             ((symbol-function 'fumos-discover-instances)
              (lambda (_root)
                (list
                 (make-fumos-instance
                  :project-root root :mod-id "demo" :pid 4242
                  :token (make-string 64 ?b)))))
             ((symbol-function 'fumos-repl-connect-instance)
              (lambda (&rest _) (setq connected t))))
          (let ((operation
                 (fumos-eval--begin-game-reload connection "temp")))
            (fumos-eval--await-game-reload connection operation))
          (setq current-start (time-add captured-start (seconds-to-time 1)))
          (funcall callback)
          (funcall callback)
          (should-not connected)
          (should (equal "temp"
                         (fumos-connection-pending-game-reload connection))))
      (fumos-repl--cancel-game-reload-timer connection)
      (delete-directory root t))))

(ert-deftest fumos-game-reload-rejects-transport-generation-change ()
  (let* ((root (make-temp-file "fumos-game-generation-root-" t))
         (connection
          (fumos-test-game-connection root (make-string 64 ?a)))
         (start (current-time)) callback connected)
    (unwind-protect
        (cl-letf
            (((symbol-function 'process-attributes)
              (lambda (_pid) (fumos-test-process-attributes-at start)))
             ((symbol-function 'run-at-time)
              (lambda (_delay _repeat function &rest _)
                (setq callback function)
                'generation-timer))
             ((symbol-function 'timerp)
              (lambda (value) (eq value 'generation-timer)))
             ((symbol-function 'cancel-timer) #'ignore)
             ((symbol-function 'fumos-discover-instances)
              (lambda (_root)
                (list
                 (make-fumos-instance
                  :project-root root :mod-id "demo" :pid 4242
                  :token (make-string 64 ?b)))))
             ((symbol-function 'fumos-repl-connect-instance)
              (lambda (&rest _) (setq connected t))))
          (let ((operation
                 (fumos-eval--begin-game-reload connection "temp")))
            (fumos-eval--await-game-reload connection operation))
          (cl-incf (fumos-connection-generation connection))
          (funcall callback)
          (should-not connected)
          (should (equal "temp"
                         (fumos-connection-pending-game-reload connection))))
      (fumos-repl--cancel-game-reload-timer connection)
      (delete-directory root t))))

(ert-deftest fumos-game-reload-timer-and-connection-print-without-token ()
  (let* ((root (make-temp-file "fumos-game-print-root-" t))
         (token (make-string 64 ?a))
         (digest (secure-hash 'sha256 token))
         (connection (fumos-test-game-connection root token))
         (start (current-time))
         (original-run-at-time (symbol-function 'run-at-time))
         operation timer)
    (unwind-protect
        (cl-letf
            (((symbol-function 'process-attributes)
              (lambda (_pid) (fumos-test-process-attributes-at start)))
             ((symbol-function 'run-at-time)
              (lambda (_delay _repeat callback &rest _)
                (funcall original-run-at-time 3600 nil callback))))
          (setq operation
                (fumos-eval--begin-game-reload connection "temp")
                timer (fumos-eval--await-game-reload connection operation))
          (let ((printed (prin1-to-string (list operation timer connection))))
            (should-not (string-match-p (regexp-quote token) printed))
            (should (string-match-p (regexp-quote digest) printed))))
      (fumos-repl--cancel-game-reload-timer connection)
      (delete-directory root t))))

(ert-deftest fumos-game-reload-descriptor-match-is-identity-bound-and-once ()
  (let* ((root (make-temp-file "fumos-game-match-root-" t))
         (other-root (make-temp-file "fumos-game-other-root-" t))
         (old-token (make-string 64 ?a))
         (new-token (make-string 64 ?b))
         (connection (fumos-test-game-connection root old-token))
         (old (fumos-connection-instance connection))
         (same-token
          (make-fumos-instance
           :project-root root :mod-id "demo" :pid 4242 :token old-token))
         (wrong-root
          (make-fumos-instance
           :project-root other-root :mod-id "demo" :pid 4242 :token new-token))
         (wrong-pid
          (make-fumos-instance
           :project-root root :mod-id "demo" :pid 5252 :token new-token))
         (match
          (make-fumos-instance
           :project-root root :mod-id "demo" :pid 4242 :token new-token))
         (start (current-time))
         (race nil) (attribute-read 0)
         candidates callback connected canceled)
    (unwind-protect
        (cl-letf
            (((symbol-function 'process-attributes)
              (lambda (_pid)
                (cl-incf attribute-read)
                (fumos-test-process-attributes-at
                 (if (and race (cl-evenp attribute-read))
                     (time-add start (seconds-to-time 1))
                   start))))
             ((symbol-function 'run-at-time)
              (lambda (_delay _repeat function &rest _)
                (setq callback function)
                'match-timer))
             ((symbol-function 'timerp)
              (lambda (value) (eq value 'match-timer)))
             ((symbol-function 'cancel-timer)
              (lambda (value) (push value canceled)))
             ((symbol-function 'fumos-discover-instances)
              (lambda (_root) candidates))
             ((symbol-function 'fumos-repl-connect-instance)
              (lambda (instance) (push instance connected) 'replacement)))
          (let ((operation
                 (fumos-eval--begin-game-reload connection "temp")))
            (fumos-eval--await-game-reload connection operation))
          (dolist (candidate (list old same-token wrong-root wrong-pid))
            (setq candidates (list candidate))
            (funcall callback)
            (should-not connected)
            (should (fumos-connection-pending-game-reload connection)))
          (setq candidates (list match) race t attribute-read 1)
          (funcall callback)
          (should-not connected)
          (should (fumos-connection-pending-game-reload connection))
          (setq race nil attribute-read 0)
          (funcall callback)
          (should (equal (list match) connected))
          (should-not (fumos-connection-pending-game-reload connection))
          (funcall callback)
          (should (equal (list match) connected))
          (should (= 1 (length canceled))))
      (fumos-repl--cancel-game-reload-timer connection)
      (delete-directory other-root t)
      (delete-directory root t))))

(ert-deftest fumos-game-reload-scheduler-exits-clear-operation ()
  (dolist (kind '(error quit throw non-timer))
    (let* ((root (make-temp-file "fumos-game-scheduler-root-" t))
           (connection
            (fumos-test-game-connection root (make-string 64 ?a)))
           (start (current-time)) outcome)
      (unwind-protect
          (cl-letf
              (((symbol-function 'fumos-repl-current-connection)
                (lambda () connection))
               ((symbol-function 'process-attributes)
                (lambda (_pid) (fumos-test-process-attributes-at start)))
               ((symbol-function 'fumos-repl-send-eval)
                (lambda (&rest _) 17))
               ((symbol-function 'run-at-time)
                (lambda (&rest _)
                  (pcase kind
                    ('error (error "scheduler failed"))
                    ('quit (signal 'quit nil))
                    ('throw (throw 'fumos-scheduler-exit :thrown))
                    ('non-timer 'not-a-timer))))
               ((symbol-function 'timerp)
                (lambda (value) (and value (not (eq value 'not-a-timer)))))
               ((symbol-function 'cancel-timer) #'ignore))
            (setq
             outcome
             (pcase kind
               ('error
                (condition-case caught
                    (fumos-eval--reload-game "temp")
                  (error (car caught))))
               ('quit
                (condition-case caught
                    (fumos-eval--reload-game "temp")
                  (quit (car caught))))
               ('throw
                (catch 'fumos-scheduler-exit
                  (fumos-eval--reload-game "temp") :not-thrown))
               ('non-timer
                (condition-case caught
                    (fumos-eval--reload-game "temp")
                  (error (car caught))))))
            (should (eq (pcase kind
                          ('error 'error)
                          ('quit 'quit)
                          ('throw :thrown)
                          ('non-timer 'user-error))
                        outcome))
            (should-not (fumos-connection-pending-game-reload connection))
            (should-not (fumos-connection-game-reload-timer connection))
            (should (> (fumos-connection-game-reload-generation connection)
                       0)))
        (fumos-repl--cancel-game-reload-timer connection)
        (delete-directory root t)))))

(ert-deftest fumos-game-reload-timer-validation-and-cancel-exits-are-field-first ()
  (let* ((root (make-temp-file "fumos-game-timer-root-" t))
         (connection
          (fumos-test-game-connection root (make-string 64 ?a)))
         (start (current-time)) canceled)
    (unwind-protect
        (cl-letf
            (((symbol-function 'fumos-repl-current-connection)
              (lambda () connection))
             ((symbol-function 'process-attributes)
              (lambda (_pid) (fumos-test-process-attributes-at start)))
             ((symbol-function 'fumos-repl-send-eval)
              (lambda (&rest _) 18))
             ((symbol-function 'run-at-time)
              (lambda (&rest _) 'validation-timer))
             ((symbol-function 'timerp)
              (lambda (&rest _) (error "timerp failed")))
             ((symbol-function 'cancel-timer)
              (lambda (timer) (push timer canceled))))
          (should-error (fumos-eval--reload-game "temp"))
          (should (equal '(validation-timer) canceled))
          (should-not (fumos-connection-pending-game-reload connection))
          (should-not (fumos-connection-game-reload-timer connection)))
      (delete-directory root t)))
  (dolist (kind '(error quit))
    (let ((connection
           (make-fumos-connection
            :game-reload-timer 'cancel-timer
            :pending-game-reload "temp" :game-reload-generation 9))
          condition)
      (cl-letf (((symbol-function 'cancel-timer)
                 (lambda (&rest _) (signal kind nil))))
        (setq condition
              (condition-case nil
                  (progn (fumos-repl--cancel-game-reload-timer connection) nil)
                ((error quit) t))))
      (should-not condition)
      (should-not (fumos-connection-game-reload-timer connection))
      (should-not (fumos-connection-pending-game-reload connection))
      (should (> (fumos-connection-game-reload-generation connection) 9)))))

(ert-deftest fumos-game-reload-errors-timeout-and-explicit-teardown-cancel ()
  ;; Synchronous send exits cannot leave intent behind.
  (dolist (kind '(error quit))
    (let* ((root (make-temp-file "fumos-game-send-root-" t))
           (connection
            (fumos-test-game-connection root (make-string 64 ?a)))
           (start (current-time)))
      (unwind-protect
          (cl-letf
              (((symbol-function 'fumos-repl-current-connection)
                (lambda () connection))
               ((symbol-function 'process-attributes)
                (lambda (_pid) (fumos-test-process-attributes-at start)))
               ((symbol-function 'fumos-repl-send-eval)
                (lambda (&rest _) (signal kind nil))))
            (condition-case nil
                (fumos-eval--reload-game "temp")
              ((error quit) nil))
            (should-not (fumos-connection-pending-game-reload connection))
            (should-not (fumos-connection-game-reload-timer connection)))
        (delete-directory root t))))
  ;; A non-connection-lost terminal cancels; expected disconnect preserves.
  (fumos-test-with-ready-connection (connection server)
    (let ((start (current-time))
          (original-run-at-time (symbol-function 'run-at-time)))
      (cl-letf
          (((symbol-function 'process-attributes)
            (lambda (_pid) (fumos-test-process-attributes-at start)))
           ((symbol-function 'run-at-time)
            (lambda (delay repeat callback &rest arguments)
              (if (and (equal delay 0.1) (equal repeat 0.1))
                  (funcall original-run-at-time 3600 nil callback)
                (apply original-run-at-time
                       delay repeat callback arguments)))))
        (setf
         (fumos-test-server-handler server)
         (lambda (state client line)
           (when (fumos-test-eval-request-p line)
             (fumos-test-send-eval-runtime-error
              state client line "quick reload rejected"))))
        (with-current-buffer (fumos-connection-repl-buffer connection)
          (fumos-reload-game-preserve))
        (should
         (fumos-test-wait-until
          (lambda ()
            (not (fumos-connection-pending-game-reload connection))))))))
  (fumos-test-with-ready-connection (connection server)
    (let ((start (current-time))
          (original-run-at-time (symbol-function 'run-at-time)))
      (cl-letf
          (((symbol-function 'process-attributes)
            (lambda (_pid) (fumos-test-process-attributes-at start)))
           ((symbol-function 'run-at-time)
            (lambda (delay repeat callback &rest arguments)
              (if (and (equal delay 0.1) (equal repeat 0.1))
                  (funcall original-run-at-time 3600 nil callback)
                (apply original-run-at-time
                       delay repeat callback arguments)))))
        (setf
         (fumos-test-server-handler server)
         (lambda (state _client line)
           (when (fumos-test-eval-request-p line)
             (fumos-test-server-drop-client state))))
        (with-current-buffer (fumos-connection-repl-buffer connection)
          (fumos-reload-game-preserve))
        (should
         (fumos-test-wait-until
          (lambda () (eq 'disconnected
                         (fumos-connection-state connection)))))
        (should (equal "temp"
                       (fumos-connection-pending-game-reload connection)))
        (should (fumos-connection-game-reload-timer connection))
        (fumos-repl--cancel-game-reload-timer connection))))
  ;; Deadline and explicit operations invalidate field-first.
  (let* ((root (make-temp-file "fumos-game-timeout-root-" t))
         (connection
          (fumos-test-game-connection root (make-string 64 ?a)))
         (start (current-time)) (now 0.0) callback messages)
    (unwind-protect
        (cl-letf
            (((symbol-function 'process-attributes)
              (lambda (_pid) (fumos-test-process-attributes-at start)))
             ((symbol-function 'float-time) (lambda (&rest _) now))
             ((symbol-function 'run-at-time)
              (lambda (_delay _repeat function &rest _)
                (setq callback function) 'timeout-timer))
             ((symbol-function 'timerp)
              (lambda (value) (eq value 'timeout-timer)))
             ((symbol-function 'cancel-timer) #'ignore)
             ((symbol-function 'fumos-discover-instances) (lambda (_) nil))
             ((symbol-function 'message)
              (lambda (format-string &rest args)
                (push (apply #'format format-string args) messages))))
          (let ((operation
                 (fumos-eval--begin-game-reload connection "temp")))
            (fumos-eval--await-game-reload connection operation))
          (setq now 11.0)
          (funcall callback)
          (should-not (fumos-connection-pending-game-reload connection))
          (should (seq-some
                   (lambda (value) (string-match-p "timed out" value))
                   messages)))
      (delete-directory root t)))
  (dolist (operation '(close disconnect))
    (let ((connection
           (make-fumos-connection
            :game-reload-timer 'explicit-timer
            :pending-game-reload "temp" :game-reload-generation 3
            :state 'ready)))
      (cl-letf (((symbol-function 'cancel-timer) #'ignore)
                ((symbol-function 'fumos-repl--teardown-transport) #'ignore)
                ((symbol-function 'fumos-repl--unregister-if-current) #'ignore)
                ((symbol-function 'fumos-repl-current-connection)
                 (lambda () connection))
                ((symbol-function 'fumos-repl--mark-disconnected) #'ignore))
        (if (eq operation 'close)
            (fumos-repl-close connection)
          (fumos-disconnect)))
      (should-not (fumos-connection-pending-game-reload connection))
      (should-not (fumos-connection-game-reload-timer connection)))))

(ert-deftest fumos-game-reload-connect-exits-are-contained-and-once ()
  (dolist (kind '(error quit))
    (let* ((root (make-temp-file "fumos-game-connect-root-" t))
           (token (make-string 64 ?a))
           (connection (fumos-test-game-connection root token))
           (start (current-time)) callback (calls 0) messages)
      (unwind-protect
          (cl-letf
              (((symbol-function 'process-attributes)
                (lambda (_pid) (fumos-test-process-attributes-at start)))
               ((symbol-function 'run-at-time)
                (lambda (_delay _repeat function &rest _)
                  (setq callback function) 'connect-timer))
               ((symbol-function 'timerp)
                (lambda (value) (eq value 'connect-timer)))
               ((symbol-function 'cancel-timer) #'ignore)
               ((symbol-function 'fumos-discover-instances)
                (lambda (_root)
                  (list
                   (make-fumos-instance
                    :project-root root :mod-id "demo" :pid 4242
                    :token (make-string 64 ?b)))))
               ((symbol-function 'fumos-repl-connect-instance)
                (lambda (&rest _)
                  (cl-incf calls)
                  (signal kind nil)))
               ((symbol-function 'message)
                (lambda (format-string &rest args)
                  (push (apply #'format format-string args) messages))))
            (let ((operation
                   (fumos-eval--begin-game-reload connection "temp")))
              (fumos-eval--await-game-reload connection operation))
            (should-not (condition-case nil (progn (funcall callback) nil)
                          ((error quit) t)))
            (funcall callback)
            (should (= 1 calls))
            (should-not (fumos-connection-pending-game-reload connection))
            (should
             (seq-some
              (lambda (value)
                (string-match-p "FUMOS game reload reconnect failed" value))
              messages))
            (dolist (value messages)
              (should-not (string-match-p (regexp-quote token) value))))
        (delete-directory root t)))))

(provide 'fumos-eval-test)
;;; fumos-eval-test.el ends here
