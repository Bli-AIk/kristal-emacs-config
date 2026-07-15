;;; fumos-repl-test.el --- FUMOS network REPL tests -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'test-helper)
(require 'seq)
(require 'support/fake-fumos-server)
(require 'fumos-repl)

(defconst fumos-test-golden-ack
  (concat "FUMOS/1 OK pid=4242 proto=0.6.4 "
          "capabilities=interrupt,cancel,detach,source-context,game-reload "
          "max=8388608\n"))

(defconst fumos-test-bootstrap-sha256
  "3c57cd018b5274d7c0a5c776a1e449b4e8039c3d7bc5dd58652bacae68e2a0e6")

(defconst fumos-test-init-frame
  (concat "(:id 0 :op \"init\" :status \"done\" :protocol \"0.6.4\" "
          ":fennel \"1.6.1\" :lua \"LuaJIT 2.1\")\n"))

(defun fumos-test-init-frame-with-protocol (protocol)
  "Return a successful init frame that advertises PROTOCOL."
  (format
   (concat "(:id 0 :op \"init\" :status \"done\" :protocol %S "
           ":fennel \"1.6.1\" :lua \"LuaJIT 2.1\")\n")
   protocol))

(defun fumos-test-bootstrap-module-name (source)
  "Structurally read the third argument of one Fennel bootstrap SOURCE."
  (with-temp-buffer
    (insert source)
    (fennel-mode)
    (goto-char (point-min))
    (skip-chars-forward " \t\r\n")
    (unless (eq (char-after) ?\()
      (ert-fail "Bootstrap is not one Fennel call"))
    (forward-char 1)
    (dotimes (_ 2)
      (skip-chars-forward " \t\r\n")
      (forward-sexp 1))
    (skip-chars-forward " \t\r\n")
    (let ((start (point)))
      (forward-sexp 1)
      (let ((module (read (buffer-substring-no-properties start (point)))))
        (skip-chars-forward " \t\r\n")
        (unless (eq (char-after) ?\))
          (ert-fail "Bootstrap has trailing arguments"))
        (forward-char 1)
        (skip-chars-forward " \t\r\n")
        (unless (eobp)
          (ert-fail "Bootstrap has a second form"))
        module))))

(defun fumos-test-ack-for-pid (pid)
  "Return the exact ACK line for PID."
  (format
   (concat "FUMOS/1 OK pid=%d proto=0.6.4 "
           "capabilities=interrupt,cancel,detach,source-context,game-reload "
           "max=8388608\n")
   pid))

(defun fumos-test-instance-for-server
    (server &optional pid root mod-id token)
  "Return a valid instance pointing at SERVER."
  (make-fumos-instance
   :descriptor-file (format "/tmp/fumos-test/%d.json" (or pid 4242))
   :project-root (file-name-as-directory
                  (file-truename (or root default-directory)))
   :mod-id (or mod-id "demo") :pid (or pid 4242) :started-at 1783940000
   :host "127.0.0.1" :port (fumos-test-server-port server)
   :token (or token (make-string 64 ?a))
   :fumos-version "0.1.0" :proto "0.6.4"
   :capabilities '("interrupt" "cancel" "detach" "source-context" "game-reload")
   :max-message-bytes 8388608))

(defun fumos-test-make-proto-handler (pid &optional request-handler)
  "Return an AUTH/bootstrap handler for PID, then use REQUEST-HANDLER."
  (lambda (server client line)
    (pcase (length (fumos-test-server-lines server))
      (1
       (should (string-prefix-p "FUMOS/1 AUTH " line))
       (fumos-test-server-send server (fumos-test-ack-for-pid pid) client))
      (2
       (should (string-match-p "___repl___" line))
       (fumos-test-server-send server fumos-test-init-frame client))
      (_
       (if (string-match-p "macro-loaded" line)
           (fumos-test-send-macro-result server client line "nil")
         (when request-handler
           (funcall request-handler server client line)))))))

(defun fumos-test-server-saw-eval-p (server code)
  "Return non-nil when SERVER received an eval containing CODE."
  (seq-some (lambda (line) (string-match-p (regexp-quote code) line))
            (fumos-test-server-lines server)))

(defun fumos-test-send-client-chunks (server client chunks)
  "Send CHUNKS from SERVER to the explicitly captured CLIENT."
  (dolist (chunk chunks)
    (fumos-test-server-send server chunk client)
    (accept-process-output nil 0.01)))

(defun fumos-test-proto-handler (server client line)
  "Authenticate and bootstrap a client connected to SERVER."
  (pcase (length (fumos-test-server-lines server))
    (1
     (should (equal line (concat "FUMOS/1 AUTH " (make-string 64 ?a))))
     (fumos-test-send-client-chunks
      server client
      (list (substring fumos-test-golden-ack 0 17)
            (substring fumos-test-golden-ack 17))))
    (2
     (should (string-match-p "___repl___" line))
     (fumos-test-send-client-chunks
      server client
      (list (substring fumos-test-init-frame 0 23)
            (substring fumos-test-init-frame 23))))
    (_
     (when (string-match-p "macro-loaded" line)
       (fumos-test-send-macro-result server client line "nil")))))

(defun fumos-test-timer-live-p (timer)
  "Return non-nil when TIMER is still scheduled."
  (and (timerp timer)
       (or (memq timer timer-list)
           (memq timer timer-idle-list))))

(defun fumos-test-connection-snapshot (connection)
  "Capture CONNECTION resources so cleanup remains observable afterward."
  (let* ((repl (fumos-connection-repl-buffer connection))
         (callbacks
          (and (buffer-live-p repl)
               (buffer-local-value
                'fennel-proto-repl--message-callbacks repl))))
    (list :process (fumos-connection-process connection)
          :process-buffer (fumos-connection-process-buffer connection)
          :repl-buffer repl
          :ui-process (fumos-connection-ui-process connection)
          :handshake-timer (fumos-connection-handshake-timer connection)
          :bootstrap-timer (fumos-connection-bootstrap-timer connection)
          :callbacks callbacks)))

(defun fumos-test-assert-closed (connection snapshot &optional token)
  "Assert CONNECTION released all resources captured in SNAPSHOT."
  (should (eq 'disconnected (fumos-connection-state connection)))
  (should (fumos-connection-closing connection))
  (dolist (key '(:process :ui-process))
    (let ((process (plist-get snapshot key)))
      (when (processp process)
        (should-not (process-live-p process)))))
  (dolist (key '(:process-buffer :repl-buffer))
    (let ((buffer (plist-get snapshot key)))
      (when (bufferp buffer)
        (should-not (buffer-live-p buffer)))))
  (dolist (key '(:handshake-timer :bootstrap-timer))
    (should-not (fumos-test-timer-live-p (plist-get snapshot key))))
  (when-let* ((callbacks (plist-get snapshot :callbacks)))
    (should (hash-table-empty-p callbacks)))
  (should-not (fumos-connection-process connection))
  (should-not (fumos-connection-process-buffer connection))
  (should-not (fumos-connection-repl-buffer connection))
  (should-not (fumos-connection-ui-process connection))
  (should-not (fumos-connection-handshake-timer connection))
  (should-not (fumos-connection-bootstrap-timer connection))
  (should-not (fumos-connection-retry-timers connection))
  (should-not (fumos-connection-callback-timers connection))
  (should-not (fumos-connection-terminal-timers connection))
  (should-not (fumos-connection-terminal-deliveries connection))
  (when-let* ((deliveries (fumos-connection-callback-deliveries connection)))
    (should (hash-table-empty-p deliveries)))
  (when token
    (should-not
     (string-match-p
      (regexp-quote token)
      (or (fumos-connection-last-error connection) ""))))
  t)

(defun fumos-test-assert-fixed-connection-condition (condition token)
  "Assert CONDITION is the fixed setup failure and excludes TOKEN."
  (should (equal condition '(fumos-repl-connection-error)))
  (should-not
   (string-match-p (regexp-quote token) (error-message-string condition))))

(defun fumos-test-assert-token-not-in-messages (token start)
  "Assert TOKEN is absent from `*Messages*' after START."
  (with-current-buffer (get-buffer "*Messages*")
    (should-not
     (string-match-p
      (regexp-quote token)
      (buffer-substring-no-properties start (point-max))))))

(cl-defmacro fumos-test-with-ready-connection ((connection server) &rest body)
  "Bind CONNECTION and SERVER to a ready fake proto session."
  (declare (indent 1) (debug ((symbolp symbolp) body)))
  `(let* ((fumos-handshake-timeout 0.5)
          (fumos-bootstrap-timeout 0.5)
          (,server (fumos-test-server-start #'fumos-test-proto-handler))
          (,connection
           (fumos-repl-connect-instance
            (fumos-test-instance-for-server ,server))))
     (unwind-protect
         (progn
           (should
            (fumos-test-wait-until
             (lambda () (eq 'ready (fumos-connection-state ,connection)))))
           (should
            (fumos-test-wait-until
             (lambda ()
               (fumos-connection-macro-cache-valid ,connection))))
           ,@body)
       (fumos-repl-close ,connection)
       (fumos-test-server-stop ,server))))

(ert-deftest fumos-repl-authenticates-and-sends-exact-pinned-bootstrap ()
  (let ((global-id (default-value 'fennel-proto-repl--message-id))
        (global-module
         (default-value 'fennel-proto-repl-fennel-module-name)))
    (fumos-test-with-ready-connection (connection server)
      (should (eq 'network
                  (process-type (fumos-connection-process connection))))
      (should (equal "*FUMOS: demo@4242*"
                     (buffer-name (fumos-connection-repl-buffer connection))))
      (let* ((lines (fumos-test-server-lines server))
             (bootstrap (cadr lines))
             (macro-query (caddr lines)))
        (should (= 3 (length lines)))
        (should (string-prefix-p "FUMOS/1 AUTH " (car lines)))
        (should (equal fumos-repl-fennel-module-name
                       (fumos-test-bootstrap-module-name bootstrap)))
        (should
         (equal fumos-test-bootstrap-sha256
                (secure-hash 'sha256 (concat bootstrap "\n"))))
        (should (string-match-p
                 (regexp-quote "(require \\\"fumos.repl.fennel\\\")")
                 macro-query)))
      (should (equal global-id
                     (default-value 'fennel-proto-repl--message-id)))
      (should (equal global-module
                     (default-value
                      'fennel-proto-repl-fennel-module-name)))
      (with-current-buffer (fumos-connection-repl-buffer connection)
        (should (local-variable-p 'fennel-proto-repl--message-id))
        (should (= 2 fennel-proto-repl--message-id))
        (should (local-variable-p
                 'fennel-proto-repl-fennel-module-name))
        (should (equal fumos-repl-fennel-module-name
                       fennel-proto-repl-fennel-module-name))))))

(ert-deftest fumos-ready-close-neutralizes-sentinel-and-cleans-every-resource ()
  (fumos-test-with-ready-connection (connection server)
    (let* ((network (fumos-connection-process connection))
           (ui-process (fumos-connection-ui-process connection))
           (repl (fumos-connection-repl-buffer connection))
           (callbacks
            (buffer-local-value 'fennel-proto-repl--message-callbacks repl))
           (handshake-timer (run-at-time 60 nil #'ignore))
           (bootstrap-timer (run-at-time 60 nil #'ignore))
           (retry-timer (run-at-time 60 nil #'ignore))
           (callback-timer (run-at-time 60 nil #'ignore))
           (terminal-timer (run-at-time 60 nil #'ignore)))
      (puthash 91 'pending callbacks)
      (setf (fumos-connection-handshake-timer connection) handshake-timer
            (fumos-connection-bootstrap-timer connection) bootstrap-timer
            (fumos-connection-retry-timers connection) (list retry-timer)
            (fumos-connection-callback-timers connection) (list callback-timer)
            (fumos-connection-terminal-timers connection) (list terminal-timer)
            (fumos-connection-terminal-deliveries connection) '(pending))
      (should (process-live-p ui-process))
      (should-not (process-query-on-exit-flag ui-process))
      (fumos-repl-close connection "Explicit close")
      (should (eq (process-filter network) #'ignore))
      (should (eq (process-sentinel network) #'ignore))
      (should (equal "Explicit close"
                     (fumos-connection-last-error connection)))
      (should (hash-table-empty-p callbacks))
      (dolist (timer (list handshake-timer bootstrap-timer retry-timer
                           callback-timer terminal-timer))
        (should-not (fumos-test-timer-live-p timer)))
      (fumos-test-assert-closed
       connection
       (list :process network :ui-process ui-process :repl-buffer repl
             :callbacks callbacks)))))

(ert-deftest fumos-repl-rejects-wrong-ack-without-leaking-token-or-resources ()
  (let* ((token (make-string 64 ?b))
         (messages (get-buffer-create "*Messages*"))
         (message-start (with-current-buffer messages (point-max)))
         (server
          (fumos-test-server-start
           (lambda (state client _line)
             (fumos-test-server-send
              state
              (replace-regexp-in-string "pid=4242" "pid=9999"
                                        fumos-test-golden-ack)
              client))))
         connection snapshot process-buffer-content process-buffer
         (original-kill-buffer (symbol-function 'kill-buffer)))
    (unwind-protect
        (cl-letf
            (((symbol-function 'kill-buffer)
              (lambda (&optional buffer-or-name)
                (let ((buffer (get-buffer (or buffer-or-name
                                              (current-buffer)))))
                  (when (and process-buffer (eq buffer process-buffer))
                    (setq process-buffer-content
                          (with-current-buffer buffer (buffer-string)))))
                (funcall original-kill-buffer buffer-or-name))))
          (let ((fumos-handshake-timeout 0.5))
            (setq connection
                  (fumos-repl-connect-instance
                   (fumos-test-instance-for-server
                    server 4242 nil nil token))
                  process-buffer (fumos-connection-process-buffer connection)
                  snapshot (fumos-test-connection-snapshot connection))
            (should
             (fumos-test-wait-until
              (lambda () (eq 'disconnected
                             (fumos-connection-state connection)))))
            ;; These assertions precede the test's own unwind cleanup.
            (fumos-test-assert-closed connection snapshot token)
            (should (stringp process-buffer-content))
            (should-not
             (string-match-p (regexp-quote token) process-buffer-content))
            (with-current-buffer messages
              (should-not
               (string-match-p
                (regexp-quote token)
                (buffer-substring-no-properties message-start (point-max)))))))
      (when connection (fumos-repl-close connection))
      (fumos-test-server-stop server))))

(ert-deftest fumos-repl-handshake-timeout-cleans-before-test-unwind ()
  (let* ((server
          (fumos-test-server-start
           (lambda (state client _line)
             (fumos-test-server-send state "FUMOS/1 O" client))))
         (fumos-handshake-timeout 0.05)
         (connection
          (fumos-repl-connect-instance
           (fumos-test-instance-for-server server)))
         (snapshot (fumos-test-connection-snapshot connection)))
    (unwind-protect
        (progn
          (should
           (fumos-test-wait-until
            (lambda () (eq 'disconnected
                           (fumos-connection-state connection)))
            0.5))
          (fumos-test-assert-closed connection snapshot (make-string 64 ?a)))
      (fumos-repl-close connection)
      (fumos-test-server-stop server))))

(ert-deftest fumos-repl-bootstrap-has-an-independent-deadline ()
  (let* ((server
          (fumos-test-server-start
           (lambda (state client _line)
             (when (= 1 (length (fumos-test-server-lines state)))
               (fumos-test-server-send
                state fumos-test-golden-ack client)))))
         (fumos-handshake-timeout 1.0)
         (fumos-bootstrap-timeout 0.05)
         (connection
          (fumos-repl-connect-instance
           (fumos-test-instance-for-server server))))
    (unwind-protect
        (progn
          (should
           (fumos-test-wait-until
            (lambda () (eq 'bootstrapping
                           (fumos-connection-state connection)))))
          (let ((snapshot (fumos-test-connection-snapshot connection)))
            (should (timerp (plist-get snapshot :bootstrap-timer)))
            (should-not (plist-get snapshot :handshake-timer))
            (should
             (fumos-test-wait-until
              (lambda () (eq 'disconnected
                             (fumos-connection-state connection)))
              0.5))
            (fumos-test-assert-closed
             connection snapshot (make-string 64 ?a))))
      (fumos-repl-close connection)
      (fumos-test-server-stop server))))

(ert-deftest fumos-repl-rejects-init-from-any-protocol-other-than-064 ()
  (let* ((server
          (fumos-test-server-start
           (lambda (state client _line)
             (pcase (length (fumos-test-server-lines state))
               (1 (fumos-test-server-send
                   state fumos-test-golden-ack client))
               (2 (fumos-test-server-send
                   state (fumos-test-init-frame-with-protocol "0.6.3")
                   client))))))
         (fumos-bootstrap-timeout 0.5)
         (connection
          (fumos-repl-connect-instance
           (fumos-test-instance-for-server server)))
         (snapshot (fumos-test-connection-snapshot connection)))
    (unwind-protect
        (progn
          (should
           (fumos-test-wait-until
            (lambda () (eq 'disconnected
                           (fumos-connection-state connection)))))
          (fumos-test-assert-closed connection snapshot (make-string 64 ?a))
          (should (equal "Proto bootstrap failed"
                         (fumos-connection-last-error connection))))
      (fumos-repl-close connection)
      (fumos-test-server-stop server))))

(ert-deftest fumos-repl-peer-drop-during-authentication-cleans-everything ()
  (let* ((server
          (fumos-test-server-start
           (lambda (state _client _line)
             (fumos-test-server-drop-client state))))
         (connection
          (fumos-repl-connect-instance
           (fumos-test-instance-for-server server)))
         (snapshot (fumos-test-connection-snapshot connection)))
    (unwind-protect
        (progn
          (should
           (fumos-test-wait-until
            (lambda () (eq 'disconnected
                           (fumos-connection-state connection)))))
          (fumos-test-assert-closed connection snapshot (make-string 64 ?a)))
      (fumos-repl-close connection)
      (fumos-test-server-stop server))))

(ert-deftest fumos-repl-handshake-limit-excludes-proto-leftover ()
  (let* ((leftover (concat (make-string 5000 ?x) "\n"))
         (connection
          (make-fumos-connection
           :instance (make-fumos-instance :pid 4242)
           :state 'authenticating
           :handshake-buffer ""))
         forwarded bootstrapped)
    (cl-letf (((symbol-function 'set-process-filter) #'ignore)
              ((symbol-function 'fumos-repl--owns-transport-p)
               (lambda (_connection _process _generation) t))
              ((symbol-function 'fumos-repl--bootstrap)
               (lambda (value _process _generation)
                 (should (eq value connection))
                 (setq bootstrapped t)))
              ((symbol-function 'fumos-repl--protocol-filter)
               (lambda (value process _generation bytes)
                 (should (eq value connection))
                 (should (eq process 'fake-process))
                 (setq forwarded bytes))))
      (fumos-repl--handshake-filter
       connection 'fake-process 17
       (concat fumos-test-golden-ack leftover)))
    (should bootstrapped)
    (should (equal leftover forwarded))
    (should (equal "" (fumos-connection-handshake-buffer connection)))
    (should-not (eq 'disconnected (fumos-connection-state connection)))))

(ert-deftest fumos-repl-open-error-is-redacted-and-quit-propagates ()
  (dolist (kind '(error quit))
    (let* ((token (make-string 64 (if (eq kind 'error) ?c ?d)))
           (server (fumos-test-server-start))
           (message-start
            (with-current-buffer (get-buffer-create "*Messages*") (point-max)))
           connection owned-process owned-buffers owned-timers condition
           (original-new (symbol-function 'fumos-repl--new-connection))
           (original-network (symbol-function 'make-network-process))
           (original-send (symbol-function 'process-send-string))
           (original-get-buffer-create (symbol-function 'get-buffer-create))
           (original-generate-new-buffer (symbol-function 'generate-new-buffer))
           (original-run-at-time (symbol-function 'run-at-time)))
      (unwind-protect
          (cl-letf
              (((symbol-function 'fumos-repl--new-connection)
                (lambda (instance)
                  (setq connection (funcall original-new instance))))
               ((symbol-function 'get-buffer-create)
                (lambda (name &optional inhibit-buffer-hooks)
                  (let ((buffer
                         (funcall original-get-buffer-create
                                  name inhibit-buffer-hooks)))
                    (push buffer owned-buffers)
                    buffer)))
               ((symbol-function 'generate-new-buffer)
                (lambda (name &optional inhibit-buffer-hooks)
                  (let ((buffer
                         (funcall original-generate-new-buffer
                                  name inhibit-buffer-hooks)))
                    (push buffer owned-buffers)
                    buffer)))
               ((symbol-function 'make-network-process)
                (lambda (&rest arguments)
                  (setq owned-process
                        (apply original-network arguments))))
               ((symbol-function 'run-at-time)
                (lambda (&rest arguments)
                  (let ((timer (apply original-run-at-time arguments)))
                    (push timer owned-timers)
                    timer)))
               ((symbol-function 'process-send-string)
                (lambda (process string)
                  (if (eq process owned-process)
                      (if (eq kind 'quit)
                          (signal 'quit nil)
                        (error "secret=%s" token))
                    (funcall original-send process string)))))
            (setq condition
                  (condition-case caught
                      (progn
                        (fumos-repl-connect-instance
                         (fumos-test-instance-for-server
                          server 4242 nil nil token))
                        (ert-fail "Injected setup failure did not propagate"))
                    (fumos-repl-connection-error caught)
                    (quit caught)))
            (if (eq kind 'quit)
                (should (equal condition '(quit)))
              (fumos-test-assert-fixed-connection-condition condition token))
            (fumos-test-assert-token-not-in-messages token message-start)
            (should connection)
            (fumos-test-assert-closed connection
                                      (list :process owned-process)
                                      token)
            (should-not (seq-some #'buffer-live-p owned-buffers))
            (should-not (seq-some #'fumos-test-timer-live-p owned-timers)))
        (when connection (fumos-repl-close connection))
        (fumos-test-server-stop server)))))

(ert-deftest fumos-repl-buffer-and-network-open-failures-roll-back ()
  (dolist (point '(process-buffer network))
    (let* ((token (make-string 64 (if (eq point 'network) ?e ?f)))
           (server (fumos-test-server-start))
           (message-start
            (with-current-buffer (get-buffer-create "*Messages*") (point-max)))
           connection snapshot condition
           (original-new (symbol-function 'fumos-repl--new-connection))
           (original-prepare (symbol-function 'fumos-repl--prepare-buffers)))
      (unwind-protect
          (cl-letf
              (((symbol-function 'fumos-repl--new-connection)
                (lambda (instance)
                  (setq connection (funcall original-new instance))))
               ((symbol-function 'fumos-repl--prepare-buffers)
                (lambda (value)
                  (if (eq point 'process-buffer)
                      (let ((repl
                             (get-buffer-create
                              (fumos-repl--buffer-name
                               (fumos-connection-instance value)))))
                        (setf (fumos-connection-repl-buffer value) repl)
                        (setq snapshot (list :repl-buffer repl))
                        (error "secret=%s" token))
                    (prog1 (funcall original-prepare value)
                      (setq snapshot
                            (fumos-test-connection-snapshot value))))))
               ((symbol-function 'make-network-process)
                (if (eq point 'network)
                    (lambda (&rest _)
                      (error "secret=%s" token))
                  (symbol-function 'make-network-process))))
            (setq condition
                  (condition-case caught
                      (progn
                        (fumos-repl-connect-instance
                         (fumos-test-instance-for-server
                          server 4242 nil nil token))
                        (ert-fail "Injected setup failure did not propagate"))
                    (fumos-repl-connection-error caught)))
            (fumos-test-assert-fixed-connection-condition condition token)
            (fumos-test-assert-token-not-in-messages token message-start)
            (should connection)
            (fumos-test-assert-closed connection snapshot token))
        (when connection (fumos-repl-close connection))
        (fumos-test-server-stop server)))))

(ert-deftest fumos-repl-bootstrap-send-error-and-quit-roll-back ()
  (dolist (kind '(error quit))
    (let* ((token (make-string 64 (if (eq kind 'error) ?a ?b)))
           (server (fumos-test-server-start))
           (message-start
            (with-current-buffer (get-buffer-create "*Messages*") (point-max)))
           (connection
            (fumos-repl-connect-instance
             (fumos-test-instance-for-server server 4242 nil nil token)))
           (snapshot (fumos-test-connection-snapshot connection))
           condition)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'fennel-proto-repl-send-message)
                       (lambda (&rest _)
                         (signal kind (list (concat "secret=" token))))))
              (setq condition
                    (condition-case caught
                        (progn
                          (fumos-repl--handshake-filter
                           connection
                           (fumos-connection-process connection)
                           (fumos-connection-generation connection)
                           fumos-test-golden-ack)
                          (ert-fail "Injected bootstrap failure did not propagate"))
                      (fumos-repl-connection-error caught))))
            (fumos-test-assert-fixed-connection-condition condition token)
            (fumos-test-assert-token-not-in-messages token message-start)
            (fumos-test-assert-closed connection snapshot token))
        (fumos-repl-close connection)
        (fumos-test-server-stop server)))))

(ert-deftest fumos-repl-upstream-start-error-and-quit-roll-back ()
  (dolist (kind '(error quit))
    (let* ((token (make-string 64 (if (eq kind 'error) ?c ?d)))
           (server (fumos-test-server-start))
           (message-start
            (with-current-buffer (get-buffer-create "*Messages*") (point-max)))
           (connection
            (fumos-repl-connect-instance
             (fumos-test-instance-for-server server 4242 nil nil token)))
           callback condition)
      (unwind-protect
          (progn
            (fumos-repl--handshake-filter
             connection
             (fumos-connection-process connection)
             (fumos-connection-generation connection)
             fumos-test-golden-ack)
            (setq callback
                  (with-current-buffer
                      (fumos-connection-repl-buffer connection)
                    (gethash 0 fennel-proto-repl--message-callbacks)))
            (should callback)
            (let ((snapshot (fumos-test-connection-snapshot connection)))
              (cl-letf (((symbol-function 'fumos-repl--start-upstream-ui)
                         (lambda (&rest _)
                           (signal kind (list (concat "secret=" token))))))
                (setq condition
                      (condition-case caught
                          (progn
                            (funcall
                             (fennel-proto-repl-callback-values callback)
                             '(ok "0.6.4" "1.6.1" "LuaJIT 2.1"))
                            (ert-fail "Injected UI failure did not propagate"))
                        (fumos-repl-connection-error caught))))
              (fumos-test-assert-fixed-connection-condition condition token)
              (fumos-test-assert-token-not-in-messages token message-start)
              (fumos-test-assert-closed connection snapshot token)))
        (fumos-repl-close connection)
        (fumos-test-server-stop server)))))

(defvar fumos-test-saved-bootstrap-callbacks
  (make-hash-table :test #'eq))

(defun fumos-test-make-project-root (root)
  "Create the three markers required for a FUMOS project at ROOT."
  (make-directory (expand-file-name "libraries/fumos" root) t)
  (make-directory (expand-file-name ".emacs" root) t)
  (with-temp-file (expand-file-name "mod.json" root)
    (insert "{\"id\":\"demo\",\"dev\":true}\n"))
  (with-temp-file (expand-file-name "libraries/fumos/lib.json" root)
    (insert "{\"id\":\"fumos\"}\n"))
  (with-temp-file (expand-file-name ".emacs/init.el" root)
    (insert "; test marker\n"))
  root)

(defun fumos-test-make-source-buffer (root relative)
  "Return an activated Fennel source buffer for RELATIVE below ROOT."
  (let* ((file (expand-file-name relative root))
         (buffer (generate-new-buffer (format " *fumos-source:%s*" relative))))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert "(+ 1 2)\n"))
    (with-current-buffer buffer
      (setq buffer-file-name file
            default-directory (file-name-directory file))
      (insert-file-contents file)
      (fennel-mode)
      (fumos-project-activate)
      (set-buffer-modified-p nil))
    buffer))

(defun fumos-test-call-saved-bootstrap-callback (connection)
  "Invoke CONNECTION's captured real init values callback late."
  (when-let ((callback (gethash connection fumos-test-saved-bootstrap-callbacks)))
    (funcall callback '(ok "0.6.4" "1.6.1" "LuaJIT 2.1"))))

(cl-defmacro fumos-test-with-replacement-servers
    ((old replacement old-server new-server) &rest body)
  "Create a ready OLD connection and a replacement server for BODY."
  (declare (indent 1) (debug ((symbolp symbolp symbolp symbolp) body)))
  `(let* ((fumos-repl--connections (make-hash-table :test #'equal))
          (root (fumos-test-make-project-root
                 (make-temp-file "fumos-replacement-root-" t)))
          (,old-server
           (fumos-test-server-start
            (lambda (server _client line)
              (pcase (length (fumos-test-server-lines server))
                (1
                 (should (string-prefix-p "FUMOS/1 AUTH " line))
                 (fumos-test-server-send server (fumos-test-ack-for-pid 4242)))
                (2 (should (string-match-p "___repl___" line)))))))
          (,old (fumos-repl-connect-instance
                 (fumos-test-instance-for-server ,old-server 4242 root)))
          (,replacement nil)
          (,new-server nil))
     (unwind-protect
         (progn
           (should (fumos-test-wait-until
                    (lambda ()
                      (= 2 (length (fumos-test-server-lines ,old-server))))))
           (with-current-buffer (fumos-connection-repl-buffer ,old)
             (let ((callbacks (gethash 0 fennel-proto-repl--message-callbacks)))
               (should callbacks)
               (puthash ,old
                        (fennel-proto-repl-callback-values callbacks)
                        fumos-test-saved-bootstrap-callbacks)))
           (fumos-test-server-send ,old-server fumos-test-init-frame)
           (should (fumos-test-wait-until
                    (lambda () (eq 'ready (fumos-connection-state ,old)))))
           (let ((delegate (fumos-test-make-proto-handler 4242)))
             (setq
              ,new-server
              (fumos-test-server-start
               (lambda (server client line)
                 (when (= 1 (length (fumos-test-server-lines server)))
                   (should-not (process-live-p
                                (fumos-connection-process ,old)))
                   (should-not (process-live-p
                                (fumos-connection-ui-process ,old)))
                   (should-not (buffer-live-p
                                (fumos-connection-process-buffer ,old)))
                   (should-not (buffer-live-p
                                (fumos-connection-repl-buffer ,old))))
                 (funcall delegate server client line)))))
           ,@body)
       (remhash ,old fumos-test-saved-bootstrap-callbacks)
       (when ,replacement (fumos-repl-close ,replacement))
       (fumos-repl-close ,old)
       (when ,new-server (fumos-test-server-stop ,new-server))
       (fumos-test-server-stop ,old-server)
       (delete-directory root t))))

(defun fumos-test-deliver-delayed-ack (server)
  "Deliver SERVER's delayed ACK if its client is still live."
  (when (process-live-p (fumos-test-server-client server))
    (fumos-test-server-send
     server
     (fumos-test-ack-for-pid
      (fumos-instance-pid
       (process-get (fumos-test-server-process server) 'fumos-instance))))))

(defun fumos-test-connect-second-delayed-server (server)
  "Connect the instance stored on delayed SERVER."
  (fumos-repl-connect-instance
   (process-get (fumos-test-server-process server) 'fumos-instance)))

(defun fumos-test-saved-handshake-timer (connection)
  "Return a callable copy of CONNECTION's current handshake timer."
  (let ((timer (fumos-connection-handshake-timer connection)))
    (should (timerp timer))
    (lambda ()
      (apply (timer--function timer) (timer--args timer)))))

(cl-defmacro fumos-test-with-delayed-ack-servers
    ((first second first-server second-server) &rest body)
  "Create FIRST authenticating connection and two delayed ACK servers."
  (declare (indent 1) (debug ((symbolp symbolp symbolp symbolp) body)))
  `(let* ((fumos-repl--connections (make-hash-table :test #'equal))
          (root (fumos-test-make-project-root
                 (make-temp-file "fumos-delayed-root-" t)))
          (delayed-handler
           (lambda (server _client line)
             (pcase (length (fumos-test-server-lines server))
               (1 (should (string-prefix-p "FUMOS/1 AUTH " line)))
               (2
                (should (string-match-p "___repl___" line))
                (fumos-test-server-send server fumos-test-init-frame)))))
          (,first-server (fumos-test-server-start delayed-handler))
          (,second-server (fumos-test-server-start delayed-handler))
          (first-instance
           (fumos-test-instance-for-server ,first-server 4242 root "demo"
                                           (make-string 64 ?a)))
          (second-instance
           (fumos-test-instance-for-server ,second-server 4242 root "demo"
                                           (make-string 64 ?b)))
          (,first nil)
          (,second nil))
     (process-put (fumos-test-server-process ,first-server)
                  'fumos-instance first-instance)
     (process-put (fumos-test-server-process ,second-server)
                  'fumos-instance second-instance)
     (setq ,first (fumos-repl-connect-instance first-instance))
     (unwind-protect
         (progn
           (should (fumos-test-wait-until
                    (lambda ()
                      (= 1 (length (fumos-test-server-lines ,first-server))))))
           ,@body)
       (when ,second (fumos-repl-close ,second))
       (fumos-repl-close ,first)
       (fumos-test-server-stop ,second-server)
       (fumos-test-server-stop ,first-server)
       (delete-directory root t))))

(cl-defstruct fumos-test-bootstrap-fixture
  connection processes buffers callback-table source server root
  next-connection next-server attach-next-function)

(defun fumos-test-bootstrap-failure-connection (fixture)
  (fumos-test-bootstrap-fixture-connection fixture))

(defun fumos-test-bootstrap-failure-processes (fixture)
  (fumos-test-bootstrap-fixture-processes fixture))

(defun fumos-test-bootstrap-failure-buffers (fixture)
  (fumos-test-bootstrap-fixture-buffers fixture))

(defun fumos-test-bootstrap-failure-live-timers (fixture)
  (let ((connection (fumos-test-bootstrap-fixture-connection fixture)))
    (seq-some
     #'timerp
     (append (list (fumos-connection-handshake-timer connection)
                   (fumos-connection-game-reload-timer connection))
             (fumos-connection-retry-timers connection)
             (fumos-connection-callback-timers connection)
             (fumos-connection-terminal-timers connection)))))

(defun fumos-test-bootstrap-failure-pending-callbacks (fixture)
  (not (hash-table-empty-p
        (fumos-test-bootstrap-fixture-callback-table fixture))))

(defun fumos-test-bootstrap-failure-attach-next (fixture)
  (funcall (fumos-test-bootstrap-fixture-attach-next-function fixture)))

(cl-defmacro fumos-test-with-bootstrap-failure ((fixture point) &rest body)
  "Create a bootstrapping connection and inject failure at POINT."
  (declare (indent 1) (debug ((symbolp form) body)))
  `(let* ((fumos-repl--connections (make-hash-table :test #'equal))
          (root (fumos-test-make-project-root
                 (make-temp-file "fumos-bootstrap-root-" t)))
          (source (fumos-test-make-source-buffer root "scripts/source.fnl"))
          (server
           (fumos-test-server-start
            (lambda (state _client line)
              (pcase (length (fumos-test-server-lines state))
                (1 (fumos-test-server-send state (fumos-test-ack-for-pid 4242)))
                (2 (should (string-match-p "___repl___" line)))))))
          (connection
           (fumos-repl-connect-instance
            (fumos-test-instance-for-server server 4242 root)))
          (,fixture nil)
          (original-start-process (symbol-function 'start-process))
          (original-set-state (symbol-function 'fumos-repl--set-state))
          (original-link (symbol-function 'fumos-repl--link-project-buffers)))
     (should (fumos-test-wait-until
              (lambda () (eq 'bootstrapping
                             (fumos-connection-state connection)))))
     (setq
      ,fixture
      (make-fumos-test-bootstrap-fixture
       :connection connection
       :processes (list (fumos-connection-process connection))
       :buffers (list (fumos-connection-process-buffer connection)
                      (fumos-connection-repl-buffer connection))
       :callback-table
       (with-current-buffer (fumos-connection-repl-buffer connection)
         fennel-proto-repl--message-callbacks)
       :source source :server server :root root))
     (setf
      (fumos-test-bootstrap-fixture-attach-next-function ,fixture)
      (lambda ()
        (let* ((next-server
                (fumos-test-server-start (fumos-test-make-proto-handler 4242)))
               (next
                (fumos-repl-connect-instance
                 (fumos-test-instance-for-server
                  next-server 4242 root "demo" (make-string 64 ?b)))))
          (setf (fumos-test-bootstrap-fixture-next-server ,fixture) next-server
                (fumos-test-bootstrap-fixture-next-connection ,fixture) next)
          (should (fumos-test-wait-until
                   (lambda () (eq 'ready (fumos-connection-state next)))))
          (fumos-connection-state next))))
     (unwind-protect
         (cl-letf
             (((symbol-function 'start-process)
               (lambda (&rest arguments)
                 (let ((process (apply original-start-process arguments)))
                   (push process
                         (fumos-test-bootstrap-fixture-processes ,fixture))
                   process)))
              ((symbol-function 'fumos-repl--set-state)
               (lambda (value state)
                 (if (and (eq value connection)
                          (eq ,point 'before-ready)
                          (eq state 'ready))
                     (error "injected before-ready failure")
                   (funcall original-set-state value state))))
              ((symbol-function 'fumos-repl--link-project-buffers)
               (lambda (value)
                 (if (not (eq value connection))
                     (funcall original-link value)
                   (pcase ,point
                     ('after-ui-process
                      (error "injected after-ui-process failure"))
                     ('link-buffer
                      (funcall original-link value)
                      (error "injected link-buffer failure"))
                     (_ (funcall original-link value)))))))
           ,@body)
       (when (fumos-test-bootstrap-fixture-next-connection ,fixture)
         (fumos-repl-close
          (fumos-test-bootstrap-fixture-next-connection ,fixture)))
       (fumos-repl-close connection)
       (when (fumos-test-bootstrap-fixture-next-server ,fixture)
         (fumos-test-server-stop
          (fumos-test-bootstrap-fixture-next-server ,fixture)))
       (fumos-test-server-stop server)
       (when (buffer-live-p source) (kill-buffer source))
       (delete-directory root t))))

(cl-defstruct fumos-test-open-fixture
  connection root instance buffers processes server)

(defun fumos-test-open-failure-root (fixture)
  (fumos-test-open-fixture-root fixture))

(defun fumos-test-open-failure-instance (fixture)
  (fumos-test-open-fixture-instance fixture))

(defun fumos-test-open-failure-buffers (fixture)
  (fumos-test-open-fixture-buffers fixture))

(defun fumos-test-open-failure-processes (fixture)
  (fumos-test-open-fixture-processes fixture))

(defun fumos-test-open-failure-live-timers (fixture)
  (when-let ((connection (fumos-test-open-fixture-connection fixture)))
    (seq-some
     #'timerp
     (append (list (fumos-connection-handshake-timer connection))
             (fumos-connection-retry-timers connection)
             (fumos-connection-callback-timers connection)
             (fumos-connection-terminal-timers connection)))))

(cl-defmacro fumos-test-with-open-failure ((fixture point) &rest body)
  "Inject one synchronous open failure at POINT while recording resources."
  (declare (indent 1) (debug ((symbolp form) body)))
  `(let* ((native-comp-enable-subr-trampolines nil)
          (fumos-repl--connections (make-hash-table :test #'equal))
          (root (fumos-test-make-project-root
                 (make-temp-file "fumos-open-root-" t)))
          (server (fumos-test-server-start))
          (instance (fumos-test-instance-for-server server 4242 root))
          (,fixture
           (make-fumos-test-open-fixture
            :root root :instance instance :server server))
          (original-get-buffer-create (symbol-function 'get-buffer-create))
          (original-generate-new-buffer (symbol-function 'generate-new-buffer))
          (original-make-network-process (symbol-function 'make-network-process))
          (original-process-send-string (symbol-function 'process-send-string))
          (original-open-instance (symbol-function 'fumos-repl--open-instance)))
     (when (eq ,point 'connection-refused)
       (fumos-test-server-stop server)
       (setf (fumos-test-open-fixture-server ,fixture) nil))
     (unwind-protect
         (cl-letf
             (((symbol-function 'get-buffer-create)
               (lambda (name &optional inhibit-buffer-hooks)
                 (let ((buffer (funcall original-get-buffer-create
                                        name inhibit-buffer-hooks)))
                   (cl-pushnew buffer (fumos-test-open-fixture-buffers ,fixture))
                   buffer)))
              ((symbol-function 'generate-new-buffer)
               (lambda (name &optional inhibit-buffer-hooks)
                 (when (eq ,point 'buffer-create)
                   (error "injected process-buffer creation failure"))
                 (let ((buffer (funcall original-generate-new-buffer
                                        name inhibit-buffer-hooks)))
                   (push buffer (fumos-test-open-fixture-buffers ,fixture))
                   buffer)))
              ((symbol-function 'make-network-process)
               (lambda (&rest arguments)
                 (when (eq ,point 'make-network-process)
                   (error "injected make-network-process failure"))
                 (let ((process (apply original-make-network-process arguments)))
                   (push process (fumos-test-open-fixture-processes ,fixture))
                   process)))
              ((symbol-function 'process-send-string)
               (lambda (process string)
                 (when (string-prefix-p "FUMOS/1 AUTH " string)
                   (pcase ,point
                     ('auth-send (error "injected auth send failure"))
                     ('quit (signal 'quit nil))))
                 (funcall original-process-send-string process string)))
              ((symbol-function 'fumos-repl--open-instance)
               (lambda (connection)
                 (setf (fumos-test-open-fixture-connection ,fixture) connection)
                 (funcall original-open-instance connection))))
           ,@body)
       (when (fumos-test-open-fixture-connection ,fixture)
         (fumos-repl-close (fumos-test-open-fixture-connection ,fixture)))
       (when (fumos-test-open-fixture-server ,fixture)
         (fumos-test-server-stop (fumos-test-open-fixture-server ,fixture)))
       (dolist (buffer (fumos-test-open-fixture-buffers ,fixture))
         (when (buffer-live-p buffer) (kill-buffer buffer)))
       (delete-directory root t))))

(cl-defmacro fumos-test-with-two-ready-connections
    ((first first-server second second-server) &rest body)
  "Create two independent ready FUMOS project connections for BODY."
  (declare (indent 1) (debug ((symbolp symbolp symbolp symbolp) body)))
  `(let* ((fumos-repl--connections (make-hash-table :test #'equal))
          (first-root (make-temp-file "fumos-first-root-" t))
          (second-root (make-temp-file "fumos-second-root-" t))
          (,first-server
           (fumos-test-server-start (fumos-test-make-proto-handler 4242)))
          (,second-server
           (fumos-test-server-start (fumos-test-make-proto-handler 5252)))
          (,first
           (fumos-repl-connect-instance
            (fumos-test-instance-for-server ,first-server 4242 first-root)))
          (,second
           (fumos-repl-connect-instance
            (fumos-test-instance-for-server ,second-server 5252 second-root))))
     (unwind-protect
         (progn
           (should (fumos-test-wait-until
                    (lambda () (eq 'ready (fumos-connection-state ,first)))))
           (should (fumos-test-wait-until
                    (lambda () (eq 'ready (fumos-connection-state ,second)))))
           ,@body)
       (fumos-repl-close ,second)
       (fumos-repl-close ,first)
       (fumos-test-server-stop ,second-server)
       (fumos-test-server-stop ,first-server)
       (delete-directory second-root t)
       (delete-directory first-root t))))

(defun fumos-test-install-retry-pair (connection label)
  "Install real outer/embedded callbacks on CONNECTION for LABEL."
  (with-current-buffer (fumos-connection-repl-buffer connection)
    (setq-local fennel-proto-repl--message-id 20)
    (let ((outer
           (fennel-proto-repl-send-message
            :eval (format "outer-%s" label) #'ignore #'ignore #'ignore))
          (embedded
           (fennel-proto-repl-send-message
            :eval label #'ignore #'ignore #'ignore)))
      (cons outer embedded))))

(defun fumos-test-remove-callback (connection id)
  "Remove ID from CONNECTION's real upstream callback table."
  (with-current-buffer (fumos-connection-repl-buffer connection)
    (remhash id fennel-proto-repl--message-callbacks)))

(defun fumos-test-retry-frame (outer-id embedded-id label)
  "Return one real plist retry frame for LABEL."
  (format "(:id %d :op \"retry\" :message %S)\n"
          outer-id (format "{:eval %S :id %d}" label embedded-id)))

(defun fumos-test-server-saw-resend-p (server id label)
  "Return non-nil if SERVER saw the exact canonical retry payload."
  (member (format "{:eval %S :id %d}" label id)
          (fumos-test-server-lines server)))

(cl-defmacro fumos-test-with-source-before-and-after-attach
    ((before after connection server) &rest body)
  "Create source buffers on both sides of one successful attach."
  (declare (indent 1) (debug ((symbolp symbolp symbolp symbolp) body)))
  `(let* ((fumos-repl--connections (make-hash-table :test #'equal))
          (root (fumos-test-make-project-root
                 (make-temp-file "fumos-source-root-" t)))
          (,before (fumos-test-make-source-buffer root "scripts/before.fnl"))
          (,server (fumos-test-server-start (fumos-test-make-proto-handler 4242)))
          (,connection
           (fumos-repl-connect-instance
            (fumos-test-instance-for-server ,server 4242 root)))
          (,after nil))
     (unwind-protect
         (progn
           (should (fumos-test-wait-until
                    (lambda () (eq 'ready
                                   (fumos-connection-state ,connection)))))
           (setq ,after
                 (fumos-test-make-source-buffer root "fnl/after.fnl"))
           ,@body)
       (fumos-repl-close ,connection)
       (fumos-test-server-stop ,server)
       (dolist (buffer (list ,before ,after))
         (when (buffer-live-p buffer) (kill-buffer buffer)))
       (delete-directory root t))))

(cl-defmacro fumos-test-with-linked-source-reconnect
    ((source old replacement server) &rest body)
  "Create linked SOURCE, OLD history and a discoverable replacement."
  (declare (indent 1) (debug ((symbolp symbolp symbolp symbolp) body)))
  `(let* ((fumos-repl--connections (make-hash-table :test #'equal))
          (root (fumos-test-make-project-root
                 (make-temp-file "fumos-reconnect-root-" t)))
          (,server (fumos-test-server-start (fumos-test-make-proto-handler 4242)))
          (next-server
           (fumos-test-server-start (fumos-test-make-proto-handler 4242)))
          (old-instance
           (fumos-test-instance-for-server ,server 4242 root "demo"
                                           (make-string 64 ?a)))
          (next-instance
           (fumos-test-instance-for-server next-server 4242 root "demo"
                                           (make-string 64 ?b)))
          (,old (fumos-repl-connect-instance old-instance))
          (,replacement nil)
          (,source nil)
          (original-connect (symbol-function 'fumos-repl-connect-instance)))
     (unwind-protect
         (progn
           (should (fumos-test-wait-until
                    (lambda () (eq 'ready (fumos-connection-state ,old)))))
           (setq ,source
                 (fumos-test-make-source-buffer root "scripts/reconnect.fnl"))
           (cl-letf (((symbol-function 'fumos-discover-instances)
                      (lambda (_root) (list next-instance)))
                     ((symbol-function 'fumos-repl-connect-instance)
                      (lambda (instance)
                        (setq ,replacement
                              (funcall original-connect instance)))))
             ,@body))
       (when ,replacement (fumos-repl-close ,replacement))
       (fumos-repl-close ,old)
       (fumos-test-server-stop next-server)
       (fumos-test-server-stop ,server)
       (when (buffer-live-p ,source) (kill-buffer ,source))
       (delete-directory root t))))

(ert-deftest fumos-real-tcp-bootstrap-delivers-init-exactly-once ()
  (let ((original-finish
         (symbol-function 'fumos-repl--finish-bootstrap))
        (finish-count 0))
    (cl-letf (((symbol-function 'fumos-repl--finish-bootstrap)
               (lambda (connection values)
                 (cl-incf finish-count)
                 (funcall original-finish connection values))))
      (fumos-test-with-ready-connection (connection server)
        (should (= 1 finish-count))
        (with-current-buffer (fumos-connection-repl-buffer connection)
          (should-not (gethash 0 fennel-proto-repl--message-callbacks)))
        (should-not
         (gethash 0 (fumos-repl--callback-delivery-table connection)))
        ;; A duplicate real TCP init frame has no callback identity left and
        ;; therefore cannot commit bootstrap a second time.
        (fumos-test-server-send server fumos-test-init-frame)
        (accept-process-output nil 0.05)
        (should (= 1 finish-count))))))

(ert-deftest fumos-proto-correlates-interleaved-request-ids ()
  (fumos-test-with-ready-connection (connection server)
    (let (first second)
      (with-current-buffer (fumos-connection-repl-buffer connection)
        (let ((first-id
               (fennel-proto-repl-send-message
                :eval "(values 1 2)"
                (lambda (values) (setq first values))))
              (second-id
               (fennel-proto-repl-send-message
                :eval "(error :boom)" #'ignore
                (lambda (type message trace)
                  (setq second (list type message trace)))
                #'ignore)))
          (fumos-test-server-send
           server
           (format
            (concat "(:id %d :op \"accept\")\n"
                    "(:id %d :op \"accept\")\n"
                    "(:id %d :op \"error\" :type \"runtime\" :data \"boom\")\n"
                    "(:id %d :op \"eval\" :values (\"1\" \"2\"))\n"
                    "(:id %d :op \"done\")\n"
                    "(:id %d :op \"done\")\n")
            first-id second-id second-id first-id second-id first-id))
          (should (fumos-test-wait-until (lambda () (and first second))))
          (should (equal '("1" "2") first))
          (should (equal '("runtime" "boom" nil) second))
          (should-not (fennel-proto-repl-callbacks-pending)))))))

(ert-deftest fumos-observer-stays-busy-until-last-active-request-done ()
  (with-temp-buffer
    (let ((connection
           (make-fumos-connection :state 'ready :repl-buffer (current-buffer))))
      (fumos-repl--observe-frame connection "(:id 11 :op \"accept\")")
      (fumos-repl--observe-frame connection "(:id 12 :op \"accept\")")
      (should (equal '(12 11)
                     (fumos-connection-active-request-ids connection)))
      (fumos-repl--observe-frame connection "(:id 11 :op \"done\")")
      (should (equal '(12) (fumos-connection-active-request-ids connection)))
      (should (eq 'busy (fumos-connection-state connection)))
      (should (equal '(":busy") mode-line-process))
      ;; An unrelated done cannot make the remaining accepted request ready.
      (fumos-repl--observe-frame connection "(:id 99 :op \"done\")")
      (should (equal '(12) (fumos-connection-active-request-ids connection)))
      (should (eq 'busy (fumos-connection-state connection)))
      (fumos-repl--observe-frame connection "(:id 12 :op \"done\")")
      (should-not (fumos-connection-active-request-ids connection))
      (should (eq 'ready (fumos-connection-state connection)))
      (should (equal '(":ready") mode-line-process)))))

(ert-deftest fumos-real-filter-reasserts-busy-after-one-of-two-done ()
  (fumos-test-with-ready-connection (connection server)
    (let (first-id second-id)
      (with-current-buffer (fumos-connection-repl-buffer connection)
        (setq first-id
              (fennel-proto-repl-send-message
               :eval "first-active" #'ignore #'ignore #'ignore)
              second-id
              (fennel-proto-repl-send-message
               :eval "second-active" #'ignore #'ignore #'ignore)))
      ;; The pinned upstream filter handles these lines in order and writes
      ;; (:ready) for FIRST-ID's done.  FUMOS must overwrite that stale UI
      ;; state after upstream returns because SECOND-ID is still active.
      (fumos-test-server-send
       server
       (format
        (concat "(:id %d :op \"accept\")\n"
                "(:id %d :op \"accept\")\n"
                "(:id %d :op \"done\")\n")
        first-id second-id first-id))
      (should
       (fumos-test-wait-until
        (lambda ()
          (and (equal (list second-id)
                      (fumos-connection-active-request-ids connection))
               (with-current-buffer
                   (fumos-connection-repl-buffer connection)
                 (equal '(":busy") mode-line-process))))))
      (should (eq 'busy (fumos-connection-state connection)))
      (fumos-test-server-send
       server (format "(:id %d :op \"done\")\n" second-id))
      (should
       (fumos-test-wait-until
        (lambda ()
          (and (eq 'ready (fumos-connection-state connection))
               (with-current-buffer
                   (fumos-connection-repl-buffer connection)
                 (equal '(":ready") mode-line-process)))))))))

(ert-deftest fumos-disconnect-fails-pending-callbacks ()
  (fumos-test-with-ready-connection (connection server)
    (let ((ui-process (fumos-connection-ui-process connection))
          (repl-buffer (fumos-connection-repl-buffer connection))
          failure)
      (with-current-buffer (fumos-connection-repl-buffer connection)
        (fennel-proto-repl-send-message
         :eval "(while true nil)" #'ignore
         (lambda (type message _trace)
           (setq failure (list type message)))
         #'ignore))
      (delete-process (fumos-test-server-client server))
      (should (fumos-test-wait-until (lambda () failure)))
      (should (equal '("connection-lost" "FUMOS connection closed") failure))
      ;; History remains visible, but upstream's fake comint process does not.
      (should (buffer-live-p repl-buffer))
      (should-not (process-live-p ui-process)))))

(ert-deftest fumos-eval-without-done-disconnect-has-one-terminal-outcome ()
  (dolist (frame-kind '(values error))
    (fumos-test-with-ready-connection (connection server)
      (let* ((original-run-at-time (symbol-function 'run-at-time))
             (original-cancel-timer (symbol-function 'cancel-timer))
             (original-set-state (symbol-function 'fumos-repl--set-state))
             scheduled delivery-timer canceled terminals
             (disconnected-transitions 0)
             id)
        (with-current-buffer (fumos-connection-repl-buffer connection)
          (setq id
                (fennel-proto-repl-send-message
                 :eval "terminal-race"
                 (lambda (_values) (push 'values terminals))
                 (lambda (type &rest _)
                   (push (if (equal type "connection-lost")
                             'connection-lost
                           'error)
                         terminals)))))
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_delay _repeat callback &rest args)
                     (if scheduled
                         (apply original-run-at-time 0 nil callback args)
                       (setq delivery-timer
                             (funcall original-run-at-time 3600 nil #'ignore)
                             scheduled (cons callback args))
                       delivery-timer)))
                  ((symbol-function 'cancel-timer)
                   (lambda (timer)
                     (when (eq timer delivery-timer) (setq canceled t))
                     (funcall original-cancel-timer timer)))
                  ((symbol-function 'fumos-repl--set-state)
                   (lambda (value state)
                     (when (eq state 'disconnected)
                       (cl-incf disconnected-transitions))
                     (funcall original-set-state value state))))
          ;; Admit one terminal frame, but keep its zero-delay timer dormant.
          (fumos-test-server-send
           server
           (format
            (concat
             "(:id %d :op \"accept\")\n"
             (if (eq frame-kind 'values)
                 "(:id %d :op \"eval\" :values (\"ok\"))\n"
               (concat "(:id %d :op \"error\" :type \"runtime\" "
                       ":data \"boom\")\n")))
            id id))
          (should (fumos-test-wait-until (lambda () scheduled)))
          (fumos-test-server-drop-client server)
          (should (fumos-test-wait-until
                   (lambda () (eq 'disconnected
                                  (fumos-connection-state connection)))))
          (should (fumos-test-wait-until (lambda () terminals))))
        (should canceled)
        (should (equal '(connection-lost) terminals))
        (should (= 1 disconnected-transitions))
        (should-not (fumos-connection-callback-timers connection))
        (should-not (fumos-connection-terminal-timers connection))
        (should (hash-table-empty-p
                 (fumos-repl--callback-delivery-table connection)))
        ;; Simulate a timer already dequeued by Emacs despite cancel-timer.
        (apply (car scheduled) (cdr scheduled))
        (should (equal '(connection-lost) terminals))))))

(ert-deftest fumos-control-commands-use-transport-frames ()
  (fumos-test-with-ready-connection (connection server)
    (with-current-buffer (fumos-connection-repl-buffer connection)
      (fumos-interrupt)
      (setf (fumos-connection-active-request-ids connection) '(73 41))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "41")))
        (fumos-cancel-active-request))
      (fumos-disconnect))
    (should
     (fumos-test-wait-until
      (lambda ()
        (let ((lines (fumos-test-server-lines server)))
          (and (member "FUMOS/1 INTERRUPT" lines)
               (member "FUMOS/1 CANCEL 41" lines)
               (member "FUMOS/1 DETACH" lines))))))))

(ert-deftest fumos-reconnect-refuses-another-pid ()
  (fumos-test-with-ready-connection (connection server)
    (let ((other
           (copy-fumos-instance (fumos-connection-instance connection))))
      (setf (fumos-instance-pid other) 5252)
      (cl-letf (((symbol-function 'fumos-discover-instances)
                 (lambda (_root) (list other))))
        (with-current-buffer (fumos-connection-repl-buffer connection)
          (should-error (fumos-reconnect) :type 'user-error))))))

(ert-deftest fumos-same-pid-reconnect-tears-down-before-new-auth ()
  (fumos-test-with-replacement-servers (old replacement old-server new-server)
    (let* ((old-process (fumos-connection-process old))
           (old-ui-process (fumos-connection-ui-process old))
           (old-process-buffer (fumos-connection-process-buffer old))
           (old-repl-buffer (fumos-connection-repl-buffer old))
           (old-generation (fumos-connection-generation old))
           (old-filter (process-filter old-process))
           (old-sentinel (process-sentinel old-process))
           (original-run-at-time (symbol-function 'run-at-time))
           (original-cancel-timer (symbol-function 'cancel-timer))
           old-failure old-value new-value request-id
           delivery-call delivery-timer canceled-timers)
      (with-current-buffer old-repl-buffer
        (setq request-id
              (fennel-proto-repl-send-message
               :eval "old"
               (lambda (values) (setq old-value values))
               (lambda (&rest _) (setq old-failure t)) #'ignore)))
      ;; Admit an old values callback but keep its zero-delay timer dormant.
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_delay _repeat callback &rest args)
                   (setq delivery-timer
                         (funcall original-run-at-time 3600 nil #'ignore)
                         delivery-call (cons callback args))
                   delivery-timer)))
        (fumos-test-server-send
         old-server
         (format (concat "(:id %d :op \"accept\")\n"
                         "(:id %d :op \"eval\" :values (\"old\"))\n")
                 request-id request-id))
        (should (fumos-test-wait-until (lambda () delivery-call))))
      (cl-letf (((symbol-function 'cancel-timer)
                 (lambda (timer)
                   (push timer canceled-timers)
                   (funcall original-cancel-timer timer))))
        (setq replacement (fumos-repl-connect-instance
                           (fumos-test-instance-for-server new-server))))
      ;; New AUTH is observed only after every old resource is dead.
      (should-not (process-live-p old-process))
      (should-not (process-live-p old-ui-process))
      (should-not (buffer-live-p old-process-buffer))
      (should-not (buffer-live-p old-repl-buffer))
      (should (fumos-test-wait-until (lambda () old-failure)))
      (should (memq delivery-timer canceled-timers))
      (should-not (fumos-connection-callback-timers old))
      (should-not (fumos-connection-terminal-timers old))
      (should (hash-table-empty-p
               (fumos-repl--callback-delivery-table old)))
      (should (/= old-generation (fumos-connection-generation replacement)))
      (should (fumos-test-wait-until
               (lambda () (eq 'ready (fumos-connection-state replacement)))))
      (with-current-buffer (fumos-connection-repl-buffer replacement)
        (fennel-proto-repl-send-message :eval "new"
                                        (lambda (v) (setq new-value v))))
      ;; Saved old closures and bootstrap callback are deliberately invoked late.
      (funcall old-filter old-process "(:id 1 :op \"eval\" :values (\"old\"))\n")
      (funcall old-sentinel old-process "closed\n")
      (fumos-test-call-saved-bootstrap-callback old)
      ;; Even a callback dequeued before cancel-timer observed it stays stale.
      (apply (car delivery-call) (cdr delivery-call))
      (should-not old-value)
      (should-not new-value)
      (should (memq (fumos-connection-state replacement) '(ready busy)))
      (should-not (eq (fumos-connection-repl-buffer replacement)
                      old-repl-buffer)))))

(ert-deftest fumos-fast-double-attach-replaces-pending-reservation ()
  (fumos-test-with-delayed-ack-servers (first second first-server second-server)
    (let ((first-repl (fumos-connection-repl-buffer first))
          (first-process (fumos-connection-process first))
          (first-filter (process-filter (fumos-connection-process first)))
          (first-timer-callback (fumos-test-saved-handshake-timer first))
          (root (fumos-instance-project-root
                 (fumos-connection-instance first))))
      ;; Starting SECOND performs replacement while FIRST is still authenticating.
      (setq second (fumos-test-connect-second-delayed-server second-server))
      (should (eq second (gethash root fumos-repl--connections)))
      (should-not (process-live-p first-process))
      (should-not (buffer-live-p first-repl))
      (should-not (eq first-repl (fumos-connection-repl-buffer second)))
      (fumos-test-deliver-delayed-ack first-server)
      (funcall first-filter first-process fumos-test-golden-ack)
      (funcall first-timer-callback)
      (should (eq second (gethash root fumos-repl--connections)))
      (should-not (memq (fumos-connection-state first) '(ready busy)))
      (should (fumos-test-wait-until
               (lambda ()
                 (= 1 (length (fumos-test-server-lines second-server))))))
      (fumos-test-deliver-delayed-ack second-server)
      (should (fumos-test-wait-until
               (lambda () (eq 'ready (fumos-connection-state second))))))))

(ert-deftest fumos-bootstrap-ready-transition-validates-reservation-identity ()
  (let* ((fumos-repl--connections (make-hash-table :test #'equal))
         (root "/work/bootstrap-identity/")
         (instance (make-fumos-instance :project-root root :pid 4242))
         (connection (make-fumos-connection
                      :instance instance :state 'bootstrapping))
         (replacement (make-fumos-connection :instance instance))
         started linked closed)
    (cl-letf (((symbol-function 'fumos-repl--start-upstream-ui)
               (lambda (value values)
                 (should (eq value connection))
                 (setq started values)))
              ((symbol-function 'fumos-repl--link-project-buffers)
               (lambda (value) (setq linked value)))
              ((symbol-function 'fumos-repl--bootstrap-commit-owned-p)
               (lambda (value)
                 (and (eq value (gethash root fumos-repl--connections))
                      (not (fumos-connection-closing value)))))
              ((symbol-function 'fumos-repl-close)
               (lambda (value &optional message)
                 (setq closed (list value message))
                 (setf (fumos-connection-state value) 'disconnected)))
              ((symbol-function 'message) #'ignore))
      ;; A stale init callback may not enter ready or touch the replacement.
      (puthash root replacement fumos-repl--connections)
      (fumos-repl--finish-bootstrap
       connection '(ok "0.6.4" "1.6.1" "LuaJIT 2.1"))
      (should (eq connection (car closed)))
      (should-not started)
      (should-not linked)
      (should (eq replacement (gethash root fumos-repl--connections)))
      ;; The same transition succeeds only while the reservation is identical.
      (setq closed nil)
      (setf (fumos-connection-state connection) 'bootstrapping)
      (puthash root connection fumos-repl--connections)
      (fumos-repl--finish-bootstrap
       connection '(ok "0.6.4" "1.6.1" "LuaJIT 2.1"))
      (should-not closed)
      (should (equal '(ok "0.6.4" "1.6.1" "LuaJIT 2.1") started))
      (should (eq connection linked))
      (should (eq 'ready (fumos-connection-state connection))))))

(ert-deftest fumos-bootstrap-revalidates-reservation-after-provisional-link ()
  (let* ((fumos-repl--connections (make-hash-table :test #'equal))
         (root "/work/bootstrap-reentrant/")
         (instance (make-fumos-instance :project-root root :pid 4242))
         (connection (make-fumos-connection
                      :instance instance :state 'bootstrapping))
         (replacement (make-fumos-connection :instance instance))
         started closed)
    (puthash root connection fumos-repl--connections)
    (cl-letf (((symbol-function 'fumos-repl--bootstrap-commit-owned-p)
               (lambda (value)
                 (and (eq value (gethash root fumos-repl--connections))
                      (not (fumos-connection-closing value)))))
              ((symbol-function 'fumos-repl--start-upstream-ui)
               (lambda (&rest _) (setq started t)))
              ((symbol-function 'fumos-repl--link-project-buffers)
               (lambda (_value)
                 ;; Simulate a replacement from an upstream synchronous query.
                 (puthash root replacement fumos-repl--connections)))
              ((symbol-function 'fumos-repl-close)
               (lambda (value &optional _message)
                 (setq closed value)
                 (setf (fumos-connection-closing value) t
                       (fumos-connection-state value) 'disconnected)))
              ((symbol-function 'message) #'ignore))
      (fumos-repl--finish-bootstrap
       connection '(ok "0.6.4" "1.6.1" "LuaJIT 2.1")))
    (should started)
    (should (eq connection closed))
    (should (eq replacement (gethash root fumos-repl--connections)))
    (should-not (eq 'ready (fumos-connection-state connection)))))

(ert-deftest fumos-bootstrap-partial-failures-release-every-resource ()
  (dolist (point '(after-ui-process before-ready link-buffer))
    (fumos-test-with-bootstrap-failure (fixture point)
      (let* ((connection (fumos-test-bootstrap-failure-connection fixture))
             (root (fumos-instance-project-root
                    (fumos-connection-instance connection))))
        (fumos-repl--finish-bootstrap
         connection '(ok "0.6.4" "1.6.1" "LuaJIT 2.1"))
        (should (eq 'disconnected (fumos-connection-state connection)))
        (should-not (gethash root fumos-repl--connections))
        (should-not (seq-some #'process-live-p
                              (fumos-test-bootstrap-failure-processes fixture)))
        (should-not (seq-some #'buffer-live-p
                              (fumos-test-bootstrap-failure-buffers fixture)))
        ;; Source buffers are user-owned: close unlinks them but never kills them.
        (let ((source (fumos-test-bootstrap-fixture-source fixture)))
          (should (buffer-live-p source))
          (with-current-buffer source
            (should-not fennel-proto-repl-minor-mode)
            (should-not fennel-proto-repl--buffer)
            (should-not
             (memq #'fumos-repl--xref-backend xref-backend-functions))))
        (should (fumos-test-wait-until
                 (lambda ()
                   (not (fumos-test-bootstrap-failure-live-timers fixture)))))
        (should-not (fumos-test-bootstrap-failure-live-timers fixture))
        (should-not (fumos-test-bootstrap-failure-pending-callbacks fixture))
        ;; A failed two-phase finish cannot poison the next reservation.
        (should (eq 'ready
                    (fumos-test-bootstrap-failure-attach-next fixture)))))))

(ert-deftest fumos-open-failures-release-reservation-and-all-resources ()
  (dolist (point '(buffer-create make-network-process auth-send quit
                   connection-refused))
    (fumos-test-with-open-failure (fixture point)
      (if (eq point 'quit)
          (let (saw-quit)
            (condition-case nil
                (progn
                  (fumos-repl-connect-instance
                   (fumos-test-open-failure-instance fixture))
                  (ert-fail "injected quit did not propagate"))
              (quit (setq saw-quit t)))
            (should saw-quit))
        (should-error
         (fumos-repl-connect-instance
          (fumos-test-open-failure-instance fixture))
         :type 'fumos-repl-connection-error))
      (should-not (gethash (fumos-test-open-failure-root fixture)
                           fumos-repl--connections))
      (should-not (seq-some #'buffer-live-p
                            (fumos-test-open-failure-buffers fixture)))
      (should-not (seq-some #'process-live-p
                            (fumos-test-open-failure-processes fixture)))
      (should-not (fumos-test-open-failure-live-timers fixture)))))

(ert-deftest fumos-foreign-repl-name-collision-is-completely-nondestructive ()
  (let* ((fumos-repl--connections (make-hash-table :test #'equal))
         (root (make-temp-file "fumos-foreign-buffer-root-" t))
         (server (fumos-test-server-start))
         (instance (fumos-test-instance-for-server server 4242 root))
         (name (fumos-repl--buffer-name instance))
         (foreign (get-buffer-create name)))
    (unwind-protect
        (progn
          (with-current-buffer foreign
            (text-mode)
            (insert "user-owned contents\n")
            (set-buffer-modified-p nil))
          (should-error (fumos-repl-connect-instance instance)
                        :type 'fumos-repl-connection-error)
          (should (buffer-live-p foreign))
          (should (equal name (buffer-name foreign)))
          (with-current-buffer foreign
            (should (eq 'text-mode major-mode))
            (should (equal "user-owned contents\n" (buffer-string))))
          (should-not
           (gethash (fumos-instance-project-root instance)
                    fumos-repl--connections))
          (should-not (fumos-test-server-lines server)))
      (when (buffer-live-p foreign) (kill-buffer foreign))
      (fumos-test-server-stop server)
      (delete-directory root t))))

(ert-deftest fumos-killing-repl-buffer-tears-down-hidden-transport ()
  (fumos-test-with-ready-connection (connection server)
    (let ((process (fumos-connection-process connection))
          (ui-process (fumos-connection-ui-process connection))
          (process-buffer (fumos-connection-process-buffer connection))
          (root (fumos-instance-project-root
                 (fumos-connection-instance connection)))
          failed)
      (should-not (process-query-on-exit-flag ui-process))
      (with-current-buffer (fumos-connection-repl-buffer connection)
        (fennel-proto-repl-send-message
         :eval "pending" #'ignore
         (lambda (&rest _) (setq failed t)) #'ignore)
        (kill-buffer (current-buffer)))
      (should (fumos-test-wait-until (lambda () failed)))
      (should-not (process-live-p process))
      (should-not (process-live-p ui-process))
      (should-not (buffer-live-p process-buffer))
      (should-not (gethash root fumos-repl--connections))
      (should-not (fumos-connection-retry-timers connection))
      (should (eq 'disconnected (fumos-connection-state connection))))))

(ert-deftest fumos-message-id-remains-local-when-second-connection-starts ()
  (let* ((fumos-repl--connections (make-hash-table :test #'equal))
         (first-root (make-temp-file "fumos-id-first-" t))
         (second-root (make-temp-file "fumos-id-second-" t))
         (first-server
          (fumos-test-server-start (fumos-test-make-proto-handler 4242)))
         (second-server
          (fumos-test-server-start (fumos-test-make-proto-handler 5252)))
         (first
          (fumos-repl-connect-instance
           (fumos-test-instance-for-server first-server 4242 first-root)))
         second first-id next-id first-callback)
    (unwind-protect
        (progn
          (should (fumos-test-wait-until
                   (lambda () (eq 'ready (fumos-connection-state first)))))
          (with-current-buffer (fumos-connection-repl-buffer first)
            (setq first-id
                  (fennel-proto-repl-send-message :eval "pending-a" #'ignore))
            (setq first-callback
                  (gethash first-id fennel-proto-repl--message-callbacks)))
          (setq second
                (fumos-repl-connect-instance
                 (fumos-test-instance-for-server
                  second-server 5252 second-root)))
          (should (fumos-test-wait-until
                   (lambda () (eq 'ready (fumos-connection-state second)))))
          (with-current-buffer (fumos-connection-repl-buffer first)
            (setq next-id
                  (fennel-proto-repl-send-message :eval "pending-b" #'ignore))
            (should (= (1+ first-id) next-id))
            (should (eq first-callback
                        (gethash first-id fennel-proto-repl--message-callbacks)))
            (should (= 2 (hash-table-count
                          fennel-proto-repl--message-callbacks)))))
      (when second (fumos-repl-close second))
      (fumos-repl-close first)
      (fumos-test-server-stop second-server)
      (fumos-test-server-stop first-server)
      (delete-directory second-root t)
      (delete-directory first-root t))))

(ert-deftest fumos-callback-nonlocal-exits-cannot-block-same-chunk-done ()
  (fumos-test-with-ready-connection (connection server)
    (dolist (kind '(error quit throw))
      (let (invoked id)
        (with-current-buffer (fumos-connection-repl-buffer connection)
          (setq id
                (fennel-proto-repl-send-message
                 :eval (symbol-name kind)
                 (lambda (_values)
                   (setq invoked kind)
                   (pcase kind
                     ('error (error "callback error"))
                     ('quit (signal 'quit nil))
                     ('throw (throw 'fumos-test-nonlocal :escaped)))))))
        (fumos-test-server-send
         server
         (format (concat "(:id %d :op \"accept\")\n"
                         "(:id %d :op \"eval\" :values (\"ok\"))\n"
                         "(:id %d :op \"done\")\n")
                 id id id))
        (should (fumos-test-wait-until (lambda () invoked)))
        (should (fumos-test-wait-until
                 (lambda ()
                   (null (fumos-connection-callback-timers connection)))))
        (with-current-buffer (fumos-connection-repl-buffer connection)
          (should-not (gethash id fennel-proto-repl--message-callbacks)))
        (should (eq 'ready (fumos-connection-state connection)))))))

(ert-deftest fumos-disconnect-cleanup-survives-callback-nonlocal-exits ()
  (dolist (kind '(error quit throw))
    (fumos-test-with-ready-connection (connection server)
      (let (invoked)
        (with-current-buffer (fumos-connection-repl-buffer connection)
          (fennel-proto-repl-send-message
           :eval "pending" #'ignore
           (lambda (&rest _)
             (setq invoked kind)
             (pcase kind
               ('error (error "disconnect callback error"))
               ('quit (signal 'quit nil))
               ('throw (throw 'fumos-test-disconnect :escaped))))))
        (fumos-test-server-drop-client server)
        (should (fumos-test-wait-until (lambda () invoked)))
        (should (fumos-test-wait-until
                 (lambda ()
                   (null (fumos-connection-callback-timers connection)))))
        (should (eq 'disconnected (fumos-connection-state connection)))
        (should-not (process-live-p (fumos-connection-process connection)))
        (should-not (process-live-p (fumos-connection-ui-process connection)))))))

(ert-deftest fumos-retry-timer-stays-with-origin-after-buffer-switch ()
  (fumos-test-with-two-ready-connections (first first-server second second-server)
    (let* ((pair-a (fumos-test-install-retry-pair first "A"))
           (pair-b (fumos-test-install-retry-pair second "B"))
           (outer-a (car pair-a)) (id-a (cdr pair-a))
           (id-b (cdr pair-b)) scheduled)
      (should (= id-a id-b))
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat callback &rest args)
                   (push (cons callback args) scheduled)
                   (timer-create))))
        (fumos-test-server-send
         first-server (fumos-test-retry-frame outer-a id-a "A"))
        (should (fumos-test-wait-until (lambda () scheduled))))
      ;; Outer active request may finish before the delayed retry fires.
      (fumos-test-remove-callback first outer-a)
      (with-current-buffer (fumos-connection-repl-buffer second)
        (apply (caar scheduled) (cdar scheduled)))
      (should (fumos-test-wait-until
               (lambda ()
                 (fumos-test-server-saw-resend-p first-server id-a "A"))))
      (should-not (fumos-test-server-saw-resend-p second-server id-a "A")))))

(ert-deftest fumos-two-connections-with-same-id-retry-independently ()
  (fumos-test-with-two-ready-connections (first first-server second second-server)
    (let* ((pair-a (fumos-test-install-retry-pair first "A"))
           (pair-b (fumos-test-install-retry-pair second "B"))
           (outer-a (car pair-a)) (id-a (cdr pair-a))
           (outer-b (car pair-b)) (id-b (cdr pair-b)) scheduled)
      (should (= id-a id-b))
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat callback &rest args)
                   (push (cons callback args) scheduled)
                   (timer-create))))
        (fumos-test-server-send first-server
                                (fumos-test-retry-frame outer-a id-a "A"))
        (fumos-test-server-send second-server
                                (fumos-test-retry-frame outer-b id-b "B"))
        (should (fumos-test-wait-until
                 (lambda () (= 2 (length scheduled))))))
      (dolist (timer scheduled)
        (with-current-buffer (fumos-connection-repl-buffer first)
          (apply (car timer) (cdr timer))))
      (should (fumos-test-wait-until
               (lambda ()
                 (and (fumos-test-server-saw-resend-p first-server id-a "A")
                      (fumos-test-server-saw-resend-p
                       second-server id-b "B")))))
      (should-not (fumos-test-server-saw-resend-p first-server id-b "B"))
      (should (fumos-test-server-saw-resend-p second-server id-b "B"))
      (should-not (fumos-test-server-saw-resend-p second-server id-a "A")))))

(ert-deftest fumos-disconnect-cancels-and-invalidates-retry-timer ()
  (fumos-test-with-ready-connection (connection server)
    (let* ((pair (fumos-test-install-retry-pair connection "late"))
           (outer (car pair)) (id (cdr pair)) scheduled)
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat callback &rest args)
                   (setq scheduled (cons callback args))
                   (timer-create)))
                ((symbol-function 'cancel-timer) #'ignore))
        (fumos-test-server-send server
                                (fumos-test-retry-frame outer id "late"))
        (should (fumos-test-wait-until (lambda () scheduled))))
      (fumos-repl-close connection)
      (apply (car scheduled) (cdr scheduled))
      (should-not (fumos-test-server-saw-resend-p server id "late"))
      (should-not (fumos-connection-retry-timers connection)))))

(ert-deftest fumos-retry-reader-finds-only-canonical-top-level-id ()
  ;; These first two strings are the actual pinned Fennel 1.6.1 view order.
  (dolist (payload '("{:eval \"42\" :id 21}"
                     "{:column 1 :eval \"42\" :file \"mods/demo/a.fnl\" :id 21 :line 1}"
                     "{:eval \"escaped \\\" :id 999\" :meta {:id 88} :id 21}"
                     "{:eval \"42\" ; :id 999\n :id 21}"))
    (should (= 21 (fumos-repl--retry-message-id payload))))
  (dolist (payload '("{:eval \":id 21\"}"
                     "{:eval \"42\" :meta {:id 21}}"
                     "{:id 20 :id 21}"
                     "{:eval \"42\" :id \"21\"}"
                     "{:eval \"42\" :id 21} trailing"))
    (should-not (fumos-repl--retry-message-id payload))))

(ert-deftest fumos-dynamic-macro-query-uses-only-reserved-module ()
  (fumos-test-with-ready-connection (connection server)
    (let ((sent (caddr (fumos-test-server-lines server))) ordinary-called)
      (with-current-buffer (fumos-connection-repl-buffer connection)
        (cl-letf (((symbol-function 'fennel-proto-repl-send-message-sync)
                   (lambda (&rest _)
                     (ert-fail "FUMOS macro lookup used sync transport")))
                  ((symbol-function 'accept-process-output)
                   (lambda (&rest _)
                     (ert-fail "FUMOS macro lookup blocked"))))
          (should-not
           (fumos-repl--obtain-macros-advice
            (lambda () (setq ordinary-called t))))))
      (should (fumos-connection-macro-cache-valid connection))
      (should (= 3 (length (fumos-test-server-lines server))))
      (should (string-match-p
               (regexp-quote "(require \\\"fumos.repl.fennel\\\")") sent))
      (should-not (string-match-p (regexp-quote "(require :fennel)") sent))
      (should-not ordinary-called)
      (with-temp-buffer
        (fennel-proto-repl-mode)
        (fumos-repl--obtain-macros-advice
         (lambda () (setq ordinary-called t))))
      (should ordinary-called))))

(ert-deftest fumos-source-buffers-link-hooks-and-send-through-project-repl ()
  (fumos-test-with-source-before-and-after-attach (before after connection server)
    (dolist (source (list before after))
      (with-current-buffer source
        (should fennel-proto-repl-minor-mode)
        (should (eq connection fumos-repl--source-owner))
        (should (eq fumos-mode-map
                    (cdr (assq 'fumos-mode
                               minor-mode-overriding-map-alist))))
        (should (eq (fumos-connection-repl-buffer connection)
                    fennel-proto-repl--buffer))
        (should (memq #'fennel-proto-repl-complete
                      completion-at-point-functions))
        (should (memq #'fennel-proto-repl--xref-backend
                      xref-backend-functions))
        (dolist (binding '(("C-x C-e" . fumos-eval-last-sexp)
                           ("C-M-x" . fumos-eval-defun)
                           ("C-c C-r" . fumos-eval-region)
                           ("C-c C-b" . fumos-eval-buffer)
                           ("C-c C-k" . fumos-reload-current-file)
                           ("C-c C-z" . fumos-switch-to-repl)))
          (should (eq (cdr binding)
                      (lookup-key fumos-mode-map (kbd (car binding)))))
          (should (eq (cdr binding)
                      (key-binding (kbd (car binding)) t))))))
    (with-current-buffer after
      (fennel-proto-repl-send-message :eval "source-buffer-route"
                                      #'ignore #'ignore #'ignore))
    (should (fumos-test-wait-until
             (lambda ()
               (fumos-test-server-saw-eval-p
                server "source-buffer-route"))))))

(ert-deftest fumos-source-module-alias-controls-real-query-wire-and-restores-global ()
  (let ((fennel-proto-repl-fennel-module-name "poison.module"))
    (fumos-test-with-source-before-and-after-attach
        (before after connection server)
      (with-current-buffer after
        (should (local-variable-p
                 'fennel-proto-repl-fennel-module-name))
        (should (equal fumos-repl-fennel-module-name
                       fennel-proto-repl-fennel-module-name))
        (fennel-proto-repl-show-arglist "game-function")
        (fennel-proto-repl-send-message
         :eval
         (fennel-proto-repl--generate-query-command
          "game-value"
          (fennel-proto-repl--doc-query-template)
          (fennel-proto-repl--multisym-doc-query-template))
         #'ignore))
      (should
       (fumos-test-wait-until
        (lambda () (>= (length (fumos-test-server-lines server)) 5))))
      (let ((query-lines (last (fumos-test-server-lines server) 2)))
        (dolist (line query-lines)
          (should (string-match-p
                   (regexp-quote
                    "(require \\\"fumos.repl.fennel\\\")")
                   line))
          (should-not (string-match-p "poison\\.module" line))))
      (with-current-buffer after
        (should (eq connection (fumos-repl-unlink-current-buffer)))
        (should-not (local-variable-p
                     'fennel-proto-repl-fennel-module-name))
        (should (equal "poison.module"
                       fennel-proto-repl-fennel-module-name))))))

(ert-deftest fumos-killing-source-buffer-immediately-releases-link-ownership ()
  (fumos-test-with-source-before-and-after-attach
      (before after connection server)
    (let (observed-after-cleanup)
      (with-current-buffer after
        ;; Simulate the Task 11 backend already being installed and retain an
        ;; unrelated local backend to prove cleanup is ownership-scoped.
        (add-hook 'xref-backend-functions #'fumos-repl--xref-backend nil t)
        (add-hook 'xref-backend-functions #'ignore t t)
        (add-hook
         'kill-buffer-hook
         (lambda ()
           (setq observed-after-cleanup
                 (list
                  (memq after
                        (fumos-connection-linked-buffers connection))
                  (memq #'fumos-repl--xref-backend xref-backend-functions)
                  (memq #'ignore xref-backend-functions)
                  fumos-repl--source-owner
                  fennel-proto-repl--buffer
                  (local-variable-p
                   'fennel-proto-repl-fennel-module-name)
                  fennel-proto-repl-fennel-module-name)))
         t t)
        (kill-buffer after))
      (should observed-after-cleanup)
      (should-not (nth 0 observed-after-cleanup))
      (should-not (nth 1 observed-after-cleanup))
      (should (nth 2 observed-after-cleanup))
      (should-not (nth 3 observed-after-cleanup))
      (should-not (nth 4 observed-after-cleanup))
      (should-not (nth 5 observed-after-cleanup))
      (should
       (equal (default-value 'fennel-proto-repl-fennel-module-name)
              (nth 6 observed-after-cleanup)))
      (should-not
       (memq after (fumos-connection-linked-buffers connection))))))

(ert-deftest fumos-source-owner-moves-immediately-from-a-to-b ()
  (fumos-test-with-two-ready-connections
      (first first-server second second-server)
    (let ((source (generate-new-buffer " *fumos-owner-relink*")))
      (unwind-protect
          (progn
            (with-current-buffer source (fennel-mode))
            (fumos-repl--link-buffer-to-connection first source)
            (should (memq source
                          (fumos-connection-linked-buffers first)))
            (fumos-repl--link-buffer-to-connection second source)
            (should-not
             (memq source (fumos-connection-linked-buffers first)))
            (should (= 1
                       (cl-count source
                                 (fumos-connection-linked-buffers second)
                                 :test #'eq)))
            (with-current-buffer source
              (should (eq second fumos-repl--source-owner))
              (should
               (eq (fumos-connection-repl-buffer second)
                   fennel-proto-repl--buffer))))
        (when (buffer-live-p source) (kill-buffer source))))))

(ert-deftest fumos-source-owner-clears-and-yields-to-ordinary-repl ()
  (fumos-test-with-ready-connection (connection server)
    (let ((clear-source (generate-new-buffer " *fumos-owner-clear*"))
          (ordinary-source (generate-new-buffer " *fumos-owner-ordinary*"))
          (ordinary-repl (generate-new-buffer " *ordinary-proto-repl*")))
      (unwind-protect
          (progn
            (with-current-buffer ordinary-repl
              (fennel-proto-repl-mode))
            (dolist (source (list clear-source ordinary-source))
              (with-current-buffer source
                (fennel-mode)
                (add-hook 'xref-backend-functions #'ignore t t))
              (fumos-repl--link-buffer-to-connection connection source)
              ;; Task 11 defines the backend function; install its owned hook
              ;; explicitly here so this Task 8 lifecycle test stays standalone.
              (with-current-buffer source
                (add-hook 'xref-backend-functions
                          #'fumos-repl--xref-backend nil t)))
            (with-current-buffer clear-source
              (should (eq connection (fumos-repl-unlink-current-buffer)))
              (should-not fumos-repl--source-owner)
              (should-not (local-variable-p
                           'fennel-proto-repl-fennel-module-name))
              (should-not fennel-proto-repl--buffer)
              (should-not fennel-proto-repl-minor-mode)
              (should-not
               (memq #'fumos-repl--xref-backend xref-backend-functions))
              (should (memq #'ignore xref-backend-functions)))
            (with-current-buffer ordinary-source
              ;; Exercise the real pinned link helper.  Its narrow FUMOS
              ;; advice must release the old owner without disabling the
              ;; already-active ordinary proto interaction mode.
              (fennel-proto-repl--link-buffer ordinary-repl)
              (should-not fumos-repl--source-owner)
              (should-not (local-variable-p
                           'fennel-proto-repl-fennel-module-name))
              (should (eq ordinary-repl fennel-proto-repl--buffer))
              (should fennel-proto-repl-minor-mode)
              (should
               (memq #'fennel-proto-repl--xref-backend
                     xref-backend-functions))
              (should-not
               (memq #'fumos-repl--xref-backend xref-backend-functions))
              (should (memq #'ignore xref-backend-functions)))
            (should-not
             (memq clear-source
                   (fumos-connection-linked-buffers connection)))
            (should-not
             (memq ordinary-source
                   (fumos-connection-linked-buffers connection))))
        (dolist (buffer (list clear-source ordinary-source ordinary-repl))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest fumos-ordinary-target-restores-after-explicit-clear ()
  (fumos-test-with-ready-connection (connection server)
    (let ((source (generate-new-buffer " *fumos-ordinary-clear-source*"))
          (ordinary (generate-new-buffer " *fumos-ordinary-clear-repl*")))
      (unwind-protect
          (progn
            (with-current-buffer ordinary (fennel-proto-repl-mode))
            (with-current-buffer source
              (fennel-mode)
              (setq-local fennel-proto-repl-fennel-module-name
                          "user.module")
              (setq-local fennel-proto-repl--buffer ordinary)
              (fennel-proto-repl-minor-mode 1)
              (fennel-proto-repl--link-buffer ordinary)
              (add-hook 'xref-backend-functions #'ignore t t))
            (fumos-repl--link-buffer-to-connection connection source)
            (with-current-buffer source
              (should (eq ordinary
                          fumos-repl--source-previous-upstream-buffer))
              (should fumos-repl--source-previous-upstream-mode)
              (should fumos-repl--source-previous-module-local-p)
              (should (equal "user.module"
                             fumos-repl--source-previous-module-value))
              (should (equal fumos-repl-fennel-module-name
                             fennel-proto-repl-fennel-module-name))
              (should-not fumos-repl--source-enabled-upstream-mode)
              (should (eq connection (fumos-repl-unlink-current-buffer)))
              (should-not fumos-repl--source-owner)
              (should-not fumos-repl--source-previous-upstream-buffer)
              (should (local-variable-p
                       'fennel-proto-repl-fennel-module-name))
              (should (equal "user.module"
                             fennel-proto-repl-fennel-module-name))
              (should (eq ordinary fennel-proto-repl--buffer))
              (should fennel-proto-repl-minor-mode)
              (should (memq #'fennel-proto-repl--xref-backend
                            xref-backend-functions))
              (should (memq #'ignore xref-backend-functions)))
            (should-not
             (memq source
                   (fumos-connection-linked-buffers connection))))
        (dolist (buffer (list source ordinary))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest fumos-ordinary-target-restores-after-transport-disconnect ()
  (fumos-test-with-ready-connection (connection server)
    (let ((source (generate-new-buffer " *fumos-ordinary-drop-source*"))
          (ordinary (generate-new-buffer " *fumos-ordinary-drop-repl*")))
      (unwind-protect
          (progn
            (with-current-buffer ordinary (fennel-proto-repl-mode))
            (with-current-buffer source
              (fennel-mode)
              (should-not (local-variable-p
                           'fennel-proto-repl-fennel-module-name))
              (setq-local fennel-proto-repl--buffer ordinary)
              (fennel-proto-repl-minor-mode 1)
              (fennel-proto-repl--link-buffer ordinary))
            (fumos-repl--link-buffer-to-connection connection source)
            (with-current-buffer source
              (should (local-variable-p
                       'fennel-proto-repl-fennel-module-name))
              (should (equal fumos-repl-fennel-module-name
                             fennel-proto-repl-fennel-module-name)))
            (fumos-repl--mark-disconnected connection "test disconnect")
            (with-current-buffer source
              (should-not fumos-repl--source-owner)
              (should-not (local-variable-p
                           'fennel-proto-repl-fennel-module-name))
              (should
               (equal (default-value
                       'fennel-proto-repl-fennel-module-name)
                      fennel-proto-repl-fennel-module-name))
              (should (eq ordinary fennel-proto-repl--buffer))
              (should fennel-proto-repl-minor-mode)
              (should (memq #'fennel-proto-repl--xref-backend
                            xref-backend-functions)))
            (should-not (fumos-connection-linked-buffers connection)))
        (dolist (buffer (list source ordinary))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest fumos-ordinary-target-survives-a-to-b-owner-transfer ()
  (fumos-test-with-two-ready-connections
      (first first-server second second-server)
    (let ((source (generate-new-buffer " *fumos-ordinary-a-b-source*"))
          (ordinary (generate-new-buffer " *fumos-ordinary-a-b-repl*")))
      (unwind-protect
          (progn
            (with-current-buffer ordinary (fennel-proto-repl-mode))
            (with-current-buffer source
              (fennel-mode)
              (setq-local fennel-proto-repl-fennel-module-name
                          "ordinary.module")
              (setq-local fennel-proto-repl--buffer ordinary)
              (fennel-proto-repl-minor-mode 1)
              (fennel-proto-repl--link-buffer ordinary))
            (fumos-repl--link-buffer-to-connection first source)
            (fumos-repl--link-buffer-to-connection second source)
            (should-not
             (memq source (fumos-connection-linked-buffers first)))
            (with-current-buffer source
              (should (eq second fumos-repl--source-owner))
              ;; A must never replace the original ordinary snapshot.
              (should (eq ordinary
                          fumos-repl--source-previous-upstream-buffer))
              (should fumos-repl--source-previous-upstream-mode)
              (should fumos-repl--source-previous-module-local-p)
              (should (equal "ordinary.module"
                             fumos-repl--source-previous-module-value))
              (should (equal fumos-repl-fennel-module-name
                             fennel-proto-repl-fennel-module-name))
              (fumos-repl-unlink-current-buffer)
              (should-not fumos-repl--source-owner)
              (should (local-variable-p
                       'fennel-proto-repl-fennel-module-name))
              (should (equal "ordinary.module"
                             fennel-proto-repl-fennel-module-name))
              (should (eq ordinary fennel-proto-repl--buffer))
              (should fennel-proto-repl-minor-mode))
            (should-not
             (memq source (fumos-connection-linked-buffers second))))
        (dolist (buffer (list source ordinary))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest fumos-source-buffer-can-reconnect-disconnected-history ()
  (fumos-test-with-linked-source-reconnect (source old replacement server)
    (fumos-test-server-drop-client server)
    (should (fumos-test-wait-until
             (lambda () (eq 'disconnected
                            (fumos-connection-state old)))))
    (with-current-buffer source
      (should (eq old (fumos-repl-current-connection)))
      (fumos-reconnect))
    (should (fumos-test-wait-until
             (lambda () (eq 'ready
                            (fumos-connection-state replacement)))))
    (with-current-buffer source
      (should (eq replacement (fumos-repl-current-connection)))
      (should (eq (fumos-connection-repl-buffer replacement)
                  fennel-proto-repl--buffer)))))

(ert-deftest fumos-disabling-mode-removes-only-its-owned-source-link ()
  (fumos-test-with-source-before-and-after-attach
      (before after connection server)
    (with-current-buffer after
      (should (memq after (fumos-connection-linked-buffers connection)))
      (fumos-mode -1)
      (should-not (assq 'fumos-mode minor-mode-overriding-map-alist))
      (should-not fennel-proto-repl-minor-mode)
      (should-not fennel-proto-repl--buffer)
      (should-not fumos-repl--source-owner)
      (should-not (local-variable-p
                   'fennel-proto-repl-fennel-module-name))
      (should-not (memq after (fumos-connection-linked-buffers connection))))
    (with-current-buffer before
      (should fumos-mode)
      (should fennel-proto-repl-minor-mode))))

(ert-deftest fumos-disabling-proto-mode-restores-ordinary-target-identity ()
  (fumos-test-with-ready-connection (connection server)
    (let ((source (generate-new-buffer " *fumos-proto-disable-source*"))
          (ordinary (generate-new-buffer " *fumos-proto-disable-repl*")))
      (unwind-protect
          (progn
            (with-current-buffer ordinary (fennel-proto-repl-mode))
            (with-current-buffer source
              (fennel-mode)
              (setq-local fennel-proto-repl-fennel-module-name
                          "proto-user.module")
              (setq-local fennel-proto-repl--buffer ordinary)
              (add-hook 'xref-backend-functions #'ignore nil t)
              (fennel-proto-repl-minor-mode 1)
              (fennel-proto-repl--link-buffer ordinary))
            ;; Task 11 才定义真实 backend；本任务只用同一 symbol identity
            ;; 安装局部 stub，从而独立冻结 Task 8 的 hook 清理契约。
            (cl-letf (((symbol-function 'fumos-repl--xref-backend)
                       (lambda () 'fumos-repl)))
              (fumos-repl--link-buffer-to-connection connection source)
              (with-current-buffer source
                (should (eq connection fumos-repl--source-owner))
                (should (eq ordinary
                            fumos-repl--source-previous-upstream-buffer))
                (should fumos-repl--source-previous-upstream-mode)
                (should (memq #'fumos-repl--xref-backend
                              xref-backend-functions))
                ;; This public minor-mode operation must run the real local hook.
                (fennel-proto-repl-minor-mode -1)
                (should-not fennel-proto-repl-minor-mode)
                (should (eq ordinary fennel-proto-repl--buffer))
                (should-not fumos-repl--source-owner)
                (should-not fumos-repl--source-enabled-upstream-mode)
                (should-not fumos-repl--source-previous-upstream-buffer)
                (should-not fumos-repl--source-previous-upstream-mode)
                (should (local-variable-p
                         'fennel-proto-repl-fennel-module-name))
                (should (equal "proto-user.module"
                               fennel-proto-repl-fennel-module-name))
                (should-not (memq #'fumos-repl--xref-backend
                                  xref-backend-functions))
                (should (memq #'ignore xref-backend-functions)))
              (should-not
               (memq source
                     (fumos-connection-linked-buffers connection)))))
        (dolist (buffer (list source ordinary))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest fumos-ack-and-init-in-one-write-bootstrap-the-real-client ()
  (let* ((fumos-repl--connections (make-hash-table :test #'equal))
         (root (fumos-test-make-project-root
                (make-temp-file "fumos-coalesced-root-" t)))
         (server
          (fumos-test-server-start
           (lambda (state client line)
             (pcase (length (fumos-test-server-lines state))
               (1
                (should (string-prefix-p "FUMOS/1 AUTH " line))
               (fumos-test-server-send
                 state (concat fumos-test-golden-ack fumos-test-init-frame)
                 client))
               (2 (should (string-match-p "___repl___" line)))
               (_
                (when (string-match-p "macro-loaded" line)
                  (fumos-test-send-macro-result state client line "nil")))))))
         (connection
          (fumos-repl-connect-instance
           (fumos-test-instance-for-server server 4242 root))))
    (unwind-protect
        (progn
          (should (fumos-test-wait-until
                   (lambda () (eq 'ready
                                  (fumos-connection-state connection)))))
          (should (fumos-test-wait-until
                   (lambda ()
                     (fumos-connection-macro-cache-valid connection))))
          (should (= 3 (length (fumos-test-server-lines server)))))
      (fumos-repl-close connection)
      (fumos-test-server-stop server)
      (delete-directory root t))))

(ert-deftest fumos-old-bootstrap-timer-is-inert-after-replacement ()
  (let* ((fumos-repl--connections (make-hash-table :test #'equal))
         (root (file-name-as-directory
                (file-truename
                 (fumos-test-make-project-root
                  (make-temp-file "fumos-old-bootstrap-root-" t)))))
         (old-server
          (fumos-test-server-start
           (lambda (state client _line)
             (when (= 1 (length (fumos-test-server-lines state)))
               (fumos-test-server-send state fumos-test-golden-ack client)))))
         (new-server
          (fumos-test-server-start (fumos-test-make-proto-handler 4242)))
         (old (fumos-repl-connect-instance
               (fumos-test-instance-for-server old-server 4242 root)))
         replacement timer-call)
    (unwind-protect
        (progn
          (should (fumos-test-wait-until
                   (lambda () (eq 'bootstrapping
                                  (fumos-connection-state old)))))
          (let ((timer (fumos-connection-bootstrap-timer old)))
            (should (timerp timer))
            (setq timer-call
                  (cons (timer--function timer) (timer--args timer))))
          (setq replacement
                (fumos-repl-connect-instance
                 (fumos-test-instance-for-server
                  new-server 4242 root "demo" (make-string 64 ?b))))
          (should (fumos-test-wait-until
                   (lambda () (eq 'ready
                                  (fumos-connection-state replacement)))))
          (apply (car timer-call) (cdr timer-call))
          (should (eq replacement (gethash root fumos-repl--connections)))
          (should (eq 'ready (fumos-connection-state replacement)))
          (should (eq 'disconnected (fumos-connection-state old))))
      (when replacement (fumos-repl-close replacement))
      (fumos-repl-close old)
      (fumos-test-server-stop new-server)
      (fumos-test-server-stop old-server)
      (delete-directory root t))))

(ert-deftest fumos-late-init-after-bootstrap-timeout-is-inert ()
  (let* ((fumos-repl--connections (make-hash-table :test #'equal))
         (root (file-name-as-directory
                (file-truename
                 (fumos-test-make-project-root
                  (make-temp-file "fumos-late-init-root-" t)))))
         (server
          (fumos-test-server-start
           (lambda (state client _line)
             (when (= 1 (length (fumos-test-server-lines state)))
               (fumos-test-server-send state fumos-test-golden-ack client)))))
         (fumos-bootstrap-timeout 0.05)
         (connection
          (fumos-repl-connect-instance
           (fumos-test-instance-for-server server 4242 root)))
         process filter)
    (unwind-protect
        (progn
          (should (fumos-test-wait-until
                   (lambda () (eq 'bootstrapping
                                  (fumos-connection-state connection)))))
          (setq process (fumos-connection-process connection)
                filter (process-filter process))
          (should (fumos-test-wait-until
                   (lambda () (eq 'disconnected
                                  (fumos-connection-state connection)))
                   0.5))
          (funcall filter process fumos-test-init-frame)
          (should (eq 'disconnected (fumos-connection-state connection)))
          (should-not (gethash root fumos-repl--connections))
          (should-not (fumos-connection-ui-process connection)))
      (fumos-repl-close connection)
      (fumos-test-server-stop server)
      (delete-directory root t))))

(ert-deftest fumos-bootstrap-revalidates-after-ui-start ()
  (let* ((fumos-repl--connections (make-hash-table :test #'equal))
         (root "/work/bootstrap-after-ui/")
         (instance (make-fumos-instance :project-root root :pid 4242))
         (connection (make-fumos-connection
                      :instance instance :state 'bootstrapping))
         (replacement (make-fumos-connection :instance instance))
         linked closed)
    (puthash root connection fumos-repl--connections)
    (cl-letf (((symbol-function 'fumos-repl--bootstrap-commit-owned-p)
               (lambda (value)
                 (and (eq value (gethash root fumos-repl--connections))
                      (not (fumos-connection-closing value)))))
              ((symbol-function 'fumos-repl--start-upstream-ui)
               (lambda (&rest _)
                 (puthash root replacement fumos-repl--connections)))
              ((symbol-function 'fumos-repl--link-project-buffers)
               (lambda (&rest _) (setq linked t)))
              ((symbol-function 'fumos-repl-close)
               (lambda (value &optional _reason)
                 (setq closed value)
                 (setf (fumos-connection-closing value) t
                       (fumos-connection-state value) 'disconnected))))
      (fumos-repl--finish-bootstrap
       connection '(ok "0.6.4" "1.6.1" "LuaJIT 2.1")))
    (should (eq connection closed))
    (should-not linked)
    (should (eq replacement (gethash root fumos-repl--connections)))))

(ert-deftest fumos-project-link-revalidates-after-each-source ()
  (let* ((fumos-repl--connections (make-hash-table :test #'equal))
         (root (fumos-test-make-project-root
                (make-temp-file "fumos-link-revalidate-root-" t)))
         (instance (make-fumos-instance
                    :project-root (file-name-as-directory (file-truename root))))
         (connection (make-fumos-connection :instance instance))
         (replacement (make-fumos-connection :instance instance))
         (first (fumos-test-make-source-buffer root "scripts/first.fnl"))
         (second (fumos-test-make-source-buffer root "scripts/second.fnl"))
         linked)
    (unwind-protect
        (progn
          (puthash (fumos-instance-project-root instance)
                   connection fumos-repl--connections)
          (cl-letf (((symbol-function 'fumos-repl--bootstrap-commit-owned-p)
                     (lambda (value)
                       (eq value
                           (gethash (fumos-instance-project-root instance)
                                    fumos-repl--connections))))
                    ((symbol-function 'fumos-repl--link-buffer-to-connection)
                     (lambda (_value buffer)
                       (push buffer linked)
                       (puthash (fumos-instance-project-root instance)
                                replacement fumos-repl--connections))))
            (should-error
             (fumos-repl--link-project-buffers connection)))
          (should (= 1 (length linked))))
      (dolist (buffer (list first second))
        (when (buffer-live-p buffer) (kill-buffer buffer)))
      (delete-directory root t))))

(ert-deftest fumos-reentrant-kill-hook-attach-wins-reservation-cas ()
  (let* ((fumos-repl--connections (make-hash-table :test #'equal))
         (root (file-name-as-directory
                (file-truename
                 (fumos-test-make-project-root
                  (make-temp-file "fumos-reentrant-attach-root-" t)))))
         (old-server
          (fumos-test-server-start (fumos-test-make-proto-handler 4242)))
         (outer-server
          (fumos-test-server-start (fumos-test-make-proto-handler 4242)))
         (inner-server
          (fumos-test-server-start (fumos-test-make-proto-handler 4242)))
         (old (fumos-repl-connect-instance
               (fumos-test-instance-for-server old-server 4242 root)))
         inner outer-result)
    (unwind-protect
        (progn
          (should (fumos-test-wait-until
                   (lambda () (eq 'ready (fumos-connection-state old)))))
          (with-current-buffer (fumos-connection-repl-buffer old)
            (add-hook
             'kill-buffer-hook
             (lambda ()
               (unless inner
                 (setq inner
                       (fumos-repl-connect-instance
                        (fumos-test-instance-for-server
                         inner-server 4242 root "demo" (make-string 64 ?c))))))
             nil t))
          (setq outer-result
                (condition-case caught
                    (fumos-repl-connect-instance
                     (fumos-test-instance-for-server
                      outer-server 4242 root "demo" (make-string 64 ?b)))
                  (fumos-repl-connection-error caught)))
          (should inner)
          (should (fumos-test-wait-until
                   (lambda () (eq 'ready (fumos-connection-state inner)))))
          (should (eq inner (gethash root fumos-repl--connections)))
          (should-not (fumos-connection-p outer-result)))
      (when (fumos-connection-p outer-result)
        (fumos-repl-close outer-result))
      (when inner (fumos-repl-close inner))
      (fumos-repl-close old)
      (fumos-test-server-stop inner-server)
      (fumos-test-server-stop outer-server)
      (fumos-test-server-stop old-server)
      (delete-directory root t))))

(ert-deftest fumos-source-link-mode-enable-failure-rolls-back ()
  (dolist (kind '(error quit))
    (let* ((source (generate-new-buffer " *fumos-link-mode-failure*"))
           (ordinary (generate-new-buffer " *fumos-link-mode-ordinary*"))
           (repl (generate-new-buffer " *fumos-link-mode-repl*"))
           (connection (make-fumos-connection :repl-buffer repl))
           caught)
      (unwind-protect
          (with-current-buffer source
            (fennel-mode)
            (setq-local fennel-proto-repl-fennel-module-name
                        "mode-failure.module")
            (setq-local fennel-proto-repl--buffer ordinary)
            (cl-letf (((symbol-function 'fennel-proto-repl-minor-mode)
                       (lambda (&optional _arg)
                         (setq fennel-proto-repl-minor-mode t)
                         (signal kind nil))))
              (setq caught
                    (condition-case condition
                        (fumos-repl--link-buffer-to-connection
                         connection source)
                      ((error quit) condition))))
            (should caught)
            (should-not fumos-repl--source-owner)
            (should (local-variable-p
                     'fennel-proto-repl-fennel-module-name))
            (should (equal "mode-failure.module"
                           fennel-proto-repl-fennel-module-name))
            (should-not fennel-proto-repl-minor-mode)
            (should (eq ordinary fennel-proto-repl--buffer))
            (should-not (memq source
                              (fumos-connection-linked-buffers connection))))
        (dolist (buffer (list source ordinary repl))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest fumos-source-a-to-b-link-failure-restores-a ()
  (dolist (kind '(error quit))
    (let* ((source (generate-new-buffer " *fumos-link-a-b-source*"))
           (a-repl (generate-new-buffer " *fumos-link-a-repl*"))
           (b-repl (generate-new-buffer " *fumos-link-b-repl*"))
           (a (make-fumos-connection :repl-buffer a-repl))
           (b (make-fumos-connection :repl-buffer b-repl))
           caught)
      (unwind-protect
          (with-current-buffer source
            (fennel-mode)
            (setq-local fennel-proto-repl-fennel-module-name
                        "a-b-original.module")
            (fumos-repl--link-buffer-to-connection a source)
            (should (equal fumos-repl-fennel-module-name
                           fennel-proto-repl-fennel-module-name))
            (cl-letf (((symbol-function 'fennel-proto-repl--link-buffer)
                       (lambda (&optional target)
                         (setq fennel-proto-repl--buffer target)
                         (signal kind nil))))
              (setq caught
                    (condition-case condition
                        (fumos-repl--link-buffer-to-connection b source)
                      ((error quit) condition))))
            (should caught)
            (should (eq a fumos-repl--source-owner))
            (should (equal fumos-repl-fennel-module-name
                           fennel-proto-repl-fennel-module-name))
            (should fumos-repl--source-previous-module-local-p)
            (should (equal "a-b-original.module"
                           fumos-repl--source-previous-module-value))
            (should (eq a-repl fennel-proto-repl--buffer))
            (should (memq source (fumos-connection-linked-buffers a)))
            (should-not (memq source (fumos-connection-linked-buffers b))))
      (when (buffer-live-p source)
          (with-current-buffer source
            (fumos-repl-unlink-current-buffer)
            (should (local-variable-p
                     'fennel-proto-repl-fennel-module-name))
            (should (equal "a-b-original.module"
                           fennel-proto-repl-fennel-module-name))))
        (dolist (buffer (list source a-repl b-repl))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest fumos-source-reentrant-link-keeps-newest-owner ()
  (let* ((source (generate-new-buffer " *fumos-link-reentrant-source*"))
         (a-repl (generate-new-buffer " *fumos-link-reentrant-a*"))
         (b-repl (generate-new-buffer " *fumos-link-reentrant-b*"))
         (c-repl (generate-new-buffer " *fumos-link-reentrant-c*"))
         (a (make-fumos-connection :repl-buffer a-repl))
         (b (make-fumos-connection :repl-buffer b-repl))
         (c (make-fumos-connection :repl-buffer c-repl))
         reentered)
    (unwind-protect
        (with-current-buffer source
          (fennel-mode)
          (setq-local fennel-proto-repl-fennel-module-name
                      "reentrant-original.module")
          (fumos-repl--link-buffer-to-connection a source)
          (cl-letf (((symbol-function 'fennel-proto-repl--link-buffer)
                     (lambda (&optional target)
                       (setq fennel-proto-repl--buffer target)
                       (unless reentered
                         (setq reentered t)
                         (fumos-repl--link-buffer-to-connection c source)))))
            (fumos-repl--link-buffer-to-connection b source))
          (should (eq c fumos-repl--source-owner))
          (should (equal fumos-repl-fennel-module-name
                         fennel-proto-repl-fennel-module-name))
          (should fumos-repl--source-previous-module-local-p)
          (should (equal "reentrant-original.module"
                         fumos-repl--source-previous-module-value))
          (should (eq c-repl fennel-proto-repl--buffer))
          (should-not (memq source (fumos-connection-linked-buffers a)))
          (should-not (memq source (fumos-connection-linked-buffers b)))
          (should (memq source (fumos-connection-linked-buffers c))))
      (when (buffer-live-p source)
        (with-current-buffer source
          (fumos-repl-unlink-current-buffer)
          (should (local-variable-p
                   'fennel-proto-repl-fennel-module-name))
          (should (equal "reentrant-original.module"
                         fennel-proto-repl-fennel-module-name))))
      (dolist (buffer (list source a-repl b-repl c-repl))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest fumos-second-teardown-cleans-link-added-after-closing ()
  (let* ((source (generate-new-buffer " *fumos-late-link-source*"))
         (repl (generate-new-buffer " *fumos-late-link-repl*"))
         (connection
          (make-fumos-connection
           :state 'ready :repl-buffer repl
           :callback-deliveries (make-hash-table :test #'eql))))
    (unwind-protect
        (progn
          (fumos-repl--teardown-transport connection "first teardown")
          (with-current-buffer source
            (fennel-mode)
            (fumos-repl--link-buffer-to-connection connection source)
            (should (eq connection fumos-repl--source-owner)))
          (fumos-repl--teardown-transport connection "second teardown")
          (with-current-buffer source
            (should-not fumos-repl--source-owner)
            (should-not fennel-proto-repl--buffer)
            (should-not fennel-proto-repl-minor-mode))
          (should-not (fumos-connection-linked-buffers connection)))
      (dolist (buffer (list source repl))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest fumos-retry-scheduler-failures-are-contained ()
  (dolist (kind '(error quit non-timer))
    (let ((connection
           (make-fumos-connection
            :state 'ready :generation 17 :retry-timers nil
            :callback-deliveries (make-hash-table :test #'eql)))
          escaped)
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (&rest _)
                   (pcase kind
                     ('error (error "injected retry scheduler error"))
                     ('quit (signal 'quit nil))
                     (_ 'not-a-timer)))))
        (condition-case condition
            (fumos-repl--schedule-retry connection 21 "{:id 21}" 'callbacks)
          ((error quit) (setq escaped condition))))
      (should-not escaped)
      (should-not (fumos-connection-retry-timers connection))
      (should (eq 'disconnected (fumos-connection-state connection))))))

(ert-deftest fumos-connection-nested-printing-redacts-token ()
  (let* ((token (make-string 64 ?s))
         (instance (make-fumos-instance :mod-id "demo" :pid 4242 :token token))
         (connection (make-fumos-connection :instance instance))
         (printed (format "%S" (list :connection connection))))
    (should (string-match-p "<redacted>" printed))
    (should-not (string-match-p (regexp-quote token) printed))))

(defun fumos-test-wire-message-id (line)
  "Return LINE's canonical top-level request ID."
  (and (string-match ":id \\([0-9]+\\)" line)
       (string-to-number (match-string 1 line))))

(defun fumos-test-send-macro-result (server client line modules)
  "Reply to macro query LINE with serialized MODULES."
  (let ((id (fumos-test-wire-message-id line)))
    (should (integerp id))
    (fumos-test-server-send
     server
     (format
      (concat "(:id %d :op \"accept\")\n"
              "(:id %d :op \"eval\" :values (%S))\n"
              "(:id %d :op \"done\")\n")
      id id modules id)
     client)))

(ert-deftest fumos-macro-cache-refresh-is-real-tcp-and-nonblocking ()
  (let* ((fumos-repl--connections (make-hash-table :test #'equal))
         (root (fumos-test-make-project-root
                (make-temp-file "fumos-macro-cache-root-" t)))
         macro-line
         (server
          (fumos-test-server-start
           (lambda (state client line)
             (pcase (length (fumos-test-server-lines state))
               (1 (fumos-test-server-send state fumos-test-golden-ack client))
               (2 (fumos-test-server-send state fumos-test-init-frame client))
               (_
                (when (string-match-p "macro-loaded" line)
                  (setq macro-line line)
                  (fumos-test-send-macro-result
                   state client line "[[\"demo.macros\" \"when-game\"]]")))))))
         connection)
    (unwind-protect
        (cl-letf (((symbol-function 'fennel-proto-repl-send-message-sync)
                   (lambda (&rest _)
                     (ert-fail "FUMOS macro refresh used sync transport"))))
          (setq connection
                (fumos-repl-connect-instance
                 (fumos-test-instance-for-server server 4242 root)))
          (should (fumos-test-wait-until
                   (lambda ()
                     (and macro-line
                          (fumos-connection-macro-cache-valid connection)))))
          (should (equal '(("demo.macros" "when-game"))
                         (fumos-connection-macro-cache connection)))
          (with-current-buffer (fumos-connection-repl-buffer connection)
            (cl-letf (((symbol-function 'accept-process-output)
                       (lambda (&rest _)
                         (ert-fail "Macro cache lookup blocked for network I/O"))))
              (should
               (equal '(("demo.macros" "when-game"))
                      (fumos-repl--obtain-macros-advice
                       (lambda () (ert-fail "ordinary macro query called"))))))))
      (when connection (fumos-repl-close connection))
      (fumos-test-server-stop server)
      (delete-directory root t))))

(ert-deftest fumos-macro-cache-invalidation-rejects-same-generation-late-values ()
  (let* ((fumos-repl--connections (make-hash-table :test #'equal))
         (root (fumos-test-make-project-root
                (make-temp-file "fumos-macro-invalidate-root-" t)))
         (macro-count 0)
         (server
          (fumos-test-server-start
           (lambda (state client line)
             (pcase (length (fumos-test-server-lines state))
               (1 (fumos-test-server-send state fumos-test-golden-ack client))
               (2 (fumos-test-server-send state fumos-test-init-frame client))
               (_
                (when (string-match-p "macro-loaded" line)
                  (cl-incf macro-count)
                  (fumos-test-send-macro-result
                   state client line
                   (pcase macro-count
                     (1 "[[\"game.macros\" \"baseline\"]]")
                     (2 "[[\"game.macros\" \"stale\"]]")
                     (_ "[[\"game.macros\" \"fresh\"]]")))))))))
         (original-run-at-time (symbol-function 'run-at-time))
         connection delayed-call delayed-timer old-id new-id)
    (unwind-protect
        (progn
          (setq connection
                (fumos-repl-connect-instance
                 (fumos-test-instance-for-server server 4242 root)))
          (should
           (fumos-test-wait-until
            (lambda ()
              (and (fumos-connection-macro-cache-valid connection)
                   (equal '(("game.macros" "baseline"))
                          (fumos-connection-macro-cache connection))))))
          (should
           (eq (fumos-connection-repl-buffer connection)
               (buffer-local-value
                'fennel-proto-repl--buffer
                (fumos-connection-process-buffer connection))))
          (cl-letf
              (((symbol-function 'run-at-time)
                (lambda (delay repeat callback &rest arguments)
                  (if (and (zerop delay) (not delayed-call))
                      (progn
                        (setq delayed-timer
                              (funcall original-run-at-time 3600 nil #'ignore)
                              delayed-call (cons callback arguments))
                        delayed-timer)
                    (apply original-run-at-time
                           delay repeat callback arguments)))))
            ;; First invalidation models an earlier refresh whose values arrive
            ;; before its deferred callback is allowed to mutate the cache.
            (fumos-repl--invalidate-macro-cache connection)
            (should
             (eq (fumos-connection-repl-buffer connection)
                 (buffer-local-value
                  'fennel-proto-repl--buffer
                  (fumos-connection-process-buffer connection))))
            (should (fumos-test-wait-until (lambda () delayed-call)))
            (setq old-id (fumos-connection-macro-refresh-id connection))
            (should (integerp old-id))
            (should-not (fumos-connection-macro-cache-valid connection))
            (should (equal '(("game.macros" "baseline"))
                           (fumos-connection-macro-cache connection)))
            ;; Reload invalidation must retire OLD-ID before allocating NEW-ID.
            (fumos-repl--invalidate-macro-cache connection)
            (setq new-id (fumos-connection-macro-refresh-id connection))
            (should (integerp new-id))
            (should (/= old-id new-id))
            (should
             (fumos-test-wait-until
              (lambda ()
                (and (fumos-connection-macro-cache-valid connection)
                     (equal '(("game.macros" "fresh"))
                            (fumos-connection-macro-cache connection)))))))
          (cancel-timer delayed-timer)
          ;; Simulate the old zero-delay timer already dequeued by Emacs.
          (apply (car delayed-call) (cdr delayed-call))
          (should (fumos-connection-macro-cache-valid connection))
          (should (equal '(("game.macros" "fresh"))
                         (fumos-connection-macro-cache connection)))
          (should-not (fumos-connection-macro-refresh-pending connection)))
      (when (timerp delayed-timer) (cancel-timer delayed-timer))
      (when connection (fumos-repl-close connection))
      (fumos-test-server-stop server)
      (delete-directory root t))))

(ert-deftest fumos-macro-cache-commit-refreshes-owned-source-font-lock ()
  (let* ((fumos-repl--connections (make-hash-table :test #'equal))
         (root (fumos-test-make-project-root
                (make-temp-file "fumos-macro-source-root-" t)))
         (source (fumos-test-make-source-buffer root "scripts/macros.fnl"))
         (server
          (fumos-test-server-start
           (lambda (state client line)
             (pcase (length (fumos-test-server-lines state))
               (1 (fumos-test-server-send state fumos-test-golden-ack client))
               (2 (fumos-test-server-send state fumos-test-init-frame client))
               (_ (when (string-match-p "macro-loaded" line)
                    (fumos-test-send-macro-result
                     state client line "[[\"game.macros\" \"defwave\"]]")))))))
         (original-refresh
          (symbol-function 'fennel-proto-repl-refresh-dynamic-font-lock))
         connection refreshed)
    (unwind-protect
        (cl-letf
            (((symbol-function 'fennel-proto-repl-refresh-dynamic-font-lock)
              (lambda ()
                (when (and connection
                           (eq source (current-buffer))
                           (eq connection fumos-repl--source-owner)
                           (fumos-connection-macro-cache-valid connection))
                  (push (current-buffer) refreshed))
                (funcall original-refresh))))
          (setq connection
                (fumos-repl-connect-instance
                 (fumos-test-instance-for-server server 4242 root)))
          (should (fumos-test-wait-until
                   (lambda ()
                     (and (fumos-connection-macro-cache-valid connection)
                          refreshed))))
          (should (equal (list source) refreshed))
          (with-current-buffer source
            (should (eq connection fumos-repl--source-owner))))
      (when connection (fumos-repl-close connection))
      (fumos-test-server-stop server)
      (when (buffer-live-p source) (kill-buffer source))
      (delete-directory root t))))

(ert-deftest fumos-macro-cache-parser-rejects-malformed-shapes ()
  (should (equal '(t) (fumos-repl--parse-macro-cache "nil")))
  (should
   (equal '(t ("pkg" "macro"))
          (fumos-repl--parse-macro-cache "[[\"pkg\" \"macro\"]]")))
  (dolist (wire '("\"not-a-cache\"" "(\"pkg\")" "[\"pkg\"]"
                  "[[\"pkg\" 42]]" "[[\"pkg\"]] trailing"))
    (should-not (fumos-repl--parse-macro-cache wire)))
  (let ((connection
         (make-fumos-connection
          :macro-cache '(("old.pkg" "old-macro"))
          :macro-cache-valid nil :macro-refresh-pending t
          :macro-refresh-id 9 :macro-refresh-generation 3))
        refreshed)
    (cl-letf (((symbol-function 'fumos-repl--macro-refresh-current-p)
               (lambda (&rest _) t))
              ((symbol-function 'fumos-repl--refresh-linked-font-lock)
               (lambda (&rest _) (setq refreshed t))))
      (fumos-repl--complete-macro-refresh
       connection nil 3 nil 9 '("[\"bad-entry\"]")))
    (should-not refreshed)
    (should-not (fumos-connection-macro-cache-valid connection))
    (should (equal '(("old.pkg" "old-macro"))
                   (fumos-connection-macro-cache connection)))))

(ert-deftest fumos-macro-font-lock-refresh-isolates-exits-and-reentry ()
  (let* ((first (generate-new-buffer " *fumos-macro-refresh-first*"))
         (second (generate-new-buffer " *fumos-macro-refresh-second*"))
         (connection
          (make-fumos-connection
           :linked-buffers (list first second) :generation 7))
         seen)
    (unwind-protect
        (progn
          (dolist (buffer (list first second))
            (with-current-buffer buffer
              (setq-local fumos-repl--source-owner connection)))
          (dolist (kind '(error quit))
            (setq seen nil)
            (cl-letf
                (((symbol-function 'fumos-repl--owns-transport-p)
                  (lambda (&rest _) t))
                 ((symbol-function
                   'fennel-proto-repl-refresh-dynamic-font-lock)
                  (lambda ()
                    (push (current-buffer) seen)
                    (when (eq first (current-buffer))
                      (signal kind nil)))))
              (fumos-repl--refresh-linked-font-lock
               connection 'process 7))
            (should (memq first seen))
            (should (memq second seen)))
          (let ((still-owned t))
            (setq seen nil)
            (cl-letf
                (((symbol-function 'fumos-repl--owns-transport-p)
                  (lambda (&rest _) still-owned))
                 ((symbol-function
                   'fennel-proto-repl-refresh-dynamic-font-lock)
                  (lambda ()
                    (push (current-buffer) seen)
                    (when (eq first (current-buffer))
                      (setq still-owned nil)))))
              (fumos-repl--refresh-linked-font-lock
               connection 'process 7))
            (should (equal (list first) seen))))
      (dolist (buffer (list first second))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest fumos-empty-macro-cache-is-valid-and-not-refetched ()
  (fumos-test-with-ready-connection (connection server)
    (should (fumos-connection-macro-cache-valid connection))
    (should-not (fumos-connection-macro-cache connection))
    (let ((before (length (fumos-test-server-lines server))))
      (with-current-buffer (fumos-connection-repl-buffer connection)
        (dotimes (_ 3)
          (should-not
           (fumos-repl--obtain-macros-advice
            (lambda () (ert-fail "ordinary macro lookup called"))))))
      (accept-process-output nil 0.05)
      (should (= before (length (fumos-test-server-lines server)))))))

(ert-deftest fumos-replacement-rejects-old-generation-macro-cache ()
  (let* ((fumos-repl--connections (make-hash-table :test #'equal))
         (root (file-name-as-directory
                (file-truename
                 (fumos-test-make-project-root
                  (make-temp-file "fumos-macro-replacement-root-" t)))))
         old-id
         (old-server
          (fumos-test-server-start
           (lambda (state client line)
             (pcase (length (fumos-test-server-lines state))
               (1 (fumos-test-server-send state fumos-test-golden-ack client))
               (2 (fumos-test-server-send state fumos-test-init-frame client))
               (_ (when (string-match-p "macro-loaded" line)
                    (setq old-id (fumos-test-wire-message-id line))))))))
         (new-server
          (fumos-test-server-start
           (lambda (state client line)
             (pcase (length (fumos-test-server-lines state))
               (1 (fumos-test-server-send state fumos-test-golden-ack client))
               (2 (fumos-test-server-send state fumos-test-init-frame client))
               (_ (when (string-match-p "macro-loaded" line)
                    (fumos-test-send-macro-result
                     state client line "[[\"new.macros\" \"fresh\"]]")))))))
         (old (fumos-repl-connect-instance
               (fumos-test-instance-for-server old-server 4242 root)))
         old-process old-filter replacement)
    (unwind-protect
        (progn
          (should (fumos-test-wait-until (lambda () old-id)))
          (setq old-process (fumos-connection-process old)
                old-filter (process-filter old-process)
                replacement
                (fumos-repl-connect-instance
                 (fumos-test-instance-for-server
                  new-server 4242 root "demo" (make-string 64 ?b))))
          (should (fumos-test-wait-until
                   (lambda ()
                     (fumos-connection-macro-cache-valid replacement))))
          (let (stale-refreshed)
            (cl-letf (((symbol-function 'fumos-repl--refresh-linked-font-lock)
                       (lambda (&rest _) (setq stale-refreshed t))))
              (funcall
               old-filter old-process
               (format
                (concat "(:id %d :op \"accept\")\n"
                        "(:id %d :op \"eval\" :values "
                        "(\"[[\\\"old.macros\\\" \\\"stale\\\"]]\"))\n"
                        "(:id %d :op \"done\")\n")
                old-id old-id old-id)))
            (should-not stale-refreshed))
          (accept-process-output nil 0.05)
          (should-not (fumos-connection-macro-cache old))
          (should
           (equal '(("new.macros" "fresh"))
                  (fumos-connection-macro-cache replacement))))
      (when replacement (fumos-repl-close replacement))
      (fumos-repl-close old)
      (fumos-test-server-stop new-server)
      (fumos-test-server-stop old-server)
      (delete-directory root t))))

(provide 'fumos-repl-test)
;;; fumos-repl-test.el ends here
