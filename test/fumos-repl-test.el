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
            (substring fumos-test-init-frame 23))))))

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
             (bootstrap (cadr lines)))
        (should (= 2 (length lines)))
        (should (string-prefix-p "FUMOS/1 AUTH " (car lines)))
        (should (equal fumos-repl-fennel-module-name
                       (fumos-test-bootstrap-module-name bootstrap)))
        (should
         (equal fumos-test-bootstrap-sha256
                (secure-hash 'sha256 (concat bootstrap "\n")))))
      (should (equal global-id
                     (default-value 'fennel-proto-repl--message-id)))
      (should (equal global-module
                     (default-value
                      'fennel-proto-repl-fennel-module-name)))
      (with-current-buffer (fumos-connection-repl-buffer connection)
        (should (local-variable-p 'fennel-proto-repl--message-id))
        (should (= 1 fennel-proto-repl--message-id))
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

(ert-deftest fumos-repl-open-error-and-quit-are-fixed-and-transactional ()
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
                      (signal kind (list (concat "secret=" token)))
                    (funcall original-send process string)))))
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

(provide 'fumos-repl-test)
;;; fumos-repl-test.el ends here
