;;; fumos-repl.el --- Attach fennel-proto-repl to Kristal -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'cl-lib)
(require 'comint)
(require 'subr-x)
(require 'fennel-proto-repl)
(require 'fumos-instance)
(require 'fumos-project)

(defcustom fumos-handshake-timeout 2.0
  "Seconds allowed for the FUMOS authentication handshake."
  :type 'number
  :group 'fennel-proto-repl)

(defcustom fumos-bootstrap-timeout 2.0
  "Seconds allowed for the proto initialization after authentication."
  :type 'number
  :group 'fennel-proto-repl)

(define-error 'fumos-repl-connection-error "FUMOS connection setup failed")

(defconst fumos-repl-fennel-module-name "fumos.repl.fennel"
  "Reserved Fennel module name implemented by a FUMOS Session.")

(defconst fumos-repl--bootstrap-sha256
  "3c57cd018b5274d7c0a5c776a1e449b4e8039c3d7bc5dd58652bacae68e2a0e6"
  "SHA-256 of the pinned one-line proto 0.6.4 bootstrap, including newline.")

(defconst fumos-repl--ack-regexp
  (concat
   "\\`FUMOS/1 OK pid=\\([0-9]+\\) proto=0\\.6\\.4 "
   "capabilities=interrupt,cancel,detach,source-context,game-reload "
   "max=8388608\\'"))

(defvar fumos-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'fumos-interrupt)
    map)
  "Keys active only in a visible FUMOS game REPL buffer.")

(define-minor-mode fumos-repl-mode
  "Mark an upstream proto buffer as a FUMOS game REPL."
  :lighter " FUMOS-REPL"
  :keymap fumos-repl-mode-map)

(cl-defstruct fumos-connection
  instance process process-buffer repl-buffer ui-process state handshake-buffer
  handshake-timer bootstrap-timer active-request-ids pending-game-reload
  last-error generation retry-timers callback-timers callback-deliveries closing
  terminal-timers terminal-deliveries game-reload-timer game-reload-generation
  linked-buffers macro-cache macro-cache-valid macro-refresh-pending
  macro-refresh-id macro-refresh-generation)

(defvar fumos-repl--connections (make-hash-table :test #'equal))
(defvar fumos-repl--attach-transitions (make-hash-table :test #'equal)
  "Latest public attach transition token for each canonical project root.")
(defvar-local fumos-repl--connection nil)
(defvar fumos-repl--next-generation 0)
(defvar fumos-repl--signal-bootstrap-failure nil
  "Non-nil while a synchronous bootstrap callback must signal setup failure.")

(defun fumos-repl--owns-transport-p (connection process generation)
  "Return non-nil when PROCESS still belongs to CONNECTION GENERATION."
  (let ((repl-buffer (fumos-connection-repl-buffer connection)))
    (and (not (fumos-connection-closing connection))
         (eq process (fumos-connection-process connection))
         (eql generation (fumos-connection-generation connection))
         (buffer-live-p repl-buffer)
         (eq connection
             (buffer-local-value 'fumos-repl--connection repl-buffer)))))

(defun fumos-repl--upgrade-code ()
  "Return the pinned one-line proto 0.6.4 bootstrap expression."
  (let ((source
         (fennel-proto-repl--minify-body
          (format "(%s %s %S)"
                  fennel-proto-repl--protocol
                  fennel-proto-repl--format-plist
                  fumos-repl-fennel-module-name)
          'delete-indentation)))
    (unless
        (equal fumos-repl--bootstrap-sha256
               (secure-hash 'sha256 (concat source "\n")))
      (error "Pinned FUMOS proto bootstrap does not match proto 0.6.4"))
    source))

(defun fumos-repl--set-state (connection state)
  "Set CONNECTION to STATE and update its visible mode line."
  (setf (fumos-connection-state connection) state)
  (when (buffer-live-p (fumos-connection-repl-buffer connection))
    (with-current-buffer (fumos-connection-repl-buffer connection)
      (setq mode-line-process (list (format ":%s" state)))
      (force-mode-line-update))))

(defun fumos-repl--cancel-timer (timer)
  "Cancel TIMER without allowing cleanup errors to escape."
  (when (timerp timer)
    (condition-case nil
        (cancel-timer timer)
      (quit nil)
      (error nil))))

(defun fumos-repl--cancel-timer-list (timers)
  "Cancel every timer object in TIMERS."
  (dolist (timer timers)
    (fumos-repl--cancel-timer timer)))

(defun fumos-repl--clear-callbacks (connection)
  "Remove all upstream and connection-owned callbacks for CONNECTION."
  (let ((repl-buffer (fumos-connection-repl-buffer connection)))
    (when (buffer-live-p repl-buffer)
      (condition-case nil
          (with-current-buffer repl-buffer
            (when (hash-table-p fennel-proto-repl--message-callbacks)
              (clrhash fennel-proto-repl--message-callbacks)))
        (quit nil)
        (error nil))))
  (when (hash-table-p (fumos-connection-callback-deliveries connection))
    (clrhash (fumos-connection-callback-deliveries connection))))

(defun fumos-repl--delete-process (process &optional neutralize)
  "Delete PROCESS, setting inert handlers first when NEUTRALIZE is non-nil."
  (when (processp process)
    (condition-case nil
        (set-process-query-on-exit-flag process nil)
      (quit nil)
      (error nil))
    (when neutralize
      (condition-case nil
          (set-process-filter process #'ignore)
        (quit nil)
        (error nil))
      (condition-case nil
          (set-process-sentinel process #'ignore)
        (quit nil)
        (error nil)))
    (condition-case nil
        (when (process-live-p process)
          (delete-process process))
      (quit nil)
      (error nil))))

(defun fumos-repl--erase-and-kill-buffer (buffer)
  "Erase and kill owned BUFFER without interrupting later cleanup."
  (when (buffer-live-p buffer)
    (condition-case nil
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)))
      (quit nil)
      (error nil))
    (condition-case nil
        (with-current-buffer buffer
          (let ((kill-buffer-hook nil)
                (kill-buffer-query-functions nil))
            (kill-buffer (current-buffer))))
      (quit nil)
      (error nil))))

(defun fumos-repl--setup-failed (connection)
  "Roll back CONNECTION and signal the fixed, redacted setup condition."
  (fumos-repl-close connection "FUMOS connection setup failed")
  (signal 'fumos-repl-connection-error nil))

(defun fumos-repl--guard-setup (connection function)
  "Call FUNCTION, rolling back CONNECTION on synchronous error or quit."
  (condition-case nil
      (funcall function)
    ((error quit)
     (fumos-repl--setup-failed connection))))

(defun fumos-repl--validate-ack (connection line)
  "Validate LINE against CONNECTION and return non-nil on success."
  (and (string-match fumos-repl--ack-regexp line)
       (= (string-to-number (match-string 1 line))
          (fumos-instance-pid (fumos-connection-instance connection)))))

(defun fumos-repl--start-upstream-ui (connection values)
  "Start and immediately own upstream's dummy comint process."
  (let ((original-start-process (symbol-function 'start-process))
        (repl-buffer (fumos-connection-repl-buffer connection))
        (fennel-proto-repl--buffer
         (fumos-connection-repl-buffer connection)))
    (cl-letf
        (((symbol-function 'start-process)
          (lambda (&rest arguments)
            (let ((ui-process (apply original-start-process arguments)))
              (when (eq (process-buffer ui-process) repl-buffer)
                (setf (fumos-connection-ui-process connection) ui-process)
                (set-process-query-on-exit-flag ui-process nil))
              ui-process))))
      (apply #'fennel-proto-repl--start-repl values))
    (let ((ui-process (or (fumos-connection-ui-process connection)
                          (get-buffer-process repl-buffer))))
      (unless (processp ui-process)
        (error "Upstream REPL did not create its UI process"))
      (setf (fumos-connection-ui-process connection) ui-process)
      (set-process-query-on-exit-flag ui-process nil)
      ui-process)))

(defun fumos-repl--cancel-bootstrap-deadline (connection)
  "Cancel and clear CONNECTION's bootstrap deadline."
  (fumos-repl--cancel-timer
   (fumos-connection-bootstrap-timer connection))
  (setf (fumos-connection-bootstrap-timer connection) nil))

(defun fumos-repl--handshake-filter (connection process generation chunk)
  "Consume authentication bytes for CONNECTION from PROCESS."
  (when (fumos-repl--owns-transport-p connection process generation)
    (fumos-repl--guard-setup
     connection
     (lambda ()
       (let* ((input
               (concat (fumos-connection-handshake-buffer connection) chunk))
              (newline (string-match "\n" input)))
         (cond
          ((and (null newline) (> (string-bytes input) 4096))
           (fumos-repl--reject connection "Handshake exceeded 4096 bytes"))
          ((null newline)
           (setf (fumos-connection-handshake-buffer connection) input))
          (t
           (let ((line (substring input 0 newline))
                 (leftover (substring input (1+ newline))))
             (setf (fumos-connection-handshake-buffer connection) "")
             (cond
              ((> (string-bytes line) 4096)
               (fumos-repl--reject
                connection "Handshake exceeded 4096 bytes"))
              ((not (fumos-repl--validate-ack connection line))
               (fumos-repl--reject
                connection "Handshake response did not match instance"))
              (t
               (fumos-repl--cancel-timer
                (fumos-connection-handshake-timer connection))
               (setf (fumos-connection-handshake-timer connection) nil)
               (set-process-filter
                process
                (lambda (transport data)
                  (fumos-repl--protocol-filter
                   connection transport generation data)))
               (fumos-repl--bootstrap connection process generation)
               (unless (string-empty-p leftover)
                 (fumos-repl--protocol-filter
                  connection process generation leftover))))))))))))

(defun fumos-repl--buffer-name (instance)
  "Return INSTANCE's deterministic visible FUMOS REPL buffer name."
  (format "*FUMOS: %s@%d*"
          (fumos-instance-mod-id instance)
          (fumos-instance-pid instance)))

(defun fumos-repl--prepare-buffers (connection)
  "Prepare upstream proto buffers for CONNECTION."
  (let* ((instance (fumos-connection-instance connection))
         (name (fumos-repl--buffer-name instance)))
    (when (get-buffer name)
      (signal 'fumos-repl-connection-error (list :buffer-name-conflict)))
    (let ((repl-buffer (get-buffer-create name)))
      (setf (fumos-connection-repl-buffer connection) repl-buffer)
      (let ((process-buffer
             (generate-new-buffer (format " %s transport" name))))
        (setf (fumos-connection-process-buffer connection) process-buffer)
        (with-current-buffer process-buffer
          (buffer-disable-undo)
          (setq-local fennel-proto-repl--buffer repl-buffer
                      fumos-repl--connection connection))
        (with-current-buffer repl-buffer
          (fennel-proto-repl-mode)
          ;; Upstream 0.6.4 uses a global defvar; each game needs its own IDs.
          (setq-local fennel-proto-repl--message-id 0)
          (fennel-proto-repl--init-callbacks)
          (setq-local fennel-proto-repl--buffer repl-buffer
                      fennel-proto-repl--process-buffer process-buffer
                      fennel-proto-repl-fennel-module-name
                      fumos-repl-fennel-module-name
                      fumos-repl--connection connection)
          (fumos-repl-mode 1)
          (setq mode-line-process '(":connecting")))
        connection))))

(defun fumos-repl--new-connection (instance)
  "Allocate an unstarted connection object for INSTANCE."
  (make-fumos-connection
   :instance instance :state 'connecting :handshake-buffer ""
   :generation (cl-incf fumos-repl--next-generation)
   :active-request-ids nil :retry-timers nil :callback-timers nil
   :callback-deliveries (make-hash-table :test #'eql)
   :terminal-timers nil :terminal-deliveries nil
   :game-reload-generation 0))

(defun fumos-repl--open-instance (connection)
  "Create buffers/socket and send AUTH for CONNECTION as one transaction."
  (let* ((instance (fumos-connection-instance connection))
         (generation (fumos-connection-generation connection)))
    (fumos-repl--prepare-buffers connection)
    (let ((process
           (make-network-process
            :name (format "fumos-%s-%d"
                          (fumos-instance-mod-id instance)
                          (fumos-instance-pid instance))
            :buffer (fumos-connection-process-buffer connection)
            :host (fumos-instance-host instance)
            :service (fumos-instance-port instance)
            :coding 'utf-8-unix
            :noquery t
            :filter
            (lambda (transport chunk)
              (fumos-repl--handshake-filter
               connection transport generation chunk))
            :sentinel
            (lambda (transport event)
              (when (and
                     (fumos-repl--owns-transport-p
                      connection transport generation)
                     (not (string-prefix-p "open" event)))
                (fumos-repl--reject connection "Transport closed"))))))
      (setf (fumos-connection-process connection) process)
      (set-process-query-on-exit-flag process nil)
      (fumos-repl--set-state connection 'authenticating)
      (setf
       (fumos-connection-handshake-timer connection)
       (run-at-time
        fumos-handshake-timeout nil
        (lambda ()
          (when (and
                 (fumos-repl--owns-transport-p
                  connection process generation)
                 (eq 'authenticating
                     (fumos-connection-state connection)))
            (fumos-repl--reject connection "Handshake timed out")))))
      (process-send-string
       process
       (format "FUMOS/1 AUTH %s\n" (fumos-instance-token instance)))
      connection)))

(defvar-local fumos-repl--observe-buffer "")

(defun fumos-repl--upstream-repl-buffer ()
  "Return the upstream REPL buffer selected by the current buffer."
  (if (eq major-mode 'fennel-proto-repl-mode)
      (current-buffer)
    (and fennel-proto-repl--buffer
         (get-buffer fennel-proto-repl--buffer))))

(defun fumos-repl--upstream-connection ()
  "Return the FUMOS connection that owns the selected upstream REPL."
  (let ((repl-buffer (fumos-repl--upstream-repl-buffer)))
    (and (buffer-live-p repl-buffer)
         (buffer-local-value 'fumos-repl--connection repl-buffer))))

(cl-defstruct fumos-callback-delivery
  connection process generation repl-buffer request-id callbacks
  values-callback error-callback print-callback timers terminal-timer valid
  terminal-kind terminal-admitted terminal-scheduled terminal-delivered
  connection-lost-state done-seen)

(defun fumos-repl--callback-delivery-table (connection)
  "Return CONNECTION's request delivery table, creating it lazily."
  (or (fumos-connection-callback-deliveries connection)
      (setf (fumos-connection-callback-deliveries connection)
            (make-hash-table :test #'eql))))

(defun fumos-repl--terminal-callback-kind-p (kind)
  "Return non-nil when KIND produces a request's sole terminal result."
  (memq kind '(values error)))

(defun fumos-repl--terminal-kind-for-message (message)
  "Return the callback terminal kind carried by protocol MESSAGE."
  (pcase (plist-get message :op)
    ("error" 'error)
    ((or "init" "eval" "complete" "doc" "reload" "return"
         "apropos" "apropos-doc" "apropos-show-docs" "find" "help"
         "compile" "reset" "exit")
     'values)))

(defun fumos-repl--invoke-callback-isolated
    (connection repl-buffer callback arguments kind)
  "Invoke CALLBACK while containing every nonlocal exit."
  (let ((invoke
         (lambda ()
           ;; Deferred timers run after the filter stack has returned, so an
           ;; unmatched `throw' becomes a catchable `no-catch' error.
           (condition-case nil
               (let ((inhibit-quit t)
                     (quit-flag nil))
                 (apply callback arguments))
             (quit (message "FUMOS %s callback quit" kind))
             (error (message "FUMOS %s callback failed" kind))))))
    (if (buffer-live-p repl-buffer)
        (with-current-buffer repl-buffer (funcall invoke))
      (with-temp-buffer
        (setq-local fumos-repl--connection connection)
        (funcall invoke)))))

(defun fumos-repl--callback-delivery-current-p
    (delivery request-id callback-identity &optional require-upstream-entry)
  "Validate every owner identity captured by DELIVERY."
  (let* ((connection (fumos-callback-delivery-connection delivery))
         (process (fumos-callback-delivery-process delivery))
         (generation (fumos-callback-delivery-generation delivery))
         (repl-buffer (fumos-callback-delivery-repl-buffer delivery))
         (table (fumos-repl--callback-delivery-table connection)))
    (and (fumos-callback-delivery-valid delivery)
         (not (fumos-connection-closing connection))
         (eq process (fumos-connection-process connection))
         (eql generation (fumos-connection-generation connection))
         (process-live-p process)
         (eql request-id (fumos-callback-delivery-request-id delivery))
         (eq callback-identity
             (fumos-callback-delivery-callbacks delivery))
         (eq delivery (gethash request-id table))
         (or
          (not require-upstream-entry)
          (and
           (buffer-live-p repl-buffer)
           (with-current-buffer repl-buffer
             (and (hash-table-p fennel-proto-repl--message-callbacks)
                  (eq callback-identity
                      (gethash request-id
                               fennel-proto-repl--message-callbacks)))))))))

(defun fumos-repl--admit-terminal-frame (message)
  "Admit MESSAGE's terminal before pinned upstream can unassign its ID."
  (let* ((kind (fumos-repl--terminal-kind-for-message message))
         (request-id (plist-get message :id))
         (repl-buffer (fumos-repl--upstream-repl-buffer))
         (connection (and (buffer-live-p repl-buffer)
                          (buffer-local-value
                           'fumos-repl--connection repl-buffer)))
         (callback-identity
          (and connection (integerp request-id)
               (with-current-buffer repl-buffer
                 (and (hash-table-p fennel-proto-repl--message-callbacks)
                      (gethash request-id
                               fennel-proto-repl--message-callbacks)))))
         (delivery
          (and callback-identity
               (gethash request-id
                        (fumos-repl--callback-delivery-table connection)))))
    (when (and kind delivery
               (fumos-repl--callback-delivery-current-p
                delivery request-id callback-identity t)
               (not (fumos-callback-delivery-terminal-admitted delivery))
               (not (fumos-callback-delivery-terminal-delivered delivery)))
      (setf (fumos-callback-delivery-terminal-kind delivery) kind
            (fumos-callback-delivery-terminal-admitted delivery) t)
      delivery)))

(defun fumos-repl--terminal-frame-advice (original message)
  "Admit one FUMOS terminal, then preserve pinned upstream ordering."
  (let ((delivery (fumos-repl--admit-terminal-frame message)))
    (unwind-protect
        (funcall original message)
      (when delivery
        (fumos-repl--maybe-forget-callback-delivery delivery)))))

(unless (advice-member-p #'fumos-repl--terminal-frame-advice
                         'fennel-proto-repl--handle-protocol-op)
  (advice-add 'fennel-proto-repl--handle-protocol-op :around
              #'fumos-repl--terminal-frame-advice))

(defun fumos-repl--maybe-forget-callback-delivery (delivery)
  "Forget DELIVERY only after done and every admitted result are settled."
  (when (and (fumos-callback-delivery-done-seen delivery)
             (null (fumos-callback-delivery-timers delivery))
             (or (not (fumos-callback-delivery-terminal-admitted delivery))
                 (fumos-callback-delivery-terminal-delivered delivery)))
    (let* ((connection (fumos-callback-delivery-connection delivery))
           (id (fumos-callback-delivery-request-id delivery))
           (table (fumos-repl--callback-delivery-table connection)))
      (when (eq delivery (gethash id table))
        (remhash id table))
      (setf (fumos-callback-delivery-valid delivery) nil))))

(defun fumos-repl--deliver-callback
    (delivery request-id callback-identity callback arguments timer kind)
  "Deliver one identity-gated CALLBACK outside the protocol filter."
  (let ((connection (fumos-callback-delivery-connection delivery)))
    (setf (fumos-callback-delivery-timers delivery)
          (delq timer (fumos-callback-delivery-timers delivery))
          (fumos-connection-callback-timers connection)
          (delq timer (fumos-connection-callback-timers connection)))
    (when (fumos-repl--callback-delivery-current-p
           delivery request-id callback-identity)
      (if (fumos-repl--terminal-callback-kind-p kind)
          (when (and
                 (fumos-callback-delivery-terminal-admitted delivery)
                 (eq kind (fumos-callback-delivery-terminal-kind delivery))
                 (fumos-callback-delivery-terminal-scheduled delivery)
                 (not (fumos-callback-delivery-terminal-delivered delivery)))
            ;; Claim the one terminal outcome before calling untrusted UI code.
            (setf (fumos-callback-delivery-terminal-scheduled delivery) nil
                  (fumos-callback-delivery-terminal-delivered delivery) t)
            (fumos-repl--invoke-callback-isolated
             connection (fumos-callback-delivery-repl-buffer delivery)
             callback arguments kind))
        (fumos-repl--invoke-callback-isolated
         connection (fumos-callback-delivery-repl-buffer delivery)
         callback arguments kind)))
    (fumos-repl--maybe-forget-callback-delivery delivery)))

(defun fumos-repl--defer-callback (delivery callback arguments kind)
  "Schedule a request-owned CALLBACK after the current protocol chunk."
  (let* ((callback-identity (fumos-callback-delivery-callbacks delivery))
         (request-id (fumos-callback-delivery-request-id delivery))
         (connection (fumos-callback-delivery-connection delivery))
         (terminal (fumos-repl--terminal-callback-kind-p kind))
         timer)
    (when
        (and
         (fumos-repl--callback-delivery-current-p
          delivery request-id callback-identity)
         (if terminal
             (and
              (fumos-callback-delivery-terminal-admitted delivery)
              (eq kind (fumos-callback-delivery-terminal-kind delivery))
              (not (fumos-callback-delivery-terminal-scheduled delivery))
              (not (fumos-callback-delivery-terminal-delivered delivery)))
           ;; print is nonterminal, so it still requires the upstream hash
           ;; entry at frame time.  A saved print wrapper cannot self-admit.
           (fumos-repl--callback-delivery-current-p
            delivery request-id callback-identity t)))
      (when terminal
        (setf (fumos-callback-delivery-terminal-scheduled delivery) t))
      (condition-case nil
          (progn
            (setq timer
                  (run-at-time
                   0 nil
                   (lambda ()
                     (fumos-repl--deliver-callback
                      delivery request-id callback-identity callback
                      (copy-sequence arguments) timer kind))))
            (unless (timerp timer)
              (error "FUMOS callback scheduler returned no timer"))
            (push timer (fumos-callback-delivery-timers delivery))
            (push timer (fumos-connection-callback-timers connection))
            timer)
        (quit
         (when terminal
           (setf (fumos-callback-delivery-terminal-kind delivery) nil
                 (fumos-callback-delivery-terminal-admitted delivery) nil
                 (fumos-callback-delivery-terminal-scheduled delivery) nil))
         (fumos-repl--maybe-forget-callback-delivery delivery)
         (fumos-repl--reject connection "Callback scheduling failed")
         nil)
        (error
         (when terminal
           (setf (fumos-callback-delivery-terminal-kind delivery) nil
                 (fumos-callback-delivery-terminal-admitted delivery) nil
                 (fumos-callback-delivery-terminal-scheduled delivery) nil))
         (fumos-repl--maybe-forget-callback-delivery delivery)
         (fumos-repl--reject connection "Callback scheduling failed")
         nil)))))

(defun fumos-repl--safe-callback (delivery callback kind)
  "Return a protocol-safe wrapper for DELIVERY's CALLBACK of KIND."
  (lambda (&rest arguments)
    (let ((connection (fumos-callback-delivery-connection delivery)))
      ;; Task 7 exposes the bootstrap callback to synchronous setup guards.
      ;; Real init frames are admitted first and still take the deferred path;
      ;; this branch only preserves transactional direct invocation in tests
      ;; and setup code before a protocol terminal exists.
      (if (and (eq kind 'values)
               (eq 'bootstrapping (fumos-connection-state connection))
               (not (fumos-callback-delivery-terminal-admitted delivery)))
          (apply callback arguments)
        (fumos-repl--defer-callback delivery callback arguments kind)))))

(defun fumos-repl--assign-callback-advice
    (original values-callback &optional error-callback print-callback)
  "Bind every FUMOS callback to its transport and callback record identity."
  (let* ((repl-buffer (fumos-repl--upstream-repl-buffer))
         (connection (and (buffer-live-p repl-buffer)
                          (buffer-local-value
                           'fumos-repl--connection repl-buffer))))
    (if (not connection)
        (funcall original values-callback error-callback print-callback)
      (let* ((resolved-error
              (or error-callback #'fennel-proto-repl--error-handler))
             (resolved-print (or print-callback #'fennel-proto-repl--print))
             (delivery
              (make-fumos-callback-delivery
               :connection connection
               :process (fumos-connection-process connection)
               :generation (fumos-connection-generation connection)
               :repl-buffer repl-buffer
               :values-callback values-callback
               :error-callback resolved-error
               :print-callback resolved-print
               :valid t))
             (id
              (funcall
               original
               (fumos-repl--safe-callback delivery values-callback 'values)
               (fumos-repl--safe-callback delivery resolved-error 'error)
               (fumos-repl--safe-callback delivery resolved-print 'print))))
        (when id
          (with-current-buffer repl-buffer
            (let ((callback-identity
                   (gethash id fennel-proto-repl--message-callbacks)))
              (setf (fumos-callback-delivery-request-id delivery) id
                    (fumos-callback-delivery-callbacks delivery)
                    callback-identity)
              (puthash id delivery
                       (fumos-repl--callback-delivery-table connection)))))
        id))))

(unless (advice-member-p #'fumos-repl--assign-callback-advice
                         'fennel-proto-repl--assign-callback)
  (advice-add 'fennel-proto-repl--assign-callback :around
              #'fumos-repl--assign-callback-advice))

(defun fumos-repl--unassign-callback-advice (original id)
  "Record upstream unassign without conflating it with terminal delivery."
  (let* ((connection (fumos-repl--upstream-connection))
         (delivery
          (and connection
               (gethash id
                        (fumos-repl--callback-delivery-table connection)))))
    (prog1 (funcall original id)
      (when delivery
        (setf (fumos-callback-delivery-done-seen delivery) t)
        (fumos-repl--maybe-forget-callback-delivery delivery)))))

(unless (advice-member-p #'fumos-repl--unassign-callback-advice
                         'fennel-proto-repl--unassign-callbacks)
  (advice-add 'fennel-proto-repl--unassign-callbacks :around
              #'fumos-repl--unassign-callback-advice))

(defun fumos-repl--observe-frame (connection line)
  "Update CONNECTION state from one proto response LINE."
  (cond
   ((string-match "\\`(:id \\([0-9]+\\) :op \"accept\"" line)
    (cl-pushnew (string-to-number (match-string 1 line))
                (fumos-connection-active-request-ids connection)
                :test #'eql)
    (fumos-repl--set-state connection 'busy))
   ((string-match "\\`(:id \\([0-9]+\\) :op \"done\"" line)
    (setf (fumos-connection-active-request-ids connection)
          (delq (string-to-number (match-string 1 line))
                (fumos-connection-active-request-ids connection)))
    (fumos-repl--set-state
     connection
     (if (fumos-connection-active-request-ids connection) 'busy 'ready)))))

(defun fumos-repl--observe-chunk (connection process generation chunk)
  "Observe complete proto lines in CHUNK without consuming upstream input."
  (when (fumos-repl--owns-transport-p connection process generation)
    (with-current-buffer (process-buffer process)
      (let ((input (concat fumos-repl--observe-buffer chunk))
            (start 0))
        (while (string-match "\n" input start)
          ;; `fumos-repl--observe-frame' performs its own regexp matches and
          ;; therefore overwrites match data.  Capture both newline bounds
          ;; before calling it or a multi-frame chunk can loop on one newline.
          (let ((line-end (match-beginning 0))
                (next-line (match-end 0)))
            (fumos-repl--observe-frame
             connection (substring input start line-end))
            (setq start next-line)))
        (setq fumos-repl--observe-buffer (substring input start))))))

(defun fumos-repl--protocol-filter (connection process generation chunk)
  "Observe CHUNK, run pinned upstream, then repair its scalar busy state."
  (when (fumos-repl--owns-transport-p connection process generation)
    (fumos-repl--observe-chunk connection process generation chunk)
    (unwind-protect
        (fennel-proto-repl--process-filter process chunk)
      (when (and (fumos-repl--owns-transport-p
                  connection process generation)
                 ;; An init frame has no active request.  Only bootstrap's
                 ;; deferred commit may transition bootstrapping to ready.
                 (memq (fumos-connection-state connection) '(ready busy)))
        (fumos-repl--set-state
         connection
         (if (fumos-connection-active-request-ids connection)
             'busy
           'ready))))))

(defun fumos-repl--cancel-callback-deliveries (connection)
  "Cancel and invalidate every deferred callback owned by CONNECTION."
  (dolist (timer (fumos-connection-callback-timers connection))
    (when (timerp timer) (cancel-timer timer)))
  (setf (fumos-connection-callback-timers connection) nil)
  (maphash
   (lambda (_id delivery)
     (setf (fumos-callback-delivery-valid delivery) nil
           (fumos-callback-delivery-timers delivery) nil
           ;; Cancellation is not delivery.  fail-pending will independently
           ;; admit connection-lost when no normal terminal reached the user.
           (fumos-callback-delivery-terminal-scheduled delivery) nil))
   (fumos-repl--callback-delivery-table connection)))

(defun fumos-repl--terminal-delivery-current-p
    (delivery request-id callback-identity)
  "Validate the independent disconnect terminal owned by DELIVERY."
  (let ((connection (fumos-callback-delivery-connection delivery)))
    (and (eq 'scheduled
             (fumos-callback-delivery-connection-lost-state delivery))
         (eq callback-identity
             (fumos-callback-delivery-callbacks delivery))
         (eql request-id
              (fumos-callback-delivery-request-id delivery))
         (eql (fumos-callback-delivery-generation delivery)
              (fumos-connection-generation connection))
         (memq delivery
               (fumos-connection-terminal-deliveries connection)))))

(defun fumos-repl--deliver-connection-lost
    (delivery request-id callback-identity timer)
  "Deliver DELIVERY's separately owned connection-lost terminal once."
  (let* ((connection (fumos-callback-delivery-connection delivery))
         (current
          (fumos-repl--terminal-delivery-current-p
           delivery request-id callback-identity)))
    (setf (fumos-connection-terminal-timers connection)
          (delq timer (fumos-connection-terminal-timers connection))
          (fumos-connection-terminal-deliveries connection)
          (delq delivery (fumos-connection-terminal-deliveries connection))
          (fumos-callback-delivery-terminal-timer delivery) nil)
    (when current
      ;; Claim before calling untrusted code; a late/manual second call is nil.
      (setf (fumos-callback-delivery-connection-lost-state delivery)
            'delivered)
      (fumos-repl--invoke-callback-isolated
       connection (fumos-callback-delivery-repl-buffer delivery)
       (fumos-callback-delivery-error-callback delivery)
       '("connection-lost" "FUMOS connection closed" nil)
       'connection-lost))))

(defun fumos-repl--schedule-connection-lost-once (delivery)
  "Schedule one terminal connection-lost delivery independent of old timers."
  (unless (memq (fumos-callback-delivery-connection-lost-state delivery)
                '(scheduled delivered))
    (let* ((connection (fumos-callback-delivery-connection delivery))
           (request-id (fumos-callback-delivery-request-id delivery))
           (callback-identity (fumos-callback-delivery-callbacks delivery))
           timer)
      (setf (fumos-callback-delivery-connection-lost-state delivery)
            'scheduled)
      (push delivery (fumos-connection-terminal-deliveries connection))
      (condition-case nil
          (progn
            (setq timer
                  (run-at-time
                   0 nil
                   (lambda ()
                     (fumos-repl--deliver-connection-lost
                      delivery request-id callback-identity timer))))
            (setf (fumos-callback-delivery-terminal-timer delivery) timer)
            (push timer (fumos-connection-terminal-timers connection)))
        (quit
         (setf (fumos-connection-terminal-deliveries connection)
               (delq delivery
                     (fumos-connection-terminal-deliveries connection))
               (fumos-callback-delivery-connection-lost-state delivery)
               'delivered)
         (fumos-repl--invoke-callback-isolated
          connection (fumos-callback-delivery-repl-buffer delivery)
          (fumos-callback-delivery-error-callback delivery)
          '("connection-lost" "FUMOS connection closed" nil)
          'connection-lost))
        (error
         (setf (fumos-connection-terminal-deliveries connection)
               (delq delivery
                     (fumos-connection-terminal-deliveries connection))
               (fumos-callback-delivery-connection-lost-state delivery)
               'delivered)
         (fumos-repl--invoke-callback-isolated
          connection (fumos-callback-delivery-repl-buffer delivery)
          (fumos-callback-delivery-error-callback delivery)
          '("connection-lost" "FUMOS connection closed" nil)
          'connection-lost))))))

(defun fumos-repl--fail-pending (connection)
  "Clear pending callbacks, then deliver each missing terminal exactly once."
  (let ((delivery-table (fumos-repl--callback-delivery-table connection))
        terminal-deliveries)
    (maphash
     (lambda (id delivery)
       (if (and (eql id 0)
                (memq (fumos-connection-state connection)
                      '(connecting authenticating bootstrapping)))
           ;; Bootstrap is internal setup work, not a user request.  A setup
           ;; failure is reported by the fixed connection condition instead.
           (setf (fumos-callback-delivery-terminal-delivered delivery) t
                 (fumos-callback-delivery-connection-lost-state delivery)
                 'delivered)
         (when (and
                (not (fumos-callback-delivery-terminal-delivered delivery))
                (not (memq
                      (fumos-callback-delivery-connection-lost-state delivery)
                      '(scheduled delivered))))
           (push delivery terminal-deliveries))))
     delivery-table)
    (when (buffer-live-p (fumos-connection-repl-buffer connection))
      (with-current-buffer (fumos-connection-repl-buffer connection)
        (when (hash-table-p fennel-proto-repl--message-callbacks)
          ;; Clear before scheduling user callbacks to make reentry terminal.
          (clrhash fennel-proto-repl--message-callbacks))))
    (dolist (delivery terminal-deliveries)
      (fumos-repl--schedule-connection-lost-once delivery))
    (clrhash delivery-table)))

(defun fumos-repl--cancel-retry-timers (connection)
  "Cancel every proto retry timer owned by CONNECTION."
  (dolist (timer (fumos-connection-retry-timers connection))
    (when (timerp timer) (cancel-timer timer)))
  (setf (fumos-connection-retry-timers connection) nil))

(defun fumos-repl--cancel-game-reload-timer (connection)
  "Cancel and invalidate CONNECTION's editor-side game reload wait."
  (let ((timer (fumos-connection-game-reload-timer connection)))
    (when (timerp timer) (cancel-timer timer)))
  (setf (fumos-connection-game-reload-timer connection) nil
        (fumos-connection-game-reload-generation connection)
        (1+ (or (fumos-connection-game-reload-generation connection) 0))
        (fumos-connection-pending-game-reload connection) nil))

(defun fumos-repl--unregister-if-current (connection)
  "Remove CONNECTION only when it is still the registered object."
  (let* ((instance (fumos-connection-instance connection))
         (root (and instance (fumos-instance-project-root instance))))
    (when (and root (eq connection (gethash root fumos-repl--connections)))
      (remhash root fumos-repl--connections))))

(defun fumos-repl--delete-ui-process (connection)
  "Delete upstream's dummy comint process owned by CONNECTION."
  (let ((ui-process (fumos-connection-ui-process connection)))
    (setf (fumos-connection-ui-process connection) nil)
    (when (processp ui-process)
      (set-process-query-on-exit-flag ui-process nil)
      (when (process-live-p ui-process) (delete-process ui-process)))))

(defvar-local fumos-repl--source-owner nil
  "FUMOS connection that owns the current source-buffer link.")

(defvar-local fumos-repl--source-enabled-upstream-mode nil
  "Non-nil when FUMOS, rather than the user, enabled proto minor mode.")

(defvar-local fumos-repl--source-previous-upstream-buffer nil
  "Exact live proto REPL target that FUMOS temporarily replaced.")

(defvar-local fumos-repl--source-previous-upstream-mode nil
  "Proto minor-mode state captured before the first FUMOS owner.")

(defvar-local fumos-repl--source-previous-module-local-p nil
  "Whether the source had a local proto module name before FUMOS ownership.")

(defvar-local fumos-repl--source-previous-module-value nil
  "Proto module name captured before the first FUMOS source owner.")

(defvar-local fumos-repl--source-link-transition nil
  "Identity token for the source link transaction currently in flight.")

(defvar fumos-repl--internal-link-target nil
  "REPL target being installed by a FUMOS source link transaction.")

(defun fumos-repl--live-previous-upstream-buffer ()
  "Return the saved ordinary target while it is still live."
  (and (buffer-live-p fumos-repl--source-previous-upstream-buffer)
       fumos-repl--source-previous-upstream-buffer))

(defun fumos-repl--restore-source-module-name (local-p value)
  "Restore the current source's proto module name locality and VALUE."
  (if local-p
      (setq-local fennel-proto-repl-fennel-module-name value)
    (kill-local-variable 'fennel-proto-repl-fennel-module-name)))

(defun fumos-repl--release-source-owner (&optional preserve-upstream)
  "Release the current source owner and optionally PRESERVE-UPSTREAM link."
  (let* ((source (current-buffer))
         (connection fumos-repl--source-owner)
         (owned-repl
          (and connection (fumos-connection-repl-buffer connection)))
         (previous-buffer (fumos-repl--live-previous-upstream-buffer))
         (previous-mode fumos-repl--source-previous-upstream-mode)
         (previous-module-local-p
          fumos-repl--source-previous-module-local-p)
         (previous-module-value fumos-repl--source-previous-module-value))
    ;; Clear identity before any minor-mode hook can re-enter cleanup.
    (setq fumos-repl--source-owner nil
          fumos-repl--source-enabled-upstream-mode nil
          fumos-repl--source-previous-upstream-buffer nil
          fumos-repl--source-previous-upstream-mode nil
          fumos-repl--source-previous-module-local-p nil
          fumos-repl--source-previous-module-value nil
          fumos-repl--source-link-transition nil)
    (when connection
      (setf (fumos-connection-linked-buffers connection)
            (delq source (fumos-connection-linked-buffers connection)))
      (fumos-repl--restore-source-module-name
       previous-module-local-p previous-module-value))
    (remove-hook 'kill-buffer-hook #'fumos-repl--source-kill-cleanup t)
    (remove-hook 'fennel-proto-repl-minor-mode-hook
                 #'fumos-repl--source-upstream-mode-change t)
    (remove-hook 'xref-backend-functions 'fumos-repl--xref-backend t)
    (when (and (not preserve-upstream)
               (eq fennel-proto-repl--buffer owned-repl))
      ;; Restore both halves of the snapshot.  Set the target before enabling
      ;; upstream mode, then link it explicitly because project integration can
      ;; otherwise choose a different REPL during the mode hook.
      (setq fennel-proto-repl--buffer previous-buffer)
      (condition-case nil
          (if previous-mode
              (progn
                (unless fennel-proto-repl-minor-mode
                  (fennel-proto-repl-minor-mode 1))
                (setq fennel-proto-repl--buffer previous-buffer)
                (when previous-buffer
                  (fennel-proto-repl--link-buffer previous-buffer)))
            (when fennel-proto-repl-minor-mode
              (fennel-proto-repl-minor-mode -1))
            (setq fennel-proto-repl--buffer previous-buffer))
        (quit
         (setq fennel-proto-repl--buffer previous-buffer))
        (error
         (setq fennel-proto-repl--buffer previous-buffer))))
    connection))

(defun fumos-repl--source-upstream-mode-change ()
  "Drop FUMOS ownership when the user disables proto minor mode."
  (when (and fumos-repl--source-owner
             (not fennel-proto-repl-minor-mode))
    ;; The explicit disable is newer user intent than the saved mode state.
    ;; Restore only the previous target and retain the current disabled mode.
    (setq fennel-proto-repl--buffer
          (fumos-repl--live-previous-upstream-buffer))
    (fumos-repl--release-source-owner t)))

(defun fumos-repl--link-buffer-advice (original &optional repl-buffer)
  "Let an ordinary upstream relink supersede a stale FUMOS source owner."
  (let ((owner fumos-repl--source-owner))
    (unwind-protect
        (funcall original repl-buffer)
      (when (and owner
                 (eq owner fumos-repl--source-owner)
                 (not (eq fennel-proto-repl--buffer
                          (fumos-connection-repl-buffer owner)))
                 (not (eq fennel-proto-repl--buffer
                          fumos-repl--internal-link-target)))
        ;; Upstream already installed the ordinary target.  Release only the
        ;; old FUMOS bookkeeping and keep that target and minor mode intact.
        (fumos-repl--release-source-owner t)))))

(unless (advice-member-p #'fumos-repl--link-buffer-advice
                         'fennel-proto-repl--link-buffer)
  (advice-add 'fennel-proto-repl--link-buffer :around
              #'fumos-repl--link-buffer-advice))

(defun fumos-repl--unlink-project-buffers (connection)
  "Remove only source-buffer links whose local owner is CONNECTION."
  (let ((buffers (copy-sequence
                  (fumos-connection-linked-buffers connection))))
    (dolist (buffer buffers)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (eq fumos-repl--source-owner connection)
            (fumos-repl--release-source-owner)))))
    ;; A source already relinked elsewhere is not touched, but any stale strong
    ;; reference left in this connection is still deterministically released.
    (setf (fumos-connection-linked-buffers connection) nil)))

(defun fumos-repl-unlink-current-buffer ()
  "Unlink the current source buffer according to its local FUMOS owner."
  (fumos-repl--release-source-owner))

(defun fumos-repl--source-kill-cleanup ()
  "Release only the FUMOS link owned by the source buffer being killed."
  (condition-case nil
      (fumos-repl-unlink-current-buffer)
    (quit nil)
    (error nil)))

(defun fumos-repl--teardown-transport (connection message)
  "Invalidate CONNECTION and release transport resources, preserving history."
  (let ((first-teardown (not (fumos-connection-closing connection))))
    ;; Closing is an ownership barrier, not a reason to skip cleanup.  A
    ;; reentrant bootstrap can acquire a source link after the first teardown
    ;; began, so every call below still sweeps concrete resources.
    (setf (fumos-connection-closing connection) t
          (fumos-connection-last-error connection) message
          (fumos-connection-active-request-ids connection) nil
          (fumos-connection-macro-cache connection) nil
          (fumos-connection-macro-cache-valid connection) nil
          (fumos-connection-macro-refresh-pending connection) nil
          (fumos-connection-macro-refresh-id connection) nil
          (fumos-connection-macro-refresh-generation connection) nil)
    (fumos-repl--cancel-callback-deliveries connection)
    (fumos-repl--cancel-timer
     (fumos-connection-handshake-timer connection))
    (fumos-repl--cancel-bootstrap-deadline connection)
    (setf (fumos-connection-handshake-timer connection) nil)
    (fumos-repl--cancel-retry-timers connection)
    (when first-teardown
      (fumos-repl--fail-pending connection))
    (fumos-repl--delete-ui-process connection)
    (fumos-repl--unlink-project-buffers connection)
    (let ((process (fumos-connection-process connection)))
      (fumos-repl--delete-process process 'neutralize))
    (fumos-repl--erase-and-kill-buffer
     (fumos-connection-process-buffer connection))
    (setf (fumos-connection-process connection) nil
          (fumos-connection-process-buffer connection) nil
          (fumos-connection-ui-process connection) nil
          (fumos-connection-handshake-buffer connection) ""
          (fumos-connection-handshake-timer connection) nil
          (fumos-connection-bootstrap-timer connection) nil
          (fumos-connection-retry-timers connection) nil
          (fumos-connection-callback-timers connection) nil)
    (fumos-repl--set-state connection 'disconnected)))

(defun fumos-repl--kill-buffer-cleanup ()
  "Tear down the FUMOS transport owned by the buffer being killed."
  (when fumos-repl--connection
    (let ((connection fumos-repl--connection))
      (fumos-repl--cancel-game-reload-timer connection)
      (fumos-repl--teardown-transport connection "REPL buffer killed")
      (fumos-repl--unregister-if-current connection))))

(defun fumos-repl--mark-disconnected (connection message)
  "Mark CONNECTION disconnected and fail all pending work."
  (fumos-repl--teardown-transport connection message))

(defun fumos-repl--reject (connection message)
  "Reject CONNECTION with token-free MESSAGE through normal teardown."
  (if (memq (fumos-connection-state connection)
            '(connecting authenticating bootstrapping))
      (fumos-repl-close connection message)
    (fumos-repl--mark-disconnected connection message)))
(defun fumos-repl--retry-message-id (wire-message)
  "Return WIRE-MESSAGE's unique top-level :id without evaluating it."
  (when (stringp wire-message)
    (with-temp-buffer
      (insert wire-message)
      ;; Pinned fennel-mode gives strings, escapes, comments and all three
      ;; delimiter families the same structural rules as Fennel 1.6.1.
      (set-syntax-table fennel-mode-syntax-table)
      (setq-local parse-sexp-ignore-comments t)
      (goto-char (point-min))
      (condition-case nil
          (progn
            (forward-comment (point-max))
            (unless (eq (char-after) ?\{) (error "retry payload is not a map"))
            (let* ((map-start (point))
                   (map-end (scan-sexps map-start 1))
                   found id)
              ;; Reject a valid map followed by a second form.
              (save-excursion
                (goto-char map-end)
                (forward-comment (point-max))
                (unless (eobp) (error "retry payload has trailing forms")))
              (goto-char (1+ map-start))
              (while (< (point) (1- map-end))
                (forward-comment (point-max))
                (when (< (point) (1- map-end))
                  (let ((key-start (point)))
                    (forward-sexp 1)
                    (let ((key (buffer-substring-no-properties
                                key-start (point))))
                      (forward-comment (point-max))
                      (when (>= (point) (1- map-end))
                        (error "retry map has an unpaired key"))
                      (let ((value-start (point)))
                        ;; One structural step skips nested maps and strings;
                        ;; :id text inside either can never be mistaken for a key.
                        (forward-sexp 1)
                        (when (string= key ":id")
                          (when found (error "retry map has duplicate :id"))
                          (let ((value (buffer-substring-no-properties
                                        value-start (point))))
                            (unless (string-match-p "\\`[0-9]+\\'" value)
                              (error "retry :id is not an integer"))
                            (setq found t id (string-to-number value)))))))))
              (and found id)))
        (error nil)))))

(defun fumos-repl--schedule-retry (connection message-id wire-message callbacks)
  "Resend WIRE-MESSAGE while CALLBACKS still own MESSAGE-ID on CONNECTION."
  (let* ((process (fumos-connection-process connection))
         (generation (fumos-connection-generation connection))
         (repl-buffer (fumos-connection-repl-buffer connection))
         timer)
    (condition-case nil
        (progn
          (setq
           timer
           (run-with-timer
            0.1 nil
            (lambda ()
              (setf (fumos-connection-retry-timers connection)
                    (delq timer (fumos-connection-retry-timers connection)))
              (when (and (stringp wire-message)
                         (process-live-p process)
                         (fumos-repl--owns-transport-p
                          connection process generation)
                         (buffer-live-p repl-buffer))
                (with-current-buffer repl-buffer
                  (when (and
                         (hash-table-p fennel-proto-repl--message-callbacks)
                         (eq callbacks
                             (gethash message-id
                                      fennel-proto-repl--message-callbacks)))
                    ;; OP=nil preserves the exact retry payload and original ID.
                    (fennel-proto-repl-send-message
                     nil wire-message nil)))))))
          (unless (timerp timer)
            (error "FUMOS retry scheduler returned no timer"))
          (push timer (fumos-connection-retry-timers connection))
          timer)
      ((error quit)
       (fumos-repl--cancel-timer timer)
       (setf (fumos-connection-retry-timers connection)
             (delq timer (fumos-connection-retry-timers connection)))
       (fumos-repl--reject connection "Retry scheduling failed")
       nil))))

(defun fumos-repl--handle-protocol-op-advice (original message)
  "Bind upstream retry scheduling to the FUMOS transport that received it."
  (let* ((repl-buffer
          (and fennel-proto-repl--buffer
               (get-buffer fennel-proto-repl--buffer)))
         (connection
          (and (buffer-live-p repl-buffer)
               (buffer-local-value 'fumos-repl--connection repl-buffer)))
         (outer-id (plist-get message :id))
         (outer-callbacks
          (and connection
               (with-current-buffer repl-buffer
                 (and (hash-table-p fennel-proto-repl--message-callbacks)
                      (gethash outer-id
                               fennel-proto-repl--message-callbacks))))))
    (if (and connection (equal "retry" (plist-get message :op)))
        (when outer-callbacks
          (let* ((wire-message (plist-get message :message))
                 (message-id (fumos-repl--retry-message-id wire-message))
                 (callbacks
                  (and message-id
                       (with-current-buffer repl-buffer
                         (gethash message-id
                                  fennel-proto-repl--message-callbacks)))))
            (when callbacks
              (fumos-repl--schedule-retry
               connection message-id wire-message callbacks))))
      (funcall original message))))

(unless (advice-member-p #'fumos-repl--handle-protocol-op-advice
                         'fennel-proto-repl--handle-protocol-op)
  (advice-add 'fennel-proto-repl--handle-protocol-op :around
              #'fumos-repl--handle-protocol-op-advice))

(defun fumos-repl--macro-query-expression ()
  "Return the asynchronous macro-cache query for the reserved module."
  (format
   (concat "(let [fennel (require %S) "
           "listified (icollect [package macs (pairs fennel.macro-loaded)] "
           "(let [result [package]] "
           "(icollect [mac (pairs macs) :into result] mac)))] "
           "(when (next listified) listified))")
   fumos-repl-fennel-module-name))

(defun fumos-repl--parse-macro-cache (wire-value)
  "Parse WIRE-VALUE into a tagged macro cache, or return nil on failure."
  (when (stringp wire-value)
    (condition-case nil
        (let* ((read-result (read-from-string wire-value))
               (parsed (car read-result)))
          (unless (string-match-p
                   "\\`[[:space:]]*\\'"
                   (substring wire-value (cdr read-result)))
            (error "Macro response has trailing data"))
          (if (null parsed)
              (cons t nil)
            (unless (vectorp parsed)
              (error "Macro response outer value is not a vector"))
            (let ((cache
                   (mapcar
                    (lambda (entry)
                      (unless (and (vectorp entry)
                                   (> (length entry) 0)
                                   (seq-every-p #'stringp entry))
                        (error "Macro response entry has invalid shape"))
                      (mapcar #'identity entry))
                    parsed)))
              (cons t cache))))
      (error nil))))

(defun fumos-repl--refresh-linked-font-lock
    (connection process generation)
  "Refresh live source buffers still owned by CONNECTION GENERATION."
  (dolist (buffer (copy-sequence
                   (fumos-connection-linked-buffers connection)))
    (when (and (fumos-repl--owns-transport-p
                connection process generation)
               (buffer-live-p buffer))
      (with-current-buffer buffer
        (when (eq fumos-repl--source-owner connection)
          (condition-case nil
              (let ((inhibit-quit t)
                    (quit-flag nil))
                (fennel-proto-repl-refresh-dynamic-font-lock))
            (quit nil)
            (error nil)))))))

(defun fumos-repl--macro-refresh-current-p
    (connection process generation repl-buffer request-id)
  "Return non-nil while one macro refresh still owns its transport."
  (and (fumos-connection-macro-refresh-pending connection)
       (eql request-id (fumos-connection-macro-refresh-id connection))
       (eql generation
            (fumos-connection-macro-refresh-generation connection))
       (eq repl-buffer (fumos-connection-repl-buffer connection))
       (fumos-repl--owns-transport-p connection process generation)))

(defun fumos-repl--complete-macro-refresh
    (connection process generation repl-buffer request-id values)
  "Commit VALUES to CONNECTION's cache when every owner identity matches."
  (when (fumos-repl--macro-refresh-current-p
         connection process generation repl-buffer request-id)
    (let ((parsed (fumos-repl--parse-macro-cache (car values))))
      (setf (fumos-connection-macro-refresh-pending connection) nil
            (fumos-connection-macro-refresh-id connection) nil
            (fumos-connection-macro-refresh-generation connection) nil)
      (when parsed
        (setf (fumos-connection-macro-cache connection) (cdr parsed)
              (fumos-connection-macro-cache-valid connection) t)
        (fumos-repl--refresh-linked-font-lock
         connection process generation)))))

(defun fumos-repl--fail-macro-refresh
    (connection process generation repl-buffer request-id &rest _error)
  "Release one failed macro refresh without replacing the previous cache."
  (when (fumos-repl--macro-refresh-current-p
         connection process generation repl-buffer request-id)
    (setf (fumos-connection-macro-refresh-pending connection) nil
          (fumos-connection-macro-refresh-id connection) nil
          (fumos-connection-macro-refresh-generation connection) nil)))

(defun fumos-repl--refresh-macro-cache (connection)
  "Start one nonblocking, generation-owned macro refresh for CONNECTION."
  (let* ((process (fumos-connection-process connection))
         (generation (fumos-connection-generation connection))
         (repl-buffer (fumos-connection-repl-buffer connection)))
    (when (and (not (fumos-connection-macro-refresh-pending connection))
               (buffer-live-p repl-buffer)
               (fumos-repl--owns-transport-p
                connection process generation))
      (setf (fumos-connection-macro-refresh-pending connection) t
            (fumos-connection-macro-refresh-generation connection) generation)
      (condition-case nil
          (with-current-buffer repl-buffer
            (let (request-id)
              (setq
               request-id
               (fennel-proto-repl-send-message
                :eval (fumos-repl--macro-query-expression)
                (lambda (values)
                  (fumos-repl--complete-macro-refresh
                   connection process generation repl-buffer
                   request-id values))
                (lambda (&rest error-data)
                  (apply #'fumos-repl--fail-macro-refresh
                         connection process generation repl-buffer
                         request-id error-data))
                #'ignore))
              (unless (integerp request-id)
                (error "FUMOS macro refresh did not allocate a request"))
              (setf (fumos-connection-macro-refresh-id connection) request-id)
              request-id))
        ((error quit)
         (setf (fumos-connection-macro-refresh-pending connection) nil
               (fumos-connection-macro-refresh-id connection) nil
               (fumos-connection-macro-refresh-generation connection) nil)
         (fumos-repl--reject connection "Macro refresh setup failed")
         (error "FUMOS macro refresh setup failed"))))))

(defun fumos-repl--invalidate-macro-cache (connection)
  "Invalidate CONNECTION's macro cache and start a fresh owned query.
The previous cache remains available as a nonblocking display fallback.  Any
in-flight refresh identity is retired first, so its deferred callback cannot
make pre-invalidation values valid again within the same transport generation."
  (when (fumos-connection-p connection)
    (setf (fumos-connection-macro-cache-valid connection) nil
          (fumos-connection-macro-refresh-pending connection) nil
          (fumos-connection-macro-refresh-id connection) nil
          (fumos-connection-macro-refresh-generation connection) nil)
    (when (and (not (fumos-connection-closing connection))
               (memq (fumos-connection-state connection) '(ready busy)))
      (fumos-repl--refresh-macro-cache connection))))

(defun fumos-repl--obtain-macros (connection)
  "Return CONNECTION's cache immediately, refreshing it when invalid."
  (unless (or (fumos-connection-macro-cache-valid connection)
              (fumos-connection-macro-refresh-pending connection))
    (fumos-repl--refresh-macro-cache connection))
  (copy-tree (fumos-connection-macro-cache connection)))

(defun fumos-repl--obtain-macros-advice (original)
  "Use the reserved FUMOS module only for a linked FUMOS transport."
  (if-let ((connection (fumos-repl--upstream-connection)))
      (fumos-repl--obtain-macros connection)
    (funcall original)))

(unless (advice-member-p #'fumos-repl--obtain-macros-advice
                         'fennel-proto-repl--obtain-macros)
  (advice-add 'fennel-proto-repl--obtain-macros :around
              #'fumos-repl--obtain-macros-advice))

(defun fumos-repl--drain-connection-lost-terminals (connection)
  "Synchronously deliver every scheduled disconnect terminal once."
  (fumos-repl--cancel-timer-list
   (fumos-connection-terminal-timers connection))
  (dolist (delivery
           (copy-sequence
            (fumos-connection-terminal-deliveries connection)))
    (when (fumos-callback-delivery-p delivery)
      (fumos-repl--deliver-connection-lost
       delivery
       (fumos-callback-delivery-request-id delivery)
       (fumos-callback-delivery-callbacks delivery)
       (fumos-callback-delivery-terminal-timer delivery))))
  (setf (fumos-connection-terminal-timers connection) nil
        (fumos-connection-terminal-deliveries connection) nil))

(defun fumos-repl-close (connection &optional message)
  "Fully close CONNECTION, unregister it, and remove both owned buffers."
  (when (fumos-connection-p connection)
    (fumos-repl--cancel-game-reload-timer connection)
    (fumos-repl--teardown-transport connection
                                    (or message "Closed by Emacs"))
    (fumos-repl--unregister-if-current connection)
    (let ((repl-buffer (fumos-connection-repl-buffer connection)))
      (when (buffer-live-p repl-buffer)
        (with-current-buffer repl-buffer
          ;; Make the local kill hook a no-op for this already-cleaned connection.
          (setq fumos-repl--connection nil)
          ;; Free the deterministic public name before user kill hooks run.
          ;; A hook may synchronously attach a newer generation while this
          ;; retiring buffer is still on the kill stack.
          (condition-case nil
              (rename-buffer
               (generate-new-buffer-name
                (format " %s closing" (buffer-name repl-buffer))) t)
            ((error quit) nil))))
      ;; Teardown may have admitted connection-lost, and a prior unexpected
      ;; disconnect may already have a zero-delay terminal in flight.  Free the
      ;; deterministic name first so a callback can reenter attach safely, then
      ;; claim and deliver every such terminal before the old buffer is killed.
      (fumos-repl--drain-connection-lost-terminals connection)
      (when (buffer-live-p repl-buffer)
        (condition-case nil
            (with-current-buffer repl-buffer
              ;; User hooks may initiate a newer explicit attach.  The public
              ;; transition CAS ensures that newer reservation wins.
              (let ((kill-buffer-query-functions nil))
                (kill-buffer (current-buffer))))
          ((error quit)
           (fumos-repl--erase-and-kill-buffer repl-buffer)))))
    (setf (fumos-connection-repl-buffer connection) nil
          (fumos-connection-linked-buffers connection) nil
          (fumos-connection-state connection) 'disconnected)
    t))

(defun fumos-repl-current-connection ()
  "Return the current buffer's FUMOS connection."
  (or fumos-repl--connection
      (let ((root (fumos-project-root)))
        (and root (gethash root fumos-repl--connections)))))

(defun fumos-repl--send-control (connection frame)
  "Send token-free control FRAME over CONNECTION."
  (unless (process-live-p (fumos-connection-process connection))
    (user-error "FUMOS is disconnected"))
  (process-send-string (fumos-connection-process connection)
                       (concat frame "\n")))

(defun fumos-interrupt ()
  "Abort the current FUMOS request."
  (interactive)
  (fumos-repl--send-control
   (or (fumos-repl-current-connection)
       (user-error "No FUMOS connection"))
   "FUMOS/1 INTERRUPT"))

(defun fumos-cancel-active-request ()
  "Explicitly choose and cancel one active request by ID."
  (interactive)
  (let* ((connection (or (fumos-repl-current-connection)
                         (user-error "No FUMOS connection")))
         (ids (fumos-connection-active-request-ids connection))
         (id
          (pcase ids
            ('() (user-error "No active FUMOS request"))
            (`(,only) only)
            (_
             (string-to-number
              (completing-read
               "Cancel FUMOS request ID: "
               (mapcar #'number-to-string ids) nil t))))))
    (fumos-repl--send-control connection
                              (format "FUMOS/1 CANCEL %d" id))))

(defun fumos-disconnect ()
  "Detach from FUMOS without stopping Kristal."
  (interactive)
  (let ((connection (or (fumos-repl-current-connection)
                        (user-error "No FUMOS connection"))))
    ;; Explicit detach cancels user intent to auto-attach after game reload.
    (fumos-repl--cancel-game-reload-timer connection)
    (when (process-live-p (fumos-connection-process connection))
      (fumos-repl--send-control connection "FUMOS/1 DETACH"))
    (fumos-repl--mark-disconnected connection "Detached by user")))

(defun fumos-switch-to-repl ()
  "Display the current FUMOS REPL."
  (interactive)
  (let ((connection (or (fumos-repl-current-connection)
                        (user-error "No FUMOS connection"))))
    (pop-to-buffer (fumos-connection-repl-buffer connection))))

(defun fumos-clear-repl ()
  "Clear the current connection's REPL buffer."
  (interactive)
  (let ((connection (or (fumos-repl-current-connection)
                        (user-error "No FUMOS connection"))))
    (with-current-buffer (fumos-connection-repl-buffer connection)
      (fennel-proto-repl-clear-buffer))))

(defun fumos-repl--source-link-snapshot (connections)
  "Capture source state and linked lists for CONNECTIONS."
  (list
   :owner fumos-repl--source-owner
   :enabled fumos-repl--source-enabled-upstream-mode
   :previous-buffer fumos-repl--source-previous-upstream-buffer
   :previous-mode fumos-repl--source-previous-upstream-mode
   :previous-module-local-p fumos-repl--source-previous-module-local-p
   :previous-module-value fumos-repl--source-previous-module-value
   :module-local-p
   (local-variable-p 'fennel-proto-repl-fennel-module-name)
   :module-value fennel-proto-repl-fennel-module-name
   :target fennel-proto-repl--buffer
   :mode fennel-proto-repl-minor-mode
   :transition fumos-repl--source-link-transition
   :kill-hooks kill-buffer-hook
   :mode-hooks fennel-proto-repl-minor-mode-hook
   :completion-hooks completion-at-point-functions
   :xref-hooks xref-backend-functions
   :eldoc-hooks eldoc-documentation-functions
   :links (mapcar (lambda (value)
                    (cons value
                          (copy-sequence
                           (fumos-connection-linked-buffers value))))
                  (delete-dups (delq nil (copy-sequence connections))))))

(defun fumos-repl--restore-source-link-snapshot (snapshot)
  "Restore the exact source-link state captured in SNAPSHOT."
  (dolist (entry (plist-get snapshot :links))
    (setf (fumos-connection-linked-buffers (car entry)) (cdr entry)))
  (setq fumos-repl--source-owner (plist-get snapshot :owner)
        fumos-repl--source-enabled-upstream-mode
        (plist-get snapshot :enabled)
        fumos-repl--source-previous-upstream-buffer
        (plist-get snapshot :previous-buffer)
        fumos-repl--source-previous-upstream-mode
        (plist-get snapshot :previous-mode)
        fumos-repl--source-previous-module-local-p
        (plist-get snapshot :previous-module-local-p)
        fumos-repl--source-previous-module-value
        (plist-get snapshot :previous-module-value)
        fennel-proto-repl--buffer (plist-get snapshot :target)
        fennel-proto-repl-minor-mode (plist-get snapshot :mode)
        fumos-repl--source-link-transition
        (plist-get snapshot :transition)
        kill-buffer-hook (plist-get snapshot :kill-hooks)
        fennel-proto-repl-minor-mode-hook (plist-get snapshot :mode-hooks)
        completion-at-point-functions
        (plist-get snapshot :completion-hooks)
        xref-backend-functions (plist-get snapshot :xref-hooks)
        eldoc-documentation-functions (plist-get snapshot :eldoc-hooks))
  (fumos-repl--restore-source-module-name
   (plist-get snapshot :module-local-p)
   (plist-get snapshot :module-value)))

(defun fumos-repl--link-buffer-to-connection (connection buffer)
  "Transactionally relink BUFFER to CONNECTION."
  (with-current-buffer buffer
    (let* ((old-owner fumos-repl--source-owner)
           (previous-buffer
            (if old-owner
                fumos-repl--source-previous-upstream-buffer
              (and fennel-proto-repl--buffer
                   (get-buffer fennel-proto-repl--buffer))))
           (previous-mode
            (if old-owner
                fumos-repl--source-previous-upstream-mode
              (and fennel-proto-repl-minor-mode t)))
           (previous-module-local-p
            (if old-owner
                fumos-repl--source-previous-module-local-p
              (local-variable-p
               'fennel-proto-repl-fennel-module-name)))
           (previous-module-value
            (if old-owner
                fumos-repl--source-previous-module-value
              fennel-proto-repl-fennel-module-name))
           (repl-buffer (fumos-connection-repl-buffer connection))
           (ticket (list 'fumos-source-link connection buffer))
           (snapshot
            (fumos-repl--source-link-snapshot
             (list old-owner connection)))
           failure superseded)
      (setq fumos-repl--source-link-transition ticket
            fennel-proto-repl--buffer repl-buffer)
      (setq-local fennel-proto-repl-fennel-module-name
                  fumos-repl-fennel-module-name)
      (condition-case caught
          (let ((fumos-repl--internal-link-target repl-buffer))
            (unless fennel-proto-repl-minor-mode
              (fennel-proto-repl-minor-mode 1))
            (unless (eq ticket fumos-repl--source-link-transition)
              (setq superseded t))
            (unless superseded
              (fennel-proto-repl--link-buffer repl-buffer)
              (unless (eq ticket fumos-repl--source-link-transition)
                (setq superseded t))))
        ((error quit) (setq failure caught)))
      (cond
       (failure
        (when (eq ticket fumos-repl--source-link-transition)
          (fumos-repl--restore-source-link-snapshot snapshot))
        (signal (car failure) (cdr failure)))
       (superseded
        ;; A nested link or teardown is newer intent and owns final state.
        fumos-repl--source-owner)
       (t
        (when old-owner
          (setf (fumos-connection-linked-buffers old-owner)
                (delq buffer
                      (fumos-connection-linked-buffers old-owner))))
        (setq fumos-repl--source-owner connection
              fumos-repl--source-enabled-upstream-mode (not previous-mode)
              fumos-repl--source-previous-upstream-buffer previous-buffer
              fumos-repl--source-previous-upstream-mode previous-mode
              fumos-repl--source-previous-module-local-p
              previous-module-local-p
              fumos-repl--source-previous-module-value previous-module-value
              fumos-repl--source-link-transition nil)
        (cl-pushnew buffer
                    (fumos-connection-linked-buffers connection) :test #'eq)
        (add-hook 'kill-buffer-hook #'fumos-repl--source-kill-cleanup nil t)
        (add-hook 'fennel-proto-repl-minor-mode-hook
                  #'fumos-repl--source-upstream-mode-change nil t)
        (when (fboundp 'fumos-repl--xref-backend)
          (add-hook 'xref-backend-functions
                    'fumos-repl--xref-backend nil t))
        connection)))))

(defun fumos-repl-link-current-buffer ()
  "Link the current ready FUMOS source buffer and upstream hooks."
  (let* ((buffer (current-buffer))
         (root (and fumos-mode (fumos-project-root)))
         (connection (and root (gethash root fumos-repl--connections)))
         (repl-buffer (and connection
                           (fumos-connection-repl-buffer connection))))
    (when (and connection
               (memq (fumos-connection-state connection) '(ready busy))
               (buffer-live-p repl-buffer))
      (fumos-repl--link-buffer-to-connection connection buffer))))

(defun fumos-repl--link-project-buffers (connection)
  "Link every active source buffer belonging to CONNECTION's project."
  (let ((root (fumos-instance-project-root
               (fumos-connection-instance connection))))
    (dolist (buffer (buffer-list))
      (unless (fumos-repl--bootstrap-commit-owned-p connection)
        (error "FUMOS bootstrap reservation changed while linking"))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (and fumos-mode buffer-file-name
                     (equal root (fumos-project-root
                                  (file-name-directory buffer-file-name))))
            ;; Bootstrap owns this direct link before publishing ready.
            (fumos-repl--link-buffer-to-connection connection buffer)
            (unless (fumos-repl--bootstrap-commit-owned-p connection)
              (error "FUMOS bootstrap reservation changed after source link"))))))))

(defun fumos-repl--bootstrap-commit-owned-p (connection)
  "Return non-nil while CONNECTION exclusively owns its bootstrap commit."
  (let* ((instance (fumos-connection-instance connection))
         (root (and instance (fumos-instance-project-root instance)))
         (process (fumos-connection-process connection))
         (generation (fumos-connection-generation connection)))
    (and root
         (eq connection (gethash root fumos-repl--connections))
         (process-live-p process)
         (fumos-repl--owns-transport-p connection process generation))))

(defun fumos-repl--rollback-provisional-links (connection previous)
  "Release CONNECTION links created after the PREVIOUS identity snapshot."
  (dolist (buffer (copy-sequence
                   (fumos-connection-linked-buffers connection)))
    (when (and (not (memq buffer previous)) (buffer-live-p buffer))
      (with-current-buffer buffer
        (when (eq fumos-repl--source-owner connection)
          (fumos-repl--release-source-owner)))))
  (setf (fumos-connection-linked-buffers connection)
        (seq-filter #'buffer-live-p previous)))

(defun fumos-repl--finish-bootstrap
    (connection values &optional signal-setup-failure)
  "Validate CONNECTION's reservation and finish bootstrap with VALUES.
When SIGNAL-SETUP-FAILURE is non-nil, normalize a synchronous setup
error or quit to `fumos-repl-connection-error' after complete cleanup."
  (pcase values
    (`(ok "0.6.4" ,(and fennel-version (pred stringp))
          ,(and lua-version (pred stringp)))
     (if (not (fumos-repl--bootstrap-commit-owned-p connection))
         (fumos-repl-close connection "Stale FUMOS bootstrap")
       (let ((previous-links
              (copy-sequence (fumos-connection-linked-buffers connection)))
             finished failure)
         (fumos-repl--cancel-bootstrap-deadline connection)
         (unwind-protect
             (condition-case caught
                 (progn
                   (fumos-repl--start-upstream-ui connection values)
                   (unless (fumos-repl--bootstrap-commit-owned-p connection)
                     (error "FUMOS bootstrap reservation changed after UI start"))
                   (fumos-repl--link-project-buffers connection)
                   (unless (fumos-repl--bootstrap-commit-owned-p connection)
                     (error "FUMOS bootstrap reservation changed after linking"))
                   (fumos-repl--set-state connection 'ready)
                   (fumos-repl--refresh-macro-cache connection)
                   (unless (fumos-repl--bootstrap-commit-owned-p connection)
                     (error "FUMOS bootstrap reservation changed during macro refresh"))
                   (message "FUMOS attached: proto 0.6.4, Fennel %s, %s"
                            fennel-version lua-version)
                   (setq finished t))
               ((error quit) (setq failure caught)))
           (unless finished
             ;; Roll links back independently: close may itself be reentrant or
             ;; identity-gated and must not be the only rollback mechanism.
             (fumos-repl--rollback-provisional-links
              connection previous-links)
             (fumos-repl-close connection "Proto bootstrap failed")))
         (when (and failure (or signal-setup-failure
                                fumos-repl--signal-bootstrap-failure))
           (signal 'fumos-repl-connection-error nil)))))
    (_
     (fumos-repl--reject connection "Proto bootstrap failed"))))

;; Replace Task 7's standalone bootstrap only after reservation lifecycle exists.
(defun fumos-repl--bootstrap (connection process generation)
  "Send and identity-gate the proto upgrade for CONNECTION."
  (fumos-repl--set-state connection 'bootstrapping)
  (setf
   (fumos-connection-bootstrap-timer connection)
   (run-at-time
    fumos-bootstrap-timeout nil
    (lambda ()
      (when (and (fumos-repl--owns-transport-p
                  connection process generation)
                 (eq 'bootstrapping (fumos-connection-state connection)))
        (fumos-repl--reject connection "Proto bootstrap timed out")))))
  (with-current-buffer (fumos-connection-repl-buffer connection)
    (fennel-proto-repl-send-message
     nil
     (fumos-repl--upgrade-code)
     (lambda (values)
       (when (fumos-repl--owns-transport-p connection process generation)
         (let ((fumos-repl--signal-bootstrap-failure t))
           (fumos-repl--finish-bootstrap connection values)))))))

(defun fumos-repl--named-connection (instance)
  "Return the connection that verifiably owns INSTANCE's named buffer."
  (let* ((buffer (get-buffer (fumos-repl--buffer-name instance)))
         (owner
          (and (buffer-live-p buffer)
               (buffer-local-value 'fumos-repl--connection buffer))))
    (and (fumos-connection-p owner)
         (eq buffer (fumos-connection-repl-buffer owner))
         owner)))

(defun fumos-repl--foreign-named-buffer (instance)
  "Return an existing deterministic buffer not owned by a FUMOS connection."
  (let ((buffer (get-buffer (fumos-repl--buffer-name instance))))
    (and (buffer-live-p buffer)
         (not (fumos-repl--named-connection instance))
         buffer)))

(defun fumos-repl-connect-instance (instance)
  "Reserve INSTANCE's project, cleanly replacing pending or ready transport."
  (let* ((foreign (fumos-repl--foreign-named-buffer instance))
         (root (fumos-instance-project-root instance))
         (old (or (gethash root fumos-repl--connections)
                  (fumos-repl--named-connection instance)))
         (connection (fumos-repl--new-connection instance))
         (transition (list 'fumos-attach connection)))
    ;; Reject before closing OLD or reserving ROOT; FOREIGN is user-owned.
    (when foreign
      (signal 'fumos-repl-connection-error (list :buffer-name-conflict)))
    ;; Publish a separate transition before closing OLD.  A kill hook may
    ;; recursively attach and replace this token while OLD is being destroyed.
    (puthash root transition fumos-repl--attach-transitions)
    (condition-case _err
        (let (opened)
          (unwind-protect
              (progn
                (when old (fumos-repl-close old))
                (unless (eq transition
                            (gethash root fumos-repl--attach-transitions))
                  (signal 'fumos-repl-connection-error nil))
                (when (gethash root fumos-repl--connections)
                  (signal 'fumos-repl-connection-error nil))
                ;; This compare-and-set is the sole reservation publication.
                (puthash root connection fumos-repl--connections)
                (fumos-repl--open-instance connection)
                (with-current-buffer (fumos-connection-repl-buffer connection)
                  (add-hook 'kill-buffer-hook
                            #'fumos-repl--kill-buffer-cleanup nil t))
                (setq opened t)
                connection)
            (when (eq transition
                      (gethash root fumos-repl--attach-transitions))
              (remhash root fumos-repl--attach-transitions))
            (unless opened
              (fumos-repl-close connection
                                "FUMOS connection setup failed"))))
      (error (signal 'fumos-repl-connection-error nil)))))

(defun fumos-connect ()
  "Connect to the only current-project instance, or ask when needed."
  (interactive)
  (let* ((root (or (fumos-project-root)
                   (user-error "Current buffer is not in a FUMOS project")))
         (instance (fumos-select-instance (fumos-discover-instances root))))
    (fumos-repl-connect-instance instance)))

(defun fumos-attach ()
  "Explicitly select and attach a current-project instance."
  (interactive)
  (let* ((root (or (fumos-project-root)
                   (user-error "Current buffer is not in a FUMOS project")))
         (instances (fumos-discover-instances root))
         (instance
          (if instances
              (let ((selection (fumos-select-instance instances))) selection)
            (user-error "No FUMOS instance is running; start Kristal with Mod.info.dev=true"))))
    (fumos-repl-connect-instance instance)))

(defun fumos-connect-or-switch ()
  "Switch to a ready REPL, or connect the current project."
  (interactive)
  (let ((connection (fumos-repl-current-connection)))
    (if (and connection (memq (fumos-connection-state connection)
                              '(ready busy)))
        (fumos-switch-to-repl)
      (fumos-connect))))

(defun fumos-reconnect ()
  "Reconnect to the same project and PID as the previous connection."
  (interactive)
  (let* ((old (or (fumos-repl-current-connection)
                  (user-error "No previous FUMOS connection")))
         (old-instance (fumos-connection-instance old))
         (pid (fumos-instance-pid old-instance))
         (matches
          (seq-filter
           (lambda (instance) (= pid (fumos-instance-pid instance)))
           (fumos-discover-instances (fumos-instance-project-root old-instance))))
         (instance
          (pcase matches
            (`(,only) only)
            ('() (user-error "FUMOS PID %d is not available; use fumos-attach to choose another instance" pid))
            (_ (user-error "Multiple descriptors claim FUMOS PID %d" pid)))))
    (fumos-repl-connect-instance instance)))

(provide 'fumos-repl)
;;; fumos-repl.el ends here
