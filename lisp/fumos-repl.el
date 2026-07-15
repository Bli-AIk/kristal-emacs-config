;;; fumos-repl.el --- Attach fennel-proto-repl to Kristal -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'cl-lib)
(require 'comint)
(require 'subr-x)
(require 'fennel-proto-repl)
(require 'fumos-instance)
(require 'fumos-project)

(declare-function fumos-interrupt "fumos-repl")

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
  linked-buffers)

(defvar fumos-repl--connections (make-hash-table :test #'equal))
(defvar-local fumos-repl--connection nil)
(defvar fumos-repl--next-generation 0)

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

(defun fumos-repl-close (connection &optional reason)
  "Close CONNECTION completely, recording optional token-free REASON."
  (when (fumos-connection-p connection)
    ;; Every callback checks this barrier; set it before touching a resource.
    (setf (fumos-connection-closing connection) t)
    (when reason
      (setf (fumos-connection-last-error connection) reason))
    (fumos-repl--set-state connection 'disconnected)
    (fumos-repl--cancel-timer
     (fumos-connection-handshake-timer connection))
    (fumos-repl--cancel-timer
     (fumos-connection-bootstrap-timer connection))
    (fumos-repl--cancel-timer
     (fumos-connection-game-reload-timer connection))
    (fumos-repl--cancel-timer-list
     (fumos-connection-retry-timers connection))
    (fumos-repl--cancel-timer-list
     (fumos-connection-callback-timers connection))
    (fumos-repl--cancel-timer-list
     (fumos-connection-terminal-timers connection))
    (fumos-repl--cancel-timer-list
     (fumos-connection-terminal-deliveries connection))
    (fumos-repl--clear-callbacks connection)
    ;; Upstream creates a dummy comint process in the visible buffer.
    (fumos-repl--delete-process
     (fumos-connection-ui-process connection))
    (fumos-repl--delete-process
     (fumos-connection-process connection) 'neutralize)
    (fumos-repl--erase-and-kill-buffer
     (fumos-connection-process-buffer connection))
    (fumos-repl--erase-and-kill-buffer
     (fumos-connection-repl-buffer connection))
    (setf (fumos-connection-process connection) nil
          (fumos-connection-process-buffer connection) nil
          (fumos-connection-repl-buffer connection) nil
          (fumos-connection-ui-process connection) nil
          (fumos-connection-handshake-buffer connection) ""
          (fumos-connection-handshake-timer connection) nil
          (fumos-connection-bootstrap-timer connection) nil
          (fumos-connection-active-request-ids connection) nil
          (fumos-connection-pending-game-reload connection) nil
          (fumos-connection-retry-timers connection) nil
          (fumos-connection-callback-timers connection) nil
          (fumos-connection-terminal-timers connection) nil
          (fumos-connection-terminal-deliveries connection) nil
          (fumos-connection-game-reload-timer connection) nil
          (fumos-connection-linked-buffers connection) nil)
    t))

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

(defun fumos-repl--reject (connection message)
  "Reject CONNECTION with token-free MESSAGE."
  (fumos-repl-close connection message))

(defun fumos-repl--validate-ack (connection line)
  "Validate LINE against CONNECTION and return non-nil on success."
  (and (string-match fumos-repl--ack-regexp line)
       (= (string-to-number (match-string 1 line))
          (fumos-instance-pid (fumos-connection-instance connection)))))

(defun fumos-repl--protocol-filter (connection process generation chunk)
  "Forward CHUNK from PROCESS to the upstream proto filter."
  (when (fumos-repl--owns-transport-p connection process generation)
    (fumos-repl--guard-setup
     connection
     (lambda ()
       (fennel-proto-repl--process-filter process chunk)))))

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

(defun fumos-repl--finish-bootstrap (connection process generation values)
  "Finish CONNECTION bootstrap using VALUES from PROCESS GENERATION."
  (when (fumos-repl--owns-transport-p connection process generation)
    (fumos-repl--cancel-bootstrap-deadline connection)
    (pcase values
      (`(ok "0.6.4" ,fennel-version ,lua-version)
       (if (and (stringp fennel-version) (stringp lua-version))
           (fumos-repl--guard-setup
            connection
            (lambda ()
              (fumos-repl--start-upstream-ui connection values)
              (fumos-repl--set-state connection 'ready)
              (message "FUMOS attached: proto 0.6.4, Fennel %s, %s"
                       fennel-version lua-version)))
         (fumos-repl--reject connection "Proto bootstrap failed")))
      (_
       (fumos-repl--reject connection "Proto bootstrap failed")))))

(defun fumos-repl--bootstrap (connection process generation)
  "Send the pinned proto upgrade expression for CONNECTION."
  (fumos-repl--set-state connection 'bootstrapping)
  (setf
   (fumos-connection-bootstrap-timer connection)
   (run-at-time
    fumos-bootstrap-timeout nil
    (lambda ()
      (when (and
             (fumos-repl--owns-transport-p
              connection process generation)
             (eq 'bootstrapping (fumos-connection-state connection)))
        (fumos-repl--reject connection "Proto bootstrap timed out")))))
  (with-current-buffer (fumos-connection-repl-buffer connection)
    (fennel-proto-repl-send-message
     nil
     (fumos-repl--upgrade-code)
     (lambda (values)
       (fumos-repl--finish-bootstrap
        connection process generation values)))))

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
  (fumos-repl--guard-setup
   connection
   (lambda ()
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
         connection)))))

;; Task 8 replaces this provisional wrapper with project reservations.
(defun fumos-repl-connect-instance (instance)
  "Open and authenticate a TCP connection to INSTANCE."
  (let (connection)
    (condition-case nil
        (progn
          (setq connection (fumos-repl--new-connection instance))
          (fumos-repl--open-instance connection))
      ((error quit)
       (fumos-repl--setup-failed connection)))))

(provide 'fumos-repl)
;;; fumos-repl.el ends here
