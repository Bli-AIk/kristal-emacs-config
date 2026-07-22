;;; fumos-repl.el --- Attach fennel-proto-repl to Kristal -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'cl-lib)
(require 'comint)
(require 'subr-x)
(require 'fennel-proto-repl)
(require 'fumos-instance)
(require 'fumos-project)

(declare-function fumos-completion-at-point "fumos-eval")
(declare-function fumos-eldoc-function "fumos-eval")
(declare-function fumos-repl--xref-backend "fumos-eval")
(declare-function fumos-eval--invalidate-source-tooling "fumos-eval")
(declare-function fumos-eval--release-tooling-markers "fumos-eval")

(defcustom fumos-handshake-timeout 2.0
  "Seconds allowed for the FUMOS authentication handshake."
  :type 'number
  :group 'fennel-proto-repl)

(defcustom fumos-bootstrap-timeout 2.0
  "Seconds allowed for the proto initialization after authentication."
  :type 'number
  :group 'fennel-proto-repl)

(defcustom fumos-launch-timeout 30.0
  "Seconds allowed for an Emacs-started Kristal to publish FUMOS."
  :type 'number
  :group 'fennel-proto-repl)

(defcustom fumos-reconnect-timeout 30.0
  "Seconds allowed for an unexpectedly restarted game to publish FUMOS."
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
  :keymap fumos-repl-mode-map
  (if fumos-repl-mode
      (fumos-repl--install-game-editing-state)
    (fumos-repl--restore-game-editing-state)))

(cl-defstruct fumos-connection
  instance process process-buffer repl-buffer ui-process state handshake-buffer
  handshake-timer bootstrap-timer active-request-ids pending-game-reload
  last-error generation retry-timers callback-timers callback-deliveries closing
  terminal-timers terminal-deliveries game-reload-timer game-reload-generation
  linked-buffers macro-cache macro-cache-valid macro-refresh-pending
  macro-refresh-id macro-refresh-generation macro-refresh-epoch error-buffer
  error-epoch help-epoch help-pending xref-epoch xref-pending xref-cache eldoc-epoch
  eldoc-pending completion-epoch completion-pending completion-cache
  reconnect-suppressed session-reset-notice attach-operation)

(cl-defstruct fumos-launch-operation
  root process buffer start-identity timer deadline candidate)

(cl-defstruct fumos-reconnect-operation
  connection root pid start-identity token-digest timer deadline show candidate)

(defvar fumos-repl--connections (make-hash-table :test #'equal))
(defvar fumos-repl--source-operations
  (make-hash-table :test #'eq :weakness 'key)
  "Authoritative latest source operation keyed by source buffer.")
(defvar fumos-repl--attach-transitions (make-hash-table :test #'equal)
  "Latest public attach transition token for each canonical project root.")
(defvar fumos-repl--launch-operations (make-hash-table :test #'equal)
  "Current Emacs-owned Kristal launch for each canonical project root.")
(defvar fumos-repl--reconnect-operations (make-hash-table :test #'equal)
  "Current unexpected game-restart wait for each canonical project root.")
(defvar fumos-repl--game-reload-operations (make-hash-table :test #'equal)
  "Current editor-requested game reload for each canonical project root.")
(defvar-local fumos-repl--connection nil)
(defvar fumos-repl--next-generation 0)
(defvar fumos-repl--signal-bootstrap-failure nil
  "Non-nil while a synchronous bootstrap callback must signal setup failure.")
(defvar fumos-repl--preserve-attach-operation nil
  "Attach operation that an internal connection replacement must preserve.")

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
  (when timer
    (condition-case nil
        (cancel-timer timer)
      (quit nil)
      (error nil))))

(defun fumos-repl--cancel-timer-list (timers)
  "Cancel every timer object in TIMERS."
  (dolist (timer timers)
    (fumos-repl--cancel-timer timer)))

(defun fumos-repl--canonical-local-root (root)
  "Return ROOT as a canonical local directory, or nil."
  (when (and (stringp root)
             (file-name-absolute-p root)
             (not (file-remote-p root)))
    (condition-case nil
        (let ((canonical (file-name-as-directory (file-truename root))))
          (and (file-directory-p canonical) canonical))
      (error nil))))

(defun fumos-repl--linux-stat-start-ticks (contents)
  "Return Linux process start ticks parsed from proc stat CONTENTS."
  (let ((text (string-trim-right contents "[\r\n]+")))
    (when (string-match "\\`[0-9]+ (.*) [[:alpha:]] \\(.*\\)\\'" text)
      (let* ((fields (split-string (match-string 1 text) "[[:space:]]+" t))
             (start (nth 18 fields)))
        (when (and start (string-match-p "\\`[0-9]+\\'" start))
          (string-to-number start))))))

(defun fumos-repl--linux-process-start-identity (pid)
  "Return PID's stable Linux kernel start identity, or nil."
  (when (and (eq system-type 'gnu/linux) (integerp pid) (> pid 0))
    (condition-case nil
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally (format "/proc/%d/stat" pid))
          (when-let* ((ticks
                       (fumos-repl--linux-stat-start-ticks (buffer-string))))
            (list 'linux-start-ticks ticks)))
      (error nil))))

(defun fumos-repl--process-start-identity (pid)
  "Return PID's normalized current-user process start identity, or nil."
  (condition-case nil
      (let* ((attributes (process-attributes pid))
             (euid (and attributes (alist-get 'euid attributes)))
             (start (and attributes (alist-get 'start attributes)))
             (comm (and attributes (alist-get 'comm attributes))))
        (when (and (integerp euid) (= euid (user-uid)) start)
          (or (and (stringp comm)
                   (fumos-repl--linux-process-start-identity pid))
              (list 'process-attributes-start (time-convert start 'list)))))
    (error nil)))

(defun fumos-repl--connection-root (connection)
  "Return CONNECTION's canonical project root, or nil."
  (when-let* ((instance (and (fumos-connection-p connection)
                             (fumos-connection-instance connection))))
    (fumos-repl--canonical-local-root
     (fumos-instance-project-root instance))))

(defun fumos-repl--token-digest (instance)
  "Return INSTANCE's bearer-token digest, or nil."
  (let ((token (and (fumos-instance-p instance)
                    (fumos-instance-token instance))))
    (when (and (stringp token) (= 64 (length token)))
      (secure-hash 'sha256 token))))

(defun fumos-repl--launch-current-p (operation)
  "Return non-nil while OPERATION owns its project launch intent."
  (and (fumos-launch-operation-p operation)
       (eq operation
           (gethash (fumos-launch-operation-root operation)
                    fumos-repl--launch-operations))))

(defun fumos-repl--attach-operation-candidate (operation)
  "Return OPERATION's provisional connection, or nil."
  (cond
   ((fumos-launch-operation-p operation)
    (fumos-launch-operation-candidate operation))
   ((fumos-reconnect-operation-p operation)
    (fumos-reconnect-operation-candidate operation))
   ((and (fboundp 'fumos-game-reload-operation-p)
         (fumos-game-reload-operation-p operation))
    (fumos-game-reload-operation-candidate operation))))

(defun fumos-repl--set-attach-operation-candidate (operation connection)
  "Set OPERATION's provisional CONNECTION and its reverse ownership link."
  (let ((previous (fumos-repl--attach-operation-candidate operation)))
    (when (and (fumos-connection-p previous)
               (eq operation (fumos-connection-attach-operation previous)))
      (setf (fumos-connection-attach-operation previous) nil))
    (cond
     ((fumos-launch-operation-p operation)
      (setf (fumos-launch-operation-candidate operation) connection))
     ((fumos-reconnect-operation-p operation)
      (setf (fumos-reconnect-operation-candidate operation) connection))
     ((and (fboundp 'fumos-game-reload-operation-p)
           (fumos-game-reload-operation-p operation))
      (fumos-eval--set-game-reload-operation-candidate
       operation connection)))
    (when (fumos-connection-p connection)
      (setf (fumos-connection-attach-operation connection) operation))
    connection))

(defun fumos-repl--attach-operation-current-p (operation)
  "Return non-nil while OPERATION still owns its asynchronous intent."
  (cond
   ((fumos-launch-operation-p operation)
    (fumos-repl--launch-current-p operation))
   ((fumos-reconnect-operation-p operation)
    (fumos-repl--reconnect-current-p operation))
   ((and (fboundp 'fumos-game-reload-operation-p)
         (fumos-game-reload-operation-p operation)
         (fboundp 'fumos-eval--game-operation-current-p))
    (fumos-eval--game-operation-current-p operation))))

(defun fumos-repl--attach-candidate-status (operation)
  "Return OPERATION's candidate status: nil, pending, ready, or failed."
  (when-let* ((candidate (fumos-repl--attach-operation-candidate operation)))
    (let ((state (and (fumos-connection-p candidate)
                      (fumos-connection-state candidate))))
      (cond
       ((and (not (fumos-connection-closing candidate))
             (memq state '(ready busy)))
        'ready)
       ((and (not (fumos-connection-closing candidate))
             (memq state '(connecting authenticating bootstrapping)))
        'pending)
       (t 'failed)))))

(defun fumos-repl--release-attach-operation-candidate (operation)
  "Release and return OPERATION's provisional connection.

Provisional connections that never reached `ready' are owned resources, not
history.  Close them here so an authentication failure or transport drop
between polls cannot leave a dead connection and its buffers registered."
  (let ((candidate (fumos-repl--attach-operation-candidate operation)))
    (fumos-repl--set-attach-operation-candidate operation nil)
    (when (and (fumos-connection-p candidate)
               (not (memq (fumos-connection-state candidate) '(ready busy))))
      (fumos-repl--close-provisional-connection
       candidate "FUMOS provisional attach canceled"))
    candidate))

(defun fumos-repl--close-provisional-connection (connection message)
  "Fully close provisional CONNECTION with token-free MESSAGE.

This remains necessary after transport teardown has set `closing': that first
pass deliberately preserves REPL history, while an abandoned attach candidate
must also release its registry entry and buffers.  `fumos-repl-close' is a
repeatable resource sweep, so completing that second phase is safe."
  (when (fumos-connection-p connection)
    (setf (fumos-connection-attach-operation connection) nil)
    (fumos-repl-close connection message)))

(defun fumos-repl--cancel-attach-operation (operation)
  "Cancel OPERATION through its owning subsystem."
  (cond
   ((fumos-launch-operation-p operation)
    (fumos-repl--cancel-launch-operation operation))
   ((fumos-reconnect-operation-p operation)
    (fumos-repl--cancel-reconnect-operation operation))
   ((and (fboundp 'fumos-game-reload-operation-p)
         (fumos-game-reload-operation-p operation)
         (fboundp 'fumos-eval--cancel-game-reload-operation))
    (fumos-eval--cancel-game-reload-operation operation))))

(defun fumos-repl--cancel-launch-operation (operation &optional terminate)
  "Cancel OPERATION, also stopping its owned process when TERMINATE is non-nil."
  (when (fumos-repl--launch-current-p operation)
    (remhash (fumos-launch-operation-root operation)
             fumos-repl--launch-operations)
    (fumos-repl--release-attach-operation-candidate operation)
    (let ((timer (fumos-launch-operation-timer operation))
          (process (fumos-launch-operation-process operation)))
      (setf (fumos-launch-operation-timer operation) nil)
      (fumos-repl--cancel-timer timer)
      (when terminate
        (fumos-repl--delete-process process 'neutralize)))
    t))

(defun fumos-repl--cancel-launch-for-root (root &optional terminate)
  "Cancel ROOT's pending launch, optionally stopping its owned process."
  (when-let* ((operation (gethash root fumos-repl--launch-operations)))
    (fumos-repl--cancel-launch-operation operation terminate)))

(defun fumos-repl--reconnect-current-p (operation)
  "Return non-nil while OPERATION owns its project reconnect intent."
  (and (fumos-reconnect-operation-p operation)
       (let ((root (fumos-reconnect-operation-root operation))
             (connection (fumos-reconnect-operation-connection operation)))
         (and (eq operation (gethash root fumos-repl--reconnect-operations))
              (not (fumos-connection-reconnect-suppressed connection))))))

(defun fumos-repl--cancel-reconnect-operation (operation)
  "Cancel OPERATION only while it owns its project reconnect intent."
  (when (and (fumos-reconnect-operation-p operation)
             (eq operation
                 (gethash (fumos-reconnect-operation-root operation)
                          fumos-repl--reconnect-operations)))
    (remhash (fumos-reconnect-operation-root operation)
             fumos-repl--reconnect-operations)
    (fumos-repl--release-attach-operation-candidate operation)
    (let ((history (fumos-reconnect-operation-connection operation)))
      (when (and (fumos-connection-p history)
                 (eq operation
                     (fumos-connection-attach-operation history)))
        (setf (fumos-connection-attach-operation history) nil)))
    (let ((timer (fumos-reconnect-operation-timer operation)))
      (setf (fumos-reconnect-operation-timer operation) nil)
      (fumos-repl--cancel-timer timer))
    t))

(defun fumos-repl--cancel-reconnect-for-root (root)
  "Cancel the reconnect intent currently registered for ROOT."
  (when-let* ((operation (gethash root fumos-repl--reconnect-operations)))
    (fumos-repl--cancel-reconnect-operation operation)))

(defun fumos-repl--cancel-game-reload-for-root (root &optional preserve)
  "Cancel ROOT's editor reload intent unless it is PRESERVE."
  (when-let* ((operation (gethash root fumos-repl--game-reload-operations)))
    (unless (eq operation preserve)
      (if (fboundp 'fumos-eval--cancel-game-reload-operation)
          (fumos-eval--cancel-game-reload-operation operation)
        (remhash root fumos-repl--game-reload-operations)))))

(defun fumos-repl--suppress-reconnect (connection)
  "Prevent CONNECTION from initiating or completing automatic reconnect."
  (when (fumos-connection-p connection)
    (setf (fumos-connection-reconnect-suppressed connection) t)
    ;; The project may have been renamed or removed while a restart was
    ;; pending.  Match the captured owner instead of canonicalizing it again.
    (maphash
     (lambda (_root operation)
       (when (and
              (not (eq operation fumos-repl--preserve-attach-operation))
              (or (eq connection
                      (fumos-reconnect-operation-connection operation))
                  (eq connection
                      (fumos-reconnect-operation-candidate operation))))
         (fumos-repl--cancel-reconnect-operation operation)))
     (copy-hash-table fumos-repl--reconnect-operations))))

(defun fumos-repl--shutdown ()
  "Cancel FUMOS-owned asynchronous work before Emacs exits."
  (maphash
   (lambda (_root operation)
     (fumos-repl--cancel-launch-operation operation 'terminate))
   (copy-hash-table fumos-repl--launch-operations))
  (maphash
   (lambda (_root operation)
     (fumos-repl--cancel-reconnect-operation operation))
   (copy-hash-table fumos-repl--reconnect-operations))
  (maphash
   (lambda (_root operation)
     (when (fboundp 'fumos-eval--cancel-game-reload-operation)
       (fumos-eval--cancel-game-reload-operation operation)))
   (copy-hash-table fumos-repl--game-reload-operations)))

(add-hook 'kill-emacs-hook #'fumos-repl--shutdown)

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
   :game-reload-generation 0 :macro-refresh-epoch 0 :error-epoch 0
   :help-epoch 0 :xref-epoch 0 :xref-cache (make-hash-table :test #'equal)
   :eldoc-epoch 0 :completion-epoch 0
   :completion-pending (make-hash-table :test #'equal)
   :completion-cache (make-hash-table :test #'equal)))

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
                (fumos-repl--transport-closed connection))))))
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

(defun fumos-repl--deliver-read
    (delivery message prior-timers timer)
  "Deliver deferred read MESSAGE after DELIVERY's PRIOR-TIMERS settle."
  (let* ((connection (fumos-callback-delivery-connection delivery))
         (request-id (fumos-callback-delivery-request-id delivery))
         (callback-identity (fumos-callback-delivery-callbacks delivery))
         (repl-buffer (fumos-callback-delivery-repl-buffer delivery)))
    (setf (fumos-callback-delivery-timers delivery)
          (delq timer (fumos-callback-delivery-timers delivery))
          (fumos-connection-callback-timers connection)
          (delq timer (fumos-connection-callback-timers connection)))
    (when (fumos-repl--callback-delivery-current-p
           delivery request-id callback-identity t)
      (let ((waiting
             (seq-filter
              (lambda (prior)
                (memq prior (fumos-callback-delivery-timers delivery)))
              prior-timers)))
        (if waiting
            (fumos-repl--defer-read delivery message waiting)
          (condition-case nil
              (with-current-buffer repl-buffer
                (fennel-proto-repl--read-handler message))
            ((error quit)
             ;; The game is waiting for an input response.  A failed prompt
             ;; cannot be recovered without terminating this transport.
             (fumos-repl--reject connection "Read handler failed"))))))
    (fumos-repl--maybe-forget-callback-delivery delivery)))

(defun fumos-repl--defer-read (delivery message &optional prior-timers)
  "Schedule request-owned read MESSAGE after any PRIOR-TIMERS."
  (let* ((connection (fumos-callback-delivery-connection delivery))
         (request-id (fumos-callback-delivery-request-id delivery))
         (callback-identity (fumos-callback-delivery-callbacks delivery))
         (waiting
          (or prior-timers
              (copy-sequence (fumos-callback-delivery-timers delivery))))
         timer)
    (when (fumos-repl--callback-delivery-current-p
           delivery request-id callback-identity t)
      (condition-case nil
          (progn
            (setq timer
                  (run-at-time
                   0 nil
                   (lambda ()
                     (fumos-repl--deliver-read
                      delivery (copy-tree message) waiting timer))))
            (unless (timerp timer)
              (error "FUMOS read scheduler returned no timer"))
            (push timer (fumos-callback-delivery-timers delivery))
            (push timer (fumos-connection-callback-timers connection))
            timer)
        ((error quit)
         (fumos-repl--cancel-timer timer)
         (setf (fumos-callback-delivery-timers delivery)
               (delq timer (fumos-callback-delivery-timers delivery))
               (fumos-connection-callback-timers connection)
               (delq timer (fumos-connection-callback-timers connection)))
         (fumos-repl--reject connection "Read scheduling failed")
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

(defun fumos-repl--rollback-callback-assignment
    (connection repl-buffer delivery request-id)
  "Invalidate and forget a partially assigned request callback."
  (let ((inhibit-quit t)
        (quit-flag nil)
        (timers (fumos-callback-delivery-timers delivery))
        (terminal-timer (fumos-callback-delivery-terminal-timer delivery)))
    (setf (fumos-callback-delivery-valid delivery) nil
          (fumos-callback-delivery-timers delivery) nil
          (fumos-callback-delivery-terminal-timer delivery) nil
          (fumos-callback-delivery-connection-lost-state delivery) 'delivered
          (fumos-connection-callback-timers connection)
          (seq-difference (fumos-connection-callback-timers connection)
                          timers #'eq)
          (fumos-connection-terminal-timers connection)
          (delq terminal-timer
                (fumos-connection-terminal-timers connection))
          (fumos-connection-terminal-deliveries connection)
          (delq delivery
                (fumos-connection-terminal-deliveries connection)))
    (fumos-repl--cancel-timer-list timers)
    (fumos-repl--cancel-timer terminal-timer)
    (when (integerp request-id)
      (condition-case nil
          (when (buffer-live-p repl-buffer)
            (with-current-buffer repl-buffer
              (when (hash-table-p fennel-proto-repl--message-callbacks)
                (remhash request-id fennel-proto-repl--message-callbacks))))
        ((error quit) nil))
      (condition-case nil
          (when (hash-table-p
                 (fumos-connection-callback-deliveries connection))
            (remhash request-id
                     (fumos-connection-callback-deliveries connection)))
        ((error quit) nil)))))

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
             id committed)
        (unwind-protect
            (progn
              (setq
               id
               (funcall
                original
                (fumos-repl--safe-callback delivery values-callback 'values)
                (fumos-repl--safe-callback delivery resolved-error 'error)
                (fumos-repl--safe-callback delivery resolved-print 'print)))
              (when id
                (with-current-buffer repl-buffer
                  (let ((callback-identity
                         (gethash id fennel-proto-repl--message-callbacks)))
                    (setf (fumos-callback-delivery-request-id delivery) id
                          (fumos-callback-delivery-callbacks delivery)
                          callback-identity)
                    (puthash
                     id delivery
                     (fumos-repl--callback-delivery-table connection)))))
              (setq committed t)
              id)
          (unless committed
            (fumos-repl--rollback-callback-assignment
             connection repl-buffer delivery id)))))))

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
  (let ((timer (fumos-connection-game-reload-timer connection))
        (operation (fumos-connection-attach-operation connection)))
    (when (and operation
               (fboundp 'fumos-game-reload-operation-p)
               (fumos-game-reload-operation-p operation))
      (let ((root (fumos-game-reload-operation-root operation)))
        (when (eq operation
                  (gethash root fumos-repl--game-reload-operations))
          (remhash root fumos-repl--game-reload-operations)))
      (fumos-repl--release-attach-operation-candidate operation)
      (when (eq operation (fumos-connection-attach-operation connection))
        (setf (fumos-connection-attach-operation connection) nil)))
    ;; Invalidate ownership before cancellation can reenter or signal.
    (setf (fumos-connection-game-reload-timer connection) nil
          (fumos-connection-game-reload-generation connection)
          (1+ (or (fumos-connection-game-reload-generation connection) 0))
          (fumos-connection-pending-game-reload connection) nil)
    (fumos-repl--cancel-timer timer)))

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
    (fumos-repl--delete-process ui-process 'neutralize)))

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

(defvar-local fumos-repl--source-previous-editing-state nil
  "Editing hooks and font-lock state captured before FUMOS ownership.")

(defvar-local fumos-repl--source-link-transition nil
  "Identity token for the newest source ownership operation.")

(defvar-local fumos-repl--source-killing nil
  "Non-nil after FUMOS source cleanup begins inside `kill-buffer-hook'.")

(defvar fumos-repl--internal-link-target nil
  "REPL target being installed by a FUMOS source link transaction.")

(defvar fumos-repl--internal-source-mode-change nil
  "Non-nil while FUMOS itself restores proto minor-mode state.")

(defvar-local fumos-repl--game-editing-owned nil
  "Non-nil when the current game REPL owns its editing configuration.")

(defvar-local fumos-repl--game-editing-state nil
  "Ordinary proto editing state replaced by `fumos-repl-mode'.")

(defun fumos-repl--capture-local-value (symbol)
  "Capture SYMBOL's exact current localness and value."
  (list :local-p (local-variable-p symbol)
        :value (and (boundp symbol) (symbol-value symbol))))

(defun fumos-repl--restore-local-value (symbol snapshot)
  "Restore SYMBOL from local-value SNAPSHOT."
  (if (plist-get snapshot :local-p)
      (set (make-local-variable symbol) (plist-get snapshot :value))
    (kill-local-variable symbol)))

(defun fumos-repl--capture-editing-state ()
  "Capture the exact buffer-local tooling state replaced by FUMOS."
  (list
   :font-lock
   (fumos-repl--capture-local-value
    'fennel-proto-repl-font-lock-dynamically)
   :completion
   (fumos-repl--capture-local-value 'completion-at-point-functions)
   :xref (fumos-repl--capture-local-value 'xref-backend-functions)
   :eldoc
   (fumos-repl--capture-local-value 'eldoc-documentation-functions)))

(defun fumos-repl--restore-editing-state (snapshot)
  "Restore the current buffer's tooling state from SNAPSHOT."
  (when snapshot
    (fumos-repl--restore-local-value
     'fennel-proto-repl-font-lock-dynamically
     (plist-get snapshot :font-lock))
    (fumos-repl--restore-local-value
     'completion-at-point-functions (plist-get snapshot :completion))
    (fumos-repl--restore-local-value
     'xref-backend-functions (plist-get snapshot :xref))
    (fumos-repl--restore-local-value
     'eldoc-documentation-functions (plist-get snapshot :eldoc))))

(defun fumos-repl--install-upstream-editing-state ()
  "Install the editing hooks owned by an active ordinary proto mode."
  (add-hook 'completion-at-point-functions
            #'fennel-proto-repl-complete nil t)
  (add-hook 'xref-backend-functions
            #'fennel-proto-repl--xref-backend nil t)
  (fennel-proto-repl--setup-eldoc))

(defun fumos-repl--without-global-font-lock (value)
  "Return VALUE with dynamic `global' font lock removed in order."
  (if (listp value)
      (delq 'global (copy-sequence value))
    value))

(defun fumos-repl--install-owned-editing-state ()
  "Install nonblocking editing hooks in the current FUMOS buffer."
  (setq-local
   fennel-proto-repl-fennel-module-name fumos-repl-fennel-module-name
   fennel-proto-repl-font-lock-dynamically
   (fumos-repl--without-global-font-lock
    fennel-proto-repl-font-lock-dynamically))
  (remove-hook 'completion-at-point-functions
               #'fennel-proto-repl-complete t)
  (remove-hook 'completion-at-point-functions
               #'fumos-completion-at-point t)
  (add-hook 'completion-at-point-functions
            #'fumos-completion-at-point nil t)
  (remove-hook 'xref-backend-functions
               #'fennel-proto-repl--xref-backend t)
  (remove-hook 'xref-backend-functions #'fumos-repl--xref-backend t)
  (add-hook 'xref-backend-functions #'fumos-repl--xref-backend nil t)
  (remove-hook 'eldoc-documentation-functions
               #'fennel-proto-repl-eldoc-fn-docstring t)
  (remove-hook 'eldoc-documentation-functions
               #'fennel-proto-repl-eldoc-var-docstring t)
  (remove-hook 'eldoc-documentation-functions #'fumos-eldoc-function t)
  (add-hook 'eldoc-documentation-functions #'fumos-eldoc-function nil t))

(defun fumos-repl--install-game-editing-state ()
  "Replace synchronous upstream tooling in a FUMOS game REPL."
  (unless fumos-repl--game-editing-owned
    (setq fumos-repl--game-editing-owned t
          fumos-repl--game-editing-state
          (list
           :module
           (fumos-repl--capture-local-value
            'fennel-proto-repl-fennel-module-name)
           :editing (fumos-repl--capture-editing-state))))
  (fumos-repl--install-owned-editing-state))

(defun fumos-repl--restore-game-editing-state ()
  "Restore ordinary proto tooling after leaving `fumos-repl-mode'."
  (when fumos-repl--game-editing-owned
    (let ((snapshot fumos-repl--game-editing-state))
      (setq fumos-repl--game-editing-owned nil
            fumos-repl--game-editing-state nil)
      (fumos-repl--restore-editing-state (plist-get snapshot :editing))
      (fumos-repl--restore-local-value
       'fennel-proto-repl-fennel-module-name
       (plist-get snapshot :module)))))

(defun fumos-repl--live-previous-upstream-buffer ()
  "Return the saved ordinary target while it is still live."
  (and (buffer-live-p fumos-repl--source-previous-upstream-buffer)
       fumos-repl--source-previous-upstream-buffer))

(defun fumos-repl--restore-source-module-name (local-p value)
  "Restore the current source's proto module name locality and VALUE."
  (if local-p
      (setq-local fennel-proto-repl-fennel-module-name value)
    (kill-local-variable 'fennel-proto-repl-fennel-module-name)))

(defun fumos-repl--latest-source-operation (source)
  "Return the authoritative latest ownership operation for SOURCE."
  (let ((operation (gethash source fumos-repl--source-operations)))
    (pcase (car-safe operation)
      ('fumos-source-link
       (and (fumos-connection-p (nth 1 operation))
            (eq source (nth 2 operation))
            operation))
      ('fumos-source-release
       (and (plist-member (cdr operation) :target)
            operation)))))

(defun fumos-repl--replay-source-release-operation
    (source operation enabled-upstream-mode previous-buffer previous-mode
            previous-module-local-p previous-module-value
            previous-editing-state)
  "Reassert completed release OPERATION for SOURCE without stale FUMOS state."
  (let ((state (cdr operation))
        (stale-owner fumos-repl--source-owner))
    (cl-labels
        ((operation-current-p
          ()
          (and (eq operation
                   (gethash source fumos-repl--source-operations))
               (not fumos-repl--source-killing)))
         (restart
          ()
          (fumos-repl--replay-latest-source-link
           source enabled-upstream-mode previous-buffer previous-mode
           previous-module-local-p previous-module-value
           previous-editing-state))
         (set-and-check
          (symbol value)
          (set symbol value)
          (unless (operation-current-p)
            (throw 'fumos-source-release-replay-superseded (restart))))
         (restore-local-and-check
          (symbol snapshot)
          (fumos-repl--restore-local-value symbol snapshot)
          (unless (operation-current-p)
            (throw 'fumos-source-release-replay-superseded (restart))))
         (mutate-and-check
          (function)
          (funcall function)
          (unless (operation-current-p)
            (throw 'fumos-source-release-replay-superseded (restart)))))
      (catch 'fumos-source-release-replay-superseded
        (when (operation-current-p)
          (set-and-check 'fumos-repl--source-link-transition operation)
          (set-and-check 'fumos-repl--source-owner nil)
          (dolist (symbol
                   '(fumos-repl--source-enabled-upstream-mode
                     fumos-repl--source-previous-upstream-buffer
                     fumos-repl--source-previous-upstream-mode
                     fumos-repl--source-previous-module-local-p
                     fumos-repl--source-previous-module-value
                     fumos-repl--source-previous-editing-state))
            (set-and-check symbol nil))
          (set-and-check 'fennel-proto-repl--buffer
                         (plist-get state :target))
          (set-and-check 'fennel-proto-repl-minor-mode
                         (plist-get state :mode))
          (if (plist-get state :module-local-p)
              (set (make-local-variable
                    'fennel-proto-repl-fennel-module-name)
                   (plist-get state :module-value))
            (kill-local-variable 'fennel-proto-repl-fennel-module-name))
          (unless (operation-current-p)
            (throw 'fumos-source-release-replay-superseded (restart)))
          (let ((editing (plist-get state :editing-state)))
            (dolist (entry
                     '((fennel-proto-repl-font-lock-dynamically . :font-lock)
                       (completion-at-point-functions . :completion)
                       (xref-backend-functions . :xref)
                       (eldoc-documentation-functions . :eldoc)))
              (restore-local-and-check
               (car entry) (plist-get editing (cdr entry)))))
          (when (and (plist-get state :install-upstream)
                     fennel-proto-repl-minor-mode)
            (mutate-and-check #'fumos-repl--install-upstream-editing-state))
          (when (fumos-connection-p stale-owner)
            (setf (fumos-connection-linked-buffers stale-owner)
                  (delq source
                        (fumos-connection-linked-buffers stale-owner)))
            (unless (operation-current-p)
              (throw 'fumos-source-release-replay-superseded (restart))))
          (setf (plist-get (cdr operation) :completed) t)
          nil)))))

(defun fumos-repl--replay-latest-source-link
    (source enabled-upstream-mode previous-buffer previous-mode
            previous-module-local-p previous-module-value
            previous-editing-state)
  "Reassert SOURCE's authoritative latest link with its inherited snapshot."
  (when (buffer-live-p source)
    (with-current-buffer source
      (let ((operation (fumos-repl--latest-source-operation source)))
        (cond
         ((eq (car-safe operation) 'fumos-source-release)
          (fumos-repl--replay-source-release-operation
           source operation enabled-upstream-mode previous-buffer previous-mode
           previous-module-local-p previous-module-value
           previous-editing-state))
         ((eq (car-safe operation) 'fumos-source-link)
          (let* ((operation-state (cdddr operation))
                 (enabled-upstream-mode
                  (plist-get operation-state :enabled))
                 (previous-buffer
                  (plist-get operation-state :previous-buffer))
                 (previous-mode
                  (plist-get operation-state :previous-mode))
                 (previous-module-local-p
                  (plist-get operation-state :previous-module-local-p))
                 (previous-module-value
                  (plist-get operation-state :previous-module-value))
                 (previous-editing-state
                  (plist-get operation-state :previous-editing-state))
                 (new-owner (nth 1 operation))
                 (repl-buffer (fumos-connection-repl-buffer new-owner))
                 (stale-owner fumos-repl--source-owner)
                 (previous-owners
                  (delete-dups
                   (delq nil
                         (append
                          (copy-sequence
                           (plist-get operation-state :previous-owners))
                          (list stale-owner))))))
            (cl-labels
                ((operation-current-p
                  ()
                  (and (eq operation
                           (gethash source fumos-repl--source-operations))
                       (buffer-live-p repl-buffer)
                       (not (fumos-connection-closing new-owner))
                       (not fumos-repl--source-killing)))
                 (restart
                  ()
                  (fumos-repl--replay-latest-source-link
                   source enabled-upstream-mode previous-buffer previous-mode
                   previous-module-local-p previous-module-value
                   previous-editing-state))
                 (set-and-check
                  (symbol value)
                  (set symbol value)
                  (unless (operation-current-p)
                    (throw 'fumos-source-link-replay-superseded (restart))))
                 (set-local-and-check
                  (symbol value)
                  (set (make-local-variable symbol) value)
                  (unless (operation-current-p)
                    (throw 'fumos-source-link-replay-superseded (restart))))
                 (mutate-and-check
                  (function)
                  (funcall function)
                  (unless (operation-current-p)
                    (throw 'fumos-source-link-replay-superseded (restart)))))
              (catch 'fumos-source-link-replay-superseded
                (when (operation-current-p)
                  ;; A variable watcher runs before its stale outer assignment.
                  ;; Rebuild directly from the committed operation.  Re-entering
                  ;; upstream's link path here would add a new failure window
                  ;; after the newer operation already won.
                  (set-and-check 'fumos-repl--source-link-transition operation)
                  (set-and-check 'fumos-repl--source-owner new-owner)
                  (set-and-check 'fumos-repl--source-enabled-upstream-mode
                                 enabled-upstream-mode)
                  (set-and-check 'fumos-repl--source-previous-upstream-buffer
                                 previous-buffer)
                  (set-and-check 'fumos-repl--source-previous-upstream-mode
                                 previous-mode)
                  (set-and-check 'fumos-repl--source-previous-module-local-p
                                 previous-module-local-p)
                  (set-and-check 'fumos-repl--source-previous-module-value
                                 previous-module-value)
                  (set-and-check 'fumos-repl--source-previous-editing-state
                                 previous-editing-state)
                  (set-and-check 'fennel-proto-repl--buffer repl-buffer)
                  (set-local-and-check
                   'fennel-proto-repl-fennel-module-name
                   fumos-repl-fennel-module-name)
                  (set-local-and-check
                   'fennel-proto-repl-font-lock-dynamically
                   (fumos-repl--without-global-font-lock
                    fennel-proto-repl-font-lock-dynamically))
                  (set-and-check 'fennel-proto-repl-minor-mode t)
                  (dolist (entry
                           `((completion-at-point-functions
                              ,#'fennel-proto-repl-complete remove)
                             (completion-at-point-functions
                              ,#'fumos-completion-at-point remove)
                             (completion-at-point-functions
                              ,#'fumos-completion-at-point add)
                             (xref-backend-functions
                              ,#'fennel-proto-repl--xref-backend remove)
                             (xref-backend-functions
                              ,#'fumos-repl--xref-backend remove)
                             (xref-backend-functions
                              ,#'fumos-repl--xref-backend add)
                             (eldoc-documentation-functions
                              ,#'fennel-proto-repl-eldoc-fn-docstring remove)
                             (eldoc-documentation-functions
                              ,#'fennel-proto-repl-eldoc-var-docstring remove)
                             (eldoc-documentation-functions
                              ,#'fumos-eldoc-function remove)
                             (eldoc-documentation-functions
                              ,#'fumos-eldoc-function add)))
                    (let ((hook (nth 0 entry))
                          (function (nth 1 entry))
                          (operation (nth 2 entry)))
                      (mutate-and-check
                       (lambda ()
                         (if (eq operation 'add)
                             (add-hook hook function nil t)
                           (remove-hook hook function t))))))
                  (dolist (previous-owner previous-owners)
                    (unless (eq previous-owner new-owner)
                      (setf (fumos-connection-linked-buffers previous-owner)
                            (delq
                             source
                             (fumos-connection-linked-buffers previous-owner))))
                    (unless (operation-current-p)
                      (throw 'fumos-source-link-replay-superseded (restart))))
                  (cl-pushnew source
                              (fumos-connection-linked-buffers new-owner)
                              :test #'eq)
                  (unless (operation-current-p)
                    (throw 'fumos-source-link-replay-superseded (restart)))
                  (condition-case nil
                      (fennel-proto-repl-refresh-dynamic-font-lock)
                    ((error quit) nil))
                  (unless (operation-current-p) (restart))
                  new-owner))))))))))

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
         (previous-module-value fumos-repl--source-previous-module-value)
         (enabled-upstream-mode fumos-repl--source-enabled-upstream-mode)
         (previous-editing-state
          fumos-repl--source-previous-editing-state))
    (when connection
      (let* ((previous-operation
              (gethash source fumos-repl--source-operations))
             (ticket
             (list 'fumos-source-release
                   :target (if preserve-upstream
                               fennel-proto-repl--buffer
                             previous-buffer)
                   :mode (if preserve-upstream
                             fennel-proto-repl-minor-mode
                           previous-mode)
                   :module-local-p previous-module-local-p
                   :module-value previous-module-value
                   :editing-state previous-editing-state
                   :install-upstream
                   (and preserve-upstream enabled-upstream-mode)
                   :completed nil))
             completed)
        (cl-labels
            ((release-current-p
              ()
              (and (eq ticket
                       (gethash source fumos-repl--source-operations))
                   (eq connection fumos-repl--source-owner)))
             (repair-new-owner
              ()
              (fumos-repl--replay-latest-source-link
               source enabled-upstream-mode previous-buffer previous-mode
               previous-module-local-p previous-module-value
               previous-editing-state))
             (ensure-current
              ()
              (unless (release-current-p)
                ;; A stale assignment can finish after the newer link's
                ;; variable watcher.  Re-run that newer transaction once to
                ;; reassert its complete module/target/tooling state.
                (repair-new-owner)
                (throw 'fumos-source-release-superseded connection)))
             (restore-editing-entry
              (symbol key)
              (fumos-repl--restore-local-value
               symbol (plist-get previous-editing-state key))
              (ensure-current))
             (recover-nonlocal-release
              ()
              (condition-case nil
                  (progn
                    (when (eq ticket
                              (gethash source fumos-repl--source-operations))
                      (repair-new-owner))
                    (when (eq connection fumos-repl--source-owner)
                      (if previous-operation
                          (puthash source previous-operation
                                   fumos-repl--source-operations)
                        (remhash source fumos-repl--source-operations))
                      (when previous-operation
                        (fumos-repl--replay-latest-source-link
                         source enabled-upstream-mode previous-buffer
                         previous-mode previous-module-local-p
                         previous-module-value previous-editing-state))
                      (when (eq connection fumos-repl--source-owner)
                        (cl-pushnew
                         source
                         (fumos-connection-linked-buffers connection)
                         :test #'eq))))
                (quit
                 (when (eq connection fumos-repl--source-owner)
                   (cl-pushnew
                    source (fumos-connection-linked-buffers connection)
                    :test #'eq)))
                (error
                 (when (eq connection fumos-repl--source-owner)
                   (cl-pushnew
                    source (fumos-connection-linked-buffers connection)
                    :test #'eq))))))
          (unwind-protect
              (prog1
                  (catch 'fumos-source-release-superseded
            ;; Keep the old owner and its original snapshot visible until all
            ;; restoration steps finish.  A nested A->B link can then inherit
            ;; that exact snapshot instead of capturing half-restored state.
            (puthash source ticket fumos-repl--source-operations)
            (setq fumos-repl--source-link-transition ticket)
            (ensure-current)
            (when (fboundp 'fumos-eval--invalidate-source-tooling)
              (condition-case nil
                  (fumos-eval--invalidate-source-tooling connection source)
                (quit nil)
                (error nil))
              (ensure-current))
            (setf (fumos-connection-linked-buffers connection)
                  (delq source (fumos-connection-linked-buffers connection)))
            (fumos-repl--restore-source-module-name
             previous-module-local-p previous-module-value)
            (ensure-current)
            (when (and (not preserve-upstream)
                       (eq fennel-proto-repl--buffer owned-repl))
              ;; Restore both halves of the snapshot.  The internal target
              ;; binding keeps the upstream link advice inside this release.
              (setq fennel-proto-repl--buffer previous-buffer)
              (ensure-current)
              (let ((fennel-proto-repl-font-lock-dynamically nil)
                    (fumos-repl--internal-source-mode-change t))
                (condition-case nil
                    (if previous-mode
                        (progn
                          (unless fennel-proto-repl-minor-mode
                            (let ((fumos-repl--internal-link-target
                                   previous-buffer))
                              (fennel-proto-repl-minor-mode 1))
                            (ensure-current))
                          (setq fennel-proto-repl--buffer previous-buffer)
                          (ensure-current)
                          (when previous-buffer
                            (let ((fumos-repl--internal-link-target
                                   previous-buffer))
                              (fennel-proto-repl--link-buffer previous-buffer))
                            (ensure-current)))
                      (when fennel-proto-repl-minor-mode
                        (fennel-proto-repl-minor-mode -1)
                        (ensure-current))
                      (setq fennel-proto-repl--buffer previous-buffer)
                      (ensure-current))
                  ((error quit)
                   (when (release-current-p)
                     (setq fennel-proto-repl--buffer previous-buffer)))))
              (ensure-current))
            (when previous-editing-state
              (restore-editing-entry
               'fennel-proto-repl-font-lock-dynamically :font-lock)
              (restore-editing-entry
               'completion-at-point-functions :completion)
              (restore-editing-entry
               'xref-backend-functions :xref)
              (restore-editing-entry
               'eldoc-documentation-functions :eldoc))
            ;; An ordinary relink can supersede FUMOS while the proto mode that
            ;; FUMOS enabled remains active.  Only that case needs upstream's
            ;; default hooks; a pre-existing custom snapshot stays exact.
            (when (and preserve-upstream enabled-upstream-mode
                       fennel-proto-repl-minor-mode)
              (fumos-repl--install-upstream-editing-state)
              (ensure-current))
            ;; Variable watchers can run before each assignment takes effect.
            ;; Keep the ticket until every observable field is cleared, and
            ;; repair a newer owner immediately if an old write lands last.
            (setq fumos-repl--source-owner nil)
            (unless (eq ticket
                        (gethash source fumos-repl--source-operations))
              (repair-new-owner)
              (throw 'fumos-source-release-superseded connection))
            (dolist (symbol
                     '(fumos-repl--source-enabled-upstream-mode
                       fumos-repl--source-previous-upstream-buffer
                       fumos-repl--source-previous-upstream-mode
                       fumos-repl--source-previous-module-local-p
                       fumos-repl--source-previous-module-value
                       fumos-repl--source-previous-editing-state))
              (set symbol nil)
              (unless (eq ticket
                          (gethash source fumos-repl--source-operations))
                (repair-new-owner)
                (throw 'fumos-source-release-superseded connection)))
            (when (fumos-connection-p fumos-repl--source-owner)
              (repair-new-owner)
              (throw 'fumos-source-release-superseded connection))
            ;; Publish the complete ordinary state without changing ticket
            ;; identity.  Any older writer which resumes after this release can
            ;; replay the ordinary operation just as precisely as a newer link.
            (when (and (not preserve-upstream)
                       previous-mode
                       (buffer-live-p previous-buffer)
                       fennel-proto-repl-minor-mode
                       (eq previous-buffer fennel-proto-repl--buffer)
                       (not fumos-repl--source-killing))
              (fumos-repl--refresh-ordinary-font-lock previous-buffer))
            (when (eq ticket
                      (gethash source fumos-repl--source-operations))
              (setf (plist-get (cdr ticket) :completed) t))
                    connection)
                (setq completed t))
            (unless completed
              (recover-nonlocal-release))))))))

(defun fumos-repl--source-upstream-mode-change ()
  "Drop FUMOS ownership when the user disables proto minor mode."
  (when (and fumos-repl--source-owner
             (not fennel-proto-repl-minor-mode))
    ;; The explicit disable is newer user intent than the saved mode state.
    ;; Restore only the previous target and retain the current disabled mode.
    (setq fennel-proto-repl--buffer
          (fumos-repl--live-previous-upstream-buffer))
    (fumos-repl--release-source-owner t)))

(defun fumos-repl--source-upstream-mode-advice (original &optional arg)
  "Run ORIGINAL proto mode command, then enforce FUMOS source ownership."
  (unwind-protect
      (funcall original arg)
    (unless fumos-repl--internal-source-mode-change
      (condition-case nil
          (fumos-repl--source-upstream-mode-change)
        (quit nil)
        (error nil)))))

(unless (advice-member-p #'fumos-repl--source-upstream-mode-advice
                         'fennel-proto-repl-minor-mode)
  (advice-add 'fennel-proto-repl-minor-mode :around
              #'fumos-repl--source-upstream-mode-advice))

(defun fumos-repl--refresh-ordinary-font-lock (target)
  "Refresh TARGET tooling only while the current source remains ordinary."
  (let ((source (current-buffer))
        (obtain-globals
         (symbol-function 'fennel-proto-repl--obtain-globals))
        (obtain-macros
         (symbol-function 'fennel-proto-repl--obtain-macros)))
    (cl-labels
        ((ordinary-link-current-p
          ()
          (and (buffer-live-p source)
               (buffer-live-p target)
               (with-current-buffer source
                 (and (not fumos-repl--source-owner)
                      (eq target fennel-proto-repl--buffer)
                      fennel-proto-repl-minor-mode))))
         (guarded-query
          (function args)
          (if (not (eq (current-buffer) source))
              (apply function args)
            (unless (ordinary-link-current-p)
              (error "Ordinary proto refresh was superseded"))
            (prog1 (apply function args)
              ;; A synchronous upstream query may dispatch an attach callback.
              ;; Abort before its result can mutate a newly owned source.
              (unless (ordinary-link-current-p)
                (error "Ordinary proto refresh was superseded"))))))
      (when (ordinary-link-current-p)
        (let ((fennel-proto-repl--reloading-buffer nil))
          (cl-letf
              (((symbol-function 'fennel-proto-repl--obtain-globals)
                (lambda (&rest args)
                  (guarded-query obtain-globals args)))
               ((symbol-function 'fennel-proto-repl--obtain-macros)
                (lambda (&rest args)
                  (guarded-query obtain-macros args))))
            (fennel-proto-repl-refresh-dynamic-font-lock)))))))

(defun fumos-repl--cancel-pending-source-link
    (source operation ordinary-target)
  "Publish ORDINARY-TARGET when it supersedes pending link OPERATION."
  (when (eq operation (gethash source fumos-repl--source-operations))
    (let* ((state (cdddr operation))
           (enabled (plist-get state :enabled))
           (previous-buffer (plist-get state :previous-buffer))
           (previous-mode (plist-get state :previous-mode))
           (previous-module-local-p
            (plist-get state :previous-module-local-p))
           (previous-module-value
            (plist-get state :previous-module-value))
           (previous-editing-state
            (plist-get state :previous-editing-state))
           (release
            (list 'fumos-source-release
                  :target ordinary-target
                  :mode fennel-proto-repl-minor-mode
                  :module-local-p previous-module-local-p
                  :module-value previous-module-value
                  :editing-state previous-editing-state
                  :install-upstream enabled
                  :completed nil)))
      (puthash source release fumos-repl--source-operations)
      (setq fumos-repl--source-link-transition release)
      (fumos-repl--replay-latest-source-link
       source enabled previous-buffer previous-mode
       previous-module-local-p previous-module-value
       previous-editing-state))))

(defun fumos-repl--link-buffer-advice (original &optional repl-buffer)
  "Let an ordinary upstream relink supersede a stale FUMOS source owner."
  (let* ((source (current-buffer))
         (source-operation
          (gethash source fumos-repl--source-operations))
         (pending-owner
          (and (eq (car-safe source-operation) 'fumos-source-link)
               (fumos-connection-p (nth 1 source-operation))
               (nth 1 source-operation)))
         (owner fumos-repl--source-owner)
         (effective-owner (or owner pending-owner))
         (owned-repl
          (and effective-owner
               (fumos-connection-repl-buffer effective-owner)))
         (target (and repl-buffer (get-buffer repl-buffer)))
         (suppress-transition-refresh
          (and effective-owner
               (or (null target)
                   (and (not (eq target owned-repl))
                        (not (eq target
                                 fumos-repl--internal-link-target))))))
         completed ordinary-target result)
    (unwind-protect
        ;; Upstream changes the target before refreshing font lock.  During an
        ;; owner-to-ordinary transition that would make retained macro options
        ;; fall through to its synchronous transport while FUMOS still owns the
        ;; source transaction.  The restored ordinary configuration applies to
        ;; every later explicit refresh; suppress only this transitional one.
        (prog1
            (setq result
                  (if suppress-transition-refresh
                      (let ((fennel-proto-repl-font-lock-dynamically nil))
                        (funcall original repl-buffer))
                    (funcall original repl-buffer)))
          (setq completed t))
      (when (and owner
                 (eq owner fumos-repl--source-owner)
                 (not (eq fennel-proto-repl--buffer
                          (fumos-connection-repl-buffer owner)))
                 (not (eq fennel-proto-repl--buffer
                          fumos-repl--internal-link-target)))
        ;; Upstream already installed the ordinary target.  Release only the
        ;; old FUMOS bookkeeping and keep that target and minor mode intact.
        (setq ordinary-target fennel-proto-repl--buffer)
        (fumos-repl--release-source-owner t))
      (when (and (not owner)
                 pending-owner
                 (eq source-operation
                     (gethash source fumos-repl--source-operations))
                 (not (eq fennel-proto-repl--buffer owned-repl))
                 (not (eq fennel-proto-repl--buffer
                          fumos-repl--internal-link-target)))
        (setq ordinary-target fennel-proto-repl--buffer)
        (fumos-repl--cancel-pending-source-link
         source source-operation ordinary-target)))
    (let ((new-owner fumos-repl--source-owner))
      (when (and (fumos-connection-p new-owner)
                 (not (eq new-owner owner))
                 (not (eq fennel-proto-repl--buffer
                          (fumos-connection-repl-buffer new-owner))))
        ;; The upstream target assignment can run a watcher which completes a
        ;; newer A->B link before the old assignment lands.  Reassert B after
        ;; the upstream call returns instead of leaving owner and target split.
        (fumos-repl--replay-latest-source-link
         (current-buffer)
         fumos-repl--source-enabled-upstream-mode
         fumos-repl--source-previous-upstream-buffer
         fumos-repl--source-previous-upstream-mode
         fumos-repl--source-previous-module-local-p
         fumos-repl--source-previous-module-value
         fumos-repl--source-previous-editing-state)))
    (when (and completed ordinary-target
               (not fumos-repl--source-owner)
               (eq ordinary-target fennel-proto-repl--buffer)
               fennel-proto-repl-minor-mode)
      (fumos-repl--refresh-ordinary-font-lock ordinary-target))
    result))

(unless (advice-member-p #'fumos-repl--link-buffer-advice
                         'fennel-proto-repl--link-buffer)
  (advice-add 'fennel-proto-repl--link-buffer :around
              #'fumos-repl--link-buffer-advice))

(defun fumos-repl--unlink-project-buffers (connection)
  "Remove only source-buffer links whose local owner is CONNECTION."
  ;; Clear each claimed batch before releasing it.  A reentrant stale writer can
  ;; only append to a fresh list, which the next fixed-point pass will sweep.
  (while (fumos-connection-linked-buffers connection)
    (let ((buffers (copy-sequence
                    (fumos-connection-linked-buffers connection))))
      (setf (fumos-connection-linked-buffers connection) nil)
      (dolist (buffer buffers)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (eq fumos-repl--source-owner connection)
              (fumos-repl--release-source-owner))))))))

(defun fumos-repl-unlink-current-buffer ()
  "Unlink the current source buffer according to its local FUMOS owner."
  (fumos-repl--release-source-owner))

(defun fumos-repl--kill-buffer-advice (original &optional buffer-or-name)
  "Keep a FUMOS source closing barrier for the full ORIGINAL kill call."
  (let ((buffer (get-buffer (or buffer-or-name (current-buffer)))))
    (if (and (buffer-live-p buffer)
             (buffer-local-value 'fumos-repl--source-owner buffer))
        (with-current-buffer buffer
          (let ((fumos-repl--source-killing t))
            (fumos-repl--source-kill-cleanup)
            (funcall original buffer)))
      (funcall original buffer-or-name))))

(unless (advice-member-p #'fumos-repl--kill-buffer-advice 'kill-buffer)
  (advice-add 'kill-buffer :around #'fumos-repl--kill-buffer-advice))

(defun fumos-repl--source-kill-cleanup ()
  "Release only the FUMOS link owned by the source buffer being killed."
  (let ((fumos-repl--source-killing t)
        (source (current-buffer))
        (connection fumos-repl--source-owner))
    (unwind-protect
        (condition-case nil
            (fumos-repl-unlink-current-buffer)
          (quit nil)
          (error nil))
      (when (fumos-connection-p connection)
        (setf (fumos-connection-linked-buffers connection)
              (delq source (fumos-connection-linked-buffers connection))))
      (remhash source fumos-repl--source-operations))))

(defun fumos-repl--invalidate-tooling-state (connection)
  "Invalidate every asynchronous editing query owned by CONNECTION."
  (when (fboundp 'fumos-eval--release-tooling-markers)
    (fumos-eval--release-tooling-markers connection))
  (setf (fumos-connection-help-epoch connection)
        (1+ (or (fumos-connection-help-epoch connection) 0))
        (fumos-connection-help-pending connection) nil
        (fumos-connection-xref-epoch connection)
        (1+ (or (fumos-connection-xref-epoch connection) 0))
        (fumos-connection-xref-pending connection) nil
        (fumos-connection-eldoc-epoch connection)
        (1+ (or (fumos-connection-eldoc-epoch connection) 0))
        (fumos-connection-eldoc-pending connection) nil
        (fumos-connection-completion-epoch connection)
        (1+ (or (fumos-connection-completion-epoch connection) 0))
        (fumos-connection-error-epoch connection)
        (1+ (or (fumos-connection-error-epoch connection) 0)))
  (when (hash-table-p (fumos-connection-xref-cache connection))
    (clrhash (fumos-connection-xref-cache connection)))
  (when (hash-table-p (fumos-connection-completion-pending connection))
    (clrhash (fumos-connection-completion-pending connection)))
  (when (hash-table-p (fumos-connection-completion-cache connection))
    (clrhash (fumos-connection-completion-cache connection))))

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
          (fumos-connection-macro-refresh-generation connection) nil
          (fumos-connection-macro-refresh-epoch connection)
          (1+ (or (fumos-connection-macro-refresh-epoch connection) 0)))
    (fumos-repl--invalidate-tooling-state connection)
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
      (fumos-repl--cancel-attach-operation
       (fumos-connection-attach-operation connection))
      (fumos-repl--suppress-reconnect connection)
      (fumos-repl--cancel-game-reload-timer connection)
      (fumos-repl--teardown-transport connection "REPL buffer killed")
      (fumos-repl--unregister-if-current connection))))

(defun fumos-repl--mark-disconnected (connection message)
  "Mark CONNECTION disconnected and fail all pending work."
  (fumos-repl--teardown-transport connection message))

(defun fumos-repl--unexpected-reconnect-snapshot (connection)
  "Return a token-free restart snapshot for CONNECTION, or nil."
  (when (and (fumos-connection-p connection)
             (not (fumos-connection-reconnect-suppressed connection))
             (memq (fumos-connection-state connection) '(ready busy))
             (not (fumos-connection-attach-operation connection))
             (not (fumos-connection-pending-game-reload connection))
             (numberp fumos-reconnect-timeout)
             (> fumos-reconnect-timeout 0))
    (let* ((instance (fumos-connection-instance connection))
           (root (fumos-repl--connection-root connection))
           (pid (and instance (fumos-instance-pid instance)))
           (repl-buffer (fumos-connection-repl-buffer connection))
           (start-identity (and pid
                                (fumos-repl--process-start-identity pid)))
           (token-digest (and instance (fumos-repl--token-digest instance))))
      (when (and root (integerp pid) start-identity token-digest)
        (make-fumos-reconnect-operation
         :connection connection :root root :pid pid
         :start-identity start-identity :token-digest token-digest
         :deadline (+ (float-time) fumos-reconnect-timeout)
         :show (and (buffer-live-p repl-buffer)
                    (get-buffer-window repl-buffer t)))))))

(defun fumos-repl--candidate-replaces-p (candidate operation)
  "Return non-nil when CANDIDATE is OPERATION's same-process replacement."
  (and (fumos-instance-p candidate)
       (= (fumos-instance-pid candidate)
          (fumos-reconnect-operation-pid operation))
       (equal (fumos-repl--canonical-local-root
               (fumos-instance-project-root candidate))
              (fumos-reconnect-operation-root operation))
       (let ((digest (fumos-repl--token-digest candidate)))
         (and digest
              (not (equal digest
                          (fumos-reconnect-operation-token-digest
                           operation)))))))

(defun fumos-repl--display-connection (connection)
  "Display CONNECTION's exact live REPL buffer."
  (let ((buffer (and (fumos-connection-p connection)
                     (fumos-connection-repl-buffer connection))))
    (unless (buffer-live-p buffer)
      (user-error "FUMOS REPL buffer is not available"))
    (pop-to-buffer buffer)))

(defun fumos-repl--insert-session-reset-notice (connection)
  "Explain the fresh lexical session in CONNECTION's visible history."
  (when-let* ((buffer (and (fumos-connection-p connection)
                           (fumos-connection-repl-buffer connection))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (unless (= (point) (line-beginning-position))
            (insert "\n"))
          (insert ";; FUMOS started a new REPL session; lexical locals were reset.\n")
          t)))))

(defun fumos-repl--poll-unexpected-reconnect (operation)
  "Poll once for OPERATION's same-process replacement descriptor."
  (if (not (fumos-repl--reconnect-current-p operation))
      ;; A registered operation that lost its connection reservation must not
      ;; leave a repeating timer behind.  Superseded operations are already
      ;; absent from the table, so this remains identity-safe.
      (fumos-repl--cancel-reconnect-operation operation)
    (condition-case nil
        (let* ((pid (fumos-reconnect-operation-pid operation))
               (deadline (fumos-reconnect-operation-deadline operation))
               (status (fumos-repl--attach-candidate-status operation))
               (candidate
                (fumos-repl--attach-operation-candidate operation))
               (expected
                (fumos-reconnect-operation-start-identity operation))
               (before (fumos-repl--process-start-identity pid)))
          (when (eq status 'failed)
            (fumos-repl--release-attach-operation-candidate operation)
            (setq status nil))
          (cond
           ((not (equal before expected))
            (when (fumos-repl--cancel-reconnect-operation operation)
              (fumos-repl--close-provisional-connection
               candidate "FUMOS reconnect process identity changed")
              (message "FUMOS reconnect process changed for PID %d" pid)))
           ((eq status 'ready)
            (fumos-repl--cancel-reconnect-operation operation))
           ((>= (float-time) deadline)
            (let ((candidate
                   (fumos-repl--attach-operation-candidate operation)))
              (when (fumos-repl--cancel-reconnect-operation operation)
                (fumos-repl--close-provisional-connection
                 candidate "FUMOS automatic reconnect timed out")
                (message "FUMOS automatic reconnect timed out for PID %d" pid))))
           ((eq status 'pending) nil)
           (t
            (let* ((candidates
                    (fumos-discover-instances
                     (fumos-reconnect-operation-root operation)))
                   (after (fumos-repl--process-start-identity pid))
                   (identity-current (equal after expected))
                   (match
                    (and identity-current
                         (seq-find
                          (lambda (candidate)
                            (fumos-repl--candidate-replaces-p
                             candidate operation))
                          candidates))))
              (cond
               ((not identity-current)
                ;; The original OS process can no longer publish a valid
                ;; replacement.  Stop instead of accepting a reused PID.
                (fumos-repl--cancel-reconnect-operation operation))
               (match
                (condition-case nil
                    (let ((replacement
                           (fumos-repl-connect-instance match operation)))
                      (fumos-repl--set-attach-operation-candidate
                       operation replacement)
                      (when (fumos-reconnect-operation-show operation)
                        (fumos-repl--display-connection replacement)))
                  ((error quit) nil))))))))
      ((error quit)
       ;; Discovery and setup failures are transient until the fixed deadline.
       nil))))

(defun fumos-repl--publish-unexpected-reconnect (operation)
  "Publish OPERATION as its project's current reconnect intent."
  (let ((root (fumos-reconnect-operation-root operation)))
    (fumos-repl--cancel-reconnect-for-root root)
    (puthash root operation fumos-repl--reconnect-operations)))

(defun fumos-repl--start-unexpected-reconnect (operation)
  "Start descriptor polling for an already published OPERATION."
  (if (not (fumos-repl--reconnect-current-p operation))
      (fumos-repl--cancel-reconnect-operation operation)
    (condition-case nil
        (let ((timer
               (run-at-time
                0.1 0.1
                (lambda ()
                  (fumos-repl--poll-unexpected-reconnect operation)))))
          (unless (timerp timer)
            (error "FUMOS reconnect scheduler returned no timer"))
          (setf (fumos-reconnect-operation-timer operation) timer)
          (unless (fumos-repl--reconnect-current-p operation)
            (fumos-repl--cancel-timer timer)
            (fumos-repl--cancel-reconnect-operation operation)))
      ((error quit)
       (fumos-repl--cancel-reconnect-operation operation)
       (message "FUMOS automatic reconnect failed")))))

(defun fumos-repl--transport-closed (connection)
  "Handle an owned transport close and watch for a native game reload."
  (let ((operation (fumos-repl--unexpected-reconnect-snapshot connection)))
    (when operation
      ;; Publish before teardown so any reentrant explicit attach can cancel it.
      (setf (fumos-connection-attach-operation connection) operation)
      (fumos-repl--publish-unexpected-reconnect operation))
    (fumos-repl--reject connection "Transport closed")
    (when operation
      (fumos-repl--start-unexpected-reconnect operation))))

(defun fumos-repl--reject (connection message)
  "Reject CONNECTION with token-free MESSAGE through normal teardown."
  (if (memq (fumos-connection-state connection)
            '(connecting authenticating bootstrapping))
      (let ((operation (fumos-connection-attach-operation connection)))
        (if (and operation
                 (fumos-repl--attach-operation-current-p operation))
            (let ((fumos-repl--preserve-attach-operation operation))
              (fumos-repl-close connection message)
              (when (eq connection
                        (fumos-repl--attach-operation-candidate operation))
                (fumos-repl--set-attach-operation-candidate operation nil)))
          (fumos-repl-close connection message)))
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
  "Own FUMOS retry and read scheduling on the receiving transport."
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
    (cond
     ((and connection (equal "retry" (plist-get message :op)))
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
             connection message-id wire-message callbacks)))))
     ((and connection (equal "read" (plist-get message :op)))
      (let ((delivery
             (and outer-callbacks
                  (gethash outer-id
                           (fumos-repl--callback-delivery-table connection)))))
        (if (and delivery
                 (eq outer-callbacks
                     (fumos-callback-delivery-callbacks delivery)))
            (fumos-repl--defer-read delivery (copy-tree message))
          (fumos-repl--reject connection "Unowned read request"))))
     (t
      (funcall original message)))))

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
    (connection process generation repl-buffer epoch request-id
                callback-identity)
  "Return non-nil while one macro refresh still owns its transport."
  (let ((delivery
         (and (integerp request-id)
              (gethash request-id
                       (fumos-repl--callback-delivery-table connection)))))
    (and (fumos-connection-macro-refresh-pending connection)
         (eql epoch (fumos-connection-macro-refresh-epoch connection))
         (eql request-id (fumos-connection-macro-refresh-id connection))
         (eql generation
              (fumos-connection-macro-refresh-generation connection))
         (eq repl-buffer (fumos-connection-repl-buffer connection))
         (fumos-repl--owns-transport-p connection process generation)
         (fumos-callback-delivery-p delivery)
         (eq callback-identity
             (fumos-callback-delivery-callbacks delivery)))))

(defun fumos-repl--complete-macro-refresh
    (connection process generation repl-buffer epoch request-id
                callback-identity values)
  "Commit VALUES to CONNECTION's cache when every owner identity matches."
  (when (fumos-repl--macro-refresh-current-p
         connection process generation repl-buffer epoch request-id
         callback-identity)
    (let ((parsed
           (and (consp values)
                (null (cdr values))
                (fumos-repl--parse-macro-cache (car values)))))
      (setf (fumos-connection-macro-refresh-pending connection) nil
            (fumos-connection-macro-refresh-id connection) nil
            (fumos-connection-macro-refresh-generation connection) nil)
      (when parsed
        (setf (fumos-connection-macro-cache connection) (cdr parsed)
              (fumos-connection-macro-cache-valid connection) t)
        (fumos-repl--refresh-linked-font-lock
         connection process generation)))))

(defun fumos-repl--fail-macro-refresh
    (connection process generation repl-buffer epoch request-id
                callback-identity &rest _error)
  "Release one failed macro refresh without replacing the previous cache."
  (when (fumos-repl--macro-refresh-current-p
         connection process generation repl-buffer epoch request-id
         callback-identity)
    (setf (fumos-connection-macro-refresh-pending connection) nil
          (fumos-connection-macro-refresh-id connection) nil
          (fumos-connection-macro-refresh-generation connection) nil)))

(defun fumos-repl--refresh-macro-cache (connection)
  "Start one nonblocking, epoch-owned macro refresh for CONNECTION."
  (let* ((process (fumos-connection-process connection))
         (generation (fumos-connection-generation connection))
         (repl-buffer (fumos-connection-repl-buffer connection))
         (state (fumos-connection-state connection)))
    (when (and (not (fumos-connection-macro-refresh-pending connection))
               (buffer-live-p repl-buffer)
               (fumos-repl--owns-transport-p
                connection process generation)
               (memq state '(bootstrapping ready busy)))
      (let ((epoch
             (1+ (or (fumos-connection-macro-refresh-epoch connection) 0)))
            request-id committed)
        (setf (fumos-connection-macro-refresh-epoch connection) epoch
              (fumos-connection-macro-refresh-pending connection) t
              (fumos-connection-macro-refresh-generation connection) generation)
        (unwind-protect
            (condition-case nil
                (with-current-buffer repl-buffer
                  (let (callback-identity)
                    (setq
                     request-id
                     (fumos-repl--send-framed-request
                      connection
                      (lambda (id)
                        (fumos-repl--format-eval-request
                         id (fumos-repl--macro-query-expression) nil))
                      (list
                       :values
                       (lambda (values)
                         (fumos-repl--complete-macro-refresh
                          connection process generation repl-buffer epoch
                          request-id callback-identity values))
                       :error
                       (lambda (&rest error-data)
                         (apply #'fumos-repl--fail-macro-refresh
                                connection process generation repl-buffer epoch
                                request-id callback-identity error-data))
                       :print #'ignore)
                      '(bootstrapping ready busy)))
                    (setq callback-identity
                          (gethash request-id
                                   fennel-proto-repl--message-callbacks))
                    (unless
                        (and
                         callback-identity
                         (fumos-connection-macro-refresh-pending connection)
                         (eql epoch
                              (fumos-connection-macro-refresh-epoch connection))
                         (eql generation
                              (fumos-connection-macro-refresh-generation
                               connection))
                         (eq repl-buffer
                             (fumos-connection-repl-buffer connection))
                         (fumos-repl--owns-transport-p
                          connection process generation))
                      (error "FUMOS macro refresh ownership changed"))
                    (setf (fumos-connection-macro-refresh-id connection)
                          request-id)
                    (setq committed t)
                    request-id))
              ((error quit) nil))
          (unless committed
            (when (integerp request-id)
              (condition-case nil
                  (when (buffer-live-p repl-buffer)
                    (with-current-buffer repl-buffer
                      (fennel-proto-repl--unassign-callbacks request-id)))
                ((error quit) nil)))
            (when (eql epoch
                       (fumos-connection-macro-refresh-epoch connection))
              (setf (fumos-connection-macro-refresh-pending connection) nil
                    (fumos-connection-macro-refresh-id connection) nil
                    (fumos-connection-macro-refresh-generation connection)
                    nil))))))))

(defun fumos-repl--invalidate-and-refresh-macro-cache (connection)
  "Invalidate CONNECTION's cache while retaining its stale fallback."
  (when (fumos-connection-p connection)
    (setf (fumos-connection-macro-refresh-epoch connection)
          (1+ (or (fumos-connection-macro-refresh-epoch connection) 0))
          (fumos-connection-macro-cache-valid connection) nil
          (fumos-connection-macro-refresh-pending connection) nil
          (fumos-connection-macro-refresh-id connection) nil
          (fumos-connection-macro-refresh-generation connection) nil)
    (when (and (not (fumos-connection-closing connection))
               (memq (fumos-connection-state connection) '(ready busy)))
      (fumos-repl--refresh-macro-cache connection))))

(defalias 'fumos-repl--invalidate-macro-cache
  #'fumos-repl--invalidate-and-refresh-macro-cache)

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
    (let ((operation (fumos-connection-attach-operation connection)))
      (unless (and operation
                   (eq operation fumos-repl--preserve-attach-operation))
        (fumos-repl--cancel-attach-operation operation)
        (fumos-repl--suppress-reconnect connection)
        (fumos-repl--cancel-game-reload-timer connection)))
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
          (fumos-connection-attach-operation connection) nil
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
    (fumos-repl--cancel-attach-operation
     (fumos-connection-attach-operation connection))
    (fumos-repl--suppress-reconnect connection)
    (fumos-repl--cancel-game-reload-timer connection)
    (unwind-protect
        (when (process-live-p (fumos-connection-process connection))
          (fumos-repl--send-control connection "FUMOS/1 DETACH"))
      (fumos-repl--mark-disconnected connection "Detached by user"))))

(defun fumos-switch-to-repl ()
  "Display the current FUMOS REPL."
  (interactive)
  (let ((connection (or (fumos-repl-current-connection)
                        (user-error "No FUMOS connection"))))
    (fumos-repl--display-connection connection)))

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
   :previous-editing-state fumos-repl--source-previous-editing-state
   :module-local-p
   (local-variable-p 'fennel-proto-repl-fennel-module-name)
   :module-value fennel-proto-repl-fennel-module-name
   :target fennel-proto-repl--buffer
   :mode fennel-proto-repl-minor-mode
   :transition fumos-repl--source-link-transition
   :operation (gethash (current-buffer) fumos-repl--source-operations)
   :editing-state (fumos-repl--capture-editing-state)
   :links (mapcar (lambda (value)
                    (cons value
                          (copy-sequence
                           (fumos-connection-linked-buffers value))))
                  (delete-dups (delq nil (copy-sequence connections))))))

(defun fumos-repl--restore-source-link-snapshot
    (snapshot ticket source enabled-upstream-mode previous-buffer previous-mode
              previous-module-local-p previous-module-value
              previous-editing-state)
  "Restore SNAPSHOT while TICKET remains SOURCE's newest operation."
  (cl-labels
      ((operation-current-p
        ()
        (and (buffer-live-p source)
             (eq ticket (gethash source fumos-repl--source-operations))))
       (restart
        ()
        (fumos-repl--replay-latest-source-link
         source enabled-upstream-mode previous-buffer previous-mode
         previous-module-local-p previous-module-value
         previous-editing-state))
       (ensure-current
        ()
        (unless (operation-current-p)
          (throw 'fumos-source-link-rollback-superseded (restart))))
       (set-and-check
        (symbol value)
        (set symbol value)
        (ensure-current))
       (restore-local-and-check
        (symbol local-snapshot)
        (fumos-repl--restore-local-value symbol local-snapshot)
        (ensure-current)))
    (catch 'fumos-source-link-rollback-superseded
      (when (operation-current-p)
        (dolist (entry (plist-get snapshot :links))
          (setf (fumos-connection-linked-buffers (car entry)) (cdr entry))
          (ensure-current))
        (set-and-check 'fumos-repl--source-owner
                       (plist-get snapshot :owner))
        (set-and-check 'fumos-repl--source-enabled-upstream-mode
                       (plist-get snapshot :enabled))
        (set-and-check 'fumos-repl--source-previous-upstream-buffer
                       (plist-get snapshot :previous-buffer))
        (set-and-check 'fumos-repl--source-previous-upstream-mode
                       (plist-get snapshot :previous-mode))
        (set-and-check 'fumos-repl--source-previous-module-local-p
                       (plist-get snapshot :previous-module-local-p))
        (set-and-check 'fumos-repl--source-previous-module-value
                       (plist-get snapshot :previous-module-value))
        (set-and-check 'fumos-repl--source-previous-editing-state
                       (plist-get snapshot :previous-editing-state))
        (set-and-check 'fennel-proto-repl--buffer
                       (plist-get snapshot :target))
        (set-and-check 'fennel-proto-repl-minor-mode
                       (plist-get snapshot :mode))
        (let ((editing (plist-get snapshot :editing-state)))
          (dolist (entry
                   '((fennel-proto-repl-font-lock-dynamically . :font-lock)
                     (completion-at-point-functions . :completion)
                     (xref-backend-functions . :xref)
                     (eldoc-documentation-functions . :eldoc)))
            (restore-local-and-check
             (car entry) (plist-get editing (cdr entry)))))
        (if (plist-get snapshot :module-local-p)
            (set (make-local-variable
                  'fennel-proto-repl-fennel-module-name)
                 (plist-get snapshot :module-value))
          (kill-local-variable 'fennel-proto-repl-fennel-module-name))
        (ensure-current)
        ;; Restore the local mirror before publishing the previous authoritative
        ;; operation.  A watcher which starts a newer operation changes the hash
        ;; and is detected before this rollback can overwrite that intent.
        (setq fumos-repl--source-link-transition
              (plist-get snapshot :transition))
        (ensure-current)
        (let ((previous-operation (plist-get snapshot :operation)))
          (if previous-operation
              (puthash source previous-operation
                       fumos-repl--source-operations)
            (remhash source fumos-repl--source-operations)))))))

(defun fumos-repl--link-buffer-to-connection (connection buffer)
  "Transactionally relink BUFFER to CONNECTION."
  (when (and (buffer-live-p buffer)
             (not (fumos-connection-closing connection)))
    (with-current-buffer buffer
      (unless fumos-repl--source-killing
        (let* ((previous-operation
                (gethash buffer fumos-repl--source-operations))
           (previous-link-p
            (eq (car-safe previous-operation) 'fumos-source-link))
           (previous-release-p
            (eq (car-safe previous-operation) 'fumos-source-release))
           (previous-operation-state
            (cond
             (previous-link-p (cdddr previous-operation))
             (previous-release-p (cdr previous-operation))))
           (old-owner
            (or fumos-repl--source-owner
                (and previous-link-p
                     (car (plist-get previous-operation-state
                                     :previous-owners)))))
           (previous-buffer
            (cond
             (previous-link-p
              (plist-get previous-operation-state :previous-buffer))
             (previous-release-p
              (if (plist-get previous-operation-state :completed)
                  (and fennel-proto-repl--buffer
                       (get-buffer fennel-proto-repl--buffer))
                (plist-get previous-operation-state :target)))
             (old-owner fumos-repl--source-previous-upstream-buffer)
             (t (and fennel-proto-repl--buffer
                     (get-buffer fennel-proto-repl--buffer)))))
           (previous-mode
            (cond
             (previous-link-p
              (plist-get previous-operation-state :previous-mode))
             (previous-release-p
              (if (plist-get previous-operation-state :completed)
                  (and fennel-proto-repl-minor-mode t)
                (plist-get previous-operation-state :mode)))
             (old-owner fumos-repl--source-previous-upstream-mode)
             (t (and fennel-proto-repl-minor-mode t))))
           (previous-module-local-p
            (cond
             (previous-link-p
              (plist-get previous-operation-state
                         :previous-module-local-p))
             (previous-release-p
              (if (plist-get previous-operation-state :completed)
                  (local-variable-p
                   'fennel-proto-repl-fennel-module-name)
                (plist-get previous-operation-state :module-local-p)))
             (old-owner fumos-repl--source-previous-module-local-p)
             (t (local-variable-p
                 'fennel-proto-repl-fennel-module-name))))
           (previous-module-value
            (cond
             (previous-link-p
              (plist-get previous-operation-state :previous-module-value))
             (previous-release-p
              (if (plist-get previous-operation-state :completed)
                  fennel-proto-repl-fennel-module-name
                (plist-get previous-operation-state :module-value)))
             (old-owner fumos-repl--source-previous-module-value)
             (t fennel-proto-repl-fennel-module-name)))
           (previous-editing-state
            (cond
             (previous-link-p
              (plist-get previous-operation-state :previous-editing-state))
             (previous-release-p
              (if (plist-get previous-operation-state :completed)
                  (fumos-repl--capture-editing-state)
                (plist-get previous-operation-state :editing-state)))
             (old-owner fumos-repl--source-previous-editing-state)
             (t (fumos-repl--capture-editing-state))))
           (repl-buffer (fumos-connection-repl-buffer connection))
           (ticket
            (list 'fumos-source-link connection buffer
                  :previous-owners
                  (delete-dups
                   (delq nil
                         (append
                          (and previous-link-p
                               (copy-sequence
                                (plist-get previous-operation-state
                                           :previous-owners)))
                          (list old-owner))))
                  :enabled (not previous-mode)
                  :previous-buffer previous-buffer
                  :previous-mode previous-mode
                  :previous-module-local-p previous-module-local-p
                  :previous-module-value previous-module-value
                  :previous-editing-state previous-editing-state))
           (previous-owners
            (plist-get (cdddr ticket) :previous-owners))
           (snapshot
            (fumos-repl--source-link-snapshot
             (append previous-owners (list connection))))
               failure superseded)
      (puthash buffer ticket fumos-repl--source-operations)
      (cl-labels
          ((setup-current-p
            ()
            (and (eq ticket
                     (gethash buffer fumos-repl--source-operations))
                 (not (fumos-connection-closing connection))
                 (not fumos-repl--source-killing)))
           (ensure-setup-current
            ()
            (unless (setup-current-p) (setq superseded t))))
        (condition-case caught
            (progn
              (setq fumos-repl--source-link-transition ticket)
              (ensure-setup-current)
              (unless superseded
                (setq fennel-proto-repl--buffer repl-buffer)
                (ensure-setup-current))
              (unless superseded
                (setq-local fennel-proto-repl-fennel-module-name
                            fumos-repl-fennel-module-name)
                (ensure-setup-current))
              ;; Upstream mode enable and link both refresh font lock
              ;; synchronously.  Remove only the global query before either
              ;; path can run.
              (unless superseded
                (setq-local
                 fennel-proto-repl-font-lock-dynamically
                 (fumos-repl--without-global-font-lock
                  fennel-proto-repl-font-lock-dynamically))
                (ensure-setup-current))
              (unless superseded
                (let ((fumos-repl--internal-link-target repl-buffer))
                  (unless fennel-proto-repl-minor-mode
                    (fennel-proto-repl-minor-mode 1))
                  (ensure-setup-current)
                  (unless superseded
                    (fennel-proto-repl--link-buffer repl-buffer)
                    (ensure-setup-current))
                  (unless superseded
                    (fumos-repl--install-owned-editing-state)
                    (ensure-setup-current)))))
          ((error quit) (setq failure caught))))
      (cond
       (failure
        (if (eq ticket (gethash buffer fumos-repl--source-operations))
            (fumos-repl--restore-source-link-snapshot
             snapshot ticket buffer (not previous-mode) previous-buffer
             previous-mode previous-module-local-p previous-module-value
             previous-editing-state)
          (fumos-repl--replay-latest-source-link
           buffer (not previous-mode) previous-buffer previous-mode
           previous-module-local-p previous-module-value
           previous-editing-state))
        (signal (car failure) (cdr failure)))
       (superseded
        ;; A nested link or teardown is newer intent and owns final state.
        (if (eq ticket (gethash buffer fumos-repl--source-operations))
            (fumos-repl--restore-source-link-snapshot
             snapshot ticket buffer (not previous-mode) previous-buffer
             previous-mode previous-module-local-p previous-module-value
             previous-editing-state)
          (fumos-repl--replay-latest-source-link
           buffer (not previous-mode) previous-buffer previous-mode
           previous-module-local-p previous-module-value
           previous-editing-state))
        fumos-repl--source-owner)
       (t
        (cl-labels
            ((commit-current-p
              ()
              (and (eq ticket
                       (gethash buffer fumos-repl--source-operations))
                   (buffer-live-p buffer)
                   (not (fumos-connection-closing connection))
                   (not fumos-repl--source-killing)))
             (abort-commit
              ()
              (if (eq ticket
                      (gethash buffer fumos-repl--source-operations))
                  (fumos-repl--restore-source-link-snapshot
                   snapshot ticket buffer (not previous-mode) previous-buffer
                   previous-mode previous-module-local-p previous-module-value
                   previous-editing-state)
                (fumos-repl--replay-latest-source-link
                 buffer (not previous-mode) previous-buffer previous-mode
                 previous-module-local-p previous-module-value
                 previous-editing-state))
              (throw 'fumos-source-link-commit-superseded
                     fumos-repl--source-owner))
             (ensure-commit-current
              ()
              (unless (commit-current-p) (abort-commit))))
          (catch 'fumos-source-link-commit-superseded
            (ensure-commit-current)
            (dolist (previous-owner previous-owners)
              (unless (eq previous-owner connection)
                (setf (fumos-connection-linked-buffers previous-owner)
                      (delq
                       buffer
                       (fumos-connection-linked-buffers previous-owner))))
              (ensure-commit-current))
            (setq fumos-repl--source-owner connection)
            (ensure-commit-current)
            (setq fumos-repl--source-enabled-upstream-mode
                  (not previous-mode))
            (ensure-commit-current)
            (setq fumos-repl--source-previous-upstream-buffer previous-buffer)
            (ensure-commit-current)
            (setq fumos-repl--source-previous-upstream-mode previous-mode)
            (ensure-commit-current)
            (setq fumos-repl--source-previous-module-local-p
                  previous-module-local-p)
            (ensure-commit-current)
            (setq fumos-repl--source-previous-module-value
                  previous-module-value)
            (ensure-commit-current)
            (setq fumos-repl--source-previous-editing-state
                  previous-editing-state)
            (ensure-commit-current)
            (cl-pushnew buffer
                        (fumos-connection-linked-buffers connection) :test #'eq)
            (ensure-commit-current)
            connection)))))))))

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
         (unwind-protect
             (condition-case caught
                 (progn
                   ;; Insert history before upstream creates its comint process
                   ;; and prompt.  Writing at process-mark afterward would
                   ;; splice this notice into the editable prompt.
                   (when (fumos-connection-session-reset-notice connection)
                     (fumos-repl--insert-session-reset-notice connection))
                   (unless (fumos-repl--bootstrap-commit-owned-p connection)
                     (error "FUMOS bootstrap reservation changed after session notice"))
                   (fumos-repl--start-upstream-ui connection values)
                   (unless (fumos-repl--bootstrap-commit-owned-p connection)
                     (error "FUMOS bootstrap reservation changed after UI start"))
                   (fumos-repl--link-project-buffers connection)
                   (unless (fumos-repl--bootstrap-commit-owned-p connection)
                     (error "FUMOS bootstrap reservation changed after linking"))
                   (fumos-repl--refresh-macro-cache connection)
                   (unless (fumos-repl--bootstrap-commit-owned-p connection)
                     (error "FUMOS bootstrap reservation changed during macro refresh"))
                   ;; The independent deadline covers every provisional step
                   ;; that can reenter or yield.  Cancellation and the ready
                   ;; transition are adjacent, non-yielding commit operations.
                   (fumos-repl--cancel-bootstrap-deadline connection)
                   (fumos-repl--set-state connection 'ready)
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

(defun fumos-repl--cancel-launch-for-instance (instance &optional preserve)
  "Resolve INSTANCE against its pending launch, except PRESERVE.
An Emacs-owned launch for another PID is terminated; the same PID is detached
from launch tracking without killing the game."
  (let* ((root (fumos-instance-project-root instance))
         (operation (gethash root fumos-repl--launch-operations)))
    (when (and operation (not (eq operation preserve)))
      (let* ((process (fumos-launch-operation-process operation))
             (owned-pid (and (processp process) (process-id process)))
             (selected-pid (fumos-instance-pid instance)))
        (fumos-repl--cancel-launch-operation
         operation
         (not (and (integerp owned-pid) (= owned-pid selected-pid))))))))

(defun fumos-repl-connect-instance (instance &optional operation)
  "Reserve INSTANCE's project, cleanly replacing its transport.
When OPERATION is non-nil, keep that internal launch or reconnect intent until
the returned connection reaches `ready' or `busy'."
  (let* ((foreign (fumos-repl--foreign-named-buffer instance))
         (root (fumos-instance-project-root instance))
         (old (or (gethash root fumos-repl--connections)
                  (fumos-repl--named-connection instance)))
         (connection (fumos-repl--new-connection instance))
         (transition (list 'fumos-attach connection)))
    ;; Reject before closing OLD or reserving ROOT; FOREIGN is user-owned.
    (when foreign
      (signal 'fumos-repl-connection-error (list :buffer-name-conflict)))
    (when operation
      (unless (fumos-repl--attach-operation-current-p operation)
        (signal 'fumos-repl-connection-error nil)))
    (when (or old operation)
      (setf (fumos-connection-session-reset-notice connection) t))
    (fumos-repl--cancel-launch-for-instance instance operation)
    (unless (eq operation (gethash root fumos-repl--reconnect-operations))
      (fumos-repl--cancel-reconnect-for-root root))
    (fumos-repl--cancel-game-reload-for-root root operation)
    (when operation
      (unless (fumos-repl--attach-operation-current-p operation)
        (signal 'fumos-repl-connection-error nil))
      (fumos-repl--set-attach-operation-candidate operation connection))
    ;; Publish a separate transition before closing OLD.  A kill hook may
    ;; recursively attach and replace this token while OLD is being destroyed.
    (puthash root transition fumos-repl--attach-transitions)
    (condition-case _err
        (let (opened)
          (unwind-protect
              (progn
                (when old
                  (let ((fumos-repl--preserve-attach-operation operation))
                    (fumos-repl-close old)))
                (unless (eq transition
                            (gethash root fumos-repl--attach-transitions))
                  ;; A reentrant explicit attach is newer user intent.  Game
                  ;; reload operations are not root-indexed, so cancel the
                  ;; passed owner explicitly before unwinding this candidate.
                  (fumos-repl--cancel-attach-operation operation)
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
              (let ((fumos-repl--preserve-attach-operation
                     (and operation
                          (fumos-repl--attach-operation-current-p operation)
                          operation)))
                (fumos-repl-close connection
                                  "FUMOS connection setup failed"))
              (when (and operation
                         (eq connection
                             (fumos-repl--attach-operation-candidate operation)))
                (fumos-repl--set-attach-operation-candidate operation nil)))))
      (error (signal 'fumos-repl-connection-error nil)))))

(defun fumos-repl--launcher-script (root)
  "Return ROOT's trusted executable Kristal launcher, or signal user error."
  (let* ((candidate (expand-file-name ".emacs/run-kristal-terminal.sh" root))
         (script (condition-case nil (file-truename candidate) (error nil))))
    (unless (and script
                 (file-in-directory-p script root)
                 (file-regular-p script)
                 (file-executable-p script))
      (user-error "FUMOS Kristal launcher is unavailable"))
    script))

(defun fumos-repl--launch-log-buffer (root)
  "Return the dedicated Kristal launch log buffer for ROOT."
  (get-buffer-create
   (format "*FUMOS Kristal: %s*"
           (file-name-nondirectory (directory-file-name root)))))

(defun fumos-repl--launch-descriptor (operation)
  "Return OPERATION's exact live descriptor instance, or nil."
  (let ((pid (process-id (fumos-launch-operation-process operation))))
    (and (integerp pid)
         (seq-find
          (lambda (instance) (= pid (fumos-instance-pid instance)))
          (fumos-discover-instances
           (fumos-launch-operation-root operation))))))

(defun fumos-repl--poll-launch (operation)
  "Poll once for the descriptor published by OPERATION's Kristal process."
  (when (fumos-repl--launch-current-p operation)
    (condition-case nil
        (let* ((process (fumos-launch-operation-process operation))
               (pid (and (processp process) (process-id process)))
               (expected (fumos-launch-operation-start-identity operation))
               (status (fumos-repl--attach-candidate-status operation))
               (candidate
                (fumos-repl--attach-operation-candidate operation))
               (before (and (processp process)
                            (process-live-p process)
                            (integerp pid)
                            (fumos-repl--process-start-identity pid))))
          (when (eq status 'failed)
            (fumos-repl--release-attach-operation-candidate operation)
            (setq status nil))
          (cond
           ((not (equal before expected))
            (when (fumos-repl--cancel-launch-operation operation)
              (fumos-repl--close-provisional-connection
               candidate "FUMOS launch process identity changed")
              (message "Kristal exited before publishing a FUMOS instance")))
           ((eq status 'ready)
            (fumos-repl--cancel-launch-operation operation))
           ((>= (float-time) (fumos-launch-operation-deadline operation))
            (let ((candidate
                   (fumos-repl--release-attach-operation-candidate operation))
                  (timer (fumos-launch-operation-timer operation)))
              ;; Keep the live game reserved.  A later command resumes the same
              ;; launch instead of starting a duplicate Kristal process.
              (setf (fumos-launch-operation-timer operation) nil)
              (fumos-repl--cancel-timer timer)
              (fumos-repl--close-provisional-connection
               candidate "FUMOS startup timed out")
              (message "Kristal is still running but FUMOS startup timed out")))
           ((eq status 'pending) nil)
           (t
            (let ((instance (fumos-repl--launch-descriptor operation))
                  (after (and (processp process)
                              (process-live-p process)
                              (fumos-repl--process-start-identity pid))))
              (cond
               ((not (equal after expected))
                (when (fumos-repl--cancel-launch-operation operation)
                  (message
                   "Kristal exited before publishing a FUMOS instance")))
               (instance
                (condition-case nil
                    (let ((replacement
                           (fumos-repl-connect-instance instance operation)))
                      (fumos-repl--set-attach-operation-candidate
                       operation replacement)
                      (fumos-repl--display-connection replacement))
                  ((error quit) nil))))))))
      ((error quit)
       ;; Keep retrying transient discovery/setup failures until the deadline.
       nil))))

(defun fumos-repl--await-launch (operation)
  "Start or resume descriptor polling for OPERATION."
  (when (fumos-repl--launch-current-p operation)
    (unless (timerp (fumos-launch-operation-timer operation))
      (unless (process-live-p (fumos-launch-operation-process operation))
        (fumos-repl--cancel-launch-operation operation)
        (user-error "Kristal exited before publishing a FUMOS instance"))
      (setf (fumos-launch-operation-deadline operation)
            (+ (float-time) fumos-launch-timeout))
      (let ((timer
             (run-at-time
              0.1 0.1
              (lambda () (fumos-repl--poll-launch operation)))))
        (unless (timerp timer)
          (fumos-repl--cancel-timer timer)
          (user-error "FUMOS launch scheduler returned no timer"))
        (if (fumos-repl--launch-current-p operation)
            (setf (fumos-launch-operation-timer operation) timer)
          (fumos-repl--cancel-timer timer))))
    operation))

(defun fumos-repl--launch-sentinel (operation process)
  "Finish OPERATION when its owned Kristal PROCESS exits early."
  (when (and (fumos-repl--launch-current-p operation)
             (not (process-live-p process)))
    ;; A descriptor can become visible just before the exit notification.
    (fumos-repl--poll-launch operation)
    (when (fumos-repl--launch-current-p operation)
      (fumos-repl--cancel-launch-operation operation)
      (message "Kristal exited before publishing a FUMOS instance"))))

(defun fumos-repl--start-kristal (root)
  "Start one foreground Kristal process for canonical project ROOT."
  (unless (and (numberp fumos-launch-timeout) (> fumos-launch-timeout 0))
    (user-error "FUMOS launch timeout must be positive"))
  (let* ((canonical
          (or (fumos-repl--canonical-local-root root)
              (user-error "Cannot canonicalize FUMOS project root")))
         (existing (gethash canonical fumos-repl--launch-operations)))
    (if (and existing (fumos-repl--launch-current-p existing))
        (fumos-repl--await-launch existing)
      (let* ((script (fumos-repl--launcher-script canonical))
             (buffer (fumos-repl--launch-log-buffer canonical))
             process operation)
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (setq default-directory canonical)))
        (condition-case nil
            (progn
              (setq
               process
               (make-process
                :name
                (generate-new-buffer-name
                 (format "fumos-kristal-%s"
                         (file-name-nondirectory
                          (directory-file-name canonical))))
                :buffer buffer :stderr buffer
                :command (list script "--foreground")
                :coding 'utf-8-unix :connection-type 'pipe :noquery t
                :sentinel
                (lambda (owned-process _event)
                  (when operation
                    (fumos-repl--launch-sentinel
                     operation owned-process)))))
              (set-process-query-on-exit-flag process nil)
              (let* ((pid (process-id process))
                     (start-identity
                      (and (integerp pid)
                           (fumos-repl--process-start-identity pid))))
                (unless start-identity
                  (error "FUMOS could not identify the Kristal process"))
                (setq operation
                      (make-fumos-launch-operation
                       :root canonical :process process :buffer buffer
                       :start-identity start-identity
                       :deadline (+ (float-time) fumos-launch-timeout))))
              (puthash canonical operation fumos-repl--launch-operations)
              (fumos-repl--await-launch operation))
          ((error quit)
           (fumos-repl--delete-process process 'neutralize)
           (when operation
             (fumos-repl--cancel-launch-operation operation))
           (user-error "Could not start Kristal for FUMOS")))))))

(defun fumos-cancel-launch ()
  "Cancel the current project's pending FUMOS launch and stop its Kristal."
  (interactive)
  (let* ((root (or (fumos-project-root)
                   (user-error "Current buffer is not in a FUMOS project")))
         (operation (gethash root fumos-repl--launch-operations)))
    (unless (and operation
                 (fumos-repl--cancel-launch-operation operation 'terminate))
      (user-error "No FUMOS Kristal launch is pending"))
    (message "Canceled pending FUMOS Kristal launch")))

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
  "Display, connect, or start the current project's FUMOS REPL."
  (interactive)
  (let* ((current (fumos-repl-current-connection))
         (root (or (fumos-project-root)
                   (fumos-repl--connection-root current)
                   (user-error "Current buffer is not in a FUMOS project")))
         (connection (or current
                         (gethash root fumos-repl--connections)))
         (state (and connection (fumos-connection-state connection)))
         (game-operation
          (gethash root fumos-repl--game-reload-operations)))
    (when (and game-operation
               (not (fumos-repl--attach-operation-current-p game-operation)))
      (fumos-repl--cancel-game-reload-for-root root)
      (setq game-operation nil))
    (cond
     ((and connection
           (memq state '(connecting authenticating bootstrapping ready busy))
           (buffer-live-p (fumos-connection-repl-buffer connection)))
      (fumos-repl--display-connection connection))
     ((gethash root fumos-repl--reconnect-operations)
      (let* ((operation (gethash root fumos-repl--reconnect-operations))
             (history (fumos-reconnect-operation-connection operation)))
        (setf (fumos-reconnect-operation-show operation) t)
        (if (buffer-live-p (fumos-connection-repl-buffer history))
            (fumos-repl--display-connection history)
          (message "Waiting for FUMOS after game reload"))))
     (game-operation
      (let* ((candidate
              (fumos-repl--attach-operation-candidate game-operation))
             (history (fumos-game-reload-operation-connection game-operation))
             (display
              (cond
               ((and (fumos-connection-p candidate)
                     (buffer-live-p
                      (fumos-connection-repl-buffer candidate)))
                candidate)
               ((and (fumos-connection-p history)
                     (buffer-live-p (fumos-connection-repl-buffer history)))
                history))))
        (if display
            (fumos-repl--display-connection display)
          (message "Waiting for FUMOS after game reload"))))
     ((gethash root fumos-repl--launch-operations)
      (let* ((operation (gethash root fumos-repl--launch-operations))
             (candidate
              (fumos-repl--attach-operation-candidate operation)))
        (fumos-repl--await-launch operation)
        (if (and (fumos-connection-p candidate)
                 (buffer-live-p (fumos-connection-repl-buffer candidate)))
            (fumos-repl--display-connection candidate)
          (pop-to-buffer (fumos-launch-operation-buffer operation)))))
     (t
      (pcase (fumos-discover-instances root)
        ('()
         (fumos-repl--start-kristal root)
         (pop-to-buffer
          (fumos-launch-operation-buffer
           (gethash root fumos-repl--launch-operations))))
        (`(,only)
         (fumos-repl--display-connection
          (fumos-repl-connect-instance only)))
        (instances
         (fumos-repl--display-connection
          (fumos-repl-connect-instance
           (fumos-select-instance instances)))))))))

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

(defconst fumos-repl--max-message-bytes 8388608
  "Maximum UTF-8 byte size of one FUMOS proto frame, excluding LF.")

(defun fumos-repl--quote-string (value)
  "Return VALUE as a single-line Fennel string literal."
  (unless (stringp value)
    (signal 'wrong-type-argument (list 'stringp value)))
  (string-replace
   "\r" "\\r"
   (fennel-proto-repl--replace-literal-newlines
    (format "%S" (substring-no-properties value)))))

(defun fumos-repl--valid-source-p (source)
  "Return non-nil when SOURCE is a complete positive source plist."
  (and (listp source)
       (stringp (plist-get source :file))
       (let ((line (plist-get source :line))
             (column (plist-get source :column)))
         (and (integerp line) (> line 0)
              (integerp column) (> column 0)))))

(defun fumos-repl--format-eval-request (id code source)
  "Format ID, CODE, and optional SOURCE as one Fennel map."
  (unless (and (integerp id) (> id 0))
    (user-error "FUMOS did not allocate a request ID"))
  (unless (stringp code)
    (signal 'wrong-type-argument (list 'stringp code)))
  (when (and source (not (fumos-repl--valid-source-p source)))
    (user-error "Invalid FUMOS source context"))
  (if source
      (format
       "{:id %d :eval %s :file %s :line %d :column %d}"
       id
       (fumos-repl--quote-string code)
       (fumos-repl--quote-string (plist-get source :file))
       (plist-get source :line)
       (plist-get source :column))
    (format "{:id %d :eval %s}" id (fumos-repl--quote-string code))))

(defun fumos-repl--utf8-bytes (value)
  "Return VALUE's exact UTF-8 wire byte count."
  (string-bytes (encode-coding-string value 'utf-8-unix)))

(defvar fumos-repl--error-context nil
  "Dynamically captured authority passed to the FUMOS error UI.")

(defun fumos-repl--default-error-handler (type message traceback)
  "Route a FUMOS eval error through the installed source-aware handler."
  (if (fboundp 'fumos-error-handler)
      (fumos-error-handler type message traceback fumos-repl--error-context)
    (fennel-proto-repl--error-handler type message traceback)))

(defun fumos-repl--send-framed-request
    (connection formatter callbacks allowed-states)
  "Transactionally send a request produced by FORMATTER for CONNECTION."
  (let* ((process (fumos-connection-process connection))
         (generation (fumos-connection-generation connection))
         (repl-buffer (fumos-connection-repl-buffer connection))
         (values-callback (or (plist-get callbacks :values) #'ignore))
         (error-callback
          (or (plist-get callbacks :error)
              #'fumos-repl--default-error-handler))
         (print-callback
          (or (plist-get callbacks :print)
              #'fennel-proto-repl--print)))
    (unless (memq (fumos-connection-state connection) allowed-states)
      (user-error "FUMOS connection is not ready"))
    (unless (and (process-live-p process)
                 (buffer-live-p repl-buffer)
                 (fumos-repl--owns-transport-p
                  connection process generation))
      (user-error "FUMOS connection is not live"))
    (with-current-buffer repl-buffer
      (let (id sent)
        (unwind-protect
            (progn
              (setq id
                    (fennel-proto-repl--assign-callback
                     values-callback error-callback print-callback))
              (unless (and (integerp id) (> id 0))
                (user-error "FUMOS could not allocate a request callback"))
              (let ((request (funcall formatter id)))
                (unless (stringp request)
                  (user-error "FUMOS formatter returned no request"))
                (when (string-match-p "[\r\n]" request)
                  (user-error "FUMOS request is not one line"))
                (when (> (fumos-repl--utf8-bytes request)
                         fumos-repl--max-message-bytes)
                  (user-error
                   "FUMOS request exceeds 8388608 bytes"))
                (unless (fumos-repl--owns-transport-p
                         connection process generation)
                  (user-error "FUMOS connection changed during request"))
                (fennel-proto-repl--send-string process request))
              (setq sent t)
              id)
          (when (and (integerp id) (not sent))
            (fennel-proto-repl--unassign-callbacks id)))))))

(defun fumos-repl-send-eval (code source callbacks)
  "Asynchronously send CODE with SOURCE and CALLBACKS over FUMOS."
  (let ((connection (or (fumos-repl-current-connection)
                        (user-error "No FUMOS connection"))))
    (fumos-repl--send-framed-request
     connection
     (lambda (id) (fumos-repl--format-eval-request id code source))
     callbacks '(ready busy))))

(defconst fumos-repl--command-ops
  '(:reload :compile :complete :doc :apropos :find)
  "Protocol operations accepted by `fumos-repl-send-command'.")

(defun fumos-repl-send-command (op data callbacks)
  "Asynchronously send whitelisted OP with DATA and CALLBACKS over FUMOS."
  (unless (memq op fumos-repl--command-ops)
    (user-error "Unsupported FUMOS protocol command"))
  (unless (stringp data)
    (signal 'wrong-type-argument (list 'stringp data)))
  (let ((connection (or (fumos-repl-current-connection)
                        (user-error "No FUMOS connection"))))
    (fumos-repl--send-framed-request
     connection
     (lambda (id)
       ;; Pinned formatting escapes LF but leaves CR literal.  Escape CR in the
       ;; completed map so DATA round-trips while the wire remains one line.
       (string-replace
        "\r" "\\r"
        (fennel-proto-repl--format-message id op data t)))
     callbacks '(ready busy))))

(provide 'fumos-repl)
;;; fumos-repl.el ends here
