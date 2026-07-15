;;; fumos-eval.el --- Explicit FUMOS evaluation commands -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'cl-lib)
(require 'subr-x)
(require 'thingatpt)
(require 'fumos-repl)

(declare-function lua-mode "lua-mode")

(defun fumos-eval--connection ()
  "Return the current FUMOS connection."
  (or (fumos-repl-current-connection)
      (user-error "No FUMOS connection")))

(defun fumos-eval--validate-local-source-file ()
  "Reject a nonlocal or relative `buffer-file-name' without filesystem I/O."
  (when (and buffer-file-name
             (or (file-remote-p buffer-file-name)
                 (not (file-name-absolute-p buffer-file-name))))
    (user-error "FUMOS source must be a local absolute file"))
  buffer-file-name)

(defun fumos-eval--source (position)
  "Return canonical source metadata for buffer POSITION."
  (when buffer-file-name
    (fumos-eval--validate-local-source-file)
    (let* ((connection (fumos-eval--connection))
           (instance (fumos-connection-instance connection))
           (root-value (fumos-instance-project-root instance))
           (mod-id (fumos-instance-mod-id instance)))
      (when (or (not (stringp root-value))
                (file-remote-p root-value)
                (not (file-name-absolute-p root-value)))
        (user-error "FUMOS project root is not local and absolute"))
      (unless (and (stringp mod-id)
                   (not (string-empty-p mod-id))
                   (not (member mod-id '("." "..")))
                   (not (string-match-p "[/\\\\]" mod-id)))
        (user-error "FUMOS mod ID is not one path segment"))
      (let* ((root
             (condition-case nil
                  (file-name-as-directory (file-truename root-value))
                (error
                 (user-error "Cannot canonicalize FUMOS project root"))))
             (absolute
              (condition-case nil
                  (file-truename buffer-file-name)
                (error
                 (user-error "Cannot canonicalize FUMOS source file")))))
        (unless (file-in-directory-p absolute root)
          (user-error "File is outside the attached FUMOS project"))
        (let ((relative (file-relative-name absolute root)))
          (unless
              (and (string-match-p "\\.fnlm?\\'" relative)
                   (not (file-name-absolute-p relative))
                   (not (string-search "\\" relative))
                   (not (string-search "//" relative))
                   (cl-every
                    (lambda (segment)
                      (not (member segment '("" "." ".."))))
                    (split-string relative "/" nil)))
            (user-error "Invalid FUMOS project-relative source path"))
          (save-restriction
            (widen)
            (save-excursion
              (goto-char position)
              (let ((prefix
                     (buffer-substring-no-properties
                      (line-beginning-position) (point))))
                (list
                 :file (concat "mods/" mod-id "/" relative)
                 :line (line-number-at-pos (point) t)
                 :column
                 (1+ (string-bytes
                      (encode-coding-string prefix 'utf-8-unix))))))))))))

(defun fumos-eval--display (values &optional end buffer)
  "Display VALUES at END in BUFFER using the upstream result UI."
  (fennel-proto-repl--display-result values end buffer))

(defun fumos-eval--region-has-form-p (beg end)
  "Return non-nil when BEG through END contains code."
  (save-restriction
    (widen)
    (narrow-to-region beg end)
    (save-excursion
      (goto-char (point-min))
      (forward-comment (point-max))
      (not (eobp)))))

(defun fumos-eval--marker-delivery (marker values-callback)
  "Return callbacks and an exactly-once finalizer for MARKER."
  (let (finished)
    (let ((finish
           (lambda ()
             (unless finished
               (setq finished t)
               (set-marker marker nil)))))
      (cons
       (list
        :values
        (lambda (values)
          (unwind-protect
              (let ((buffer (marker-buffer marker))
                    (position (marker-position marker)))
                (when (and (buffer-live-p buffer) position)
                  (condition-case nil
                      (funcall values-callback values buffer position)
                    (quit (message "FUMOS result display quit"))
                    (error (message "FUMOS result display failed")))))
            (funcall finish)))
        :error
        (lambda (type message traceback)
          (unwind-protect
              (fumos-repl--default-error-handler type message traceback)
            (funcall finish))))
       finish))))

(defun fumos-eval-region (beg end &optional display-overlay)
  "Asynchronously evaluate the region from BEG to END."
  (interactive "r")
  (save-restriction
    (widen)
    (unless (fumos-eval--region-has-form-p beg end)
      (user-error "FUMOS eval region contains no form"))
    (let* ((code (buffer-substring-no-properties beg end))
           (source (fumos-eval--source beg))
           (marker (copy-marker end t))
           (delivery
            (fumos-eval--marker-delivery
             marker
             (lambda (values buffer position)
               (let ((fennel-proto-repl-eval-overlay display-overlay))
                 (fumos-eval--display values position buffer)))))
           (callbacks (car delivery))
           (finish (cdr delivery))
           sent)
      (unwind-protect
          (prog1 (fumos-repl-send-eval code source callbacks)
            (setq sent t))
        (unless sent (funcall finish))))))

(defun fumos-eval-buffer ()
  "Asynchronously evaluate the current buffer."
  (interactive)
  (fumos-eval-region (point-min) (point-max)))

(defun fumos-eval--reader-form-start (start)
  "Include Fennel's hashfn reader prefix immediately before START."
  (if (and (> start (point-min))
           (eq (char-after start) ?\()
           (eq (char-before start) ?#))
      (1- start)
    start))

(defun fumos-eval--forward-reader-form ()
  "Move across one sexp, including a Fennel hashfn reader prefix."
  (when (and (eq (char-after) ?#)
             (eq (char-after (1+ (point))) ?\())
    (forward-char 1))
  (forward-sexp 1))

(defun fumos-eval--defun-bounds ()
  "Return exact bounds of the current Fennel top-level form."
  (save-restriction
    (widen)
    (save-excursion
      (let* ((state (syntax-ppss))
             (depth (car state))
             start)
        (cond
         ((> depth 0)
          (setq start (nth 1 state))
          (let (parent)
            (while (setq parent (nth 1 (syntax-ppss start)))
              (setq start parent))))
         ((nth 3 state)
          (setq start (nth 8 state)))
         (t
          (when (nth 4 state)
            (goto-char (nth 8 state)))
          (forward-comment (point-max))
          (cond
           ((and (eq (char-after) ?#)
                 (eq (char-after (1+ (point))) ?\())
            (setq start (point)))
           ((memq (char-after) '(?\( ?\[ ?\{ ?\"))
            (setq start (point)))
           ((bounds-of-thing-at-point 'sexp)
            (setq start (car (bounds-of-thing-at-point 'sexp))))
           (t
            (goto-char (point-max))
            (forward-comment (- (point-max)))
            (condition-case nil
                (progn
                  (backward-sexp 1)
                  (setq start (point)))
              (error nil))))))
        (when start
          (setq start (fumos-eval--reader-form-start start)))
        (unless start
          (user-error "No Fennel top-level form at point"))
        (goto-char start)
        (let ((end
               (condition-case nil
                   (progn (fumos-eval--forward-reader-form) (point))
                 (error
                  (user-error "Incomplete Fennel top-level form")))))
          (cons start end))))))

(defun fumos-eval--last-sexp-bounds ()
  "Return reader-aware bounds of the expression before point."
  (save-restriction
    (widen)
    (save-excursion
      (let ((end (point)))
        (backward-sexp 1)
        (cons (fumos-eval--reader-form-start (point)) end)))))

(defun fumos-eval-defun ()
  "Evaluate the current top-level form and echo its result asynchronously."
  (interactive)
  (pcase-let ((`(,beg . ,end) (fumos-eval--defun-bounds)))
    (fumos-eval-region beg end)))

(defun fumos-eval-defun-overlay ()
  "Evaluate the current top-level form with a result overlay."
  (interactive)
  (pcase-let ((`(,beg . ,end) (fumos-eval--defun-bounds)))
    (fumos-eval-region beg end t)))

(defun fumos-eval-defun-async ()
  "Queue the current top-level form for echo-area delivery."
  (interactive)
  (fumos-eval-defun))

(defun fumos-eval-last-sexp ()
  "Evaluate the expression before point asynchronously."
  (interactive)
  (pcase-let ((`(,beg . ,end) (fumos-eval--last-sexp-bounds)))
    (fumos-eval-region beg end)))

(defun fumos-eval-print-last-sexp ()
  "Evaluate the expression before point and insert its values."
  (interactive)
  (pcase-let* ((`(,beg . ,end-position) (fumos-eval--last-sexp-bounds))
               (end (copy-marker end-position t))
               (delivery
                (fumos-eval--marker-delivery
                 end
                 (lambda (values buffer position)
                   (with-current-buffer buffer
                     (goto-char position)
                     (insert "\n" (string-join values "\t"))))))
               (callbacks (car delivery))
               (finish (cdr delivery))
               (sent nil))
    (unwind-protect
        (prog1
            (fumos-repl-send-eval
             (buffer-substring-no-properties beg end)
             (fumos-eval--source beg)
             callbacks)
          (setq sent t))
      (unless sent (funcall finish)))))

(defun fumos-eval-form-and-next ()
  "Evaluate the current top-level form and move to the next form."
  (interactive)
  (pcase-let ((`(,beg . ,end) (fumos-eval--defun-bounds)))
    (fumos-eval-region beg end)
    (goto-char end)
    (forward-comment (point-max))))

(defconst fumos-eval--tooling-guard
  "(assert (= _G _G._G) \"FUMOS tooling requires unshadowed _G\")"
  "Guard required before every FUMOS tooling side effect.")

(defun fumos-eval--source-relative-path (source connection)
  "Return SOURCE's authenticated project-relative path for CONNECTION."
  (let* ((instance (fumos-connection-instance connection))
         (prefix (format "mods/%s/" (fumos-instance-mod-id instance)))
         (virtual (plist-get source :file)))
    (unless (and (stringp virtual) (string-prefix-p prefix virtual))
      (user-error "FUMOS source does not belong to the attached mod"))
    (let ((relative (substring virtual (length prefix))))
      (unless (and (not (string-empty-p relative))
                   (not (file-name-absolute-p relative))
                   (not (string-search "\\" relative))
                   (not (string-search "//" relative))
                   (cl-every
                    (lambda (segment)
                      (not (member segment '("" "." ".."))))
                    (split-string relative "/" nil)))
        (user-error "Invalid FUMOS project-relative source path"))
      relative)))

(defun fumos-eval--reloadable-relative-p (relative)
  "Return non-nil when RELATIVE is a v0.1 semantic reload target."
  (or (equal relative "mod.fnl")
      (string-match-p "\\`scripts/.+\\.fnl\\'" relative)
      (string-match-p "\\`fnl/.+\\.fnlm?\\'" relative)))

(defun fumos-eval--refresh-after-success
    (connection process generation values &optional require-true)
  "Refresh CONNECTION's macro cache after successful reload VALUES."
  (when (and (fumos-repl--owns-transport-p
              connection process generation)
             (or (not require-true) (equal "true" (car values))))
    (fumos-repl--invalidate-and-refresh-macro-cache connection))
  (fumos-eval--display values))

(defun fumos-reload-current-file ()
  "Semantically reload the current saved FUMOS source file."
  (interactive)
  (unless buffer-file-name
    (user-error "FUMOS reload requires a visited file"))
  ;; Fail before project discovery can invoke a TRAMP file handler.
  (fumos-eval--validate-local-source-file)
  (when (buffer-modified-p)
    (user-error "Save the FUMOS source explicitly before reloading"))
  (let* ((connection (fumos-eval--connection))
         (process (fumos-connection-process connection))
         (generation (fumos-connection-generation connection))
         (source (fumos-eval--source (point-min)))
         (relative (fumos-eval--source-relative-path source connection)))
    (unless (fumos-eval--reloadable-relative-p relative)
      (user-error "FUMOS cannot semantically reload %s" relative))
    (let ((code
           (format
            (concat "(do\n"
                    "  %s\n"
                    "  (let [(ok report) "
                    "(_G.Mod.libs.fumos.reload {:path %s})]\n"
                    "    (if ok\n"
                    "        (values ok report)\n"
                    "        (error report))))")
            fumos-eval--tooling-guard
            (fumos-repl--quote-string relative))))
      (fumos-repl-send-eval
       code source
       (list
        :values
        (lambda (values)
          (fumos-eval--refresh-after-success
           connection process generation values t))
        :error #'fumos-repl--default-error-handler)))))

(defun fumos-reload-module (module)
  "Reload Fennel MODULE through the native Session command."
  (interactive (list (read-string "Fennel module: ")))
  (unless (and (stringp module) (not (string-empty-p module)))
    (user-error "FUMOS module name is empty"))
  (let* ((connection (fumos-eval--connection))
         (process (fumos-connection-process connection))
         (generation (fumos-connection-generation connection)))
    (fumos-repl-send-command
     :reload module
     (list
      :values
      (lambda (values)
        (fumos-eval--refresh-after-success
         connection process generation values))
      :error #'fumos-repl--default-error-handler))))

(defvar fumos-eval--last-generated-lua nil
  "Last Lua source returned by an explicit compile preview.")

(defun fumos-eval--show-lua-values (values)
  "Validate and display the single generated Lua value in VALUES."
  (if (not (and (consp values)
                (null (cdr values))
                (stringp (car values))))
      (fumos-repl--default-error-handler
       "compile" "FUMOS compile returned an invalid result" nil)
    (setq fumos-eval--last-generated-lua (car values))
    (with-current-buffer (get-buffer-create "*FUMOS Lua*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert fumos-eval--last-generated-lua "\n")
        (if (require 'lua-mode nil t)
            (lua-mode)
          (prog-mode))
        (view-mode 1))
      (display-buffer (current-buffer)))))

(defun fumos-show-generated-lua ()
  "Show the last generated Lua, compiling the current form when absent."
  (interactive)
  (if fumos-eval--last-generated-lua
      (fumos-eval--show-lua-values (list fumos-eval--last-generated-lua))
    (fumos-compile-defun)))

(defun fumos-eval--remap-compile-message
    (message source source-start-line source-start-column)
  "Remap pinned compile MESSAGE through authenticated SOURCE metadata."
  (if (and
       (stringp message)
       (fumos-repl--valid-source-p source)
       (string-match
        (concat
         "\\`Error compiling expression: unknown:"
         "\\([0-9]+\\):\\([0-9]+\\):")
        message))
      (let* ((wire-line (string-to-number (match-string 1 message)))
             (wire-column (string-to-number (match-string 2 message)))
             (suffix (substring message (match-end 0))))
        (if (< wire-line 3)
            message
          (let* ((source-line (- wire-line 2))
                 (absolute-line (+ source-start-line source-line -1))
                 (absolute-column
                  (if (= source-line 1)
                      (+ source-start-column wire-column)
                    (1+ wire-column))))
            (format
             "Error compiling expression: %s:%d:%d:%s"
             (plist-get source :file) absolute-line absolute-column suffix))))
    message))

(defun fumos-eval--compile-error-callback
    (connection process generation source source-start-line
                source-start-column)
  "Return an identity-gated source-aware compile error callback."
  (lambda (type message traceback)
    (let ((compilation-error-screen-columns nil))
      (fumos-repl--default-error-handler
       type
       (if (fumos-repl--owns-transport-p
            connection process generation)
           (fumos-eval--remap-compile-message
            message source source-start-line source-start-column)
         message)
       traceback))))

(defun fumos-eval--compile-region (beg end)
  "Compile BEG through END as one wrapped unit without executing it."
  (save-restriction
    (widen)
    (let* ((source-text (buffer-substring-no-properties beg end))
           (source (fumos-eval--source beg))
           (source-start-line (line-number-at-pos beg t))
           (source-start-column
            (save-excursion
              (goto-char beg)
              (1+ (- (point) (line-beginning-position)))))
           (connection (fumos-eval--connection))
           (process (fumos-connection-process connection))
           (generation (fumos-connection-generation connection)))
      (fumos-repl-send-command
       :compile (concat "(do\n" source-text "\n)")
       (list
        :values #'fumos-eval--show-lua-values
        :error
        (fumos-eval--compile-error-callback
         connection process generation source source-start-line
         source-start-column))))))

(defun fumos-compile-buffer ()
  "Compile the widened in-memory buffer as one unit without saving it."
  (interactive)
  (save-restriction
    (widen)
    (fumos-eval--compile-region (point-min) (point-max))))

(defun fumos-compile-defun ()
  "Compile the current top-level form without executing or saving it."
  (interactive)
  (pcase-let ((`(,beg . ,end) (fumos-eval--defun-bounds)))
    (fumos-eval--compile-region beg end)))

(cl-defstruct fumos-game-reload-operation
  connection generation transport-generation mode pid root start-identity
  token-digest deadline)

(defun fumos-eval--process-start-identity (pid)
  "Return PID's normalized current-user process start identity, or nil."
  (condition-case nil
      (let* ((attributes (process-attributes pid))
             (euid (and attributes (alist-get 'euid attributes)))
             (start (and attributes (alist-get 'start attributes))))
        (when (and (integerp euid) (= euid (user-uid)) start)
          (time-convert start 'list)))
    (error nil)))

(defun fumos-eval--canonical-game-root (root)
  "Return ROOT as a canonical local directory, or signal `user-error'."
  (when (or (not (stringp root))
            (file-remote-p root)
            (not (file-name-absolute-p root)))
    (user-error "FUMOS game reload root is not local and absolute"))
  (let ((canonical
         (condition-case nil
             (file-name-as-directory (file-truename root))
           (error nil))))
    (unless (and canonical (file-directory-p canonical))
      (user-error "Cannot canonicalize FUMOS game reload root"))
    canonical))

(defun fumos-eval--begin-game-reload (connection mode)
  "Reserve and return one token-free game reload operation."
  (unless (member mode '("temp" "save" "none"))
    (user-error "Invalid FUMOS game reload mode"))
  (when (fumos-connection-pending-game-reload connection)
    (user-error "A FUMOS game reload is already pending"))
  (let* ((instance (fumos-connection-instance connection))
         (pid (fumos-instance-pid instance))
         (root
          (fumos-eval--canonical-game-root
           (fumos-instance-project-root instance)))
         (start-identity (fumos-eval--process-start-identity pid))
         (token-digest
          (let ((value (fumos-instance-token instance)))
            (unless (and (stringp value) (= 64 (length value)))
              (user-error "FUMOS game reload token is unavailable"))
            (secure-hash 'sha256 value))))
    (unless start-identity
      (user-error "FUMOS process start identity is unavailable"))
    (let ((generation
           (1+ (or (fumos-connection-game-reload-generation connection) 0))))
      (setf (fumos-connection-game-reload-generation connection) generation
            (fumos-connection-pending-game-reload connection) mode)
      (make-fumos-game-reload-operation
       :connection connection :generation generation
       :transport-generation (fumos-connection-generation connection)
       :mode mode :pid pid :root root :start-identity start-identity
       :token-digest token-digest :deadline (+ (float-time) 10.0)))))

(defun fumos-eval--game-operation-current-p (operation)
  "Return non-nil while OPERATION still owns its connection intent."
  (let ((connection (fumos-game-reload-operation-connection operation)))
    (and (fumos-connection-p connection)
         (eql (fumos-game-reload-operation-generation operation)
              (fumos-connection-game-reload-generation connection))
         (eql (fumos-game-reload-operation-transport-generation operation)
              (fumos-connection-generation connection))
         (equal (fumos-game-reload-operation-mode operation)
                (fumos-connection-pending-game-reload connection)))))

(defun fumos-eval--cancel-game-reload-operation (operation)
  "Cancel OPERATION only while it still owns its connection."
  (when (fumos-eval--game-operation-current-p operation)
    (fumos-repl--cancel-game-reload-timer
     (fumos-game-reload-operation-connection operation))
    t))

(defun fumos-eval--candidate-token-changed-p (candidate operation)
  "Return non-nil when CANDIDATE has a new token for OPERATION."
  (let ((value (fumos-instance-token candidate)))
    (and (stringp value)
         (= 64 (length value))
         (not
          (equal (secure-hash 'sha256 value)
                 (fumos-game-reload-operation-token-digest operation))))))

(defun fumos-eval--candidate-root-matches-p (candidate root)
  "Return non-nil when CANDIDATE's canonical local root equals ROOT."
  (condition-case nil
      (equal root
             (fumos-eval--canonical-game-root
              (fumos-instance-project-root candidate)))
    (error nil)))

(defun fumos-eval--poll-game-reload (operation)
  "Poll once for OPERATION's same-process replacement descriptor."
  (when (fumos-eval--game-operation-current-p operation)
    (condition-case nil
        (let* ((pid (fumos-game-reload-operation-pid operation))
               (root (fumos-game-reload-operation-root operation)))
          (if (>= (float-time)
                  (fumos-game-reload-operation-deadline operation))
              (when (fumos-eval--cancel-game-reload-operation operation)
                (message "FUMOS game reload timed out waiting for PID %d" pid))
            (let ((before (fumos-eval--process-start-identity pid)))
              (when (and before
                         (equal before
                                (fumos-game-reload-operation-start-identity
                                 operation)))
                (let* ((candidates (fumos-discover-instances root))
                       (after (fumos-eval--process-start-identity pid))
                       (match
                        (and (equal before after)
                             (equal after
                                    (fumos-game-reload-operation-start-identity
                                     operation))
                             (seq-find
                              (lambda (candidate)
                                (and
                                 (= pid (fumos-instance-pid candidate))
                                 (fumos-eval--candidate-root-matches-p
                                  candidate root)
                                 (fumos-eval--candidate-token-changed-p
                                  candidate operation)))
                              candidates))))
                  (when (and match
                             (fumos-eval--cancel-game-reload-operation
                              operation))
                    (condition-case nil
                        (fumos-repl-connect-instance match)
                      ((error quit)
                       (message "FUMOS game reload reconnect failed")))))))))
      ((error quit)
       (when (fumos-eval--cancel-game-reload-operation operation)
         (message "FUMOS game reload polling failed"))))))

(defun fumos-eval--await-game-reload (connection operation)
  "Install OPERATION's token-free replacement polling timer."
  (unless (eq connection
              (fumos-game-reload-operation-connection operation))
    (user-error "FUMOS game reload operation has the wrong owner"))
  (let ((timer
         (run-at-time
          0.1 0.1
          (lambda () (fumos-eval--poll-game-reload operation)))))
    (unless
        (condition-case nil
            (timerp timer)
          ((error quit) nil))
      (fumos-repl--cancel-timer timer)
      (user-error "FUMOS game reload scheduler returned no timer"))
    (if (fumos-eval--game-operation-current-p operation)
        (setf (fumos-connection-game-reload-timer connection) timer)
      (fumos-repl--cancel-timer timer))
    timer))

(defun fumos-eval--game-error-callback (operation)
  "Return the terminal error callback owned by OPERATION."
  (lambda (type message traceback)
    (unless (equal type "connection-lost")
      (when (fumos-eval--cancel-game-reload-operation operation)
        (fumos-repl--default-error-handler type message traceback)))))

(defun fumos-eval--reload-game (mode)
  "Ask Kristal to reload with MODE and await its same-PID replacement."
  (let* ((connection (fumos-eval--connection))
         operation request-id installed)
    (unwind-protect
        (progn
          (setq operation (fumos-eval--begin-game-reload connection mode))
          (setq request-id
                (fumos-repl-send-eval
                 (format
                  (concat "(do\n"
                          "  %s\n"
                          "  (_G.Kristal.quickReload %S))")
                  fumos-eval--tooling-guard mode)
                 nil
                 (list :values #'ignore
                       :error (fumos-eval--game-error-callback operation))))
          (fumos-eval--await-game-reload connection operation)
          (setq installed t)
          request-id)
      (unless installed
        (when operation
          (fumos-eval--cancel-game-reload-operation operation))))))

(defun fumos-reload-game-preserve ()
  "Reload Kristal while preserving temporary state."
  (interactive)
  (fumos-eval--reload-game "temp"))

(defun fumos-reload-game-save ()
  "Reload Kristal from the latest save."
  (interactive)
  (fumos-eval--reload-game "save"))

(defun fumos-reload-game-from-start ()
  "Reload Kristal from the beginning."
  (interactive)
  (fumos-eval--reload-game "none"))

(provide 'fumos-eval)
;;; fumos-eval.el ends here
