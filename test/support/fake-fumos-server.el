;;; fake-fumos-server.el --- Scripted loopback server -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'cl-lib)

(cl-defstruct fumos-test-server
  process client clients lines handler)

(defun fumos-test-server--consume (server client chunk)
  "Consume CHUNK from CLIENT and dispatch complete lines for SERVER."
  (let ((input (concat (or (process-get client 'fumos-input) "") chunk))
        (start 0)
        lines)
    (while (string-match "\n" input start)
      (push (substring input start (match-beginning 0)) lines)
      (setq start (match-end 0)))
    (process-put client 'fumos-input (substring input start))
    (dolist (line (nreverse lines))
      (setf (fumos-test-server-lines server)
            (append (fumos-test-server-lines server) (list line)))
      (when (fumos-test-server-handler server)
        (funcall (fumos-test-server-handler server) server client line)))))

(defun fumos-test-server-start (&optional handler)
  "Start a loopback server using HANDLER for complete input lines."
  (let ((state (make-fumos-test-server :lines nil :handler handler)))
    (setf
     (fumos-test-server-process state)
     (make-network-process
      :name "fumos-test-server"
      :server t
      :host "127.0.0.1"
      :service t
      :coding 'utf-8-unix
      :noquery t
      :log
      (lambda (_server client _message)
        (setf (fumos-test-server-client state) client
              (fumos-test-server-clients state)
              (cons client (fumos-test-server-clients state)))
        (set-process-query-on-exit-flag client nil)
        (set-process-filter
         client
         (lambda (process chunk)
           (fumos-test-server--consume state process chunk))))))
    state))

(defun fumos-test-server-port (server)
  "Return SERVER's chosen TCP port."
  (process-contact (fumos-test-server-process server) :service))

(defun fumos-test-server-send (server string)
  "Send STRING to SERVER's current client."
  (process-send-string (fumos-test-server-client server) string))

(defun fumos-test-server-send-chunks (server chunks)
  "Send each string in CHUNKS and allow each write to be observed."
  (dolist (chunk chunks)
    (fumos-test-server-send server chunk)
    (accept-process-output nil 0.01)))

(defun fumos-test-server-drop-client (server)
  "Close SERVER's current accepted client while keeping its listener alive."
  (when (process-live-p (fumos-test-server-client server))
    (delete-process (fumos-test-server-client server))))

(defun fumos-test-server-stop (server)
  "Close SERVER and every client accepted by it."
  (dolist (client (fumos-test-server-clients server))
    (when (process-live-p client)
      (delete-process client)))
  (when (process-live-p (fumos-test-server-process server))
    (delete-process (fumos-test-server-process server))))

(defmacro fumos-test-with-server (binding &rest body)
  "Bind BINDING to a fake server while evaluating BODY."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,binding (fumos-test-server-start)))
     (unwind-protect
         (progn ,@body)
       (fumos-test-server-stop ,binding))))

(provide 'support/fake-fumos-server)
;;; fake-fumos-server.el ends here
