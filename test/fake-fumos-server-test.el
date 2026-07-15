;;; fake-fumos-server-test.el --- Fake server tests -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'test-helper)
(require 'support/fake-fumos-server)

(defun fumos-test-open-network-client (server name &optional filter sentinel)
  "Connect a noquery client named NAME to SERVER.
Use FILTER and SENTINEL when they are non-nil."
  (make-network-process
   :name name
   :host "127.0.0.1"
   :service (fumos-test-server-port server)
   :filter filter
   :sentinel sentinel
   :coding 'utf-8-unix
   :noquery t))

(ert-deftest fumos-fake-server-reassembles-lines ()
  (fumos-test-with-server server
    (let ((client (fumos-test-open-network-client
                   server "fumos-test-reassembly-client")))
      (unwind-protect
          (progn
            (process-send-string client "FUMOS/1 AU")
            (should
             (fumos-test-wait-until
              (lambda ()
                (when-let* ((accepted (fumos-test-server-client server)))
                  (equal "FUMOS/1 AU"
                         (process-get accepted 'fumos-input))))))
            (should-not (fumos-test-server-lines server))
            (process-send-string client "TH secret\nfirst\nsecond\n")
            (should
             (fumos-test-wait-until
              (lambda () (= 3 (length (fumos-test-server-lines server))))))
            (should
             (equal '("FUMOS/1 AUTH secret" "first" "second")
                    (fumos-test-server-lines server))))
        (when (process-live-p client)
          (delete-process client))))))

(ert-deftest fumos-fake-server-dispatches-scripted-lines ()
  (fumos-test-with-server server
    (let (handled received)
      (setf (fumos-test-server-handler server)
            (lambda (state client line)
              (setq handled (append handled (list line)))
              (fumos-test-server-send
               state (concat "reply:" line "\n") client)))
      (let ((client
             (fumos-test-open-network-client
              server "fumos-test-script-client"
              (lambda (_process chunk)
                (setq received (concat received chunk))))))
        (unwind-protect
            (progn
              (process-send-string client "one\ntwo\n")
              (should
               (fumos-test-wait-until
                (lambda ()
                  (and (equal '("one" "two") handled)
                       (equal "reply:one\nreply:two\n" received))))))
          (when (process-live-p client)
            (delete-process client)))))))

(ert-deftest fumos-fake-server-can-fragment-output ()
  (fumos-test-with-server server
    (let (received chunks)
      (let ((client
             (fumos-test-open-network-client
              server "fumos-test-fragment-client"
              (lambda (_process chunk)
                (setq chunks (append chunks (list chunk)))
                (setq received (concat received chunk))))))
        (unwind-protect
            (progn
              (should
               (fumos-test-wait-until
                (lambda () (fumos-test-server-client server))))
              (fumos-test-server-send server "alpha\n")
              (should
               (fumos-test-wait-until
                (lambda () (equal '("alpha\n") chunks))))
              (fumos-test-server-send server "beta\n")
              (should
               (fumos-test-wait-until
                (lambda ()
                  (and (equal received "alpha\nbeta\n")
                       (equal '("alpha\n" "beta\n") chunks))))))
          (when (process-live-p client)
            (delete-process client)))))))

(ert-deftest fumos-fake-server-routes-replies-to-originating-client ()
  (fumos-test-with-server server
    (let (first-output second-output first second)
      (setf (fumos-test-server-handler server)
            (lambda (state client line)
              (fumos-test-server-send state (concat line "\n") client)))
      (unwind-protect
          (progn
            (setq first
                  (fumos-test-open-network-client
                   server "fumos-test-first-client"
                   (lambda (_process chunk)
                     (setq first-output (concat first-output chunk)))))
            (should
             (fumos-test-wait-until
              (lambda () (= 1 (length (fumos-test-server-clients server))))))
            (setq second
                  (fumos-test-open-network-client
                   server "fumos-test-second-client"
                   (lambda (_process chunk)
                     (setq second-output (concat second-output chunk)))))
            (should
             (fumos-test-wait-until
              (lambda () (= 2 (length (fumos-test-server-clients server))))))
            (process-send-string first "from-first\n")
            (should
             (fumos-test-wait-until
              (lambda () (equal "from-first\n" first-output))))
            (should-not second-output))
        (dolist (client (list first second))
          (when (process-live-p client)
            (delete-process client)))))))

(ert-deftest fumos-fake-server-can-drop-its-client ()
  (fumos-test-with-server server
    (let (client-status)
      (let ((client
             (fumos-test-open-network-client
              server "fumos-test-drop-client" nil
              (lambda (process _event)
                (setq client-status (process-status process))))))
        (unwind-protect
            (progn
              (should
               (fumos-test-wait-until
                (lambda () (fumos-test-server-client server))))
              (let ((accepted (fumos-test-server-client server)))
                (fumos-test-server-drop-client server)
                (should-not (process-live-p accepted))
                (should
                 (fumos-test-wait-until
                  (lambda () (not (process-live-p client)))))
                (should (memq client-status '(closed failed)))
                (should (eq client-status (process-status client)))
                (should
                 (process-live-p (fumos-test-server-process server)))))
          (when (process-live-p client)
            (delete-process client)))))))

(ert-deftest fumos-fake-server-processes-are-noquery-and-always-cleaned ()
  (let (listener accepted client)
    (unwind-protect
        (progn
          (catch 'fumos-test-cleanup
            (fumos-test-with-server server
              (setq listener (fumos-test-server-process server)
                    client
                    (fumos-test-open-network-client
                     server "fumos-test-cleanup-client"))
              (should
               (fumos-test-wait-until
                (lambda () (fumos-test-server-client server))))
              (setq accepted (fumos-test-server-client server))
              (should-not (process-query-on-exit-flag listener))
              (should-not (process-query-on-exit-flag accepted))
              (should-not (process-query-on-exit-flag client))
              (throw 'fumos-test-cleanup t)))
          (should listener)
          (should accepted)
          (should-not (process-live-p listener))
          (should-not (process-live-p accepted)))
      (when (process-live-p client)
        (delete-process client)))))

;;; fake-fumos-server-test.el ends here
