;;; fumos-instance-test.el --- FUMOS instance tests -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'cl-lib)
(require 'json)
(require 'test-helper)
(require 'fumos-instance)

(defconst fumos-test-token (make-string 64 ?a))

(defun fumos-test-instance-document (root pid started-at &optional token)
  "Return a complete valid descriptor for ROOT, PID, and STARTED-AT."
  (let ((document (make-hash-table :test #'equal)))
    (dolist (entry
             `(("schema" . 1)
               ("project_root" . ,(file-truename root))
               ("mod_id" . "demo")
               ("pid" . ,pid)
               ("started_at" . ,started-at)
               ("host" . "127.0.0.1")
               ("port" . 49152)
               ("token" . ,(or token fumos-test-token))
               ("fumos_version" . "0.1.0")
               ("proto" . "0.6.4")
               ("capabilities" . ["interrupt" "cancel" "detach"
                                   "source-context" "game-reload"])
               ("max_message_bytes" . 8388608)))
      (puthash (car entry) (cdr entry) document))
    document))

(defun fumos-test-write-instance
    (directory pid root started-at &optional mutate token)
  "Write a complete descriptor in DIRECTORY, optionally applying MUTATE."
  (let ((path (expand-file-name (format "%d.json" pid) directory))
        (document (fumos-test-instance-document root pid started-at token)))
    (when mutate
      (funcall mutate document))
    (with-temp-file path
      (insert (json-serialize document) "\n"))
    (set-file-modes path #o600)
    path))

(defun fumos-test-rewrite-field (path field value)
  "Mutate only FIELD to VALUE in the complete descriptor at PATH."
  (let ((document
         (with-temp-buffer
           (insert-file-contents path)
           (json-parse-buffer :object-type 'hash-table :array-type 'array))))
    (puthash field value document)
    (with-temp-file path
      (insert (json-serialize document) "\n"))
    (set-file-modes path #o600)))

(defun fumos-test-instance-error-data (thunk)
  "Run THUNK and return fixed `fumos-instance-error' condition data."
  (condition-case condition
      (progn
        (funcall thunk)
        (ert-fail "Expected fumos-instance-error"))
    (fumos-instance-error (cdr condition))))

(defmacro fumos-test-with-process (binding &rest body)
  "Start a sleeping process as BINDING while evaluating BODY."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,binding
          (make-process
           :name (generate-new-buffer-name "fumos-test-process")
           :command '("sleep" "30")
           :noquery t)))
     (unwind-protect
         (progn ,@body)
       (when (process-live-p ,binding)
         (delete-process ,binding)))))

(ert-deftest fumos-runtime-directory-follows-xdg-with-private-create-mode ()
  (fumos-test-with-directory xdg
    (let ((process-environment (copy-sequence process-environment))
          (real-make-directory (symbol-function 'make-directory))
          create-mode)
      (setenv "XDG_RUNTIME_DIR" xdg)
      (cl-letf (((symbol-function 'make-directory)
                 (lambda (directory &optional parents)
                   (setq create-mode (default-file-modes))
                   (funcall real-make-directory directory parents))))
        (should
         (equal (file-name-as-directory (expand-file-name "fumos" xdg))
                (fumos-runtime-directory))))
      (should (= #o700 (logand create-mode #o777)))
      (should (= #o700 (file-modes (fumos-runtime-directory)))))))

(ert-deftest fumos-runtime-directory-empty-xdg-uses-private-tmp-fallback ()
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "XDG_RUNTIME_DIR" "")
    (cl-letf (((symbol-function 'user-uid) (lambda () 4242)))
      (should (equal "/tmp/fumos-4242"
                     (fumos-instance--runtime-path))))))

(ert-deftest fumos-runtime-directory-rejects-relative-and-remote-xdg ()
  (dolist (xdg '("relative/runtime" "/ssh:example.invalid:/tmp"))
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "XDG_RUNTIME_DIR" xdg)
      (should
       (equal '(:invalid-path "runtime_directory")
              (fumos-test-instance-error-data
               (lambda () (fumos-instance--runtime-path))))))))

(ert-deftest fumos-runtime-directory-rejects-unsafe-existing-paths ()
  (fumos-test-with-directory xdg
    (fumos-test-with-directory target
      (let* ((process-environment (copy-sequence process-environment))
             (runtime (expand-file-name "fumos" xdg)))
        (setenv "XDG_RUNTIME_DIR" xdg)
        (make-symbolic-link target runtime)
        (should
         (equal '(:unsafe-path "runtime_directory")
                (fumos-test-instance-error-data #'fumos-runtime-directory)))
        (delete-file runtime)
        (make-directory runtime)
        (set-file-modes runtime #o755)
        (should
         (equal '(:unsafe-path "runtime_directory")
                (fumos-test-instance-error-data #'fumos-runtime-directory)))
        (set-file-modes runtime #o700)
        (let ((real-file-attributes (symbol-function 'file-attributes)))
          (cl-letf (((symbol-function 'file-attributes)
                     (lambda (file &optional id-format)
                       (let ((attributes
                              (copy-sequence
                               (funcall real-file-attributes file id-format))))
                         (setf (nth 2 attributes) (1+ (user-uid)))
                         attributes))))
            (should
             (equal '(:unsafe-path "runtime_directory")
                    (fumos-test-instance-error-data
                     #'fumos-runtime-directory)))))))))

(ert-deftest fumos-private-path-check-fails-closed-on-mode-races ()
  (fumos-test-with-directory directory
    (let ((path (expand-file-name "42.json" directory)))
      (with-temp-file path (insert "{}\n"))
      (set-file-modes path #o600)
      (dolist (mode '(nil "0600"))
        (cl-letf (((symbol-function 'file-modes)
                   (lambda (&rest _) mode)))
          (should-not
           (fumos-instance--private-path-p path 'regular #o600))))
      (let ((real-file-modes (symbol-function 'file-modes)))
        (cl-letf (((symbol-function 'file-modes)
                   (lambda (file &optional flag)
                     (delete-file file)
                     (funcall real-file-modes file flag))))
          (should-not
           (fumos-instance--private-path-p path 'regular #o600)))))))

(ert-deftest fumos-read-instance-validates-and-redacts-native-printing ()
  (fumos-test-with-directory runtime
    (fumos-test-with-directory root
      (let* ((path (fumos-test-write-instance runtime 42 root 100))
             (instance (fumos-read-instance path))
             (copy (copy-fumos-instance instance))
             (condition (list 'fumos-instance-error instance)))
        (should (equal fumos-test-token (fumos-instance-token instance)))
        (should (equal fumos-test-token (fumos-instance-token copy)))
        (should (equal (file-name-as-directory (file-truename root))
                       (fumos-instance-project-root instance)))
        (dolist (rendered
                 (list (format "%S" instance)
                       (format "%S" copy)
                       (format "%S" (list :instance instance))
                       (cl-prin1-to-string instance)
                       (error-message-string condition)))
          (should (string-match-p "<redacted>" rendered))
          (should-not
           (string-match-p (regexp-quote fumos-test-token) rendered)))))))

(ert-deftest fumos-read-instance-requires-exact-fields-capabilities-and-name ()
  (fumos-test-with-directory runtime
    (fumos-test-with-directory root
      (let ((path (fumos-test-write-instance runtime 42 root 100)))
        (fumos-test-rewrite-field path "capabilities"
                                  ["cancel" "interrupt" "detach"
                                   "source-context" "game-reload"])
        (should
         (equal '(:invalid-field "capabilities")
                (fumos-test-instance-error-data
                 (lambda () (fumos-read-instance path)))))
        (setq path (fumos-test-write-instance runtime 42 root 100
                                              (lambda (document)
                                                (puthash "extra" t document))))
        (should
         (equal '(:invalid-field "descriptor_fields")
                (fumos-test-instance-error-data
                 (lambda () (fumos-read-instance path)))))
        (setq path (fumos-test-write-instance runtime 42 root 100))
        (let ((renamed (expand-file-name "43.json" runtime)))
          (rename-file path renamed)
          (should
           (equal '(:invalid-field "descriptor_file")
                  (fumos-test-instance-error-data
                   (lambda () (fumos-read-instance renamed))))))))))

(ert-deftest fumos-read-instance-validates-each-complete-json-field ()
  (fumos-test-with-directory runtime
    (fumos-test-with-directory root
      (dolist (case
               `(("schema" 2)
                 ("mod_id" "")
                 ("mod_id" "bad\nmod")
                 ("pid" 0)
                 ("pid" -1)
                 ("started_at" -1)
                 ("started_at" 1.5)
                 ("started_at" ,(ash 1 62))
                 ("host" "0.0.0.0")
                 ("port" 0)
                 ("token" ,(make-string 63 ?a))
                 ("fumos_version" "")
                 ("fumos_version" "bad\tversion")
                 ("proto" "0.6.3")
                 ("max_message_bytes" 1024)))
        (pcase-let ((`(,field ,value) case))
          (let ((path (fumos-test-write-instance runtime 42 root 100)))
            (fumos-test-rewrite-field path field value)
            (should
             (equal (list :invalid-field field)
                    (fumos-test-instance-error-data
                     (lambda () (fumos-read-instance path)))))))))))

(ert-deftest fumos-read-instance-rejects-relative-remote-missing-and-loop-roots ()
  (fumos-test-with-directory runtime
    (fumos-test-with-directory root
      (let ((missing (expand-file-name "missing" root))
            (loop-a (expand-file-name "loop-a" root))
            (loop-b (expand-file-name "loop-b" root)))
        (make-symbolic-link loop-b loop-a)
        (make-symbolic-link loop-a loop-b)
        (dolist (invalid-root
                 (list "relative/root" "/ssh:example.invalid:/tmp"
                       missing loop-a))
          (let ((path (fumos-test-write-instance runtime 42 root 100)))
            (fumos-test-rewrite-field path "project_root" invalid-root)
            (should
             (equal '(:invalid-field "project_root")
                    (fumos-test-instance-error-data
                     (lambda () (fumos-read-instance path)))))))))))

(ert-deftest fumos-read-instance-rejects-oversize-before-parsing ()
  (fumos-test-with-directory runtime
    (fumos-test-with-directory root
      (let ((path (fumos-test-write-instance runtime 42 root 100)))
        (with-temp-buffer
          (insert-file-contents path)
          (goto-char (point-max))
          (insert (make-string (1+ fumos-instance-max-descriptor-bytes) ?\s))
          (write-region (point-min) (point-max) path nil 'silent))
        (set-file-modes path #o600)
        (let ((data
               (fumos-test-instance-error-data
                (lambda () (fumos-read-instance path)))))
          (should (equal '(:descriptor-too-large) data))
          (should-not
           (string-match-p fumos-test-token (format "%S" data))))))))

(ert-deftest fumos-read-instance-rejects-same-basename-symlink-and-bad-mode ()
  (fumos-test-with-directory first
    (fumos-test-with-directory second
      (fumos-test-with-directory root
        (let* ((target (fumos-test-write-instance second 42 root 100))
               (link (expand-file-name "42.json" first)))
          (make-symbolic-link target link)
          (should
           (equal '(:unsafe-path "descriptor_file")
                  (fumos-test-instance-error-data
                   (lambda () (fumos-read-instance link)))))
          (delete-file link)
          (setq target (fumos-test-write-instance first 42 root 100))
          (set-file-modes target #o400)
          (should
           (equal '(:unsafe-path "descriptor_file")
                  (fumos-test-instance-error-data
                   (lambda () (fumos-read-instance target))))))))))

(ert-deftest fumos-read-instance-rejects-owner-and-read-races-with-fixed-code ()
  (fumos-test-with-directory runtime
    (fumos-test-with-directory root
      (let ((path (fumos-test-write-instance runtime 42 root 100))
            (real-file-attributes (symbol-function 'file-attributes)))
        (cl-letf (((symbol-function 'file-attributes)
                   (lambda (file &optional id-format)
                     (let ((attributes
                            (copy-sequence
                             (funcall real-file-attributes file id-format))))
                       (setf (nth 2 attributes) (1+ (user-uid)))
                       attributes))))
          (should
           (equal '(:unsafe-path "descriptor_file")
                  (fumos-test-instance-error-data
                   (lambda () (fumos-read-instance path))))))
        (let ((real-insert (symbol-function 'insert-file-contents-literally)))
          (cl-letf (((symbol-function 'insert-file-contents-literally)
                     (lambda (file &rest arguments)
                       (delete-file file)
                       (apply real-insert file arguments))))
            (should
             (equal '(:descriptor-unavailable)
                    (fumos-test-instance-error-data
                     (lambda () (fumos-read-instance path)))))))))))

(ert-deftest fumos-invalid-documents-and-token-never-leak ()
  (fumos-test-with-directory runtime
    (fumos-test-with-directory root
      (let* ((secret (concat (make-string 63 ?A) "Z"))
             (path (fumos-test-write-instance runtime 42 root 100))
             (messages (get-buffer-create "*Messages*"))
             (message-start (with-current-buffer messages (point-max))))
        (dolist (writer
                 (list
                  (lambda ()
                    (fumos-test-write-instance
                     runtime 42 root 100
                     (lambda (document) (puthash "token" secret document))))
                  (lambda ()
                    (with-temp-file path
                      (insert "{\"token\":\"" secret "\""))
                    (set-file-modes path #o600))
                  (lambda ()
                    (with-temp-file path
                      (insert (json-serialize (vector secret)) "\n"))
                    (set-file-modes path #o600))))
          (funcall writer)
          (let ((condition
                 (condition-case caught
                     (progn
                       (fumos-read-instance path)
                       (ert-fail "Expected rejected secret"))
                   (fumos-instance-error caught))))
            (dolist (rendered
                     (list (format "%S" condition)
                           (error-message-string condition)))
              (should-not
               (string-match-p (regexp-quote secret) rendered)))))
        (with-current-buffer messages
          (should-not
           (string-match-p
            (regexp-quote secret)
            (buffer-substring-no-properties message-start (point-max)))))))))

(ert-deftest fumos-instance-stale-checks-real-pid-euid-and-start-time ()
  (fumos-test-with-directory runtime
    (fumos-test-with-directory root
      (let* ((pid (emacs-pid))
             (path (fumos-test-write-instance runtime pid root 100))
             (instance (fumos-read-instance path))
             (attributes (process-attributes pid))
             (start (alist-get 'start attributes)))
        (should-not (fumos-instance-stale-p instance))
        (cl-letf (((symbol-function 'process-attributes)
                   (lambda (_pid)
                     `((euid . ,(1+ (user-uid))) (start . ,start)))))
          (should (fumos-instance-stale-p instance)))
        (set-file-times path (time-subtract start (seconds-to-time 10)))
        (should (fumos-instance-stale-p instance))))))

(ert-deftest fumos-discovery-uses-real-live-stale-and-foreign-root-evidence ()
  (fumos-test-with-directory xdg
    (fumos-test-with-directory root
      (fumos-test-with-directory foreign-root
        (let ((process-environment (copy-sequence process-environment))
              (stale-pid 99999999))
          (setenv "XDG_RUNTIME_DIR" xdg)
          (should-not (process-attributes stale-pid))
          (let ((runtime (fumos-runtime-directory)))
            (fumos-test-write-instance runtime (emacs-pid) root 200)
            (fumos-test-write-instance runtime stale-pid root 300)
            (should
             (equal (list (emacs-pid))
                    (mapcar #'fumos-instance-pid
                            (fumos-discover-instances root))))
            (should-not (fumos-discover-instances foreign-root))))))))

(ert-deftest fumos-discovery-sorts-two-real-processes-newest-first ()
  (fumos-test-with-directory xdg
    (fumos-test-with-directory root
      (fumos-test-with-process first
        (fumos-test-with-process second
          (let ((process-environment (copy-sequence process-environment)))
            (setenv "XDG_RUNTIME_DIR" xdg)
            (let ((runtime (fumos-runtime-directory))
                  (first-pid (process-id first))
                  (second-pid (process-id second)))
              (fumos-test-write-instance runtime first-pid root 100)
              (fumos-test-write-instance runtime second-pid root 200)
              (should
               (equal (list second-pid first-pid)
                      (mapcar #'fumos-instance-pid
                              (fumos-discover-instances root)))))))))))

(ert-deftest fumos-select-instance-does-not-display-token ()
  (let* ((first (make-fumos-instance :mod-id "first" :pid 10
                                     :started-at 100
                                     :token fumos-test-token))
         (second (make-fumos-instance :mod-id "second" :pid 20
                                      :started-at 200
                                      :token fumos-test-token))
         prompt-candidates)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt candidates &rest _arguments)
                 (setq prompt-candidates candidates)
                 (car (last candidates)))))
      (should (eq second (fumos-select-instance (list first second)))))
    (should-not
     (string-match-p fumos-test-token (format "%S" prompt-candidates)))))

(provide 'fumos-instance-test)
;;; fumos-instance-test.el ends here
