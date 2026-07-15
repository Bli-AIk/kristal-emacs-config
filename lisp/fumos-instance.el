;;; fumos-instance.el --- Discover FUMOS game processes -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'cl-lib)
(require 'cl-print)
(require 'json)
(require 'seq)
(require 'subr-x)

(define-error 'fumos-instance-error "Invalid FUMOS instance descriptor")

(defconst fumos-instance-required-capabilities
  '("interrupt" "cancel" "detach" "source-context" "game-reload"))

(defconst fumos-instance-required-fields
  '("schema" "project_root" "mod_id" "pid" "started_at" "host" "port"
    "token" "fumos_version" "proto" "capabilities" "max_message_bytes"))

(defconst fumos-instance-max-descriptor-bytes 16384
  "Maximum accepted size of one FUMOS descriptor, in bytes.")

(defconst fumos-instance-process-time-tolerance 1
  "Seconds allowed for process-start and filesystem timestamp precision.")

(defconst fumos-instance--redacted-token "<redacted>")

(defvar fumos-instance--tokens
  (make-hash-table :test #'eq :weakness 'key)
  "Bearer tokens keyed by instance identity.")

(cl-defstruct (fumos-instance
               (:constructor fumos-instance--make)
               (:copier fumos-instance--copy))
  descriptor-file project-root mod-id pid started-at host port
  (token-redacted fumos-instance--redacted-token :read-only t)
  fumos-version proto capabilities max-message-bytes)

(defun make-fumos-instance (&rest arguments)
  "Create a FUMOS instance while keeping its bearer token out of the record."
  (let ((token (plist-get arguments :token))
        (public-arguments
         (cl-loop for (key value) on arguments by #'cddr
                  unless (memq key '(:token :token-redacted))
                  append (list key value))))
    (let ((instance
           (apply #'fumos-instance--make
                  :token-redacted fumos-instance--redacted-token
                  public-arguments)))
      (when token
        (puthash instance token fumos-instance--tokens))
      instance)))

(defun copy-fumos-instance (instance)
  "Return a copy of INSTANCE, including its separately held bearer token."
  (let ((copy (fumos-instance--copy instance)))
    (when-let* ((token (gethash instance fumos-instance--tokens)))
      (puthash copy token fumos-instance--tokens))
    copy))

(defun fumos-instance-token (instance)
  "Return INSTANCE's real bearer token without exposing it to printers."
  (unless (fumos-instance-p instance)
    (signal 'wrong-type-argument (list 'fumos-instance-p instance)))
  (gethash instance fumos-instance--tokens))

(cl-defmethod cl-print-object ((instance fumos-instance) stream)
  "Print INSTANCE without its separately held bearer token."
  (princ "#<fumos-instance " stream)
  (prin1 (fumos-instance-mod-id instance) stream)
  (princ " pid=" stream)
  (prin1 (fumos-instance-pid instance) stream)
  (princ " token=<redacted>>" stream))

(defun fumos-instance--invalid-field (name)
  "Signal a fixed descriptor field error for NAME."
  (signal 'fumos-instance-error (list :invalid-field name)))

(defun fumos-instance--invalid-path (name)
  "Signal a fixed invalid path error for NAME."
  (signal 'fumos-instance-error (list :invalid-path name)))

(defun fumos-instance--unsafe-path (name)
  "Signal a fixed unsafe path error for NAME."
  (signal 'fumos-instance-error (list :unsafe-path name)))

(defun fumos-instance--visible-string-p (value)
  "Return non-nil for a nonempty string without control characters."
  (and (stringp value)
       (not (string-empty-p value))
       (not (string-match-p "[[:cntrl:]]" value))))

(defun fumos-instance--local-absolute-path-p (path)
  "Return non-nil when PATH is a local absolute path."
  (and (fumos-instance--visible-string-p path)
       (file-name-absolute-p path)
       (condition-case nil
           (not (file-remote-p path))
         (error nil))))

(defun fumos-instance--runtime-path ()
  "Return the local absolute FUMOS runtime path without creating it."
  (let ((xdg (getenv "XDG_RUNTIME_DIR")))
    (if (or (null xdg) (string-empty-p xdg))
        (format "/tmp/fumos-%d" (user-uid))
      (unless (fumos-instance--local-absolute-path-p xdg)
        (fumos-instance--invalid-path "runtime_directory"))
      (expand-file-name "fumos" xdg))))

(defun fumos-instance--private-path-attributes (path kind mode)
  "Return PATH attributes when KIND, ownership, and exact MODE are private."
  (condition-case nil
      (when (fumos-instance--local-absolute-path-p path)
        (let* ((expanded (expand-file-name path))
               (attributes (file-attributes expanded 'integer))
               (actual-mode (and attributes (file-modes expanded 'nofollow))))
          (when (and attributes
                     (pcase kind
                       ('directory (eq t (file-attribute-type attributes)))
                       ('regular (null (file-attribute-type attributes)))
                       (_ nil))
                     (integerp (file-attribute-user-id attributes))
                     (= (file-attribute-user-id attributes) (user-uid))
                     (integerp actual-mode)
                     (= actual-mode mode))
            (when (equal (file-truename expanded) expanded)
              attributes))))
    (error nil)))

(defun fumos-instance--private-path-p (path kind mode)
  "Return non-nil when PATH has KIND, current ownership, and exact MODE."
  (and (fumos-instance--private-path-attributes path kind mode) t))

(defun fumos-instance--assert-private-path (path kind mode name)
  "Require PATH to have KIND, current ownership, and exact MODE."
  (unless (fumos-instance--private-path-p path kind mode)
    (fumos-instance--unsafe-path name))
  path)

(defun fumos-runtime-directory ()
  "Return the secure per-user FUMOS runtime directory."
  (let ((directory (fumos-instance--runtime-path))
        created)
    (unless (condition-case nil (file-exists-p directory) (error nil))
      (condition-case condition
          (with-file-modes #o700
            (make-directory directory)
            (setq created t))
        (file-already-exists nil)
        (error
         (ignore condition)
         (fumos-instance--unsafe-path "runtime_directory"))))
    (when created
      (condition-case nil
          (set-file-modes directory #o700)
        (error (fumos-instance--unsafe-path "runtime_directory"))))
    (fumos-instance--assert-private-path
     directory 'directory #o700 "runtime_directory")
    (file-name-as-directory directory)))

(defun fumos-instance--field (data name predicate)
  "Read NAME from DATA and require PREDICATE to accept it."
  (let ((value (gethash name data)))
    (unless (funcall predicate value)
      (fumos-instance--invalid-field name))
    value))

(defun fumos-instance--canonical-project-root (path)
  "Return canonical PATH when it is a local existing directory, else nil."
  (when (fumos-instance--local-absolute-path-p path)
    (condition-case nil
        (let ((canonical (file-truename path)))
          (when (and (fumos-instance--local-absolute-path-p canonical)
                     (file-directory-p canonical))
            (file-name-as-directory canonical)))
      (error nil))))

(defun fumos-instance--formattable-timestamp-p (value)
  "Return non-nil for a nonnegative integer accepted by Emacs time APIs."
  (and (integerp value)
       (>= value 0)
       (condition-case nil
           (stringp
            (format-time-string
             "%Y-%m-%d %H:%M:%S" (seconds-to-time value) t))
         (error nil))))

(defun fumos-instance--read-bytes (file)
  "Read at most the descriptor limit plus one byte from FILE."
  (condition-case nil
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally
         file nil 0 (1+ fumos-instance-max-descriptor-bytes))
        (if (> (buffer-size) fumos-instance-max-descriptor-bytes)
            :too-large
          (buffer-string)))
    (error :unavailable)))

(defun fumos-instance--same-file-snapshot-p (before after bytes)
  "Return non-nil when BEFORE and AFTER describe the bytes just read."
  (and before after
       (equal (file-attribute-file-identifier before)
              (file-attribute-file-identifier after))
       (equal (file-attribute-size before) (file-attribute-size after))
       (equal (file-attribute-modification-time before)
              (file-attribute-modification-time after))
       (= (file-attribute-size after) (string-bytes bytes))))

(defun fumos-read-instance (descriptor-file)
  "Parse and validate local private DESCRIPTOR-FILE."
  (unless (fumos-instance--local-absolute-path-p descriptor-file)
    (fumos-instance--unsafe-path "descriptor_file"))
  (let* ((file (expand-file-name descriptor-file))
         (before
          (fumos-instance--private-path-attributes file 'regular #o600)))
    (unless before
      (fumos-instance--unsafe-path "descriptor_file"))
    (unless (integerp (file-attribute-size before))
      (signal 'fumos-instance-error '(:descriptor-unavailable)))
    (when (> (file-attribute-size before)
             fumos-instance-max-descriptor-bytes)
      (signal 'fumos-instance-error '(:descriptor-too-large)))
    (let ((bytes (fumos-instance--read-bytes file)))
      (pcase bytes
        (:too-large
         (signal 'fumos-instance-error '(:descriptor-too-large)))
        (:unavailable
         (signal 'fumos-instance-error '(:descriptor-unavailable))))
      (let ((after
             (fumos-instance--private-path-attributes file 'regular #o600)))
        (unless (fumos-instance--same-file-snapshot-p before after bytes)
          (signal 'fumos-instance-error '(:descriptor-unavailable))))
      (let* ((data
              (condition-case nil
                  (json-parse-string
                   bytes :object-type 'hash-table :array-type 'array
                   :null-object :fumos-json-null
                   :false-object :fumos-json-false)
                (error
                 (signal 'fumos-instance-error '(:invalid-json)))))
             (_object
              (unless (hash-table-p data)
                (fumos-instance--invalid-field "descriptor_object")))
             (keys (hash-table-keys data))
             (_exact-fields
              (unless
                  (and (= (length keys)
                          (length fumos-instance-required-fields))
                       (equal
                        (sort (copy-sequence keys) #'string<)
                        (sort (copy-sequence fumos-instance-required-fields)
                              #'string<)))
                (fumos-instance--invalid-field "descriptor_fields")))
             (schema (fumos-instance--field data "schema" #'integerp))
             (root-value
              (fumos-instance--field data "project_root" #'stringp))
             (project-root
              (fumos-instance--canonical-project-root root-value))
             (mod-id
              (fumos-instance--field
               data "mod_id" #'fumos-instance--visible-string-p))
             (pid
              (fumos-instance--field
               data "pid" (lambda (value)
                            (and (integerp value) (> value 0)))))
             (started-at
              (fumos-instance--field
               data "started_at" #'fumos-instance--formattable-timestamp-p))
             (host (fumos-instance--field data "host" #'stringp))
             (port
              (fumos-instance--field
               data "port" (lambda (value)
                             (and (integerp value) (<= 1 value 65535)))))
             (token
              (fumos-instance--field
               data "token" (lambda (value)
                              (and (stringp value)
                                   (string-match-p
                                    "\\`[0-9a-f]\\{64\\}\\'" value)))))
             (fumos-version
              (fumos-instance--field
               data "fumos_version" #'fumos-instance--visible-string-p))
             (proto (fumos-instance--field data "proto" #'stringp))
             (capability-values
              (fumos-instance--field
               data "capabilities"
               (lambda (value)
                 (and (vectorp value)
                      (equal (append value nil)
                             fumos-instance-required-capabilities)))))
             (capabilities (append capability-values nil))
             (max-bytes
              (fumos-instance--field
               data "max_message_bytes" #'natnump)))
        (unless (= schema 1)
          (fumos-instance--invalid-field "schema"))
        (unless project-root
          (fumos-instance--invalid-field "project_root"))
        (unless (equal host "127.0.0.1")
          (fumos-instance--invalid-field "host"))
        (unless (equal proto "0.6.4")
          (fumos-instance--invalid-field "proto"))
        (unless (= max-bytes 8388608)
          (fumos-instance--invalid-field "max_message_bytes"))
        (unless (equal (file-name-nondirectory file)
                       (format "%d.json" pid))
          (fumos-instance--invalid-field "descriptor_file"))
        (make-fumos-instance
         :descriptor-file file :project-root project-root :mod-id mod-id
         :pid pid :started-at started-at :host host :port port :token token
         :fumos-version fumos-version :proto proto :capabilities capabilities
         :max-message-bytes max-bytes)))))

(defun fumos-instance-stale-p (instance)
  "Return non-nil if INSTANCE is stale, foreign-owned, or PID-reused."
  (condition-case nil
      (let* ((attributes (process-attributes (fumos-instance-pid instance)))
             (euid (and attributes (alist-get 'euid attributes)))
             (start (and attributes (alist-get 'start attributes)))
             (descriptor-attributes
              (fumos-instance--private-path-attributes
               (fumos-instance-descriptor-file instance) 'regular #o600))
             (mtime
              (and descriptor-attributes
                   (file-attribute-modification-time
                    descriptor-attributes))))
        (or (null attributes)
            (not (integerp euid))
            (/= euid (user-uid))
            (null start)
            (null mtime)
            (null (time-convert start 'list))
            (time-less-p
             (time-add
              mtime (seconds-to-time fumos-instance-process-time-tolerance))
             start)))
    (error t)))

(defun fumos-discover-instances (&optional project-root)
  "Return valid live instances for PROJECT-ROOT, newest first."
  (let ((canonical-root
         (when project-root
           (or (fumos-instance--canonical-project-root project-root)
               (fumos-instance--invalid-field "project_root"))))
        (runtime (fumos-runtime-directory))
        instances)
    (dolist
        (file
         (condition-case nil
             (directory-files runtime t "\\.json\\'")
           (error (fumos-instance--unsafe-path "runtime_directory"))))
      (condition-case nil
          (let ((instance (fumos-read-instance file)))
            (when (and (not (fumos-instance-stale-p instance))
                       (or (null canonical-root)
                           (equal canonical-root
                                  (fumos-instance-project-root instance))))
              (push instance instances)))
        (error nil)))
    (sort instances
          (lambda (left right)
            (> (fumos-instance-started-at left)
               (fumos-instance-started-at right))))))

(defun fumos-instance--label (instance)
  "Return a token-free selection label for INSTANCE."
  (format "%s | PID %d | started %s"
          (fumos-instance-mod-id instance)
          (fumos-instance-pid instance)
          (format-time-string
           "%Y-%m-%d %H:%M:%S"
           (seconds-to-time (fumos-instance-started-at instance)))))

(defun fumos-select-instance (instances)
  "Select one instance from INSTANCES."
  (pcase instances
    ('()
     (user-error
      "No FUMOS instance is running; start Kristal with Mod.info.dev=true"))
    (`(,only) only)
    (_
     (let ((instances-by-label (make-hash-table :test #'equal))
           labels)
       (dolist (instance instances)
         (let ((label (fumos-instance--label instance)))
           (puthash label instance instances-by-label)
           (push label labels)))
       (let ((choice
              (completing-read "FUMOS instance: " (nreverse labels) nil t)))
         (gethash choice instances-by-label))))))

(provide 'fumos-instance)
;;; fumos-instance.el ends here
