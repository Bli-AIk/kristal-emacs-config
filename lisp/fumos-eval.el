;;; fumos-eval.el --- Explicit FUMOS evaluation commands -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'cl-lib)
(require 'compile)
(require 'eldoc)
(require 'subr-x)
(require 'thingatpt)
(require 'xref)
(require 'fumos-repl)

(declare-function lua-mode "lua-mode")

(defcustom fumos-game-reload-timeout 30.0
  "Seconds allowed for a same-process Kristal game reload to reattach."
  :type 'number
  :group 'fennel-proto-repl)

(defun fumos-eval--connection ()
  "Return the current FUMOS connection."
  (or (fumos-repl-current-connection)
      (user-error "No FUMOS connection")))

(cl-defstruct (fumos-source-authority
               (:constructor fumos-source-authority--create))
  connection process generation root-input root mod-id candidate file relative
  virtual source-buffer)

(cl-defstruct (fumos-locus-record
               (:constructor fumos-locus-record--create))
  authority line column column-unit token pinned-buffer request-marker)

(cl-defstruct (fumos-request-context
               (:constructor fumos-request-context--create))
  connection process generation authority marker marker-line marker-column
  marker-byte-column column-unit)

(defun fumos-eval--valid-mod-id-p (value)
  "Return non-nil when VALUE is exactly one safe mod path segment."
  (and (stringp value)
       (not (string-empty-p value))
       (not (member value '("." "..")))
       (not (string-match-p "[/\\\\\r\n\0]" value))))

(defun fumos-eval--valid-relative-path-p (value)
  "Return non-nil when VALUE is a strict project-relative Fennel path."
  (and (stringp value)
       (not (string-empty-p value))
       (not (file-remote-p value))
       (not (file-name-absolute-p value))
       (not (string-match-p "[\\\\\r\n\0]" value))
       (string-match-p "\\.fnlm?\\'" value)
       (cl-every
        (lambda (segment) (not (member segment '("" "." ".."))))
        (split-string value "/" nil))))

(defun fumos-eval--canonical-root (connection)
  "Return CONNECTION's authenticated root input and canonical directory."
  (let* ((instance (fumos-connection-instance connection))
         (value (fumos-instance-project-root instance)))
    (when (or (not (stringp value))
              (file-remote-p value)
              (not (file-name-absolute-p value)))
      (user-error "FUMOS project root is not local and absolute"))
    (let* ((input (file-name-as-directory (expand-file-name value)))
           (canonical
            (condition-case nil
                (file-name-as-directory (file-truename input))
              (error
               (user-error "Cannot canonicalize FUMOS project root")))))
      (list input canonical))))

(defun fumos-eval--relative-from-wire (path mod-id)
  "Return PATH relative to MOD-ID, rejecting every ambiguous spelling."
  (when (or (not (stringp path))
            (file-remote-p path)
            (file-name-absolute-p path)
            (string-match-p "[\\\\\r\n\0]" path))
    (user-error "Invalid FUMOS wire source path"))
  (let* ((mods-prefix "mods/")
         (owned-prefix (format "mods/%s/" mod-id))
         (relative
          (cond
           ((string-prefix-p owned-prefix path)
            (substring path (length owned-prefix)))
           ((string-prefix-p mods-prefix path)
            (user-error "FUMOS source belongs to another mod"))
           (t path))))
    (unless (fumos-eval--valid-relative-path-p relative)
      (user-error "Invalid FUMOS project-relative source path"))
    relative))

(defun fumos-eval--authority
    (connection path &optional absolute-source source-buffer)
  "Authenticate PATH for captured CONNECTION and return one authority.

When ABSOLUTE-SOURCE is non-nil PATH must be a local absolute visited file.
Otherwise PATH is an untrusted wire path."
  (unless (fumos-connection-p connection)
    (user-error "No FUMOS connection"))
  (let* ((instance (fumos-connection-instance connection))
         (mod-id (fumos-instance-mod-id instance)))
    (unless (fumos-eval--valid-mod-id-p mod-id)
      (user-error "FUMOS mod ID is not one path segment"))
    (pcase-let* ((`(,root-input ,root)
                   (fumos-eval--canonical-root connection))
                  (candidate
                   (if absolute-source
                       (progn
                         (when (or (not (stringp path))
                                   (file-remote-p path)
                                   (not (file-name-absolute-p path)))
                           (user-error
                            "FUMOS source must be a local absolute file"))
                         (expand-file-name path))
                     (expand-file-name
                      (fumos-eval--relative-from-wire path mod-id) root)))
                  (file
                   (condition-case nil
                       (file-truename candidate)
                     (error
                      (user-error "Cannot canonicalize FUMOS source file")))))
      (unless (and (not (file-remote-p candidate))
                   (file-in-directory-p file root)
                   (string-match-p "\\.fnlm?\\'" file)
                   (file-regular-p file))
        (user-error "File is outside the attached FUMOS project"))
      (let ((relative
             (if absolute-source
                 (file-relative-name file root)
               (fumos-eval--relative-from-wire path mod-id))))
        (unless (fumos-eval--valid-relative-path-p relative)
          (user-error "Invalid FUMOS project-relative source path"))
        (fumos-source-authority--create
         :connection connection
         :process (fumos-connection-process connection)
         :generation (fumos-connection-generation connection)
         :root-input root-input :root root :mod-id mod-id
         :candidate candidate :file file :relative relative
         :virtual (concat "mods/" mod-id "/" relative)
         :source-buffer
         (and (buffer-live-p source-buffer) source-buffer))))))

(defun fumos-eval--wire-authority (connection path &optional context)
  "Return a fail-closed authority for untrusted PATH, or nil."
  (condition-case nil
      (let* ((authority (fumos-eval--authority connection path))
             (captured (and (fumos-request-context-p context)
                            (fumos-request-context-authority context))))
        (when (and (fumos-source-authority-p captured)
                   (equal (fumos-source-authority-file authority)
                          (fumos-source-authority-file captured)))
          (setf (fumos-source-authority-source-buffer authority)
                (fumos-source-authority-source-buffer captured)))
        authority)
    ((error quit) nil)))

(defun fumos-eval--authority-current-file (connection)
  "Return source authority for the current visited buffer."
  (when buffer-file-name
    (fumos-eval--authority connection buffer-file-name t (current-buffer))))

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
           (authority (fumos-eval--authority-current-file connection)))
      (save-restriction
        (widen)
        (save-excursion
          (goto-char position)
          (let ((prefix
                 (buffer-substring-no-properties
                  (line-beginning-position) (point))))
            (list
             :file (fumos-source-authority-virtual authority)
             :line (line-number-at-pos (point) t)
             :column
             (1+ (string-bytes
                  (encode-coding-string prefix 'utf-8-unix)))
             :authority authority)))))))

(defun fumos-source-context-at-position (position)
  "Return authenticated wire source context for buffer POSITION."
  (fumos-eval--source position))

(defun fumos-eval--authority-live-buffer (authority)
  "Return AUTHORITY's matching live file buffer, if any."
  (let ((source (fumos-source-authority-source-buffer authority))
        (file (fumos-source-authority-file authority)))
    (or
     (and
      (buffer-live-p source)
      (with-current-buffer source
        (and (stringp buffer-file-name)
             (not (file-remote-p buffer-file-name))
             (condition-case nil
                 (equal file (file-truename buffer-file-name))
               (file-error nil))))
      source)
     (get-file-buffer file))))

(defun fumos-eval--authority-revalidate (authority)
  "Return AUTHORITY's canonical file while its filesystem identity is safe."
  (condition-case nil
      (let* ((root-input (fumos-source-authority-root-input authority))
             (root (file-name-as-directory
                    (file-truename root-input)))
             (candidate (fumos-source-authority-candidate authority))
             (file (file-truename candidate)))
        (and (not (file-remote-p root-input))
             (not (file-remote-p candidate))
             (equal root (fumos-source-authority-root authority))
             (equal file (fumos-source-authority-file authority))
             (file-in-directory-p file root)
             (string-match-p "\\.fnlm?\\'" file)
             (file-regular-p file)
             file))
    ((error quit) nil)))

(defun fumos-eval--line-string-from-live-buffer (buffer line)
  "Return decoded LINE from widened live BUFFER, or nil."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (when (and (integerp line) (> line 0)
                 (<= line (line-number-at-pos (point-max) t)))
        (save-excursion
          (goto-char (point-min))
          (when (zerop (forward-line (1- line)))
            (buffer-substring-no-properties
             (line-beginning-position) (line-end-position))))))))

(defun fumos-eval--strict-utf8-decode (bytes)
  "Decode unibyte UTF-8 BYTES, rejecting truncated or malformed sequences."
  (let ((decoded (decode-coding-string bytes 'utf-8-unix)))
    (when (and (equal bytes (encode-coding-string decoded 'utf-8-unix))
               (cl-every (lambda (character)
                           (not (eq 'eight-bit (char-charset character))))
                         (string-to-list decoded)))
      decoded)))

(defun fumos-eval--line-string-from-file (file line)
  "Return strictly decoded UTF-8 LINE from FILE, or nil."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (when (and (integerp line) (> line 0)
               (<= line (line-number-at-pos (point-max) t)))
      (goto-char (point-min))
      (when (zerop (forward-line (1- line)))
        (fumos-eval--strict-utf8-decode
         (buffer-substring-no-properties
          (line-beginning-position) (line-end-position)))))))

(defun fumos-eval--authority-line-string (authority line)
  "Return AUTHORITY's current LINE, preferring a widened live buffer."
  (when-let* ((file (fumos-eval--authority-revalidate authority)))
    (if-let* ((buffer (fumos-eval--authority-live-buffer authority)))
        (fumos-eval--line-string-from-live-buffer buffer line)
      (fumos-eval--line-string-from-file file line))))

(defun fumos-eval--character-column (line column unit)
  "Convert one-based COLUMN in LINE from UNIT to a character column."
  (when (and (stringp line) (integerp column) (> column 0))
    (pcase unit
      ('character
       (let ((offset (1- column)))
         (and (<= offset (length line)) offset)))
      ('utf8-byte
       (let* ((bytes (encode-coding-string line 'utf-8-unix))
              (offset (1- column)))
         (when (and (<= offset (length bytes))
                    (or (= offset (length bytes))
                        (let ((byte (aref bytes offset)))
                          (not (<= #x80 byte #xBF)))))
           (when-let* ((decoded
                        (fumos-eval--strict-utf8-decode
                         (substring bytes 0 offset))))
             (length decoded))))))))

(defun fumos-eval--context-matches-authority-p (context authority)
  "Return non-nil when CONTEXT and AUTHORITY name the same source file."
  (let ((captured (and (fumos-request-context-p context)
                       (fumos-request-context-authority context))))
    (and (fumos-source-authority-p captured)
         (equal (fumos-source-authority-file captured)
                (fumos-source-authority-file authority)))))

(defun fumos-eval--adjust-request-locus
    (context authority line column unit)
  "Adjust LINE and COLUMN through CONTEXT's unique live request marker."
  (let ((marker (and (fumos-request-context-p context)
                     (fumos-request-context-marker context))))
    (if (not (and (markerp marker)
                  (marker-buffer marker)
                  (marker-position marker)
                  (fumos-eval--context-matches-authority-p context authority)))
        (cons line column)
      (with-current-buffer (marker-buffer marker)
        (save-restriction
          (widen)
          (save-excursion
            (goto-char marker)
            (let* ((old-line (fumos-request-context-marker-line context))
                   (current-line (line-number-at-pos (point) t))
                   (adjusted-line (+ current-line (- line old-line)))
                   (adjusted-column column))
              (when (= line old-line)
                (setq
                 adjusted-column
                 (+ column
                    (-
                     (pcase unit
                       ('character
                        (1+ (- (point) (line-beginning-position))))
                       ('utf8-byte
                        (1+ (string-bytes
                             (encode-coding-string
                              (buffer-substring-no-properties
                               (line-beginning-position) (point))
                              'utf-8-unix)))))
                     (pcase unit
                       ('character
                        (fumos-request-context-marker-column context))
                       ('utf8-byte
                        (fumos-request-context-marker-byte-column context)))))))
              (cons adjusted-line adjusted-column))))))))

(defun fumos-eval--locus-record
    (connection path line column unit &optional context)
  "Return a validated locus record for untrusted wire location values."
  (when (and (memq unit '(utf8-byte character))
             (integerp line) (> line 0)
             (integerp column) (> column 0))
    (when-let* ((authority
                 (fumos-eval--wire-authority connection path context)))
      (pcase-let* ((`(,adjusted-line . ,adjusted-column)
                     (fumos-eval--adjust-request-locus
                      context authority line column unit))
                    (line-string
                     (fumos-eval--authority-line-string
                      authority adjusted-line))
                    (character-column
                     (and line-string
                          (fumos-eval--character-column
                           line-string adjusted-column unit))))
        (when (integerp character-column)
          (fumos-locus-record--create
           :authority authority :line adjusted-line
           :column character-column :column-unit unit
           :request-marker
           (and (fumos-request-context-p context)
                (fumos-request-context-marker context))))))))

(defvar-local fumos-compilation--records nil
  "Opaque locus ID to `fumos-locus-record' table in an error buffer.")

(defvar-local fumos-compilation--tokens nil
  "Opaque filename token to pinned locus record table.")

(defvar-local fumos-compilation--owner nil
  "Connection that owns the current historical error buffer.")

(defvar-local fumos-compilation--generation nil
  "Transport generation that created the current error buffer.")

(defvar-local fumos-compilation--root nil
  "Canonical project root that owns the current error buffer.")

(defun fumos-compilation--matched-record ()
  "Return the record named by the current regexp match, or nil."
  (let ((id (match-string-no-properties 1)))
    (save-match-data
      (and (stringp id)
           (hash-table-p fumos-compilation--records)
           (gethash id fumos-compilation--records)))))

(defun fumos-compilation--safe-open-buffer (record)
  "Open RECORD's revalidated canonical file and return the exact buffer."
  (let* ((authority (fumos-locus-record-authority record))
         (file (fumos-eval--authority-revalidate authority)))
    (when file
      (condition-case nil
          (let ((buffer
                 (or (fumos-eval--authority-live-buffer authority)
                     (find-file-noselect file))))
            (when (and (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (and (stringp buffer-file-name)
                              (not (file-remote-p buffer-file-name))
                              (equal file (file-truename buffer-file-name)))))
              buffer))
        ((error quit) nil)))))

(defun fumos-compilation--file ()
  "Return an opaque token only after parse-time authority revalidation."
  (let ((id (match-string-no-properties 1)))
    (when-let* ((record (fumos-compilation--matched-record)))
    (save-match-data
      (let ((buffer (fumos-locus-record-pinned-buffer record))
            (token (fumos-locus-record-token record)))
        (unless (buffer-live-p buffer)
          (setq buffer (fumos-compilation--safe-open-buffer record))
          (when buffer
            (setq token
                  (format "FUMOS-BUFFER-%s-%s"
                          (substring
                           (secure-hash 'sha256
                                        (fumos-source-authority-file
                                        (fumos-locus-record-authority record)))
                           0 12)
                          id))
            (setf (fumos-locus-record-pinned-buffer record) buffer
                  (fumos-locus-record-token record) token)
            (puthash token record fumos-compilation--tokens)))
        (and (buffer-live-p buffer) (stringp token) token))))))

(defun fumos-compilation--line ()
  "Return the authenticated line for the current internal locus."
  (when-let* ((record (fumos-compilation--matched-record)))
    (fumos-locus-record-line record)))

(defun fumos-compilation--column ()
  "Return the authenticated one-based character column for this locus."
  (when-let* ((record (fumos-compilation--matched-record)))
    (1+ (fumos-locus-record-column record))))

(defun fumos-compilation--filename (token)
  "Resolve only an opaque TOKEN to its already pinned live buffer."
  (save-match-data
    (let ((record (and (stringp token)
                       (hash-table-p fumos-compilation--tokens)
                       (gethash token fumos-compilation--tokens))))
      (and (fumos-locus-record-p record)
           (buffer-live-p (fumos-locus-record-pinned-buffer record))
           (fumos-locus-record-pinned-buffer record)))))

(defconst fumos-compilation-error-regexp-alist-alist
  '((fumos-fennel "^FUMOS-LOC \\([0-9]+\\):"
     fumos-compilation--file
     fumos-compilation--line
     fumos-compilation--column
     2))
  "Compilation regexp whose captures are only opaque FUMOS record IDs.")

(define-compilation-mode fumos-compilation-mode "FUMOS Error"
  "Compilation mode backed only by authenticated FUMOS locus records."
  (setq-local compilation-error-regexp-alist '(fumos-fennel)
              compilation-error-regexp-alist-alist
              fumos-compilation-error-regexp-alist-alist
              compilation-parse-errors-filename-function
              #'fumos-compilation--filename
              compilation-first-column 1
              compilation-error-screen-columns nil)
  (setq-local fumos-compilation--records (make-hash-table :test #'equal)
              fumos-compilation--tokens (make-hash-table :test #'equal)))

(defun fumos-eval--error-context (context)
  "Normalize CONTEXT into a captured request context, or nil."
  (cond
   ((fumos-request-context-p context) context)
   ((fumos-source-authority-p context)
    (fumos-request-context--create
     :connection (fumos-source-authority-connection context)
     :process (fumos-source-authority-process context)
     :generation (fumos-source-authority-generation context)
     :authority context :column-unit 'utf8-byte))
   ((and (listp context)
         (fumos-source-authority-p (plist-get context :authority)))
    (fumos-eval--error-context (plist-get context :authority)))
   (t
    (when-let* ((connection
                 (or (and (boundp 'fumos-repl--source-owner)
                          fumos-repl--source-owner)
                     (and (boundp 'fumos-repl--connection)
                          fumos-repl--connection))))
      (fumos-request-context--create
       :connection connection
       :process (fumos-connection-process connection)
       :generation (fumos-connection-generation connection)
       :column-unit 'utf8-byte)))))

(defvar fumos-eval--error-current-p nil
  "Optional dynamic predicate which further owns the current error UI.")

(defun fumos-eval--error-context-current-p (context)
  "Return non-nil while CONTEXT still owns its exact transport generation."
  (let ((marker (and (fumos-request-context-p context)
                     (fumos-request-context-marker context))))
    (and (fumos-request-context-p context)
         (or (null marker)
             (and (markerp marker)
                  (marker-buffer marker)
                  (marker-position marker)))
         (fumos-repl--owns-transport-p
          (fumos-request-context-connection context)
          (fumos-request-context-process context)
          (fumos-request-context-generation context))
         (or (not (functionp fumos-eval--error-current-p))
             (condition-case nil
                 (funcall fumos-eval--error-current-p)
               ((error quit) nil))))))

(defun fumos-eval--error-operation-current-p (context epoch)
  "Return non-nil while EPOCH still owns error UI for CONTEXT."
  (and (fumos-eval--error-context-current-p context)
       (eql epoch
            (fumos-connection-error-epoch
             (fumos-request-context-connection context)))))

(defun fumos-eval--owned-error-buffer-p (buffer connection generation)
  "Return non-nil when BUFFER has CONNECTION's complete local ownership."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (eq major-mode 'fumos-compilation-mode)
              (local-variable-p 'fumos-compilation--owner)
              (eq fumos-compilation--owner connection)
              (local-variable-p 'fumos-compilation--generation)
              (eql fumos-compilation--generation generation)))))

(defun fumos-eval--error-staging-current-p
    (buffer context epoch root records tokens)
  "Validate BUFFER and every local identity owned by error EPOCH."
  (and (fumos-eval--error-operation-current-p context epoch)
       (fumos-eval--owned-error-buffer-p
        buffer (fumos-request-context-connection context)
        (fumos-request-context-generation context))
       (with-current-buffer buffer
         (and (local-variable-p 'fumos-compilation--root)
              (equal fumos-compilation--root root)
              (local-variable-p 'fumos-compilation--records)
              (eq fumos-compilation--records records)
              (hash-table-p fumos-compilation--records)
              (local-variable-p 'fumos-compilation--tokens)
              (eq fumos-compilation--tokens tokens)
              (hash-table-p fumos-compilation--tokens)))))

(defun fumos-eval--strip-known-error-prefix (line)
  "Strip only FUMOS-known diagnostic prefixes from raw LINE."
  (let ((value (string-trim-left line)))
    (dolist (prefix '("Error compiling expression: "
                      "Compile error: "
                      "Runtime error: "))
      (when (string-prefix-p prefix value)
        (setq value (substring value (length prefix)))))
    (string-remove-prefix "@" value)))

(defun fumos-eval--raw-line-locus (connection raw unit context)
  "Parse one RAW line into an authenticated record, or return nil."
  (save-match-data
    (unless (string-match-p "\\`[[:space:]]*FUMOS-LOC[[:space:]]" raw)
      (let ((line (fumos-eval--strip-known-error-prefix raw)))
        (when (string-match
               "\\`\\(.+\\.fnlm?\\):\\([0-9]+\\):\\([0-9]+\\)\\(?:\\'\\|:.*\\|[[:space:]].*\\)"
               line)
          ;; Copy every capture before any filesystem helper can alter match data.
          (let ((path (substring-no-properties (match-string 1 line)))
                (line-number
                 (string-to-number (match-string-no-properties 2 line)))
                (column
                 (string-to-number (match-string-no-properties 3 line))))
            (fumos-eval--locus-record
             connection path line-number column unit context)))))))

(defun fumos-eval--error-buffer-name (connection)
  "Return CONNECTION's generation-owned error buffer name."
  (let* ((instance (fumos-connection-instance connection))
         (root (fumos-instance-project-root instance)))
    (format "*FUMOS Error: %s@%d %s g%d*"
            (fumos-instance-mod-id instance)
            (fumos-instance-pid instance)
            (substring (secure-hash 'sha256 root) 0 8)
            (fumos-connection-generation connection))))

(defun fumos-error-handler (type message traceback &optional context)
  "Display one generation-owned FUMOS error with authenticated locations."
  (let* ((context (fumos-eval--error-context context))
         (connection (and context
                          (fumos-request-context-connection context))))
    (when (and connection (fumos-eval--error-context-current-p context))
      (let* ((epoch (1+ (or (fumos-connection-error-epoch connection) 0)))
             (unit (or (fumos-request-context-column-unit context)
                       'utf8-byte))
             (raw-lines
              (append (split-string (if (stringp message) message
                                      (format "%s" message))
                                    "\n" nil)
                      (and traceback
                           (split-string (if (stringp traceback) traceback
                                           (format "%s" traceback))
                                         "\n" nil))))
             records previous previous-owned staging staging-root
             staging-records staging-tokens committed)
        (setf (fumos-connection-error-epoch connection) epoch)
        (dolist (raw raw-lines)
          (when-let* ((record
                       (fumos-eval--raw-line-locus
                        connection raw unit context)))
            (push record records)))
        ;; Build in a private buffer because mode hooks and variable watchers
        ;; may recursively deliver a newer error in the same generation.
        (when (fumos-eval--error-operation-current-p context epoch)
          (setq
           previous (fumos-connection-error-buffer connection)
           previous-owned
           (and (buffer-live-p previous)
                (with-current-buffer previous
                  (and (eq fumos-compilation--owner connection)
                       (eql fumos-compilation--generation
                            (fumos-request-context-generation context)))))
           staging (generate-new-buffer " *FUMOS Error staging*"))
          (unwind-protect
              (progn
                (with-current-buffer staging
                  (fumos-compilation-mode))
                (when (and (buffer-live-p staging)
                           (fumos-eval--error-operation-current-p
                            context epoch))
                  (setq
                   staging-root
                   (and (car records)
                        (fumos-source-authority-root
                         (fumos-locus-record-authority (car records))))
                   staging-records (make-hash-table :test #'equal)
                   staging-tokens (make-hash-table :test #'equal))
                  (with-current-buffer staging
                    (setq-local
                     fumos-compilation--owner connection
                     fumos-compilation--generation
                     (fumos-request-context-generation context)
                     fumos-compilation--root staging-root
                     fumos-compilation--records staging-records
                     fumos-compilation--tokens staging-tokens)
                    (let ((inhibit-read-only t)
                          (inhibit-modification-hooks t)
                          (id 0))
                      (erase-buffer)
                      (insert
                       (format "%s error:\n"
                               (capitalize (format "%s" type))))
                      (dolist (raw raw-lines)
                        (insert "FUMOS-RAW " raw "\n"))
                      (dolist (record (nreverse records))
                        (let ((key (number-to-string (cl-incf id))))
                          (puthash key record fumos-compilation--records)
                          (insert
                           (format "FUMOS-LOC %s: %s:%d:%d\n"
                                   key
                                   (fumos-source-authority-virtual
                                    (fumos-locus-record-authority record))
                                   (fumos-locus-record-line record)
                                   (1+ (fumos-locus-record-column record))))))
                      (goto-char (point-min))))
                  (when (and
                         (fumos-eval--error-staging-current-p
                          staging context epoch staging-root
                          staging-records staging-tokens)
                         (eq previous
                             (fumos-connection-error-buffer connection)))
                    (setf (fumos-connection-error-buffer connection) staging)
                    (setq committed t)
                    (unwind-protect
                        (let ((name
                               (fumos-eval--error-buffer-name connection)))
                          (when (and previous-owned
                                     (buffer-live-p previous)
                                     (eq staging
                                         (fumos-connection-error-buffer
                                          connection))
                                     (fumos-eval--error-staging-current-p
                                      staging context epoch staging-root
                                      staging-records staging-tokens))
                            (condition-case nil
                                (with-current-buffer previous
                                  (rename-buffer
                                   (generate-new-buffer-name
                                    (format " %s retired" name))
                                   t))
                              ((error quit) nil)))
                          (when (and (buffer-live-p staging)
                                     (eq staging
                                         (fumos-connection-error-buffer
                                          connection))
                                     (fumos-eval--error-staging-current-p
                                      staging context epoch staging-root
                                      staging-records staging-tokens))
                            (condition-case nil
                                (with-current-buffer staging
                                  (rename-buffer
                                   (generate-new-buffer-name name) t))
                              ((error quit) nil)))
                          (when (and (buffer-live-p staging)
                                     (eq staging
                                         (fumos-connection-error-buffer
                                          connection))
                                     (fumos-eval--error-staging-current-p
                                      staging context epoch staging-root
                                      staging-records staging-tokens))
                            (pcase fennel-proto-repl-error-buffer-action
                              ('jump (pop-to-buffer staging))
                              ('show (display-buffer staging)))
                            (and
                             (buffer-live-p staging)
                             (eq staging
                                 (fumos-connection-error-buffer connection))
                             (fumos-eval--error-staging-current-p
                              staging context epoch staging-root
                              staging-records staging-tokens)
                             staging)))
                      (let ((published
                             (fumos-connection-error-buffer connection)))
                        (cond
                         ((eq published staging)
                          (if (fumos-eval--error-staging-current-p
                               staging context epoch staging-root
                               staging-records staging-tokens)
                              (when (and previous-owned
                                         (buffer-live-p previous))
                                (fumos-repl--erase-and-kill-buffer previous))
                            ;; Publish is a single slot CAS.  Restore history
                            ;; only while the corrupted staging buffer still
                            ;; owns that slot; a newer committed buffer wins.
                            (setf (fumos-connection-error-buffer connection)
                                  (and (buffer-live-p previous) previous))
                            (setq committed nil)))
                         ((eq published previous)
                          (setq committed nil))
                         ((fumos-eval--owned-error-buffer-p
                           published connection
                           (fumos-request-context-generation context))
                          (when (and previous-owned
                                     (buffer-live-p previous))
                            (fumos-repl--erase-and-kill-buffer previous)))))))))
            (when (and (buffer-live-p staging)
                       (or (not committed)
                           (not (eq staging
                                    (fumos-connection-error-buffer
                                     connection)))))
              (fumos-repl--erase-and-kill-buffer staging))))))))

(defun fumos-eval--owned-error-buffer ()
  "Return only the error buffer owned by the current FUMOS context."
  (cond
   ((derived-mode-p 'fumos-compilation-mode)
    (and (fumos-connection-p fumos-compilation--owner) (current-buffer)))
   (t
    (let* ((connection
            (or (and (boundp 'fumos-repl--source-owner)
                     fumos-repl--source-owner)
                (and (boundp 'fumos-repl--connection)
                     fumos-repl--connection)))
           (buffer (and (fumos-connection-p connection)
                        (fumos-connection-error-buffer connection))))
      (and (buffer-live-p buffer)
           (eq connection
               (buffer-local-value 'fumos-compilation--owner buffer))
           buffer)))))

(defun fumos-eval--navigate-owned-error (count reset)
  "Navigate COUNT errors in the current connection-owned buffer."
  (let ((buffer (or (fumos-eval--owned-error-buffer)
                    (user-error "No FUMOS error buffer for this connection"))))
    (with-current-buffer buffer
      (funcall next-error-function count reset))))

(defun fumos-next-error (&optional count)
  "Visit the next error owned by the current FUMOS connection."
  (interactive "p")
  (fumos-eval--navigate-owned-error (or count 1) nil))

(defun fumos-previous-error (&optional count)
  "Visit the previous error owned by the current FUMOS connection."
  (interactive "p")
  (fumos-eval--navigate-owned-error (- (or count 1)) nil))

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

(defun fumos-eval--request-context (source marker unit)
  "Capture SOURCE, MARKER, and UNIT for one asynchronous request."
  (let* ((authority (and (listp source) (plist-get source :authority)))
         (connection
          (if (fumos-source-authority-p authority)
              (fumos-source-authority-connection authority)
            (fumos-eval--connection)))
         marker-line marker-column marker-byte-column)
    (when (and (markerp marker) (marker-buffer marker))
      (with-current-buffer (marker-buffer marker)
        (save-restriction
          (widen)
          (save-excursion
            (goto-char marker)
            (setq marker-line (line-number-at-pos (point) t)
                  marker-column
                  (1+ (- (point) (line-beginning-position)))
                  marker-byte-column
                  (1+ (string-bytes
                       (encode-coding-string
                        (buffer-substring-no-properties
                         (line-beginning-position) (point))
                        'utf-8-unix))))))))
    (fumos-request-context--create
     :connection connection
     :process (fumos-connection-process connection)
     :generation (fumos-connection-generation connection)
     :authority authority :marker marker :marker-line marker-line
     :marker-column marker-column :marker-byte-column marker-byte-column
     :column-unit unit)))

(defun fumos-eval--connection-context
    (connection &optional authority column-unit)
  "Capture CONNECTION and optional AUTHORITY for an asynchronous error."
  (fumos-request-context--create
   :connection connection :process (fumos-connection-process connection)
   :generation (fumos-connection-generation connection)
   :authority authority :column-unit (or column-unit 'utf8-byte)))

(defun fumos-eval--error-callback (connection &optional authority column-unit)
  "Return an error callback owned by CONNECTION and AUTHORITY."
  (let ((context
         (fumos-eval--connection-context
          connection authority column-unit)))
    (lambda (type message traceback)
      (let ((fumos-repl--error-context context))
        (fumos-repl--default-error-handler type message traceback)))))

(defun fumos-eval--marker-delivery
    (marker values-callback &optional source column-unit)
  "Return callbacks and an exactly-once finalizer for MARKER."
  (let ((context
         (fumos-eval--request-context
          source marker (or column-unit 'utf8-byte)))
        finished)
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
              (when (and (marker-buffer marker)
                         (marker-position marker))
                (let ((fumos-repl--error-context context))
                  (fumos-repl--default-error-handler
                   type message traceback)))
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
                 (fumos-eval--display values position buffer)))
             source 'utf8-byte))
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
               (source (fumos-eval--source beg))
               (end (copy-marker end-position t))
               (delivery
                (fumos-eval--marker-delivery
                 end
                 (lambda (values buffer position)
                   (with-current-buffer buffer
                     (goto-char position)
                     (insert "\n" (string-join values "\t"))))
                 source 'utf8-byte))
               (callbacks (car delivery))
               (finish (cdr delivery))
               (sent nil))
    (unwind-protect
        (prog1
            (fumos-repl-send-eval
             (buffer-substring-no-properties beg end)
             source
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

(cl-defstruct (fumos-tool-query
               (:constructor fumos-tool-query--create))
  kind connection process generation repl-buffer source-buffer source-context
  authority epoch request-id callback-identity marker window display-action
  symbol point callback show-errors)

(cl-defstruct (fumos-completion-query
               (:constructor fumos-completion-query--create))
  connection process generation repl-buffer source-buffer source-context key
  prefix epoch request-id callback-identity candidates annotate)

(cl-defstruct (fumos-completion-cache
               (:constructor fumos-completion-cache--create))
  generation candidates kinds)

(defun fumos-eval--owned-connection ()
  "Return the exact connection owning the current FUMOS editing buffer."
  (or
   (and (boundp 'fumos-repl--source-owner)
        (fumos-connection-p fumos-repl--source-owner)
        fumos-repl--source-owner)
   (and (boundp 'fumos-repl--connection)
        (bound-and-true-p fumos-repl-mode)
        (fumos-connection-p fumos-repl--connection)
        fumos-repl--connection)
   (user-error "Current buffer is not owned by a FUMOS connection")))

(defun fumos-eval--source-owned-p (connection source repl-buffer)
  "Return non-nil when CONNECTION still owns SOURCE and REPL-BUFFER."
  (and (buffer-live-p source)
       (if (eq source repl-buffer)
           (with-current-buffer source
             (and (bound-and-true-p fumos-repl-mode)
                  (eq fumos-repl--connection connection)))
         (with-current-buffer source
           (eq fumos-repl--source-owner connection)))))

(defun fumos-eval--query-epoch (connection kind)
  "Advance and return CONNECTION's epoch for query KIND."
  (pcase kind
    ('help
     (setf (fumos-connection-help-epoch connection)
           (1+ (or (fumos-connection-help-epoch connection) 0))))
    ('xref
     (setf (fumos-connection-xref-epoch connection)
           (1+ (or (fumos-connection-xref-epoch connection) 0))))
    ('eldoc
     (setf (fumos-connection-eldoc-epoch connection)
           (1+ (or (fumos-connection-eldoc-epoch connection) 0))))
    (_ (error "Unknown FUMOS query kind: %S" kind))))

(defun fumos-eval--query-current-epoch (connection kind)
  "Return CONNECTION's current epoch for KIND."
  (pcase kind
    ('help (fumos-connection-help-epoch connection))
    ('xref (fumos-connection-xref-epoch connection))
    ('eldoc (fumos-connection-eldoc-epoch connection))))

(defun fumos-eval--query-pending (connection kind)
  "Return CONNECTION's pending query for KIND."
  (pcase kind
    ('help (fumos-connection-help-pending connection))
    ('xref (fumos-connection-xref-pending connection))
    ('eldoc (fumos-connection-eldoc-pending connection))))

(defun fumos-eval--set-query-pending (connection kind value)
  "Set CONNECTION's pending KIND query to VALUE."
  (pcase kind
    ('help (setf (fumos-connection-help-pending connection) value))
    ('xref (setf (fumos-connection-xref-pending connection) value))
    ('eldoc (setf (fumos-connection-eldoc-pending connection) value))))

(defun fumos-eval--query-delivery-current-p (query)
  "Return non-nil when QUERY owns its exact deferred callback delivery."
  (let* ((connection (fumos-tool-query-connection query))
         (id (fumos-tool-query-request-id query))
         (delivery
          (and (integerp id)
               (gethash id
                        (fumos-repl--callback-delivery-table connection)))))
    (and (fumos-callback-delivery-p delivery)
         (eq (fumos-tool-query-callback-identity query)
             (fumos-callback-delivery-callbacks delivery)))))

(defun fumos-eval--query-current-p (query)
  "Validate every transport, source, epoch, and callback owner of QUERY."
  (let ((connection (fumos-tool-query-connection query))
        (kind (fumos-tool-query-kind query)))
    (and (eq query (fumos-eval--query-pending connection kind))
         (eql (fumos-tool-query-epoch query)
              (fumos-eval--query-current-epoch connection kind))
         (eq (fumos-tool-query-repl-buffer query)
             (fumos-connection-repl-buffer connection))
         (fumos-repl--owns-transport-p
          connection (fumos-tool-query-process query)
          (fumos-tool-query-generation query))
         (fumos-eval--source-owned-p
          connection (fumos-tool-query-source-buffer query)
          (fumos-tool-query-repl-buffer query))
         (fumos-eval--query-delivery-current-p query))))

(defun fumos-eval--clear-query-if-current (query)
  "Clear QUERY's pending slot without disturbing newer intent."
  (let ((connection (fumos-tool-query-connection query))
        (kind (fumos-tool-query-kind query)))
    (when (eq query (fumos-eval--query-pending connection kind))
      (fumos-eval--set-query-pending connection kind nil))))

(defun fumos-eval--release-query-marker (query)
  "Release QUERY's temporary origin marker."
  (when-let* ((marker (fumos-tool-query-marker query)))
    (when (and (markerp marker) (marker-buffer marker))
      (set-marker marker nil))))

(defun fumos-eval--cancel-request (connection repl-buffer request-id)
  "Cancel REQUEST-ID's callback, delivery, timer, and active ownership."
  (when (integerp request-id)
    (let* ((deliveries (fumos-repl--callback-delivery-table connection))
           (delivery (and (hash-table-p deliveries)
                          (gethash request-id deliveries))))
      (when (fumos-callback-delivery-p delivery)
        (condition-case nil
            (fumos-repl--rollback-callback-assignment
             connection repl-buffer delivery request-id)
          ((error quit) nil)))
      (when (hash-table-p deliveries)
        (remhash request-id deliveries))
      (when (fumos-callback-delivery-p delivery)
        ;; Canceled timer objects can outlive their registry entry until GC.
        ;; Break their path back to query closures and source buffers now.
        (setf (fumos-callback-delivery-process delivery) nil
              (fumos-callback-delivery-repl-buffer delivery) nil
              (fumos-callback-delivery-callbacks delivery) nil
              (fumos-callback-delivery-values-callback delivery) nil
              (fumos-callback-delivery-error-callback delivery) nil
              (fumos-callback-delivery-print-callback delivery) nil)))
    (when (buffer-live-p repl-buffer)
      (condition-case nil
          (with-current-buffer repl-buffer
            (when (hash-table-p fennel-proto-repl--message-callbacks)
              (remhash request-id fennel-proto-repl--message-callbacks)))
        ((error quit) nil)))
    (setf (fumos-connection-active-request-ids connection)
          (delq request-id
                (fumos-connection-active-request-ids connection)))
    (when (and (null (fumos-connection-active-request-ids connection))
               (eq 'busy (fumos-connection-state connection))
               (fumos-repl--owns-transport-p
                connection (fumos-connection-process connection)
                (fumos-connection-generation connection)))
      (condition-case nil
          (fumos-repl--set-state connection 'ready)
        ((error quit) nil)))))

(defun fumos-eval--dispose-query (query &optional cancel-request)
  "Release QUERY, canceling its request when CANCEL-REQUEST is non-nil."
  (let ((connection (fumos-tool-query-connection query))
        (repl-buffer (fumos-tool-query-repl-buffer query))
        (request-id (fumos-tool-query-request-id query)))
    (fumos-eval--clear-query-if-current query)
    (when cancel-request
      (fumos-eval--cancel-request connection repl-buffer request-id))
    (fumos-eval--release-query-marker query)
    (setf (fumos-tool-query-process query) nil
          (fumos-tool-query-repl-buffer query) nil
          (fumos-tool-query-source-buffer query) nil
          (fumos-tool-query-source-context query) nil
          (fumos-tool-query-authority query) nil
          (fumos-tool-query-request-id query) nil
          (fumos-tool-query-callback-identity query) nil
          (fumos-tool-query-marker query) nil
          (fumos-tool-query-window query) nil
          (fumos-tool-query-display-action query) nil
          (fumos-tool-query-callback query) nil)))

(defun fumos-eval--query-error-context (query)
  "Return a captured error context for QUERY."
  (fumos-eval--connection-context
   (fumos-tool-query-connection query)
   (fumos-tool-query-authority query) 'utf8-byte))

(defun fumos-eval--query-values (query handler values)
  "Deliver terminal VALUES to current QUERY through HANDLER."
  (when (fumos-eval--query-current-p query)
    (let ((fumos-eval--error-current-p
           (lambda () (fumos-eval--query-current-p query))))
      (unwind-protect
          (funcall handler query values)
        (fumos-eval--dispose-query query t)))))

(defun fumos-eval--query-error (query type message traceback)
  "Deliver one owned QUERY error, or discard it when it is stale."
  (when (fumos-eval--query-current-p query)
    (let ((fumos-eval--error-current-p
           (lambda () (fumos-eval--query-current-p query))))
      (unwind-protect
          (when (fumos-tool-query-show-errors query)
            (let ((fumos-repl--error-context
                   (fumos-eval--query-error-context query)))
              (fumos-repl--default-error-handler type message traceback)))
        (fumos-eval--dispose-query query t)))))

(defun fumos-eval--query-print (query handler data)
  "Deliver nonterminal print DATA to current QUERY through HANDLER."
  (when (fumos-eval--query-current-p query)
    (funcall handler query data)))

(cl-defun fumos-eval--start-query
    (kind op data values-handler
          &key print-handler symbol point callback marker window
          display-action show-errors)
  "Start one fully asynchronous owned tooling query and return its ID."
  (let* ((source-buffer (current-buffer))
         (connection (fumos-eval--owned-connection))
         (process (fumos-connection-process connection))
         (generation (fumos-connection-generation connection))
         (repl-buffer (fumos-connection-repl-buffer connection))
         (source-context
          (and buffer-file-name
               (fumos-source-context-at-position (point))))
         (authority (and source-context
                         (plist-get source-context :authority)))
         (epoch (fumos-eval--query-epoch connection kind))
         (query
          (fumos-tool-query--create
           :kind kind :connection connection :process process
           :generation generation :repl-buffer repl-buffer
           :source-buffer source-buffer :source-context source-context
           :authority authority :epoch epoch :symbol symbol :point point
           :callback callback :marker marker :window window
           :display-action display-action :show-errors show-errors))
         request-id committed)
    (when-let* ((previous (fumos-eval--query-pending connection kind)))
      (when (fumos-tool-query-p previous)
        (fumos-eval--dispose-query previous t)))
    (fumos-eval--set-query-pending connection kind query)
    (unwind-protect
        (condition-case caught
            (let ((fumos-repl--connection connection))
              (setq
               request-id
               (if (eq op :eval)
                   (fumos-repl-send-eval
                    data source-context
                    (list
                     :values
                     (lambda (values)
                       (fumos-eval--query-values
                        query values-handler values))
                     :error
                     (lambda (type message traceback)
                       (fumos-eval--query-error
                        query type message traceback))
                     :print
                     (if print-handler
                         (lambda (value)
                           (fumos-eval--query-print
                            query print-handler value))
                       #'ignore)))
                 (fumos-repl-send-command
                  op data
                  (list
                   :values
                   (lambda (values)
                     (fumos-eval--query-values
                      query values-handler values))
                   :error
                   (lambda (type message traceback)
                     (fumos-eval--query-error
                      query type message traceback))
                   :print
                   (if print-handler
                       (lambda (value)
                         (fumos-eval--query-print
                          query print-handler value))
                     #'ignore)))))
              (setf
               (fumos-tool-query-request-id query) request-id
               (fumos-tool-query-callback-identity query)
               (and (buffer-live-p repl-buffer)
                    (buffer-local-value
                     'fennel-proto-repl--message-callbacks repl-buffer)
                    (with-current-buffer repl-buffer
                      (gethash request-id
                               fennel-proto-repl--message-callbacks))))
              (unless (fumos-eval--query-current-p query)
                (error "FUMOS query ownership changed during setup"))
              (setq committed t)
              request-id)
          ((error quit)
           (signal (car caught) (cdr caught))))
      (unless committed
        (fumos-eval--cancel-request connection repl-buffer request-id)
        (fumos-eval--dispose-query query nil)))))

(defun fumos-eval--print-to-query-repl (query string)
  "Print STRING in QUERY's captured game REPL buffer."
  (let ((repl-buffer (fumos-tool-query-repl-buffer query)))
    (when (and (buffer-live-p repl-buffer)
               (fumos-eval--query-current-p query))
      (with-current-buffer repl-buffer
        (fennel-proto-repl--print string)))))

(defun fumos-eval--single-symbol (value prompt)
  "Return VALUE or the symbol at point, otherwise prompt with PROMPT."
  (let ((symbol (or value (thing-at-point 'symbol t))))
    (unless (and (stringp symbol) (not (string-empty-p symbol)))
      (user-error "%s requires a symbol" prompt))
    (substring-no-properties symbol)))

(defun fumos-macroexpand ()
  "Asynchronously print the macroexpansion of the form at point."
  (interactive)
  (let ((form (thing-at-point 'sexp t)))
    (unless (and (stringp form) (not (string-empty-p form)))
      (user-error "FUMOS macroexpand requires a form"))
    (fumos-eval--start-query
     'help :eval (format "(macrodebug %s)" form) #'ignore
     :print-handler
     (lambda (query value)
       (fumos-eval--print-to-query-repl
        query (if (string-suffix-p "\n" value) value (concat value "\n"))))
     :show-errors t)))

(defun fumos-show-documentation (&optional symbol)
  "Asynchronously show documentation for SYMBOL in the game REPL."
  (interactive
   (list (read-string "Documentation: " (thing-at-point 'symbol t))))
  (setq symbol (fumos-eval--single-symbol symbol "Documentation"))
  (fumos-eval--start-query
   'help :doc symbol
   (lambda (query values)
     (when-let* ((value (and (consp values) (car values))))
       (fumos-eval--print-to-query-repl
        query (concat (string-trim-right value) "\n"))))
   :symbol symbol :show-errors t))

(defun fumos-eval--reserved-query
    (symbol template-function multisym-template-function)
  "Build SYMBOL metadata query with the reserved FUMOS module identity."
  (let ((fennel-proto-repl-fennel-module-name
         fumos-repl-fennel-module-name))
    (fennel-proto-repl--generate-query-command
     symbol (funcall template-function) (funcall multisym-template-function))))

(defun fumos-show-arglist (&optional symbol)
  "Asynchronously show SYMBOL's arglist in the game REPL."
  (interactive (list (read-string "Arglist: " (thing-at-point 'symbol t))))
  (setq symbol (fumos-eval--single-symbol symbol "Arglist"))
  (fumos-eval--start-query
   'help :eval
   (fumos-eval--reserved-query
    symbol #'fennel-proto-repl--arglist-query-template
    #'fennel-proto-repl--multisym-arglist-query-template)
   (lambda (query values)
     (let* ((wire (and (consp values) (car values)))
            (parsed
             (and (stringp wire)
                  (condition-case nil
                      (car (read-from-string wire))
                    (error nil)))))
       (when (vectorp parsed)
         (fumos-eval--print-to-query-repl
          query
          (format "Arglist for %s: [%s]\n"
                  symbol
                  (mapconcat (lambda (value) (format "%s" value))
                             (append parsed nil) " "))))))
   :symbol symbol :show-errors t))

(defun fumos-apropos (&optional pattern)
  "Asynchronously show every symbol matching PATTERN in the game REPL."
  (interactive (list (read-string "Apropos: " (thing-at-point 'symbol t))))
  (unless (and (stringp pattern) (not (string-empty-p pattern)))
    (user-error "Apropos requires a pattern"))
  (fumos-eval--start-query
   'help :apropos pattern
   (lambda (query values)
     (dolist (value values)
       (when (stringp value)
         (fumos-eval--print-to-query-repl
          query (concat (string-trim-right value) "\n")))))
   :show-errors t))

(defun fumos-eval--find-record (query locus)
  "Validate one real proto find LOCUS for QUERY."
  (save-match-data
    (when (and (stringp locus)
               (string-match
                "\\`@?\\(.+\\.fnlm?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?\\'"
                locus))
      (let ((path (substring-no-properties (match-string 1 locus)))
            (line (string-to-number (match-string-no-properties 2 locus)))
            (column
             (if (match-beginning 3)
                 (string-to-number (match-string-no-properties 3 locus))
               1)))
        (fumos-eval--locus-record
         (fumos-tool-query-connection query) path line column 'utf8-byte
         (fumos-eval--connection-context
          (fumos-tool-query-connection query)
          (fumos-tool-query-authority query) 'utf8-byte))))))

(defun fumos-eval--record-xref (record summary)
  "Return a pinned buffer xref for RECORD and SUMMARY, or nil."
  (when-let* ((buffer (fumos-compilation--safe-open-buffer record)))
    (setf (fumos-locus-record-pinned-buffer record) buffer)
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- (fumos-locus-record-line record)))
          (forward-char (fumos-locus-record-column record))
          (xref-make summary
                     (xref-make-buffer-location buffer (point))))))))

(defun fumos-eval--xref-ui-current-p (query)
  "Return non-nil when QUERY may still cross an xref UI boundary."
  (fumos-eval--query-current-p query))

(defun fumos-eval--display-definition-xrefs (query xrefs)
  "Display immutable XREFS with the public definition UX for QUERY."
  (let ((marker (fumos-tool-query-marker query))
        (window (fumos-tool-query-window query)))
    (if (not (and (fumos-eval--xref-ui-current-p query)
                  (markerp marker) (marker-buffer marker)
                  (window-live-p window)))
        (fumos-eval--release-query-marker query)
      (let* ((immutable (copy-sequence xrefs))
             (fetcher (lambda () immutable))
             (alist
              `((window . ,window)
                (display-action . ,(fumos-tool-query-display-action query))
                (auto-jump . ,xref-auto-jump-to-first-definition))))
        (unwind-protect
            (with-selected-window window
              (when (fumos-eval--xref-ui-current-p query)
                (xref-push-marker-stack (copy-marker marker))
                (when (fumos-eval--xref-ui-current-p query)
                  (funcall xref-show-definitions-function fetcher alist))))
          (fumos-eval--release-query-marker query))))))

(defun fumos-eval--find-values (query values)
  "Validate and display real proto find VALUES for QUERY."
  (cond
   ((null values)
    (fumos-eval--release-query-marker query)
    (message "No definition found for %s" (fumos-tool-query-symbol query)))
   ((not (and (consp values) (null (cdr values))
              (stringp (car values))))
    (fumos-eval--release-query-marker query)
    (fumos-error-handler
     "find" "FUMOS find returned an invalid result" nil
     (fumos-eval--query-error-context query)))
   (t
    (let* ((record (fumos-eval--find-record query (car values)))
           (xref (and record
                      (fumos-eval--record-xref
                       record (fumos-tool-query-symbol query)))))
      (if (not xref)
          (progn
            (fumos-eval--release-query-marker query)
            (fumos-error-handler
             "find" "FUMOS find returned an unsafe locus" nil
             (fumos-eval--query-error-context query)))
        (if (not (fumos-eval--query-current-p query))
            (fumos-eval--release-query-marker query)
          (puthash
           (fumos-tool-query-symbol query)
           (list :generation (fumos-tool-query-generation query)
                 :xrefs (list xref))
           (fumos-connection-xref-cache
            (fumos-tool-query-connection query)))
          (fumos-eval--display-definition-xrefs query (list xref))))))))

(defun fumos-eval--find-definition (symbol display-action)
  "Start an asynchronous definition lookup using DISPLAY-ACTION."
  (setq symbol (fumos-eval--single-symbol symbol "Find definition"))
  (fumos-eval--start-query
   'xref :find symbol #'fumos-eval--find-values
   :symbol symbol :marker (point-marker) :window (selected-window)
   :display-action display-action :show-errors t))

(defun fumos-find-definition (&optional symbol)
  "Asynchronously find SYMBOL using the standard definition UX."
  (interactive)
  (fumos-eval--find-definition symbol nil))

(defun fumos-find-definition-other-window (&optional symbol)
  "Asynchronously find SYMBOL and display it in another window."
  (interactive)
  (fumos-eval--find-definition symbol 'window))

(defun fumos-repl--xref-backend ()
  "Return the cache-only FUMOS xref backend in an owned editing buffer."
  (condition-case nil
      (progn (fumos-eval--owned-connection) 'fumos)
    (user-error nil)))

(cl-defmethod xref-backend-identifier-at-point ((_ (eql fumos)))
  "Return a plain Fennel identifier at point."
  (when-let* ((symbol (thing-at-point 'symbol t)))
    (unless (string-prefix-p ":" symbol)
      (car (fennel-proto-repl--method-to-sym symbol)))))

(cl-defmethod xref-backend-definitions ((_ (eql fumos)) symbol)
  "Return only the current generation's already committed SYMBOL cache."
  (condition-case nil
      (let* ((connection (fumos-eval--owned-connection))
             (entry (gethash symbol (fumos-connection-xref-cache connection))))
        (when (eql (plist-get entry :generation)
                   (fumos-connection-generation connection))
          (copy-sequence (plist-get entry :xrefs))))
    (user-error nil)))

(cl-defmethod xref-backend-identifier-completion-table ((_ (eql fumos)))
  nil)

(defun fumos-eldoc-function (callback &rest _ignored)
  "Asynchronously query Eldoc and call CALLBACK only for current point."
  (let* ((point (point))
         (fn-info (fennel-proto-repl--eldoc-fn-in-current-sexp))
         (symbol
          (or (and fn-info (substring-no-properties (car fn-info)))
              (thing-at-point 'symbol t))))
    (when (and (stringp symbol) (not (string-empty-p symbol)))
      (condition-case nil
          (progn
            (fumos-eval--start-query
             'eldoc :eval
             (if fn-info
                 (fumos-eval--reserved-query
                  symbol #'fennel-proto-repl--arglist-query-template
                  #'fennel-proto-repl--multisym-arglist-query-template)
               (fumos-eval--reserved-query
                symbol #'fennel-proto-repl--doc-query-template
                #'fennel-proto-repl--multisym-doc-query-template))
             (lambda (query values)
               (let ((buffer (fumos-tool-query-source-buffer query)))
                 (when (and (buffer-live-p buffer)
                            (with-current-buffer buffer
                              (and (= (point) (fumos-tool-query-point query))
                                   (equal
                                    (thing-at-point 'symbol t)
                                    (fumos-tool-query-symbol query)))))
                   (if fn-info
                       (fennel-proto-repl--eldoc-fn-handler
                        values callback symbol fn-info)
                     (fennel-proto-repl--eldoc-var-handler
                      values callback symbol)))))
             :symbol symbol :point point :callback callback)
            t)
        ((error quit) nil)))))

(defun fumos-eval--completion-query-current-p (query)
  "Validate every owner identity captured by completion QUERY."
  (let* ((connection (fumos-completion-query-connection query))
         (id (fumos-completion-query-request-id query))
         (delivery
          (and (integerp id)
               (gethash id
                        (fumos-repl--callback-delivery-table connection)))))
    (and
     (eq query
         (gethash (fumos-completion-query-key query)
                  (fumos-connection-completion-pending connection)))
     (eql (fumos-completion-query-epoch query)
          (fumos-connection-completion-epoch connection))
     (eq (fumos-completion-query-repl-buffer query)
         (fumos-connection-repl-buffer connection))
     (fumos-repl--owns-transport-p
      connection (fumos-completion-query-process query)
      (fumos-completion-query-generation query))
     (fumos-eval--source-owned-p
      connection (fumos-completion-query-source-buffer query)
      (fumos-completion-query-repl-buffer query))
     (fumos-callback-delivery-p delivery)
     (eq (fumos-completion-query-callback-identity query)
         (fumos-callback-delivery-callbacks delivery)))))

(defun fumos-eval--clear-completion-query (query)
  "Clear completion QUERY without disturbing newer work for its key."
  (let* ((connection (fumos-completion-query-connection query))
         (table (fumos-connection-completion-pending connection))
         (key (fumos-completion-query-key query)))
    (when (eq query (gethash key table))
      (remhash key table))))

(defun fumos-eval--dispose-completion-query (query &optional cancel-request)
  "Release completion QUERY and optionally cancel its transport request."
  (let ((connection (fumos-completion-query-connection query))
        (repl-buffer (fumos-completion-query-repl-buffer query))
        (request-id (fumos-completion-query-request-id query)))
    (fumos-eval--clear-completion-query query)
    (when cancel-request
      (fumos-eval--cancel-request connection repl-buffer request-id))
    (setf (fumos-completion-query-process query) nil
          (fumos-completion-query-repl-buffer query) nil
          (fumos-completion-query-source-buffer query) nil
          (fumos-completion-query-source-context query) nil
          (fumos-completion-query-key query) nil
          (fumos-completion-query-prefix query) nil
          (fumos-completion-query-request-id query) nil
          (fumos-completion-query-callback-identity query) nil
          (fumos-completion-query-candidates query) nil)))

(defun fumos-eval--completion-wire-kinds (value candidates)
  "Parse VALUE into an immutable candidate kind alist."
  (when (stringp value)
    (condition-case nil
        (let* ((read-result (read-from-string value))
               (parsed (car read-result)))
          (when (and (vectorp parsed)
                     (= (length parsed) (length candidates))
                     (seq-every-p #'stringp parsed)
                     (string-match-p
                      "\\`[[:space:]]*\\'"
                      (substring value (cdr read-result))))
            (cl-mapcar #'cons candidates (append parsed nil))))
      (error nil))))

(defun fumos-eval--commit-completion (query candidates kinds)
  "Commit CANDIDATES and KINDS when completion QUERY still owns its key."
  (when (fumos-eval--completion-query-current-p query)
    (puthash
     (fumos-completion-query-key query)
     (fumos-completion-cache--create
      :generation (fumos-completion-query-generation query)
      :candidates (copy-sequence candidates) :kinds (copy-tree kinds))
     (fumos-connection-completion-cache
      (fumos-completion-query-connection query)))
    (fumos-eval--dispose-completion-query query t)))

(defun fumos-eval--completion-kinds-values (query values)
  "Commit a kind-annotated completion QUERY from terminal VALUES."
  (when (fumos-eval--completion-query-current-p query)
    (let ((kinds
           (and (consp values) (null (cdr values))
                (fumos-eval--completion-wire-kinds
                 (car values) (fumos-completion-query-candidates query)))))
      (if kinds
          (fumos-eval--commit-completion
           query (fumos-completion-query-candidates query) kinds)
        (fumos-eval--dispose-completion-query query t)))))

(defun fumos-eval--completion-send-kinds (query candidates)
  "Start QUERY's optional asynchronous kind request for CANDIDATES."
  (let* ((connection (fumos-completion-query-connection query))
         (repl-buffer (fumos-completion-query-repl-buffer query))
         (source-buffer (fumos-completion-query-source-buffer query))
         (command
          (fennel-proto-repl--minify-body
           (format
            fennel-proto-repl--symbol-types
            (mapconcat
             (lambda (candidate) (format "(symbol-type %S)" candidate))
             candidates " "))
           t))
         (previous-request-id (fumos-completion-query-request-id query))
         request-id committed)
    ;; The values callback that reached this point has already validated the
    ;; first request.  Replace only this key with a new callback identity.
    (fumos-eval--cancel-request connection repl-buffer previous-request-id)
    (setf (fumos-completion-query-candidates query) candidates
          (fumos-completion-query-request-id query) nil
          (fumos-completion-query-callback-identity query) nil)
    (unwind-protect
        (condition-case nil
            (with-current-buffer source-buffer
              (let ((fumos-repl--connection connection))
                (setq request-id
                      (fumos-repl-send-eval
                       command (fumos-completion-query-source-context query)
                       (list
                        :values
                        (lambda (values)
                          (fumos-eval--completion-kinds-values query values))
                        :error
                        (lambda (&rest _)
                          (when (fumos-eval--completion-query-current-p query)
                            (fumos-eval--dispose-completion-query query t)))
                        :print #'ignore)))
                (setf
                 (fumos-completion-query-request-id query) request-id
                 (fumos-completion-query-callback-identity query)
                 (with-current-buffer repl-buffer
                   (gethash request-id fennel-proto-repl--message-callbacks)))
                (unless (fumos-eval--completion-query-current-p query)
                  (error "FUMOS completion kind ownership changed"))
                (setq committed t)))
          ((error quit) nil))
      (unless committed
        (fumos-eval--cancel-request connection repl-buffer request-id)
        (fumos-eval--dispose-completion-query query nil)))))

(defun fumos-eval--completion-values (query values)
  "Validate completion VALUES and commit or request optional kinds."
  (when (fumos-eval--completion-query-current-p query)
    (if (not (and (proper-list-p values) (seq-every-p #'stringp values)))
        (fumos-eval--dispose-completion-query query t)
      (let ((candidates (delete-dups (copy-sequence values))))
        (if (and (fumos-completion-query-annotate query) candidates)
            (fumos-eval--completion-send-kinds query candidates)
          (fumos-eval--commit-completion query candidates nil))))))

(defun fumos-eval--refresh-completion (connection source prefix source-context)
  "Start a nonblocking completion refresh for SOURCE and PREFIX."
  (let* ((process (fumos-connection-process connection))
         (generation (fumos-connection-generation connection))
         (repl-buffer (fumos-connection-repl-buffer connection))
         (key (cons source prefix))
         (pending (fumos-connection-completion-pending connection))
         (existing (gethash key pending)))
    (unless (and (fumos-completion-query-p existing)
                 (fumos-eval--completion-query-current-p existing))
      (let (obsolete)
        (maphash
         (lambda (_old-key old-query)
           (when (fumos-completion-query-p old-query)
             (push old-query obsolete)))
         pending)
        (dolist (old-query obsolete)
          (fumos-eval--dispose-completion-query old-query t)))
      (let* ((epoch
              (setf (fumos-connection-completion-epoch connection)
                    (1+ (or (fumos-connection-completion-epoch connection) 0))))
             (query
              (fumos-completion-query--create
               :connection connection :process process :generation generation
               :repl-buffer repl-buffer :source-buffer source
               :source-context source-context :key key :prefix prefix
               :epoch epoch :annotate fennel-proto-repl-annotate-completion))
             request-id committed)
        (puthash key query pending)
        (unwind-protect
            (condition-case nil
                (let ((fumos-repl--connection connection))
                  (setq request-id
                        (fumos-repl-send-command
                         :complete prefix
                         (list
                          :values
                          (lambda (values)
                            (fumos-eval--completion-values query values))
                          :error
                          (lambda (&rest _)
                            (when (fumos-eval--completion-query-current-p query)
                              (fumos-eval--dispose-completion-query query t)))
                          :print #'ignore)))
                  (setf
                   (fumos-completion-query-request-id query) request-id
                   (fumos-completion-query-callback-identity query)
                   (with-current-buffer repl-buffer
                     (gethash request-id fennel-proto-repl--message-callbacks)))
                  (unless (fumos-eval--completion-query-current-p query)
                    (error "FUMOS completion ownership changed"))
                  (setq committed t))
              ((error quit) nil))
          (unless committed
            (fumos-eval--cancel-request connection repl-buffer request-id)
            (fumos-eval--dispose-completion-query query nil)))))))

(defun fumos-eval--completion-kind (kinds item)
  "Return completion kind symbol for ITEM from immutable KINDS."
  (pcase (cdr (assoc-string item kinds))
    ("function" 'function) ("table" 'module) ("special" 'keyword)
    ("macro" 'macro) ("number" 'constant) ("boolean" 'boolean)
    ("string" 'string)
    (_ (if (string-match-p "\\." item) 'field 'variable))))

(defun fumos-eval--completion-annotation (kinds item)
  "Return a textual annotation for ITEM using KINDS."
  (let ((kind (fumos-eval--completion-kind kinds item)))
    (cond ((eq kind 'module) " table")
          ((eq kind 'variable) " definition")
          (kind (format " %s" kind))
          (t ""))))

(defun fumos-completion-at-point ()
  "Return current-generation cached completions and refresh asynchronously."
  (when-let* ((bounds (bounds-of-thing-at-point 'symbol)))
    (let* ((source (current-buffer))
           (connection (fumos-eval--owned-connection))
           (start (car bounds)) (end (cdr bounds))
           (prefix (buffer-substring-no-properties start end))
           (source-context
            (and buffer-file-name
                 (fumos-source-context-at-position (point))))
           (key (cons source prefix))
           (cache (gethash key
                           (fumos-connection-completion-cache connection))))
      (fumos-eval--refresh-completion
       connection source prefix source-context)
      (when (and (fumos-completion-cache-p cache)
                 (eql (fumos-completion-cache-generation cache)
                      (fumos-connection-generation connection)))
        (let ((candidates
               (copy-sequence (fumos-completion-cache-candidates cache)))
              (kinds (copy-tree (fumos-completion-cache-kinds cache))))
          (list start end candidates
                :annotation-function
                (apply-partially #'fumos-eval--completion-annotation kinds)
                :company-kind
                (apply-partially #'fumos-eval--completion-kind kinds)))))))

(defun fumos-eval--release-tooling-markers (connection)
  "Cancel and release every pending tooling resource for CONNECTION."
  (dolist (query (list (fumos-connection-help-pending connection)
                       (fumos-connection-xref-pending connection)
                       (fumos-connection-eldoc-pending connection)))
    (when (fumos-tool-query-p query)
      (fumos-eval--dispose-query query t)))
  (let (queries)
    (when (hash-table-p (fumos-connection-completion-pending connection))
      (maphash
       (lambda (_key query)
         (when (fumos-completion-query-p query) (push query queries)))
       (fumos-connection-completion-pending connection)))
    (dolist (query queries)
      (fumos-eval--dispose-completion-query query t))))

(defun fumos-eval--invalidate-source-tooling (connection _source)
  "Invalidate pending source tooling before CONNECTION ownership changes."
  (fumos-eval--release-tooling-markers connection)
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
        (1+ (or (fumos-connection-completion-epoch connection) 0)))
  (dolist (table (list (fumos-connection-xref-cache connection)
                       (fumos-connection-completion-pending connection)
                       (fumos-connection-completion-cache connection)))
    (when (hash-table-p table)
      (clrhash table))))

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
        :error
        (fumos-eval--error-callback
         connection (plist-get source :authority) 'utf8-byte))))))

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
      :error (fumos-eval--error-callback connection)))))

(defvar fumos-eval--last-generated-lua nil
  "Last Lua source returned by an explicit compile preview.")

(defconst fumos-eval--compile-wire-prefix-lines 2
  "Synthetic lines before source in a pinned persistent REPL compile.")

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
        (if (<= wire-line fumos-eval--compile-wire-prefix-lines)
            message
          (let* ((source-line
                  (- wire-line fumos-eval--compile-wire-prefix-lines))
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
  (let ((context
         (fumos-eval--connection-context
          connection (plist-get source :authority) 'character)))
    (lambda (type message traceback)
      (let ((compilation-error-screen-columns nil)
            (fumos-repl--error-context context))
        (fumos-repl--default-error-handler
         type
         (if (fumos-repl--owns-transport-p
              connection process generation)
             (fumos-eval--remap-compile-message
              message source source-start-line source-start-column)
           message)
         traceback)))))

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
  token-digest deadline candidate)

(defun fumos-eval--set-game-reload-operation-candidate (operation connection)
  "Set OPERATION's provisional CONNECTION."
  (setf (fumos-game-reload-operation-candidate operation) connection))

(defun fumos-eval--linux-stat-start-ticks (contents)
  "Return Linux proc stat start ticks parsed from CONTENTS, or nil."
  (fumos-repl--linux-stat-start-ticks contents))

(defun fumos-eval--linux-process-start-identity (pid)
  "Return PID's stable Linux kernel start identity, or nil."
  (fumos-repl--linux-process-start-identity pid))

(defun fumos-eval--process-start-identity (pid)
  "Return PID's normalized current-user process start identity, or nil."
  (fumos-repl--process-start-identity pid))

(defun fumos-eval--canonical-game-root (root)
  "Return ROOT as a canonical local directory, or signal `user-error'."
  (or (fumos-repl--canonical-local-root root)
      (user-error "Cannot canonicalize FUMOS game reload root")))

(defun fumos-eval--begin-game-reload (connection mode)
  "Reserve and return one token-free game reload operation."
  (unless (member mode '("temp" "save" "none"))
    (user-error "Invalid FUMOS game reload mode"))
  (unless (and (numberp fumos-game-reload-timeout)
               (> fumos-game-reload-timeout 0))
    (user-error "FUMOS game reload timeout must be positive"))
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
    (fumos-repl--cancel-attach-operation
     (fumos-connection-attach-operation connection))
    (fumos-repl--cancel-launch-for-instance instance)
    (fumos-repl--cancel-reconnect-for-root root)
    (fumos-repl--cancel-game-reload-for-root root)
    (let ((generation
           (1+ (or (fumos-connection-game-reload-generation connection) 0))))
      (setf (fumos-connection-game-reload-generation connection) generation
            (fumos-connection-pending-game-reload connection) mode)
      (let ((operation
             (make-fumos-game-reload-operation
              :connection connection :generation generation
              :transport-generation (fumos-connection-generation connection)
              :mode mode :pid pid :root root :start-identity start-identity
              :token-digest token-digest
              :deadline (+ (float-time) fumos-game-reload-timeout))))
        (puthash root operation fumos-repl--game-reload-operations)
        (setf (fumos-connection-attach-operation connection) operation)
        operation))))

(defun fumos-eval--game-operation-current-p (operation)
  "Return non-nil while OPERATION still owns its connection intent."
  (let ((connection (fumos-game-reload-operation-connection operation)))
    (and (fumos-connection-p connection)
         (eq operation
             (gethash (fumos-game-reload-operation-root operation)
                      fumos-repl--game-reload-operations))
         (eql (fumos-game-reload-operation-generation operation)
              (fumos-connection-game-reload-generation connection))
         (eql (fumos-game-reload-operation-transport-generation operation)
              (fumos-connection-generation connection))
         (equal (fumos-game-reload-operation-mode operation)
                (fumos-connection-pending-game-reload connection)))))

(defun fumos-eval--cancel-game-reload-operation (operation)
  "Cancel OPERATION only while it still owns its connection."
  (when (eq operation
            (gethash (fumos-game-reload-operation-root operation)
                     fumos-repl--game-reload-operations))
    (remhash (fumos-game-reload-operation-root operation)
             fumos-repl--game-reload-operations)
    (fumos-repl--release-attach-operation-candidate operation)
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
  (if (not (fumos-eval--game-operation-current-p operation))
      (fumos-eval--cancel-game-reload-operation operation)
    (condition-case nil
        (let* ((pid (fumos-game-reload-operation-pid operation))
               (root (fumos-game-reload-operation-root operation))
               (status (fumos-repl--attach-candidate-status operation))
               (candidate
                (fumos-repl--attach-operation-candidate operation))
               (expected
                (fumos-game-reload-operation-start-identity operation))
               (before (fumos-eval--process-start-identity pid)))
          (when (eq status 'failed)
            (fumos-repl--release-attach-operation-candidate operation)
            (setq status nil))
          (cond
           ((not (equal before expected))
            (when (fumos-eval--cancel-game-reload-operation operation)
              (fumos-repl--close-provisional-connection
               candidate "FUMOS game reload process identity changed")
              (message "FUMOS game reload process changed for PID %d" pid)))
           ((eq status 'ready)
            (fumos-eval--cancel-game-reload-operation operation))
           ((>= (float-time)
                (fumos-game-reload-operation-deadline operation))
            (let ((candidate
                   (fumos-repl--attach-operation-candidate operation)))
              (when (fumos-eval--cancel-game-reload-operation operation)
                (fumos-repl--close-provisional-connection
                 candidate "FUMOS game reload reconnect timed out")
                (message "FUMOS game reload timed out waiting for PID %d"
                         pid))))
           ((eq status 'pending) nil)
           (t
            (let* ((candidates (fumos-discover-instances root))
                   (after (fumos-eval--process-start-identity pid))
                   (identity-current (equal after expected))
                   (match
                    (and
                     identity-current
                     (seq-find
                      (lambda (candidate)
                        (and
                         (= pid (fumos-instance-pid candidate))
                         (fumos-eval--candidate-root-matches-p candidate root)
                         (fumos-eval--candidate-token-changed-p
                          candidate operation)))
                      candidates))))
              (cond
               ((not identity-current)
                (fumos-eval--cancel-game-reload-operation operation))
               (match
                (condition-case nil
                    (let ((replacement
                           (fumos-repl-connect-instance match operation)))
                      (fumos-repl--set-attach-operation-candidate
                       operation replacement))
                  ((error quit) nil))))))))
      ((error quit)
       ;; Keep the operation retryable until its fixed deadline.
       nil))))

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
                          "  (let [(ok err) "
                          "(_G.Mod.libs.fumos.requestGameReload %S)]\n"
                          "    (if ok ok (error err))))")
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
