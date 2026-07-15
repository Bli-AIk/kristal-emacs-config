;;; fumos-eval.el --- Explicit FUMOS evaluation commands -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'cl-lib)
(require 'subr-x)
(require 'thingatpt)
(require 'fumos-repl)

(defun fumos-eval--connection ()
  "Return the current FUMOS connection."
  (or (fumos-repl-current-connection)
      (user-error "No FUMOS connection")))

(defun fumos-eval--source (position)
  "Return canonical source metadata for buffer POSITION."
  (when buffer-file-name
    (when (or (file-remote-p buffer-file-name)
              (not (file-name-absolute-p buffer-file-name)))
      (user-error "FUMOS source must be a local absolute file"))
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

(provide 'fumos-eval)
;;; fumos-eval.el ends here
