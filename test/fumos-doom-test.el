;;; fumos-doom-test.el --- Doom FUMOS binding tests -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(defun fumos-test-doom--harness-value (function)
  "Call FUNCTION, mapping pre-ERT harness errors to exit status 2."
  (condition-case error-data
      (funcall function)
    (error
     (message "FUMOS Doom harness setup error: %S" error-data)
     (princ (format "FUMOS Doom harness setup error: %S\n" error-data)
            'external-debugging-output)
     (kill-emacs 2))))

;; These dependencies must be resident before the project entrypoint is loaded.
;; In particular, the snapshots below describe the user's ordinary modes.
(fumos-test-doom--harness-value
 (lambda ()
   (require 'fennel-mode)
   (require 'fennel-proto-repl)
   (require 'lua-mode)
   (require 'evil)
   (require 'which-key)))

(defconst fumos-test-doom-states
  '(normal visual motion insert emacs))

(defconst fumos-test-doom-source-bindings
  '(("'" . fumos-connect-or-switch)
    (";" . fumos-attach)
    ("m" . fumos-macroexpand)
    ("R" . fumos-reload-game-preserve)
    ("S" . fumos-reload-game-save)
    ("0" . fumos-reload-game-from-start)
    ("c c" . fumos-reload-current-file)
    ("c m" . fumos-reload-module)
    ("c f" . fumos-compile-defun)
    ("c b" . fumos-compile-buffer)
    ("e b" . fumos-eval-buffer)
    ("e d" . fumos-eval-defun-overlay)
    ("e e" . fumos-eval-last-sexp)
    ("e E" . fumos-eval-print-last-sexp)
    ("e f" . fumos-eval-defun-async)
    ("e n" . fumos-eval-form-and-next)
    ("e r" . fumos-eval-region)
    ("g b" . xref-go-back)
    ("g d" . fumos-find-definition)
    ("g D" . fumos-find-definition-other-window)
    ("g n" . fumos-next-error)
    ("g N" . fumos-previous-error)
    ("h a" . fumos-apropos)
    ("h h" . fumos-show-documentation)
    ("h A" . fumos-show-arglist)
    ("h m" . fumos-macroexpand)
    ("h l" . fumos-show-generated-lua)
    ("r a" . fumos-attach)
    ("r c" . fumos-clear-repl)
    ("r i" . fumos-interrupt)
    ("r q" . fumos-disconnect)
    ("r r" . fumos-reconnect)
    ("r s" . fumos-switch-to-repl)
    ("r R" . fumos-reload-game-preserve)
    ("r L" . fumos-reload-game-save)
    ("r 0" . fumos-reload-game-from-start)))

(defconst fumos-test-doom-prefix-descriptions
  '(("c" . "compile/reload")
    ("e" . "evaluate")
    ("g" . "goto")
    ("h" . "help")
    ("r" . "repl")))

(defconst fumos-test-doom-repo
  (fumos-test-doom--harness-value
   (lambda ()
     (let ((repo (getenv "FUMOS_DOOM_REPO")))
       (unless (and repo (file-directory-p repo))
         (error "FUMOS_DOOM_REPO must name the repository under test"))
       (file-name-as-directory (file-truename repo))))))

(defconst fumos-test-doom-entry
  (expand-file-name "init.el" fumos-test-doom-repo))

(defun fumos-test-doom-localleader (state)
  "Return the configured localleader for STATE."
  (if (memq state '(normal visual motion))
      doom-localleader-key
    doom-localleader-alt-key))

(defun fumos-test-doom-wrong-localleader (state)
  "Return the localleader that must not be installed for STATE."
  (if (memq state '(normal visual motion))
      doom-localleader-alt-key
    doom-localleader-key))

(defun fumos-test-doom-enter-state (state)
  "Enter Evil STATE in the current test buffer."
  (evil-local-mode 1)
  (funcall (intern (format "evil-%s-state" state))))

(defun fumos-test-doom-key (state suffix)
  "Build STATE's final localleader key ending in SUFFIX."
  (concat (fumos-test-doom-localleader state) " " suffix))

(defun fumos-test-doom-wrong-key (state suffix)
  "Build STATE's incorrect localleader key ending in SUFFIX."
  (concat (fumos-test-doom-wrong-localleader state) " " suffix))

(defun fumos-test-doom-observe-binding (mode state key)
  "Return MODE's binding for KEY in a fresh buffer in Evil STATE."
  (with-temp-buffer
    (setq default-directory temporary-file-directory)
    (funcall mode)
    (fumos-test-doom-enter-state state)
    (key-binding (kbd key))))

(defun fumos-test-doom-snapshot-source (wrong-prefix-p)
  "Snapshot ordinary Fennel source bindings.
When WRONG-PREFIX-P is non-nil, query the localleader for the wrong state."
  (let (snapshot)
    (dolist (state fumos-test-doom-states)
      (dolist (binding fumos-test-doom-source-bindings)
        (let* ((suffix (car binding))
               (key (if wrong-prefix-p
                        (fumos-test-doom-wrong-key state suffix)
                      (fumos-test-doom-key state suffix))))
          (push (cons (cons state suffix)
                      (fumos-test-doom-observe-binding
                       #'fennel-mode state key))
                snapshot))))
    (nreverse snapshot)))

(defun fumos-test-doom-snapshot-proto (wrong-prefix-p)
  "Snapshot ordinary proto REPL `r i' in all Evil states.
When WRONG-PREFIX-P is non-nil, query the localleader for the wrong state."
  (let (snapshot)
    (dolist (state fumos-test-doom-states)
      (push (cons state
                  (fumos-test-doom-observe-binding
                   #'fennel-proto-repl-mode state
                   (if wrong-prefix-p
                       (fumos-test-doom-wrong-key state "r i")
                     (fumos-test-doom-key state "r i"))))
            snapshot))
    (nreverse snapshot)))

(defun fumos-test-doom-snapshot-lua ()
  "Snapshot the user's ordinary Lua `l' and `L' bindings in all states."
  (let (snapshot)
    (dolist (state fumos-test-doom-states)
      (dolist (suffix '("l" "L"))
        (push (cons (cons state suffix)
                    (fumos-test-doom-observe-binding
                     #'lua-mode state (fumos-test-doom-key state suffix)))
              snapshot)))
    (nreverse snapshot)))

(fumos-test-doom--harness-value
 (lambda ()
   (when (or (featurep 'kristal-emacs-config)
             (featurep 'fumos-doom)
             (fboundp 'fumos-doom-install))
     (error "A FUMOS entrypoint was loaded before Doom identity snapshots"))))

(defconst fumos-test-doom-source-before
  (fumos-test-doom--harness-value
   (lambda () (fumos-test-doom-snapshot-source nil))))
(defconst fumos-test-doom-source-wrong-before
  (fumos-test-doom--harness-value
   (lambda () (fumos-test-doom-snapshot-source t))))
(defconst fumos-test-doom-proto-before
  (fumos-test-doom--harness-value
   (lambda () (fumos-test-doom-snapshot-proto nil))))
(defconst fumos-test-doom-proto-wrong-before
  (fumos-test-doom--harness-value
   (lambda () (fumos-test-doom-snapshot-proto t))))
(defconst fumos-test-doom-lua-before
  (fumos-test-doom--harness-value
   #'fumos-test-doom-snapshot-lua))

;; The public project entrypoint is the only permitted route to fumos-doom.
(fumos-test-doom--harness-value
 (lambda ()
   (load fumos-test-doom-entry nil 'nomessage)
   (require 'ert)))

(defvar fumos-test-doom--late-startup-calls 0)

(defun fumos-test-doom--record-late-startup (&rest _)
  "Record a `doom-startup' call made after this harness was loaded."
  (setq fumos-test-doom--late-startup-calls
        (1+ fumos-test-doom--late-startup-calls)))

;; This advice is deliberately installed only after the normal init completed.
(advice-add 'doom-startup :after #'fumos-test-doom--record-late-startup)

(defun fumos-test-doom-snapshot-value (snapshot state &optional suffix)
  "Read STATE and optional SUFFIX from identity SNAPSHOT."
  (cdr (assoc (if suffix (cons state suffix) state) snapshot)))

(defun fumos-test-doom-expected-command (command)
  "Return COMMAND after standard command remapping in the current buffer."
  (or (command-remapping command) command))

(defun fumos-test-doom-assert-source-enabled ()
  "Assert every source binding and wrong-prefix identity in this buffer."
  (dolist (state fumos-test-doom-states)
    (fumos-test-doom-enter-state state)
    (dolist (binding fumos-test-doom-source-bindings)
      (let ((suffix (car binding))
            (command (cdr binding)))
        (should
         (eq (key-binding (kbd (fumos-test-doom-key state suffix)))
             (fumos-test-doom-expected-command command)))
        (should
         (eq (key-binding (kbd (fumos-test-doom-wrong-key state suffix)))
             (fumos-test-doom-snapshot-value
              fumos-test-doom-source-wrong-before state suffix)))))))

(defun fumos-test-doom-assert-source-disabled ()
  "Assert this buffer has exactly its ordinary source binding identities."
  (should-not fumos-mode)
  (should-not (assq 'fumos-mode minor-mode-overriding-map-alist))
  (dolist (state fumos-test-doom-states)
    (fumos-test-doom-enter-state state)
    (dolist (binding fumos-test-doom-source-bindings)
      (let ((suffix (car binding)))
        (should
         (eq (key-binding (kbd (fumos-test-doom-key state suffix)))
             (fumos-test-doom-snapshot-value
              fumos-test-doom-source-before state suffix)))
        (should
         (eq (key-binding (kbd (fumos-test-doom-wrong-key state suffix)))
             (fumos-test-doom-snapshot-value
              fumos-test-doom-source-wrong-before state suffix)))))))

(defun fumos-test-doom-assert-prefix-line (text key description)
  "Assert a which-key dump TEXT has KEY and DESCRIPTION on one line."
  (should
   (string-match-p
    (format "^%s[[:space:]]*:[^\n]*%s[[:space:]]*$"
            (regexp-quote key)
            (regexp-quote description))
    text)))

(defun fumos-test-doom-dump-current (state)
  "Return the public which-key localleader dump for STATE's current buffer."
  (let ((name (generate-new-buffer-name " *fumos-doom-which-key*")))
    (unwind-protect
        (save-window-excursion
          (which-key-dump-bindings
           (fumos-test-doom-localleader state) name)
          (with-current-buffer name
            (buffer-substring-no-properties (point-min) (point-max))))
      (when (get-buffer name)
        (kill-buffer name)))))

(defun fumos-test-doom-source-dumps ()
  "Return all five public which-key dumps for an enabled FUMOS source buffer."
  (with-temp-buffer
    (setq default-directory temporary-file-directory)
    (fennel-mode)
    (fumos-mode 1)
    (let (dumps)
      (dolist (state fumos-test-doom-states)
        (fumos-test-doom-enter-state state)
        (push (cons state (fumos-test-doom-dump-current state)) dumps))
      (nreverse dumps))))

(defun fumos-test-doom-prefix-map-identities ()
  "Return source prefix map identities for all five Evil states."
  (let (maps)
    (dolist (state fumos-test-doom-states)
      (let ((state-map (evil-get-auxiliary-keymap fumos-mode-map state)))
        (unless state-map
          (error "fumos-mode-map has no Evil %s state map" state))
        (dolist (prefix fumos-test-doom-prefix-descriptions)
          (push (cons (cons state (car prefix))
                      (lookup-key
                       state-map
                       (kbd (fumos-test-doom-key state (car prefix)))))
                maps))))
    (nreverse maps)))

(defun fumos-test-doom-assert-snapshot-identical (before after)
  "Assert every value in identity snapshots BEFORE and AFTER is `eq'."
  (should (= (length before) (length after)))
  (dolist (entry before)
    (let ((after-entry (assoc (car entry) after)))
      (should after-entry)
      (should (eq (cdr entry) (cdr after-entry))))))

(ert-deftest fumos-doom-entry-loads-pinned-vendor ()
  (should (featurep 'kristal-emacs-config))
  (should (featurep 'fumos-doom))
  (let ((vendor (expand-file-name "vendor/fennel-mode/"
                                  fumos-test-doom-repo)))
    (dolist (library '(("fennel-mode" fennel-mode "fennel-mode.el")
                       ("fennel-proto-repl" fennel-proto-repl-mode
                        "fennel-proto-repl.el")))
      (let ((expected (file-truename (expand-file-name (nth 2 library) vendor))))
        (should (equal (file-truename (locate-library (nth 0 library)))
                       expected))
        (should (equal (file-truename (symbol-file (nth 1 library) 'defun))
                       expected))))
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (expand-file-name "UPSTREAM" vendor))
        (buffer-substring-no-properties (point-min) (point-max)))
      (concat "https://git.sr.ht/~technomancy/fennel-mode\n"
              "bbc28a629405de628880d8fb485fce23ff7fab69\n"))))
  (should (stringp fennel-proto-repl--protocol))
  (should (string-match-p ":version \\\"0\\.6\\.4\\\""
                          fennel-proto-repl--protocol)))

(ert-deftest fumos-doom-source-has-all-bindings-in-five-states ()
  (let ((suffixes (mapcar #'car fumos-test-doom-source-bindings)))
    (should (= 36 (length suffixes)))
    (should (= (length suffixes)
               (length (delete-dups (copy-sequence suffixes))))))
  (with-temp-buffer
    (setq default-directory temporary-file-directory)
    (fennel-mode)
    (fumos-mode 1)
    (fumos-test-doom-assert-source-enabled)))

(ert-deftest fumos-doom-prefixes-remain-keymaps-in-five-states ()
  (with-temp-buffer
    (setq default-directory temporary-file-directory)
    (fennel-mode)
    (fumos-mode 1)
    (dolist (state fumos-test-doom-states)
      (fumos-test-doom-enter-state state)
      (dolist (prefix fumos-test-doom-prefix-descriptions)
        (should
         (keymapp
          (key-binding
           (kbd (fumos-test-doom-key state (car prefix)))))))
      (dolist (binding fumos-test-doom-source-bindings)
        (should
         (key-binding
          (kbd (fumos-test-doom-key state (car binding)))))))))

(ert-deftest fumos-doom-which-key-shows-five-prefix-groups ()
  (dolist (dump (fumos-test-doom-source-dumps))
    (dolist (prefix fumos-test-doom-prefix-descriptions)
      (fumos-test-doom-assert-prefix-line
       (cdr dump) (car prefix) (cdr prefix)))))

(ert-deftest fumos-doom-disable-and-reenable-is-final ()
  (with-temp-buffer
    (setq default-directory temporary-file-directory)
    (fennel-mode)
    (fumos-mode 1)
    (fumos-test-doom-assert-source-enabled)
    (fumos-mode -1)
    (fumos-test-doom-assert-source-disabled)
    (fumos-mode 1)
    (fumos-test-doom-assert-source-enabled)))

(ert-deftest fumos-doom-entry-and-installer-are-idempotent ()
  (with-temp-buffer
    (setq default-directory temporary-file-directory)
    (fennel-mode)
    (fumos-mode 1)
    (let ((source-map fumos-mode-map)
          (repl-map fumos-repl-mode-map)
          (prefix-maps (fumos-test-doom-prefix-map-identities))
          (dumps (fumos-test-doom-source-dumps))
          (replacements (copy-tree which-key-replacement-alist)))
      (load fumos-test-doom-entry nil 'nomessage)
      (fumos-doom-install)
      (fumos-doom-install)
      (should (eq source-map fumos-mode-map))
      (should (eq repl-map fumos-repl-mode-map))
      (dolist (entry prefix-maps)
        (let ((current (assoc (car entry)
                              (fumos-test-doom-prefix-map-identities))))
          (should current)
          (should (eq (cdr entry) (cdr current)))))
      (fumos-test-doom-assert-source-enabled)
      (should (equal dumps (fumos-test-doom-source-dumps)))
      (should (equal replacements which-key-replacement-alist)))))

(ert-deftest fumos-doom-repl-interrupt-exists-in-five-states ()
  (with-temp-buffer
    (setq default-directory temporary-file-directory)
    (fennel-proto-repl-mode)
    (dolist (state fumos-test-doom-states)
      (fumos-test-doom-enter-state state)
      (should
       (eq (key-binding (kbd (fumos-test-doom-key state "r i")))
           (fumos-test-doom-snapshot-value
            fumos-test-doom-proto-before state)))
      (should
       (eq (key-binding (kbd (fumos-test-doom-wrong-key state "r i")))
           (fumos-test-doom-snapshot-value
            fumos-test-doom-proto-wrong-before state))))
    (fumos-repl-mode 1)
    (dolist (state fumos-test-doom-states)
      (fumos-test-doom-enter-state state)
      (should
       (eq (key-binding (kbd (fumos-test-doom-key state "r i")))
           (fumos-test-doom-expected-command 'fumos-interrupt)))
      (should
       (keymapp (key-binding (kbd (fumos-test-doom-key state "r")))))
      (fumos-test-doom-assert-prefix-line
       (fumos-test-doom-dump-current state) "r" "repl")
      (should
       (eq (key-binding (kbd (fumos-test-doom-wrong-key state "r i")))
           (fumos-test-doom-snapshot-value
            fumos-test-doom-proto-wrong-before state))))
    (fumos-repl-mode -1)
    (should-not fumos-repl-mode)
    (dolist (state fumos-test-doom-states)
      (fumos-test-doom-enter-state state)
      (should
       (eq (key-binding (kbd (fumos-test-doom-key state "r i")))
           (fumos-test-doom-snapshot-value
            fumos-test-doom-proto-before state)))
      (should
       (eq (key-binding (kbd (fumos-test-doom-wrong-key state "r i")))
           (fumos-test-doom-snapshot-value
            fumos-test-doom-proto-wrong-before state))))
    (fumos-repl-mode 1)
    (dolist (state fumos-test-doom-states)
      (fumos-test-doom-enter-state state)
      (should
       (eq (key-binding (kbd (fumos-test-doom-key state "r i")))
           (fumos-test-doom-expected-command 'fumos-interrupt)))
      (should
       (eq (key-binding (kbd (fumos-test-doom-wrong-key state "r i")))
           (fumos-test-doom-snapshot-value
            fumos-test-doom-proto-wrong-before state))))))

(ert-deftest fumos-doom-leaves-ordinary-modes-untouched ()
  (fumos-test-doom-assert-snapshot-identical
   fumos-test-doom-source-before
   (fumos-test-doom-snapshot-source nil))
  (fumos-test-doom-assert-snapshot-identical
   fumos-test-doom-source-wrong-before
   (fumos-test-doom-snapshot-source t))
  (fumos-test-doom-assert-snapshot-identical
   fumos-test-doom-proto-before
   (fumos-test-doom-snapshot-proto nil))
  (fumos-test-doom-assert-snapshot-identical
   fumos-test-doom-proto-wrong-before
   (fumos-test-doom-snapshot-proto t))
  (fumos-test-doom-assert-snapshot-identical
   fumos-test-doom-lua-before
   (fumos-test-doom-snapshot-lua)))

(ert-deftest fumos-doom-uses-completed-interactive-startup ()
  (should-not noninteractive)
  (dolist (feature '(doom evil which-key general))
    (should (featurep feature)))
  (should (numberp doom-init-time))
  (should (> doom-init-time 0))
  (should-not (doom-context-p 'startup))
  ;; This harness attached after normal init; it can only rule out a replay.
  (should (= 0 fumos-test-doom--late-startup-calls)))

(defun fumos-test-doom--write-result (format-string &rest arguments)
  "Write one harness result using FORMAT-STRING and ARGUMENTS to stderr."
  (princ (apply #'format format-string arguments)
         'external-debugging-output))

(defun fumos-test-doom--report-stats (stats)
  "Report dynamic totals and unexpected test names from ERT STATS."
  (let ((tests (ert--stats-tests stats))
        (results (ert--stats-test-results stats))
        (unexpected (ert-stats-completed-unexpected stats)))
    (fumos-test-doom--write-result
     "FUMOS Doom ERT: total=%d unexpected=%d\n"
     (ert-stats-total stats) unexpected)
    (dotimes (index (length tests))
      (let ((test (aref tests index))
            (result (aref results index)))
        (unless (ert-test-result-expected-p test result)
          (fumos-test-doom--write-result
           "FUMOS Doom ERT unexpected: %S%s\n"
           (ert-test-name test)
           (ert-reason-for-test-result result)))))))

(defun fumos-test-doom-runner ()
  "Run Doom ERT interactively and preserve the 0/1/2 harness contract."
  (condition-case error-data
      (let* ((stats (ert-run-tests-batch "^fumos-doom-"))
             (unexpected (ert-stats-completed-unexpected stats)))
        (fumos-test-doom--report-stats stats)
        (kill-emacs (if (zerop unexpected) 0 1)))
    (error
     (message "FUMOS Doom harness error: %S" error-data)
     (fumos-test-doom--write-result
      "FUMOS Doom harness error: %S\n" error-data)
     (kill-emacs 2))))

(add-hook 'emacs-startup-hook #'fumos-test-doom-runner 100)

;;; fumos-doom-test.el ends here
