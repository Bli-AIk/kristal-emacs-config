;;; fumos-doom.el --- Doom bindings for FUMOS -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'fumos-eval)

(defvar fumos-doom--installed nil)

(defun fumos-doom-install ()
  "Install FUMOS Doom bindings once."
  (unless fumos-doom--installed
    (unless (fboundp 'map!)
      (error "fumos-doom requires Doom's map!"))
    (map! :map fumos-mode-map
          :localleader
          :desc "FUMOS connect/REPL" "'" #'fumos-connect-or-switch
          :desc "FUMOS attach instance" ";" #'fumos-attach
          :desc "Expand macro" "m" #'fumos-macroexpand
          :desc "Quick reload (Ctrl+R)" "R" #'fumos-reload-game-preserve
          :desc "Reload save (Ctrl+Shift+R)" "S" #'fumos-reload-game-save
          :desc "Restart mod (Ctrl+Alt+R)" "0" #'fumos-reload-game-from-start
          (:prefix ("c" . "compile/reload")
           :desc "Reload current file" "c" #'fumos-reload-current-file
           :desc "Reload module" "m" #'fumos-reload-module
           :desc "Compile top-level form" "f" #'fumos-compile-defun
           :desc "Compile buffer" "b" #'fumos-compile-buffer)
          (:prefix ("e" . "evaluate")
           :desc "Evaluate buffer" "b" #'fumos-eval-buffer
           :desc "Evaluate form with overlay" "d" #'fumos-eval-defun-overlay
           :desc "Evaluate previous sexp" "e" #'fumos-eval-last-sexp
           :desc "Evaluate and insert result" "E" #'fumos-eval-print-last-sexp
           :desc "Evaluate form asynchronously" "f" #'fumos-eval-defun-async
           :desc "Evaluate form and advance" "n" #'fumos-eval-form-and-next
           :desc "Evaluate region" "r" #'fumos-eval-region)
          (:prefix ("g" . "goto")
           :desc "Go back" "b" #'xref-go-back
           :desc "Go to definition" "d" #'fumos-find-definition
           :desc "Go to definition other window" "D" #'fumos-find-definition-other-window
           :desc "Next FUMOS error" "n" #'fumos-next-error
           :desc "Previous FUMOS error" "N" #'fumos-previous-error)
          (:prefix ("h" . "help")
           :desc "Apropos" "a" #'fumos-apropos
           :desc "Symbol documentation" "h" #'fumos-show-documentation
           :desc "Argument list" "A" #'fumos-show-arglist
           :desc "Expand macro" "m" #'fumos-macroexpand
           :desc "Generated Lua" "l" #'fumos-show-generated-lua)
          (:prefix ("r" . "repl")
           :desc "Attach instance" "a" #'fumos-attach
           :desc "Clear REPL" "c" #'fumos-clear-repl
           :desc "Interrupt evaluation" "i" #'fumos-interrupt
           :desc "Disconnect" "q" #'fumos-disconnect
           :desc "Reconnect same PID" "r" #'fumos-reconnect
           :desc "Switch to REPL" "s" #'fumos-switch-to-repl
           :desc "Reload preserving state" "R" #'fumos-reload-game-preserve
           :desc "Reload latest save" "L" #'fumos-reload-game-save
           :desc "Reload from beginning" "0" #'fumos-reload-game-from-start))
    (map! :map fumos-repl-mode-map
          :localleader
          (:prefix ("r" . "repl")
           :desc "Interrupt evaluation" "i" #'fumos-interrupt))
    ;; Commit only after both maps are complete; replaying the first is safe.
    (setq fumos-doom--installed t)))

(fumos-doom-install)

(provide 'fumos-doom)
;;; fumos-doom.el ends here
