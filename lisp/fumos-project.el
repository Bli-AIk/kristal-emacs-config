;;; fumos-project.el --- Project-local FUMOS support -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'cl-lib)

(defvar fumos-project--installed nil)

(defvar fumos-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-e") #'fumos-eval-last-sexp)
    (define-key map (kbd "C-M-x") #'fumos-eval-defun)
    (define-key map (kbd "C-c C-r") #'fumos-eval-region)
    (define-key map (kbd "C-c C-b") #'fumos-eval-buffer)
    (define-key map (kbd "C-c C-k") #'fumos-reload-current-file)
    (define-key map (kbd "C-c C-z") #'fumos-switch-to-repl)
    map))

(define-minor-mode fumos-mode
  "Enable explicit FUMOS live-development commands."
  :lighter " FUMOS"
  :keymap fumos-mode-map
  (if fumos-mode
      (setq-local
       minor-mode-overriding-map-alist
       (cons
        (cons 'fumos-mode fumos-mode-map)
        (assq-delete-all
         'fumos-mode (copy-tree minor-mode-overriding-map-alist))))
    (when (assq 'fumos-mode minor-mode-overriding-map-alist)
      (setq-local
       minor-mode-overriding-map-alist
       (assq-delete-all
        'fumos-mode (copy-tree minor-mode-overriding-map-alist)))))
  (unless fumos-mode
    (when (fboundp 'fumos-repl-unlink-current-buffer)
      (fumos-repl-unlink-current-buffer))))

(defun fumos-project-p (&optional directory)
  "Return the canonical FUMOS root containing DIRECTORY, or nil."
  (let* ((start (file-name-as-directory
                 (expand-file-name (or directory default-directory))))
         (root
          (locate-dominating-file
           start
           (lambda (candidate)
             (and (file-readable-p (expand-file-name "mod.json" candidate))
                  (file-readable-p
                   (expand-file-name "libraries/fumos/lib.json" candidate))
                  (file-readable-p
                   (expand-file-name ".emacs/init.el" candidate)))))))
    (when root
      (file-name-as-directory (file-truename root)))))

(defalias 'fumos-project-root #'fumos-project-p)

(defun fumos-project-activate ()
  "Enable `fumos-mode' when the current file belongs to a FUMOS project."
  (if (and buffer-file-name
           (fumos-project-root (file-name-directory buffer-file-name)))
      (progn
        (fumos-mode 1)
        (when (fboundp 'fumos-repl-link-current-buffer)
          (fumos-repl-link-current-buffer)))
    (fumos-mode -1)))

(defun fumos-project-install ()
  "Install project hooks once."
  (unless fumos-project--installed
    (setq fumos-project--installed t)
    (add-to-list 'auto-mode-alist '("\\.fnlm\\'" . fennel-mode))
    (add-hook 'fennel-mode-hook #'fumos-project-activate)))

(provide 'fumos-project)
;;; fumos-project.el ends here
