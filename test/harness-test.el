;;; harness-test.el --- Harness tests -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'test-helper)

(ert-deftest fumos-test-helper-observes-success ()
  (let ((ready nil))
    (run-at-time 0.01 nil (lambda () (setq ready t)))
    (should (fumos-test-wait-until (lambda () ready) 0.5))))

(ert-deftest fumos-test-helper-does-not-repeat-successful-predicate ()
  (let ((calls 0))
    (should
     (fumos-test-wait-until
      (lambda () (= 1 (cl-incf calls)))
      0.5))
    (should (= 1 calls))))

(ert-deftest fumos-test-helper-times-out ()
  (should-not (fumos-test-wait-until (lambda () nil) 0.02)))
