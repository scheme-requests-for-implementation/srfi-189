;;;; Test framework for SRFI 64.

(import (srfi 64))

;;; SRFI 78 wrapper

(define-syntax check
  (syntax-rules (=>)
    ((check expr => expect)
     (test-equal expect expr))))

(define (check-report)
  (newline)
  (display ";;; Testing completed.")
  (newline))

(include "test-body.scm")
(include "test-syntax.scm")
