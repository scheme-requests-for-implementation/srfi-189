;;;; Test framework for CHICKEN Scheme's 'test' egg.
(import test)

;;; SRFI 78 wrapper
(define-syntax check
  (syntax-rules (=>)
    ((check expr => expect)
     (test expect expr))))

(define (check-report)
  (newline)
  (display ";;; Testing completed.")
  (newline))

(include "test-body.scm")
(include "test-syntax.scm")
