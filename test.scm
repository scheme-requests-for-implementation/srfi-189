;; Copyright (C) 2020 Wolfgang Corcoran-Mathe

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be included
;; in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(import (scheme base))
(import (scheme write))
(import (srfi 189))

(cond-expand
  ((library (srfi 78))
   (import (srfi 78)))
  (else
    (begin
      (define-syntax check
        (syntax-rules (=>)
          ((check expr)
           (check expr => #t))
          ((check expr => expected)
           (if expr
             (begin
               (display 'expr)
               (newline))
             (begin
               (display "FAILED: for ")
               (display 'expr)
               (display " expected ")
               (display expected)
               (display " but got ")
               (display expr)
               (newline))
             )))))))


;;; Utility

(define-syntax constantly
  (syntax-rules ()
    ((_ obj) (lambda (_) obj))))

(define always (constantly #t))
(define never (constantly #f))

;; verify that an Either is a Left of 'z, a dummy object
(define (left-of-z? e)
  (and (either? e) (either= eqv? e (left 'z))))

(define (left-of-nothing? e)
  (and (either? e) (either= eqv? e (left (nothing)))))

;;; Tests

(define (check-misc)
  ;; Uniqueness of the Nothing object.
  (check (eq? (nothing) (nothing)) => #t)

  ;; either-swap
  (check (either= eqv? (left #t) (either-swap (right #t))) => #t)
  (check (either= eqv? (right #t) (either-swap (left #t))) => #t))

;;; Predicates

(define (check-predicates)
  (check (just? (just 1))     => #t)
  (check (just? (nothing))    => #f)
  (check (nothing? (just 1))  => #f)
  (check (nothing? (nothing)) => #t)
  (check (maybe? (just 1))    => #t)
  (check (maybe? (nothing))   => #t)

  (check (right? (right 1))  => #t)
  (check (right? (left 1))   => #f)
  (check (left? (right 1))   => #f)
  (check (left? (left 1))    => #t)
  (check (either? (right 1)) => #t)
  (check (either? (left 1))  => #t)

  (check (maybe= eqv? (just #t) (just #t)) => #t)
  (check (maybe= eqv? (just #t) (just #f)) => #f)
  (check (maybe= eqv? (nothing) (nothing)) => #t)
  (check (maybe= eqv? (just #t) (nothing)) => #f)

  (check (either= eqv? (right #t) (right #t)) => #t)
  (check (either= eqv? (right #t) (right #f)) => #f)
  (check (either= eqv? (left #t) (left #t))   => #t)
  (check (either= eqv? (left #t) (left #f))   => #f)
  (check (either= eqv? (right #t) (left #t))  => #f))

;;; Accessors

(define (check-accessors)
  (check (maybe-ref (just #t))                       => #t)
  (check (maybe-ref (nothing) (lambda () #f))        => #f)
  (check (maybe-ref (just #t) (lambda () #f) values) => #t)
  (check (maybe-ref (nothing) (lambda () #f) values) => #f)

  (check (either-ref (right #t))                        => #t)
  (check (either-ref (left #t) (lambda (_) #f))         => #f)
  (check (either-ref (right #t) (lambda (_) #f) values) => #t)
  (check (either-ref (left #t) values (lambda (_) #f))  => #t)

  (check (maybe-ref/default (just #t) #f) => #t)
  (check (maybe-ref/default (nothing) #f) => #f)

  (check (either-ref/default (right #t) #f) => #t)
  (check (either-ref/default (left #t) #f)  => #f))

;;; Join and bind

(define (check-join-and-bind)
  ;; maybe-join
  (check (maybe= eqv? (maybe-join (just (just #t))) (just #t)) => #t)
  (check (maybe= eqv? (maybe-join (just #t)) (just #t))        => #t)
  (check (nothing? (maybe-join (just (nothing))))              => #t)
  (check (nothing? (maybe-join (nothing)))                     => #t)

  ;; either-join
  (check (either= eqv? (either-join (right (right #t))) (right #t)) => #t)
  (check (either= eqv? (either-join (right (left #t))) (left #t))   => #t)
  (check (either= eqv? (either-join (right #t)) (right #t))         => #t)
  (check (either= eqv? (either-join (left #t)) (left #t))           => #t)

  ;; maybe-bind
  (check (nothing? (maybe-bind (nothing) just)) => #t)

  (let ((m (just #t)))
    (check (maybe= eqv? m (maybe-bind m just)) => #t))

  (let ((k (lambda (n) (just (* n 2))))
        (h (lambda (n) (just (+ n 5))))
        (m (just 1)))
    (check
     (maybe= eqv? (maybe-bind m (lambda (n) (maybe-bind (k n) h)))
                  (maybe-bind (maybe-bind m k) h))
     => #t))

  (let ((mp (lambda (b) (just (not b)))))
    (check (maybe= eqv? (just #f) (maybe-bind (just #t) mp mp mp))
      => #t)
    (check (nothing? (maybe-bind (just #t) mp (lambda (_) (nothing)) mp))
      => #t))

  ;; maybe-compose
  (check (nothing? ((maybe-compose just) (nothing)))              => #t)
  (check (maybe= eqv? (just #t) ((maybe-compose just) (just #t))) => #t)

  (let ((mp (lambda (b) (just (not b)))))
    (check (maybe= eqv? (just #t) ((maybe-compose mp mp mp) (just #f)))
      => #t))

  ;; either-bind
  (check (left? (either-bind (left #f) right)) => #t)

  (let ((e (right #t)))
    (check (either= eqv? e (either-bind e right)) => #t))

  (let ((k (lambda (n) (right (* n 2))))
        (h (lambda (n) (right (+ n 5))))
        (e (right 1)))
    (check
     (either= eqv? (either-bind e (lambda (n) (either-bind (k n) h)))
                   (either-bind (either-bind e k) h))
     => #t))

  (let ((ep (lambda (b) (right (not b)))))
    (check (either= eqv? (right #f) (either-bind (right #t) ep ep ep))  => #t)
    (check (either= eqv? (left #f) (either-bind (right #t) ep left ep)) => #t))

  ;; either-compose
  (check (left? ((either-compose right) (left 'z)))                    => #t)
  (check (either= eqv? (right #t) ((either-compose right) (right #t))) => #t)

  (let ((ep (lambda (b) (right (not b)))))
    (check (either= eqv? (right #t) ((either-compose ep ep ep) (right #f)))
      => #t)))

;;; Sequence operations

(define (check-sequence-operations)
  (check (maybe-length (nothing)) => 0)
  (check (maybe-length (just #t)) => 1)

  (check (either-length (left #t))  => 0)
  (check (either-length (right #t)) => 1)

  ;; maybe-filter
  (check (maybe= eqv? (just #t) (maybe-filter always (just #t))) => #t)
  (check (nothing? (maybe-filter never (just #t)))               => #t)
  (check (nothing? (maybe-filter always (nothing)))              => #t)

  ;; maybe-remove
  (check (maybe= eqv? (just #t) (maybe-remove never (just #t)))  => #t)
  (check (nothing? (maybe-remove always (just #t)))              => #t)
  (check (nothing? (maybe-remove always (nothing)))              => #t)

  ;; maybe-sequence
  (check (maybe= equal? (maybe-sequence (map just (list 1 2)) map)
                        (just (list 1 2)))                          => #t)
  (check (nothing? (maybe-sequence (list (just #t) (nothing)) map)) => #t)

  ;; either-filter & either-remove
  (check (either= eqv? (right #t) (either-filter always (right #t) 'z)) => #t)
  (check (left-of-z? (either-filter never (right #t) 'z))               => #t)
  (check (left-of-z? (either-filter always (left #t) 'z))               => #t)

  (check (either= eqv? (right #t) (either-remove never (right #t) 'z)) => #t)
  (check (left-of-z? (either-remove always (right #t) 'z))             => #t)
  (check (left-of-z? (either-remove never (left #t) 'z))               => #t)

  ;; either-sequence
  (check (either= equal? (either-sequence (map right (list 1 2)) map #f)
                         (right (list 1 2)))
    => #t)
  (check (left-of-z? (either-sequence (list (right #t) (left #t)) map 'z))
    => #t))

;;; Conversion procedures

(define (check-conversions)
  ;; maybe->either and either->maybe
  (check (left-of-z? (maybe->either (nothing) 'z))           => #t)
  (check (either= eqv? (right #t) (maybe->either (just #t) 'z)) => #t)
  (check (nothing? (either->maybe (left #t)))                => #t)
  (check (maybe= eqv? (just #t) (either->maybe (right #t)))  => #t)

  ;; list->maybe and list->either
  (check (nothing? (list->maybe '()))                   => #t)
  (check (maybe= eqv? (just #t) (list->maybe '(#t)))    => #t)
  (check (left-of-z? (list->either '() 'z))             => #t)
  (check (either= eqv? (right #t) (list->either '(#t) 'z)) => #t)

  ;; maybe->list and either->list
  (check (maybe->list (nothing))   => '())
  (check (maybe->list (just #t))   => '(#t))
  (check (either->list (left #t))  => '())
  (check (either->list (right #t)) => '(#t))

  ;; maybe->lisp and lisp->maybe
  (check (maybe->lisp (nothing))                  => #f)
  (check (maybe->lisp (just #t))                  => #t)
  (check (nothing? (lisp->maybe #f))              => #t)
  (check (maybe= eqv? (lisp->maybe #t) (just #t)) => #t)

  ;; maybe->eof and eof->maybe
  (check (eof-object? (maybe->eof (nothing)))    => #t)
  (check (maybe->eof (just #t))                  => #t)
  (check (nothing? (eof->maybe (eof-object)))    => #t)
  (check (maybe= eqv? (eof->maybe #t) (just #t)) => #t)

  ;; maybe->values and friends
  (check (maybe->values (just #t)) => #t)
  (check (call-with-values (lambda () (maybe->two-values (nothing))) list)
    => '(#f #f))
  (check (call-with-values (lambda () (maybe->two-values (just #t))) list)
    => '(#t #t))
  (check (nothing? (values->maybe (lambda () (values))))
    => #t)
  (check (maybe= eqv? (just #t) (values->maybe (lambda () #t)))
    => #t)
  (check (maybe= eqv? (just #t) (values->maybe (lambda () (values #t #t))))
    => #t)
  (check (nothing? (values->maybe (lambda () (values #t #f))))
    => #t)

  ;; either->values and friends
  (check (either->values (right #t)) => #t)
  (check (call-with-values (lambda () (either->two-values (left #t))) list)
    => '(#t #f))
  (check (call-with-values (lambda () (either->two-values (right #t))) list)
    => '(#t #t))
  (check (left-of-nothing? (values->either (lambda () (values))))  => #t)
  (check (either= eqv? (right #t) (values->either (lambda () #t))) => #t)
  (check (either= eqv? (right #t) (values->either (lambda () (values #t #t))))
    => #t)
  (check (either= eqv? (left #t) (values->either (lambda () (values #t #f))))
    => #t))

;;; Map, fold, and unfold

(define (check-map-fold-and-unfold)
  ;; maybe-map
  (check (nothing? (maybe-map not (nothing)))              => #t)
  (check (maybe= eqv? (just #f) (maybe-map not (just #t))) => #t)

  ;; either-map
  (let ((e-left (left #t)))
    (check (eqv? e-left (either-map not e-left)) => #t))
  (check (either= eqv? (right #f) (either-map not (right #t))) => #t)

  ;; maybe-for-each
  (check (let ((x #f))
           (maybe-for-each (lambda (y) (set! x y)) (just #t))
           x)
    => #t)
  ; Given Nothing, ensure the proc argument is not executed.
  (check (let ((x #f))
           (maybe-for-each (lambda (_) (set! x #t)) (nothing))
           x)
    => #f)

  ;; either-for-each
  (check (let ((x #f))
           (either-for-each (lambda (y) (set! x y)) (right #t))
           x)
    => #t)
  ; Given a Left, ensure the proc argument is not executed.
  (check (let ((x #f))
           (either-for-each (lambda (_) (set! x #t)) (left 'z))
           x)
    => #f)

  (check (maybe-fold cons '() (nothing)) => '())
  (check (maybe-fold cons '() (just #t)) => '(#t))

  (check (either-fold cons '() (left #t))  => '())
  (check (either-fold cons '() (right #t)) => '(#t))

  (check (nothing? (maybe-unfold always not #f #f))              => #t)
  (check (maybe= eqv? (just #t) (maybe-unfold never not #f #f))  => #t)

  (check (left-of-z? (either-unfold always not #f 'z))               => #t)
  (check (either= eqv? (right #t) (either-unfold never not #f #f))   => #t))

;;; Trivalent logic

(define (check-trivalent)
  (define (tri-true? m)
    (and (just? m) (maybe-ref m)))

  (define (tri-false? m)
    (and (just? m) (not (maybe-ref m))))

  (check (tri-true? (tri-not (just #f)))  => #t)
  (check (tri-false? (tri-not (just #t))) => #t)
  (check (nothing? (tri-not (nothing)))   => #t)

  (check (tri-true? (tri=? (just #t) (just 1) (just 'x))) => #t)
  (check (tri-true? (tri=? (just #f) (just #f)))          => #t)
  (check (tri-true? (tri=? (just #f) (just #f)))          => #t)
  (check (tri-false? (tri=? (just #f) (just #t)))         => #t)
  (check (tri-false? (tri=? (just #f) (nothing)))         => #t)

  (check (tri-true? (tri-and (just #t) (just 1) (just 'x))) => #t)
  (check (nothing? (tri-and (just #t) (nothing)))           => #t)
  (check (tri-false? (tri-and (just #f) (just #t)))         => #t)
  (check (tri-true? (tri-and))                              => #t)

  (check (tri-false? (tri-or (just #f) (just #f) (just #f))) => #t)
  (check (nothing? (tri-or (just #f) (nothing)))             => #t)
  (let ((m-true (just 'x)))
    (check (maybe= eqv? m-true (tri-or (just #f) m-true))    => #t))
  (check (tri-false? (tri-or))                               => #t)

  (check (nothing? (tri-merge (nothing) (nothing) (nothing)))    => #t)
  (let ((m-true (just 'x)))
    (check (maybe= eqv? m-true (tri-merge (nothing) m-true))     => #t))
  (let ((m-false (just #f)))
    (check (maybe= eqv? m-false (tri-merge (nothing) m-false))   => #t))
  (check (nothing? (tri-merge))                                  => #t))

(define (check-all)
  (check-misc)
  (check-predicates)
  (check-accessors)
  (check-join-and-bind)
  (check-sequence-operations)
  (check-conversions)
  (check-map-fold-and-unfold)
  (check-trivalent))
