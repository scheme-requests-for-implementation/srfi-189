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

(define-record-type <just>
  (just obj)
  just?
  (obj just-obj))

(define-record-type <nothing>
  (make-nothing)
  nothing?)

(define-record-type <left>
  (left obj)
  left?
  (obj left-obj))

(define-record-type <right>
  (right obj)
  right?
  (obj right-obj))

(define nothing-obj (make-nothing))

(define (nothing)
  nothing-obj)

;;; Utility

(define-syntax const
  (syntax-rules ()
    ((_ obj) (lambda (_) obj))))

;;; Predicates

(define (maybe? obj)
  (or (just? obj) (nothing? obj)))

(define (nothing? obj)
  (eqv? obj nothing-obj))

(define (maybe= equal maybe1 maybe2)
  (assume (procedure? equal))
  (assume (maybe? maybe1))
  (assume (maybe? maybe2))
  (cond ((and (nothing? maybe1) (nothing? maybe2)) #t)
        ((and (just? maybe1) (just? maybe2))
         (equal (just-obj maybe1) (just-obj maybe2)))
        (else #f)))

(define (either? obj)
  (or (left? obj) (right? obj)))

(define (either-swap either)
  (assume (either? either))
  (either-ref either right left))

;; True if either1 and either2 are both lefts or both rights and their
;; payloads are equal in the sense of the procedure equal.
(define (either= equal either1 either2)
  (assume (procedure? equal))
  (assume (either? either1))
  (assume (either? either2))
  (cond ((and (left? either1) (left? either2))
         (equal (left-obj either1) (left-obj either2)))
        ((and (right? either1) (right? either2))
         (equal (right-obj either1) (right-obj either2)))
        (else #f)))

;;; Accessors

(define maybe-ref
  (case-lambda
   ((maybe) (maybe-ref maybe (lambda () (error "maybe-ref: failure" maybe))))
   ((maybe failure) (maybe-ref maybe failure values))
   ((maybe failure success)
    (assume (maybe? maybe))
    (assume (procedure? failure))
    (assume (procedure? success))
    (if (just? maybe) (success (just-obj maybe)) (failure)))))

(define (maybe-ref/default maybe default)
  (assume (maybe? maybe))
  (if (just? maybe) (just-obj maybe) default))

(define either-ref
  (case-lambda
   ((either) (either-ref either raise))
   ((either failure) (either-ref either failure values))
   ((either failure success)
    (assume (either? either))
    (assume (procedure? failure))
    (assume (procedure? success))
    (if (right? either)
        (success (right-obj either))
        (failure (left-obj either))))))

(define (either-ref/default either default)
  (assume (either? either))
  (if (right? either) (right-obj either) default))

;;; Join and bind

(define (maybe-join maybe)
  (assume (maybe? maybe))
  (maybe-ref maybe
             (lambda () maybe)
             (lambda (obj) (if (maybe? obj) obj maybe))))

(define (maybe-bind maybe mproc . mprocs)
  (assume (maybe? maybe))
  (if (null? mprocs)
      (maybe-ref maybe nothing mproc)  ; fast path
      (let lp ((m maybe) (mp mproc) (mprocs mprocs))
        (maybe-ref m
                   nothing
                   (lambda (obj)
                     (if (null? mprocs)
                         (mp obj)  ; tail-call last
                         (lp (mp obj) (car mprocs) (cdr mprocs))))))))

(define (maybe-compose . mprocs)
  (assume (pair? mprocs))
  (lambda (maybe)
    (assume (maybe? maybe))
    (apply maybe-bind maybe mprocs)))

(define (either-join either)
  (assume (either? either))
  (either-ref either
              (const either)
              (lambda (obj) (if (either? obj) obj either))))

(define (either-bind either mproc . mprocs)
  (assume (either? either))
  (if (null? mprocs)
      (either-ref either (const either) mproc)  ; fast path
      (let lp ((e either) (ep mproc) (mprocs mprocs))
        (either-ref e
                    (const e)
                    (lambda (obj)
                      (if (null? mprocs)
                          (ep obj)  ; tail-call last
                          (lp (ep obj) (car mprocs) (cdr mprocs))))))))


(define (either-compose . mprocs)
  (assume (pair? mprocs))
  (lambda (either)
    (assume (either? either))
    (apply either-bind either mprocs)))


;;; Sequence operations

(define (maybe-length maybe)
  (assume (maybe? maybe))
  (if (just? maybe) 1 0))

(define (maybe-contains? equal maybe obj)
  (assume (procedure? equal))
  (assume (maybe? maybe))
  (maybe-ref maybe
             (lambda () #f)
             (lambda (x) (equal x obj))))

(define (maybe-filter pred maybe)
  (assume (procedure? pred))
  (assume (maybe? maybe))
  (maybe-bind maybe (lambda (x) (if (pred x) maybe nothing-obj))))

(define (maybe-remove pred maybe)
  (assume (procedure? pred))
  (assume (maybe? maybe))
  (maybe-bind maybe (lambda (x) (if (not (pred x)) maybe nothing-obj))))

(define (maybe-sequence container cmap)
  (assume (procedure? cmap))
  (call-with-current-continuation
   (lambda (return)
     (just (cmap (lambda (m)
                   (maybe-ref m (lambda () (return m))))
                 container)))))

(define (either-length either)
  (assume (either? either))
  (if (right? either) 1 0))

(define (either-contains? equal either obj)
  (assume (procedure? equal))
  (assume (either? either))
  (either-ref either
              (const #f)
              (lambda (x) (equal x obj))))

(define (either-filter pred either obj)
  (assume (procedure? pred))
  (assume (either? either))
  (either-ref either
              (const (left obj))
              (lambda (x)
                (if (pred x) either (left obj)))))

(define (either-remove pred either obj)
  (assume (procedure? pred))
  (assume (either? either))
  (either-ref either
              (const (left obj))
              (lambda (x)
                (if (pred x) (left obj) either))))

(define (either-sequence container cmap obj)
  (assume (procedure? cmap))
  (call-with-current-continuation
   (lambda (return)
     (right (cmap (lambda (e)
                    (either-ref e (const (return (left obj)))))
                  container)))))

;;; Conversion

(define (maybe->either maybe obj)
  (assume (maybe? maybe))
  (maybe-ref maybe (lambda () (left obj)) right))

(define (either->maybe either)
  (assume (either? either))
  (either-ref either (const nothing-obj) just))

(define (list->maybe lis)
  (assume (list? lis))
  (if (null? lis) nothing-obj (just (car lis))))

(define (list->either lis obj)
  (assume (list? lis))
  (if (null? lis) (left obj) (right (car lis))))

(define (maybe->list maybe)
  (assume (maybe? maybe))
  (maybe-ref maybe (lambda () '()) list))

(define (either->list either)
  (assume (either? either))
  (either-ref either (const '()) list))

;; Converts a Maybe to the usual Lisp "true object or #f" protocol.
(define (maybe->lisp maybe)
  (assume (maybe? maybe))
  (maybe-ref/default maybe #f))

;; Converts the usual Lisp "true object or #f" protocol to a Maybe.
(define (lisp->maybe obj)
  (if obj (just obj) nothing-obj))

(define (maybe->eof maybe)
  (maybe-ref/default maybe (eof-object)))

(define (eof->maybe obj)
  (if (eof-object? obj) nothing-obj (just obj)))

(define (maybe->values maybe)
  (assume (maybe? maybe))
  (maybe-ref maybe values values))

(define (maybe->two-values maybe)
  (assume (maybe? maybe))
  (maybe-ref maybe
             (lambda () (values #f #f))
             (lambda (obj) (values obj #t))))

(define (values->maybe producer)
  (assume (procedure? producer))
  (call-with-values
   producer
   (case-lambda
    (() nothing-obj)
    ((x) (just x))
    ((x b) (if b (just x) nothing-obj)))))

(define (either->values either)
  (assume (either? either))
  (either-ref either (const (values)) values))

(define (either->two-values either)
  (assume (either? either))
  (either-ref either
              (lambda (obj) (values obj #f))
              (lambda (obj) (values obj #t))))

(define (values->either producer)
  (assume (procedure? producer))
  (call-with-values
   producer
   (case-lambda
    (() (left nothing-obj))
    ((x) (right x))
    ((x b) (if b (right x) (left x))))))

;;; Map, fold, and unfold

(define (maybe-map proc maybe)
  (assume (procedure? proc))
  (assume (maybe? maybe))
  (maybe-bind maybe (lambda (obj) (just (proc obj)))))

(define (maybe-for-each proc maybe)
  (assume (procedure? proc))
  (assume (maybe? maybe))
  (maybe-bind maybe (lambda (obj) (proc obj))))

(define (maybe-fold kons nil maybe)
  (assume (procedure? kons))
  (assume (maybe? maybe))
  (maybe-ref maybe
             (lambda () nil)
             (lambda (obj) (kons obj nil))))

;; The unused `successor' argument is for consistency only and may
;; be anything.
(define (maybe-unfold stop? mapper successor seed)
  (assume (procedure? stop?))
  (assume (procedure? mapper))
  (if (stop? seed) nothing-obj (just (mapper seed))))

(define (either-map proc either)
  (assume (procedure? proc))
  (assume (either? either))
  (either-bind either (lambda (obj) (right (proc obj)))))

(define (either-for-each proc either)
  (assume (procedure? proc))
  (assume (either? either))
  (either-bind either (lambda (obj) (proc obj))))

(define (either-fold kons nil either)
  (assume (procedure? kons))
  (assume (either? either))
  (either-ref either
              (const nil)
              (lambda (obj) (kons obj nil))))

;; The unused `successor' argument is for consistency only and may
;; be anything.
(define (either-unfold stop? mapper successor seed)
  (assume (procedure? stop?))
  (assume (procedure? mapper))
  (if (stop? seed) (left seed) (right (mapper seed))))

;; Conditional syntax

(define-syntax maybe-if
  (syntax-rules ()
    ((maybe-expr just-expr nothing-expr)
     (let ((maybe-expr maybe-expr))
       (if (just? maybe-expr) just-expr nothing-expr)))))

;;; Trivalent logic

;; In the following procedures, (just #f) is considered false.  All
;; other Just values are true.
(define (just->boolean maybe)
  (not (eqv? (just-obj maybe) #f)))

(define (tri-not maybe)
  (assume (maybe? maybe))
  (maybe-bind maybe (lambda (x) (just (not x)))))

;; Returns #t if all arguments are true or all false.  If any argument
;; is Nothing or if any two arguments have different (tri-)truth values,
;; #f is returned.
(define (tri=? maybe . ms)
  (define (make-pred b)
    (lambda (m)
      (assume (maybe? m))
      (and (just? m) (eqv? (just->boolean m) b))))

  (if (nothing? maybe)
      (just #f)
      (let ((tri-same? (make-pred (just->boolean maybe))))
        (if (every tri-same? ms) (just #t) (just #f)))))

;; Returns #t if all arguments are true.  If any argument is false or
;; Nothing, return the first such object.
(define (tri-and . maybes)
  (or (find (lambda (m)
              (assume (maybe? m))
              (or (nothing? m) (not (just->boolean m))))
            maybes)
      (just #t)))

;; Returns #f if all arguments are false.  If any argument is true or
;; Nothing, return the first such object.
(define (tri-or . maybes)
  (or (find (lambda (m)
              (assume (maybe? m))
              (or (nothing? m) (just->boolean m)))
            maybes)
      (just #f)))

;; If all arguments are Nothing, then return Nothing.  Otherwise,
;; return the first Just value.
(define (tri-merge . maybes)
  (or (find just? maybes) nothing-obj))
