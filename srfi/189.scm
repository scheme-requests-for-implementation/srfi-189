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
  (raw-just objs)
  just?
  (objs just-objs))

(define-record-type <nothing>
  (make-nothing)
  nothing?)

(define-record-type <left>
  (raw-left objs)
  left?
  (objs left-objs))

(define-record-type <right>
  (raw-right objs)
  right?
  (objs right-objs))

(define nothing-obj (make-nothing))

(define (nothing)
  nothing-obj)

;; Error type for objects raised by maybe-ref.
(define-record-type <maybe-ref-error>
  (maybe-ref-error)
  maybe-ref-error?)

;;;; Utility

(define-syntax const
  (syntax-rules ()
    ((_ obj) (lambda _ obj))))

(define (singleton? xs)
  (and (pair? xs) (null? (cdr xs))))

(define (ensure-singleton lis msg)
  (unless (singleton? lis)
    (error msg lis)))

(define unspecified (if #f #f))

;;;; Constructors

(define (just . objs)
  (raw-just objs))

(define (left . objs)
  (raw-left objs))

(define (right . objs)
  (raw-right objs))

;;;; Predicates

(define (maybe? obj)
  (or (just? obj) (nothing? obj)))

(define (nothing? obj)
  (eqv? obj nothing-obj))

;; True if maybe1 and maybe2 are both Nothing, or if both are Justs
;; whose payloads are equal in the sense of equal.
(define (maybe= equal maybe1 maybe2)
  (assume (procedure? equal))
  (assume (maybe? maybe1))
  (assume (maybe? maybe2))
  (cond ((and (nothing? maybe1) (nothing? maybe2)) #t)
        ((and (just? maybe1) (just? maybe2))
         (list= equal (just-objs maybe1) (just-objs maybe2)))
        (else #f)))

(define (either? obj)
  (or (left? obj) (right? obj)))

(define (either-swap either)
  (assume (either? either))
  (either-ref either right left))

;; True if either1 and either2 are both Lefts or both Rights and their
;; payloads are equal in the sense of equal.
(define (either= equal either1 either2)
  (assume (procedure? equal))
  (assume (either? either1))
  (assume (either? either2))
  (let ((e= (lambda (acc) (list= equal (acc either1) (acc either2)))))
    (cond ((and (left? either1) (left? either2)) (e= left-objs))
          ((and (right? either1) (right? either2)) (e= right-objs))
          (else #f))))

;;;; Accessors

(define maybe-ref
  (case-lambda
   ((maybe) (maybe-ref maybe (lambda () (raise (maybe-ref-error)))))
   ((maybe failure) (maybe-ref maybe failure values))
   ((maybe failure success)
    (assume (maybe? maybe))
    (assume (procedure? failure))
    (assume (procedure? success))
    (if (just? maybe)
        (apply success (just-objs maybe))
        (failure)))))

(define (maybe-ref/default maybe . defaults)
  (assume (maybe? maybe))
  (apply values (if (just? maybe) (just-objs maybe) defaults)))

(define either-ref
  (case-lambda
   ((either) (either-ref either raise))
   ((either failure) (either-ref either failure values))
   ((either failure success)
    (assume (either? either))
    (assume (procedure? failure))
    (assume (procedure? success))
    (if (right? either)
        (apply success (right-objs either))
        (apply failure (left-objs either))))))

(define (either-ref/default either . defaults)
  (assume (either? either))
  (apply values (if (right? either) (right-objs either) defaults)))

;;;; Join and bind

;; If maybe is a Just containing a single Maybe, return that Maybe.
(define (maybe-join maybe)
  (maybe-ref maybe
             nothing
             (lambda objs
               (if (and (singleton? objs) (maybe? (car objs)))
                   (car objs)
                   (error "maybe-join: invalid payload" objs)))))

(define (maybe-bind maybe mproc . mprocs)
  (assume (maybe? maybe))
  (if (null? mprocs)
      (maybe-ref maybe nothing mproc)  ; fast path
      (let lp ((m maybe) (mp mproc) (mprocs mprocs))
        (maybe-ref m
                   nothing
                   (lambda objs
                     (if (null? mprocs)
                         (apply mp objs)  ; tail-call last
                         (lp (apply mp objs)
                             (car mprocs)
                             (cdr mprocs))))))))

(define (maybe-compose . mprocs)
  (assume (pair? mprocs))
  (lambda (maybe)
    (apply maybe-bind maybe mprocs)))

;; If either is a Right containing a single Either, return that Either.
(define (either-join either)
  (either-ref either
              (const either)
              (lambda objs
                (if (and (singleton? objs) (either? (car objs)))
                    (car objs)
                    (error "either-join: invalid payload" objs)))))

(define (either-bind either mproc . mprocs)
  (assume (either? either))
  (if (null? mprocs)
      (either-ref either (const either) mproc)  ; fast path
      (let lp ((e either) (mp mproc) (mprocs mprocs))
        (either-ref e
                    (const e)
                    (lambda objs
                      (if (null? mprocs)
                          (apply mp objs)  ; tail-call last
                          (lp (apply mp objs)
                              (car mprocs)
                              (cdr mprocs))))))))

(define (either-compose . mprocs)
  (assume (pair? mprocs))
  (lambda (either)
    (apply either-bind either mprocs)))


;;;; Sequence operations

(define (maybe-length maybe)
  (assume (maybe? maybe))
  (if (just? maybe) 1 0))

(define (maybe-filter pred maybe)
  (assume (procedure? pred))
  (maybe-bind maybe
              (lambda objs
                (if (apply pred objs) maybe nothing-obj))))

(define (maybe-remove pred maybe)
  (assume (procedure? pred))
  (maybe-bind maybe
              (lambda objs
                (if (apply pred objs) nothing-obj maybe))))

;; Traverse a `container' of Maybes with `cmap', collect the payload
;; objects with `aggregator', and wrap the new collection in a Just.
;; If a Nothing is encountered while traversing, return it immediately.
(define maybe-sequence
  (case-lambda
   ((container cmap) (maybe-sequence container cmap list))
   ((container cmap aggregator)
    (assume (procedure? cmap))
    (assume (procedure? aggregator))
    (call-with-current-continuation
     (lambda (return)
       (just (cmap (lambda (m)
                     (maybe-ref m (lambda () (return m)) aggregator))
                   container)))))))

(define (either-length either)
  (assume (either? either))
  (if (right? either) 1 0))

(define (either-filter pred either obj)
  (assume (procedure? pred))
  (assume (either? either))
  (either-ref either
              (const (left obj))
              (lambda objs
                (if (apply pred objs) either (left obj)))))

(define (either-remove pred either obj)
  (assume (procedure? pred))
  (assume (either? either))
  (either-ref either
              (const (left obj))
              (lambda objs
                (if (apply pred objs) (left obj) either))))

;; Traverse a `container' of Eithers with `cmap', collect the payload
;; objects with `aggregator', and wrap the new collection in a Right.
;; If a Left is encountered while traversing, return it immediately.
(define either-sequence
  (case-lambda
   ((container cmap) (either-sequence container cmap list))
   ((container cmap aggregator)
    (assume (procedure? cmap))
    (assume (procedure? aggregator))
    (call-with-current-continuation
     (lambda (return)
       (right (cmap (lambda (e)
                      (either-ref e (const (return e)) aggregator))
                    container)))))))

;;;; Conversion

(define (maybe->either maybe obj)
  (maybe-ref maybe (lambda () (left obj)) right))

(define (either->maybe either)
  (either-ref either (const nothing-obj) just))

(define (list->just lis)
  (assume (or (null? lis) (pair? lis)))
  (apply just lis))

(define (list->right lis)
  (assume (or (null? lis) (pair? lis)))
  (apply right lis))

(define (maybe->list maybe)
  (assume (maybe? maybe))
  (if (nothing? maybe) '() (just-objs maybe)))

(define (either->list either)
  (assume (either? either))
  ((if (right? either) right-objs left-objs) either))

;; If `maybe' is a Just, return its payload; otherwise, return false.
(define (maybe->lisp maybe)
  (maybe-ref maybe
             (lambda () #f)
             (lambda objs
               (ensure-singleton objs "maybe->lisp: invalid payload")
               (car objs))))

(define (lisp->maybe obj)
  (if obj (just obj) nothing-obj))

;; If `maybe' is a Just whose payload is a single value, return that
;; value.  Otherwise, return EOF.
(define (maybe->eof maybe)
  (maybe-ref maybe
             (lambda () (eof-object))
             (lambda objs
               (ensure-singleton objs "maybe->eof: invalid payload")
               (car objs))))

(define (eof->maybe obj)
  (if (eof-object? obj) nothing-obj (just obj)))

(define (maybe->values maybe)
  (maybe-ref maybe values values))

;; If `maybe' is a Just whose payload is a single value, return that
;; value and #t.  Otherwise, return two false values.
(define (maybe->lisp-values maybe)
  (maybe-ref maybe
             (lambda () (values #f #f))
             (lambda objs
               (ensure-singleton objs
                                 "maybe->lisp-values: invalid payload")
               (values (car objs) #t))))

(define (values->maybe producer)
  (assume (procedure? producer))
  (call-with-values
   producer
   (lambda objs
     (if (null? objs) nothing-obj (raw-just objs)))))

(define (lisp-values->maybe producer)
  (values->maybe producer))

(define (either->values either)
  (either-ref either (const (values)) values))

;; If `either' is a Right whose payload is a single value, return that
;; value and #t.  Otherwise, return two false values.
(define (either->lisp-values either)
  (either-ref either
              (const (values #f #f))
              (lambda objs
                (ensure-singleton objs
                                  "either->lisp-values: invalid payload")
                (values (car objs) #t))))

(define (values->either producer obj)
  (assume (procedure? producer))
  (call-with-values
   producer
   (lambda objs
     (if (null? objs) (left obj) (raw-right objs)))))

(define (lisp-values->either producer obj)
  (values->either producer obj))

;;;; Map, fold, and unfold

(define (maybe-map proc maybe)
  (assume (procedure? proc))
  (maybe-bind maybe (lambda objs
                      (call-with-values (lambda () (apply proc objs))
                                        just))))

(define (maybe-for-each proc maybe)
  (assume (procedure? proc))
  (maybe-ref maybe (const #f) proc)
  unspecified)

(define (maybe-fold kons nil maybe)
  (assume (procedure? kons))
  (maybe-ref maybe
             (lambda () nil)
             (lambda objs  ; apply kons to all payload values plus nil
               (apply kons (append objs (list nil))))))

;; The unused `successor' argument is for consistency only and may
;; be anything.
(define (maybe-unfold stop? mapper successor . seeds)
  (assume (procedure? stop?))
  (assume (procedure? mapper))
  (if (apply stop? seeds)
      nothing-obj
      (call-with-values (lambda () (apply mapper seeds)) just)))

(define (either-map proc either)
  (assume (procedure? proc))
  (either-bind either (lambda objs
                        (call-with-values (lambda () (apply proc objs))
                                          right))))

(define (either-for-each proc either)
  (assume (procedure? proc))
  (either-ref either (const #f) proc)
  unspecified)

(define (either-fold kons nil either)
  (assume (procedure? kons))
  (either-ref either
              (const nil)
              (lambda objs  ; apply kons to all payload values plus nil
                (apply kons (append objs (list nil))))))

;; The unused `successor' argument is for consistency only and may
;; be anything.
(define (either-unfold stop? mapper successor . seeds)
  (assume (procedure? stop?))
  (assume (procedure? mapper))
  (if (apply stop? seeds)
      (apply left seeds)
      (call-with-values (lambda () (apply mapper seeds)) right)))

;;;; Conditional syntax

(define-syntax maybe-if
  (syntax-rules ()
    ((_ maybe-expr just-expr nothing-expr)
     (let ((mval maybe-expr))
       (assume (maybe? mval))
       (if (just? mval) just-expr nothing-expr)))))

;;;; Trivalent logic

;;; In the following procedures, (just #f) is considered to be false.
;;; All other Just values are taken to be true.

(define (just->boolean maybe)
  (not (equal? (just-objs maybe) '(#f))))

(define (tri-not maybe)
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
