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

(define left-of-no-values (raw-left '()))

(define nothing-obj (make-nothing))

(define (nothing)
  nothing-obj)

;;; Utility

(define-syntax const
  (syntax-rules ()
    ((_ obj) (lambda _ obj))))

(define (singleton? xs)
  (and (pair? xs) (null? (cdr xs))))

;;; Constructors

(define (just . objs)
  (raw-just objs))

(define (left . objs)
  (raw-left objs))

(define (right . objs)
  (raw-right objs))

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
         (list= equal (just-objs maybe1) (just-objs maybe2)))
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
  (let ((e= (lambda (acc) (list= equal (acc either1) (acc either2)))))
    (cond ((and (left? either1) (left? either2)) (e= left-objs))
          ((and (right? either1) (right? either2)) (e= right-objs))
          (else #f))))

;;; Accessors

(define maybe-ref
  (case-lambda
   ((maybe) (maybe-ref maybe (lambda () (error "maybe-ref: failure" maybe))))
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
   ((either) (either-ref either (lambda objs
                                  (raise (if (null? objs) #f (car objs))))))
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

;;; Join and bind

(define (maybe-join maybe)
  (assume (maybe? maybe))
  (maybe-ref maybe
             nothing
             (lambda objs  ;; payload must be a single Maybe
               (if (and (singleton? objs) (maybe? (car objs)))
                   (car objs)
                   (error "maybe-join: invalid payload" maybe objs)))))

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
                         (lp (apply mp objs) (car mprocs) (cdr mprocs))))))))

(define (maybe-compose . mprocs)
  (assume (pair? mprocs))
  (lambda (maybe)
    (assume (maybe? maybe))
    (apply maybe-bind maybe mprocs)))

(define (either-join either)
  (assume (either? either))
  (either-ref either
              (const either)
              (lambda objs  ;; payload must be a single Either
                (if (and (singleton? objs) (either? (car objs)))
                    (car objs)
                    (error "either-join: invalid payload" either objs)))))

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
                          (lp (apply mp objs) (car mprocs) (cdr mprocs))))))))

(define (either-compose . mprocs)
  (assume (pair? mprocs))
  (lambda (either)
    (assume (either? either))
    (apply either-bind either mprocs)))


;;; Sequence operations

(define (maybe-length maybe)
  (assume (maybe? maybe))
  (if (just? maybe) 1 0))

(define (maybe-filter pred maybe)
  (assume (procedure? pred))
  (assume (maybe? maybe))
  (maybe-bind maybe
              (lambda objs
                (if (apply pred objs) maybe nothing-obj))))

(define (maybe-remove pred maybe)
  (assume (procedure? pred))
  (assume (maybe? maybe))
  (maybe-bind maybe
              (lambda objs
                (if (apply pred objs) nothing-obj maybe))))

(define (maybe-sequence container cmap aggregator)
  (assume (procedure? cmap))
  (assume (procedure? aggregator))
  (call-with-current-continuation
   (lambda (return)
     (just (cmap (lambda (m)
                   (maybe-ref m (lambda () (return m)) aggregator))
                 container)))))

(define (either-length either)
  (assume (either? either))
  (if (right? either) 1 0))

(define (either-filter pred either)
  (assume (procedure? pred))
  (assume (either? either))
  (either-ref either
              (const left-of-no-values)
              (lambda objs
                (if (apply pred objs) either left-of-no-values))))

(define (either-remove pred either)
  (assume (procedure? pred))
  (assume (either? either))
  (either-ref either
              (const left-of-no-values)
              (lambda objs
                (if (apply pred objs) left-of-no-values either))))

;; FIXME: Same questions as maybe-sequence.
(define (either-sequence container cmap aggregator)
  (assume (procedure? cmap))
  (assume (procedure? aggregator))
  (call-with-current-continuation
   (lambda (return)
     (right (cmap (lambda (e)
                    (either-ref e (const (return e)) aggregator))
                  container)))))

;;; Conversion

(define (maybe->either maybe)
  (assume (maybe? maybe))
  (maybe-ref maybe left right))

(define (either->maybe either)
  (assume (either? either))
  (either-ref either (const nothing-obj) just))

(define (list->maybe lis)
  (assume (list? lis))
  (if (null? lis) nothing-obj (apply just lis)))

(define (list->either lis)
  (assume (list? lis))
  (if (null? lis) left-of-no-values (apply right lis)))

(define (maybe->list maybe)
  (assume (maybe? maybe))
  (if (nothing? maybe) '() (just-objs maybe)))

(define (either->list either)
  (assume (either? either))
  (if (right? either) (right-objs either) (left-objs either)))

(define (maybe->lisp maybe)
  (assume (maybe? maybe))
  (maybe-ref/default maybe #f))

(define (lisp->maybe obj)
  (if obj (just obj) nothing-obj))

(define (maybe->eof maybe)
  (assume (maybe? maybe))
  (maybe-ref/default maybe (eof-object)))

(define (eof->maybe obj)
  (if (eof-object? obj) nothing-obj (just obj)))

(define (maybe->values maybe)
  (assume (maybe? maybe))
  (maybe-ref maybe values values))

(define (maybe->lisp-values maybe)
  (assume (maybe? maybe))
  (maybe-ref maybe
             (lambda () (values #f #f))
             (lambda objs
               (apply values (append objs '(#t))))))

(define (values->maybe producer)
  (assume (procedure? producer))
  (call-with-values
   producer
   (case-lambda
    (() nothing-obj)
    ((x) (just x))
    (xs
     (let-values (((vals last-pair) (split-at xs (- (length xs) 1))))
       (if (car last-pair)
           (raw-just vals)
           nothing-obj))))))

(define (either->values either)
  (assume (either? either))
  (either-ref either (const (values)) values))

(define (either->lisp-values either)
  (assume (either? either))
  (either-ref either
              (const (values #f #f))
              (lambda objs
                (apply values (append objs '(#t))))))

(define (values->either producer)
  (assume (procedure? producer))
  (call-with-values
   producer
   (case-lambda
    (() left-of-no-values)
    ((x) (right x))
    (xs
     (let-values (((vals last-pair) (split-at xs (- (length xs) 1))))
       (if (car last-pair)
           (raw-right vals)
           left-of-no-values))))))

;;; Map, fold, and unfold

(define (maybe-map proc maybe)
  (assume (procedure? proc))
  (assume (maybe? maybe))
  (maybe-bind maybe (lambda objs
                      (call-with-values (lambda () (apply proc objs))
                                        just))))

(define (maybe-for-each proc maybe)
  (assume (procedure? proc))
  (assume (maybe? maybe))
  (maybe-bind maybe proc))

(define (maybe-fold kons nil maybe)
  (assume (procedure? kons))
  (assume (maybe? maybe))
  (maybe-ref maybe
             (lambda () nil)
             (lambda objs  ; apply kons to all payload values plus nil
               (apply kons (append objs (list nil))))))

;; The unused `successor' argument is for consistency only and may
;; be anything.
(define (maybe-unfold stop? mapper successor seed)
  (assume (procedure? stop?))
  (assume (procedure? mapper))
  (if (stop? seed) nothing-obj (just (mapper seed))))

(define (either-map proc either)
  (assume (procedure? proc))
  (assume (either? either))
  (either-bind either (lambda objs
                        (call-with-values (lambda () (apply proc objs))
                                          right))))

(define (either-for-each proc either)
  (assume (procedure? proc))
  (assume (either? either))
  (either-bind either proc))

;; FIXME: How does this work with multiple values?
(define (either-fold kons nil either)
  (assume (procedure? kons))
  (assume (either? either))
  (either-ref either
              (const nil)
              (lambda objs  ; apply kons to all payload values plus nil
                (apply kons (append objs (list nil))))))

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
  (not (equal? (just-objs maybe) '(#f))))

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
