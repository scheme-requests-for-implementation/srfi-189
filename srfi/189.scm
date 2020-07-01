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

;;;; Utility

(define-syntax const
  (syntax-rules ()
    ((_ obj) (lambda _ obj))))

(define (singleton? lis)
  (and (pair? lis) (null? (cdr lis))))

;; Calls proc on the car of args if args is a singleton.  Otherwise,
;; calls proc with the elements of args as its arguments.
;; Both proc and args should be identifiers.
(define-syntax fast-apply
  (syntax-rules ()
    ((_ proc args)
     (if (singleton? args) (proc (car args)) (apply proc args)))))

;; Return the elements of vals as values, with singleton fast path.
;; vals should be an identifier.
(define-syntax fast-list->values
  (syntax-rules ()
    ((_ vals)
     (if (singleton? vals) (car vals) (apply values vals)))))

(define unspecified (if #f #f))

;;;; Constructors

(define (just . objs)
  (raw-just objs))

(define (left . objs)
  (raw-left objs))

(define (right . objs)
  (raw-right objs))

(define (list->just lis)
  (assume (or (null? lis) (pair? lis)))
  (raw-just (list-copy lis)))

(define (list->right lis)
  (assume (or (null? lis) (pair? lis)))
  (raw-right (list-copy lis)))

(define (list->left lis)
  (assume (or (null? lis) (pair? lis)))
  (raw-left (list-copy lis)))

(define (maybe->either maybe . default-objs)
  (assume (maybe? maybe))
  (if (nothing? maybe)
      (raw-left default-objs)
      (raw-right (just-objs maybe))))

(define (either->maybe either)
  (assume (either? either))
  (if (left? either)
      nothing-obj
      (raw-just (right-objs either))))

(define (either-swap either)
  (assume (either? either))
  (either-ref either right left))

;;;; Predicates

(define (maybe? obj)
  (or (just? obj) (nothing? obj)))

(define (nothing? obj)
  (eqv? obj nothing-obj))

;; True if all maybes are Nothing, or if all are Justs whose payloads
;; are equal in the sense of equal.
(define (maybe= equal . maybes)
  (assume (procedure? equal))
  (assume (pair? maybes))
  (let ((maybe1 (car maybes)))
    (every (lambda (maybe2) (%maybe=2 equal maybe1 maybe2))
           (cdr maybes))))

(define (%maybe=2 equal maybe1 maybe2)
  (or (eqv? maybe1 maybe2)  ; Also handles the Nothing = Nothing case.
      (and (just? maybe1)
           (just? maybe2)
           (list= equal (just-objs maybe1) (just-objs maybe2)))))

(define (either? obj)
  (or (left? obj) (right? obj)))

;; True if all eithers are all Lefts or all Rights and their payloads
;; are equal in the sense of equal.
(define (either= equal . eithers)
  (assume (procedure? equal))
  (assume (pair? eithers))
  (let ((either1 (car eithers)))
    (every (lambda (either2) (%either=2 equal either1 either2))
           (cdr eithers))))

(define (%either=2 equal either1 either2)
  (let ((e= (lambda (acc) (list= equal (acc either1) (acc either2)))))
    (or (eqv? either1 either2)
        (and (left? either1) (left? either2) (e= left-objs))
        (and (right? either1) (right? either2) (e= right-objs)))))

;;;; Accessors

(define (maybe-ref maybe failure . %opt-args)
  (assume (maybe? maybe))
  (assume (procedure? failure))
  (if (just? maybe)
      (let ((objs (just-objs maybe))
            (success (if (pair? %opt-args) (car %opt-args) values)))
        (fast-apply success objs))
      (failure)))

(define (maybe-ref/default maybe . defaults)
  (assume (maybe? maybe))
  (if (just? maybe)
      (let ((objs (just-objs maybe)))
        (fast-list->values objs))
      (fast-list->values defaults)))

(define (%either-ref-single either accessor cont)
  (let ((objs (accessor either)))
    (fast-apply cont objs)))

(define (either-ref either failure . %opt-args)
  (assume (either? either))
  (assume (procedure? failure))
  (if (right? either)
      (%either-ref-single either right-objs (if (pair? %opt-args)
                                                (car %opt-args)
                                                values))
      (%either-ref-single either left-objs failure)))

(define (either-ref/default either . defaults)
  (assume (either? either))
  (if (right? either)
      (let ((objs (right-objs either)))
        (fast-list->values objs))
      (fast-list->values defaults)))

;;;; Join and bind

;; If maybe is a Just containing a single Maybe, return that Maybe.
(define (maybe-join maybe)
  (assume (maybe? maybe))
  (if (nothing? maybe)
      nothing-obj
      (let ((objs (just-objs maybe)))
        (assume (and (singleton? objs) (maybe? (car objs)))
                "maybe-join: invalid payload")
        (car objs))))

(define (maybe-bind maybe mproc . mprocs)
  (assume (maybe? maybe))
  (if (null? mprocs)
      (maybe-ref maybe nothing mproc)  ; fast path
      (let lp ((m maybe) (mp mproc) (mprocs mprocs))
        (maybe-ref m
                   nothing
                   (lambda objs
                     (if (null? mprocs)
                         (fast-apply mp objs) ; tail-call last
                         (lp (fast-apply mp objs)
                             (car mprocs)
                             (cdr mprocs))))))))

(define (maybe-compose . mprocs)
  (assume (pair? mprocs))
  (lambda args
    (let lp ((args args) (mproc (car mprocs)) (rest (cdr mprocs)))
      (if (null? rest)
          (fast-apply mproc args)
          (maybe-ref (fast-apply mproc args)
                     nothing
                     (lambda objs
                       (lp objs (car rest) (cdr rest))))))))

;; If either is a Right containing a single Either, return that Either.
(define (either-join either)
  (assume (either? either))
  (if (left? either)
      either
      (let ((objs (right-objs either)))
        (assume (and (singleton? objs) (either? (car objs)))
                "either-join: invalid payload")
        (car objs))))

(define (either-bind either mproc . mprocs)
  (assume (either? either))
  (if (null? mprocs)
      (either-ref either (const either) mproc)  ; fast path
      (let lp ((e either) (mp mproc) (mprocs mprocs))
        (either-ref e
                    (const e)
                    (lambda objs
                      (if (null? mprocs)
                          (fast-apply mp objs)  ; tail-call last
                          (lp (fast-apply mp objs)
                              (car mprocs)
                              (cdr mprocs))))))))

(define (either-compose . mprocs)
  (assume (pair? mprocs))
  (lambda args
    (let lp ((args args) (mproc (car mprocs)) (rest (cdr mprocs)))
      (if (null? rest)
          (fast-apply mproc args)
          (either-ref (fast-apply mproc args)
                      left
                      (lambda objs
                        (lp objs (car rest) (cdr rest))))))))

;;;; Sequence operations

(define (maybe-length maybe)
  (assume (maybe? maybe))
  (if (just? maybe) 1 0))

(define (maybe-filter pred maybe)
  (assume (procedure? pred))
  (assume (maybe? maybe))
  (if (and (just? maybe) (fast-apply pred (just-objs maybe)))
      maybe
      nothing-obj))

(define (maybe-remove pred maybe)
  (assume (procedure? pred))
  (assume (maybe? maybe))
  (if (and (just? maybe)
           (not (fast-apply pred (just-objs maybe))))
      maybe
      nothing-obj))

;; Traverse a container of Maybes with cmap, collect the payload
;; objects with aggregator, and wrap the new collection in a Just.
;; If a Nothing is encountered while traversing, return it
;; immediately.
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

(define (either-filter pred either . default-objs)
  (assume (procedure? pred))
  (assume (either? either))
  (if (and (right? either) (fast-apply pred (right-objs either)))
      either
      (raw-left default-objs)))

(define (either-remove pred either . default-objs)
  (assume (procedure? pred))
  (assume (either? either))
  (if (and (right? either)
           (not (fast-apply pred (right-objs either))))
      either
      (raw-left default-objs)))

;; Traverse a container of Eithers with cmap, collect the payload
;; objects with aggregator, and wrap the new collection in a Right.
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

;;;; Protocol conversion

(define (maybe->list maybe)
  (assume (maybe? maybe))
  (if (nothing? maybe) '() (just-objs maybe)))

(define (either->list either)
  (assume (either? either))
  ((if (right? either) right-objs left-objs) either))

(define (list->maybe lis)
  (assume (or (null? lis) (pair? lis)))
  (if (null? lis) nothing-obj (raw-just (list-copy lis))))

(define (list->either lis . default-objs)
  (assume (or (null? lis) (pair? lis)))
  (if (null? lis)
      (raw-left default-objs)
      (raw-right (list-copy lis))))

;; If maybe is a Just, return its payload; otherwise, return false.
(define (maybe->truth maybe)
  (assume (maybe? maybe))
  (if (nothing? maybe)
      #f
      (begin
       (assume (singleton? (just-objs maybe))
               "maybe->truth: invalid payload")
       (car (just-objs maybe)))))

;; If either is a Right, return its payload; otherwise, return false.
(define (either->truth either)
  (assume (either? either))
  (if (left? either)
      #f
      (begin
       (assume (singleton? (right-objs either))
               "either->truth: invalid payload")
       (car (right-objs either)))))

(define (truth->maybe obj)
  (if obj (just obj) nothing-obj))

(define (truth->either obj . default-objs)
  (if obj (right obj) (raw-left default-objs)))

(define (maybe->list-truth maybe)
  (assume (maybe? maybe))
  (if (just? maybe) (just-objs maybe) #f))

(define (either->list-truth either)
  (assume (either? either))
  (if (right? either) (right-objs either) #f))

(define (list-truth->maybe list-or-false)
  (if list-or-false
      (begin
       (assume (or (null? list-or-false) (pair? list-or-false)))
       (raw-just (list-copy list-or-false)))
      nothing-obj))

(define (list-truth->either list-or-false . default-objs)
  (if list-or-false
      (begin
       (assume (or (null? list-or-false) (pair? list-or-false)))
       (raw-right (list-copy list-or-false)))
      (raw-left default-objs)))

;;; The following procedures interface between the Maybe protocol and
;;; the generation protocol, which uses an EOF object to represent
;;; failure and any other value to represent success.

(define (maybe->generation maybe)
  (assume (maybe? maybe))
  (if (nothing? maybe)
      (eof-object)
      (begin
       (assume (singleton? (just-objs maybe))
               "maybe->generation: invalid payload")
       (car (just-objs maybe)))))

(define (either->generation either)
  (assume (either? either))
  (if (left? either)
      (eof-object)
      (begin
       (assume (singleton? (right-objs either))
               "either->generation: invalid payload")
       (car (right-objs either)))))

(define (generation->maybe obj)
  (if (eof-object? obj) nothing-obj (just obj)))

(define (generation->either obj . default-objs)
  (if (eof-object? obj) (raw-left default-objs) (right obj)))

(define (maybe->values maybe)
  (maybe-ref maybe values values))

(define (values->maybe producer)
  (assume (procedure? producer))
  (call-with-values
   producer
   (lambda objs
     (if (null? objs) nothing-obj (raw-just objs)))))

;;; The following procedures interface between the Maybe protocol and
;;; the two-values protocol, which returns |#f, #f| to represent
;;; failure and |<any object>, #t| to represent success.

(define (maybe->two-values maybe)
  (assume (maybe? maybe))
  (if (nothing? maybe)
      (values #f #f)
      (begin
       (assume (singleton? (just-objs maybe))
               "maybe->two-values: invalid payload")
       (values (car (just-objs maybe)) #t))))

(define (two-values->maybe producer)
  (assume (procedure? producer))
  (call-with-values
   producer
   (lambda (obj success)
     (if success (just obj) nothing-obj))))

(define (either->values either)
  (either-ref/default either))

(define (values->either producer . default-objs)
  (assume (procedure? producer))
  (call-with-values
   producer
   (lambda objs
     (if (null? objs) (raw-left default-objs) (raw-right objs)))))

;;;; Map, fold, and unfold

(define (maybe-map proc maybe)
  (assume (procedure? proc))
  (assume (maybe? maybe))
  (if (nothing? maybe)
      nothing-obj
      (call-with-values (lambda () (fast-apply proc (just-objs maybe)))
                        just)))

(define (maybe-for-each proc maybe)
  (assume (procedure? proc))
  (maybe-ref maybe (const #f) proc)
  unspecified)

(define (maybe-fold kons nil maybe)
  (assume (procedure? kons))
  (assume (maybe? maybe))
  (if (nothing? maybe)
      nil
      (let ((objs (just-objs maybe)))
        (if (singleton? objs)
            (kons (car objs) nil)
            (apply kons (append objs (list nil)))))))

(define (maybe-unfold stop? mapper successor . seeds)
  (assume (procedure? stop?))
  (assume (procedure? mapper))
  (if (singleton? seeds)
      (let ((seed (car seeds)))  ; fast path
        (if (stop? seed)
            nothing-obj
            (begin
             ;; successor might return multiple seeds.
             (assume (call-with-values (lambda () (successor seed)) stop?))
             (call-with-values (lambda () (mapper (car seeds))) just))))
      (if (apply stop? seeds)
          nothing-obj
          (begin
           (assume (call-with-values (lambda () (apply successor seeds))
                                     stop?))
           (call-with-values (lambda () (apply mapper seeds)) just)))))

(define (either-map proc either)
  (assume (procedure? proc))
  (assume (either? either))
  (if (left? either)
      either
      (call-with-values (lambda () (fast-apply proc (right-objs either)))
                        right)))

(define (either-for-each proc either)
  (assume (procedure? proc))
  (either-ref either (const #f) proc)
  unspecified)

(define (either-fold kons nil either)
  (assume (procedure? kons))
  (assume (either? either))
  (if (left? either)
      nil
      (let ((objs (right-objs either)))
        (if (singleton? objs)
            (kons (car objs) nil)
            (apply kons (append objs (list nil)))))))

(define (either-unfold stop? mapper successor . seeds)
  (assume (procedure? stop?))
  (assume (procedure? mapper))
  (if (singleton? seeds)
      (let ((seed (car seeds)))  ; fast path
        (if (stop? seed)
            (raw-left seeds)
            (begin
             ;; successor might return multiple values.
             (assume (call-with-values (lambda () (successor seed)) stop?))
             (call-with-values (lambda () (apply mapper seeds)) right))))
      (if (apply stop? seeds)
          (raw-left seeds)
          (begin
           (assume (call-with-values (lambda () (apply successor seeds))
                                     stop?))
           (call-with-values (lambda () (apply mapper seeds)) right)))))

;;;; Syntax

(define-syntax maybe-if
  (syntax-rules ()
    ((_ maybe-expr just-expr nothing-expr)
     (let ((mval maybe-expr))
       (assume (maybe? mval))
       (if (just? mval) just-expr nothing-expr)))))

(define-syntax maybe-and
  (syntax-rules ()
    ((_) (just unspecified))
    ((_ expr)
     (let ((maybe expr))
       (assume (maybe? maybe))
       maybe))
    ((_ expr1 expr2 ...)
     (let ((maybe expr1))
       (assume (maybe? maybe))
       (if (just? maybe) (maybe-and expr2 ...) nothing-obj)))))

(define-syntax maybe-or
  (syntax-rules ()
    ((_) (nothing))
    ((_ expr)
     (let ((maybe expr))
       (assume (maybe? maybe))
       maybe))
    ((_ expr1 expr2 ...)
     (let ((maybe expr1))
       (assume (maybe? maybe))
       (if (just? maybe) maybe (maybe-or expr2 ...))))))

(define-syntax maybe-let*
  (syntax-rules ()
    ((_ ()) (just unspecified))
    ((_ () expr1 expr2 ...) (begin expr1 expr2 ...))
    ((_ ((_ expr)))
     (let ((maybe expr))
       (assume (maybe? maybe))
       maybe))
    ((_ ((expr)))
     (let ((maybe expr))
       (assume (maybe? maybe))
       maybe))
    ((_ (id)) (begin (assume (maybe? id)) id))
    ((_ ((id expr) . claws) . body)
     (maybe-bind expr
                 (lambda (id)
                   (maybe-let* claws . body))))
    ((_ ((expr) . claws) . body)
     (maybe-and expr (maybe-let* claws . body)))
    ((_ (id . claws) . body)
     (maybe-and id (maybe-let* claws . body)))
    ((_ . _)
     (syntax-error "ill-formed maybe-let* form"))))

(define-syntax either-and
  (syntax-rules ()
    ((_) (right unspecified))
    ((_ expr)
     (let ((either expr))
       (assume (either? either))
       either))
    ((_ expr1 expr2 ...)
     (let ((either expr1))
       (assume (either? either))
       (if (right? either) (either-and expr2 ...) either)))))

(define-syntax either-or
  (syntax-rules ()
    ((_) (left unspecified))
    ((_ expr)
     (let ((either expr))
       (assume (either? either))
       either))
    ((_ expr1 expr2 ...)
     (let ((either expr1))
       (assume (either? either))
       (if (right? either) either (either-or expr2 ...))))))

(define-syntax either-let*
  (syntax-rules ()
    ((_ ()) (right unspecified))
    ((_ () expr1 expr2 ...) (begin expr1 expr2 ...))
    ((_ ((_ expr)))
     (let ((either expr))
       (assume (either? either))
       either))
    ((_ ((expr)))
     (let ((either expr))
       (assume (either? either))
       either))
    ((_ (id)) (begin (assume (either? id)) id))
    ((_ ((id expr) . claws) . body)
     (either-bind expr
                 (lambda (id)
                   (either-let* claws . body))))
    ((_ ((expr) . claws) . body)
     (either-and expr (either-let* claws . body)))
    ((_ (id . claws) . body)
     (either-and id (either-let* claws . body)))
    ((_ . _)
     (syntax-error "ill-formed either-let* form"))))

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
