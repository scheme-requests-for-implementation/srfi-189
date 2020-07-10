;;;; Conditional syntax

(define (check-syntax)
  (print-header "Testing syntax...")

  (check (maybe-if (just #t) #t #f) => #t)
  (check (maybe-if (nothing) #t #f) => #f)

  ;;; maybe-and, -or, -let*, and -let*-values

  (check (just? (maybe-and))                                  => #t)
  (check (just-of-z? (maybe-and (just 'z)))                   => #t)
  (check (just-of-z? (maybe-and (just #t) (just 'z)))         => #t)
  (check (nothing? (maybe-and (just #t) (nothing) (just 'z))) => #t)
  ;; and / bind identities
  (check (maybe= eqv?
                 (maybe-bind (just #f) (constantly (just #t)))
                 (maybe-and (just #f) (just #t))
                 (just #t))
   => #t)
  (check (maybe= eqv?
                 (maybe-bind (nothing) (constantly (just #t)))
                 (maybe-and (nothing) (just #t))
                 (nothing))
   => #t)

  (check (nothing? (maybe-or))                               => #t)
  (check (just-of-z? (maybe-or (just 'z)))                   => #t)
  (check (just-of-z? (maybe-or (nothing) (just 'z)))         => #t)
  (check (nothing? (maybe-or (nothing) (nothing) (nothing))) => #t)

  (check (just? (maybe-let* ())) => #t)
  (check (just-of-z? (maybe-let* (((just 'z))))) => #t)
  (check (just-of-z? (maybe-let* ((x (just 'z))))) => #t)
  (check (just-of-z?
          (maybe-let* (((maybe-bind (just #t) just)))
            (just 'z))) => #t)
  (check (nothing?
          (maybe-let* ((x (just #t))
                       (y (nothing)))
            (just x)))
   => #t)
  (check (maybe= eqv?
                 (maybe-let* ((x (just 2))
                              (y (just 3)))
                   (just (* x y)))
                 (just 6))
   => #t)
  (check (nothing?
          (maybe-let* ((x (just 2))
                       ((maybe-bind (just 'z) (constantly (nothing)))))
            (just x)))
   => #t)
  (check (maybe= eqv?
                 (maybe-let* ((b (just #t)) ((truth->maybe b)))
                   (just b))
                 (just #t))
   => #t)
  ;; Behavior of bound-variable claws.
  (let ((just-of-z (just 'z)) (zilch (nothing)))
    (check (just-of-z? (maybe-let* (just-of-z) just-of-z)) => #t)
    (check (maybe= eqv?
                   (maybe-let* ((x (just 2)) just-of-z (y (just 3)))
                     (just (* x y)))
                   (just 6))
     => #t)
    (check (just-of-z? (maybe-let* (just-of-z ((just 'x)))
                         just-of-z))
     => #t)
    (check (nothing? (maybe-let* (zilch))) => #t)
    (check (nothing? (maybe-let* ((x (just 2)) zilch (y (just 3)))
                       (just (* x y))))
     => #t))
  ;; let* / bind identities.
  (let ((just-neg (lambda (b) (just (not b)))))
    (check (maybe= eqv?
                   (maybe-bind (just #t) just-neg)
                   (maybe-let* ((b (just #t))) (just-neg b))
                   (just #f))
     => #t)
    (check (maybe= eqv?
                   (maybe-bind (nothing) just-neg)
                   (maybe-let* ((b (nothing))) (just-neg b))
                   (nothing))
     => #t))

  (check (just? (maybe-let*-values ())) => #t)
  (check (just-of-z? (maybe-let*-values (((just 'z))))) => #t)
  (check (just-of-z? (maybe-let*-values (((x) (just 'z))))) => #t)
  (check (just-of-z?
          (maybe-let*-values (((maybe-bind (just #t) just)))
            (just 'z))) => #t)
  (check (nothing?
          (maybe-let*-values (((x) (just #t))
                              (y (nothing)))
            (just x)))
   => #t)
  (check (maybe= eqv?
                 (maybe-let*-values (((x y) (just 2 3)))
                   (just (* x y)))
                 (just 6))
   => #t)
  (check (nothing?
          (maybe-let*-values (((x) (just 2))
                              ((maybe-bind (just 'z) (constantly (nothing)))))
            (just x)))
   => #t)
  (check (maybe= eqv?
                 (maybe-let*-values (((b) (just #t)) ((truth->maybe b)))
                   (just b))
                 (just #t))
   => #t)
  (check (just-of-z? (maybe-let*-values ((vals (just 'z #t)))
                       (just (car vals))))
   => #t)
  (check (just-of-z? (maybe-let*-values (((x . _) (just 'z #t)))
                       (just x)))
   => #t)
  (check (maybe= eqv?
                 (maybe-let*-values ((vals (just 'z #t))
                                     ((b c . _) (just #t 'y #f)))
                   (just (car vals) b))
                 (just 'z #t))
   => #t)
  ;; Behavior of bound-variable claws.
  (let ((just-of-z (just 'z)) (zilch (nothing)))
    (check (just-of-z? (maybe-let*-values (just-of-z) just-of-z)) => #t)
    (check (maybe= eqv?
                   (maybe-let*-values (((x) (just 2))
                                       just-of-z
                                       ((y) (just 3)))
                     (just (* x y)))
                   (just 6))
     => #t)
    (check (just-of-z? (maybe-let*-values (just-of-z ((just 'x)))
                         just-of-z))
     => #t)
    (check (nothing? (maybe-let*-values (zilch))) => #t)
    (check (nothing? (maybe-let*-values (((x) (just 2))
                                         zilch
                                         ((y) (just 3)))
                       (just (* x y))))
     => #t))
  ;; let*-values / bind identities.
  (let ((just-neg-both (lambda (b c) (just (not b) (not c)))))
    (check (maybe= eqv?
                   (maybe-bind (just #t #t) just-neg-both)
                   (maybe-let*-values (((b c) (just #t #t)))
                     (just-neg-both b c))
                   (just #f #f))
     => #t)
    (check (maybe= eqv?
                   (maybe-bind (nothing) just-neg-both)
                   (maybe-let*-values (((b c) (nothing))) (just-neg b))
                   (nothing))
     => #t))

  ;;; either-and, -or, and -let*

  (check (right? (either-and))                               => #t)
  (check (right-of-z? (either-and (right 'z)))               => #t)
  (check (right-of-z? (either-and (right #t) (right 'z)))    => #t)
  (check (left-of-z? (either-and (right) (left 'z) (right))) => #t)
  ;; and / bind identities
  (check (either= eqv?
                  (either-bind (right #f) (constantly (right #t)))
                  (either-and (right #f) (right #t))
                  (right #t))
   => #t)
  (check (either= eqv?
                  (either-bind (left #f) (constantly (right #t)))
                  (either-and (left #f) (right #t))
                  (left #f))
   => #t)

  (check (left? (either-or))                              => #t)
  (check (right-of-z? (either-or (right 'z)))             => #t)
  (check (right-of-z? (either-or (left) (right 'z)))      => #t)
  (check (left-of-z? (either-or (left) (left) (left 'z))) => #t)

  (check (right? (either-let* ())) => #t)
  (check (right-of-z? (either-let* (((right 'z))))) => #t)
  (check (right-of-z? (either-let* ((x (right 'z))))) => #t)
  (check (right-of-z?
          (either-let* (((either-bind (right #t) right)))
            (right 'z)))
   => #t)
  (check (left-of-z? (either-let* ((x (right #t)) (y (left 'z)))
                       (right x)))
   => #t)
  (check (either= eqv?
                  (either-let* ((x (right 2)) (y (right 3)))
                    (right (* x y)))
                  (right 6))
   => #t)
  (check (left-of-z?
          (either-let* ((x (right 2))
                        ((either-swap (right 'z))))
            (right x)))
   => #t)
  (check (either= eqv?
                  (either-let* ((b (right #t)) ((truth->either b)))
                    (right b))
                  (right #t))
   => #t)
  ;; Behavior of bound-variable claws.
  (let ((right-of-z (right 'z)) (left-of-z (left 'z)))
    (check (right-of-z? (either-let* (right-of-z) right-of-z)) => #t)
    (check (either= eqv?
                    (either-let* ((x (right 2))
                                  right-of-z
                                  (y (right 3)))
                      (right (* x y)))
                    (right 6))
     => #t)
    (check (right-of-z?
            (either-let* (right-of-z ((right 'x)))
              right-of-z))
     => #t)
    (check (left-of-z? (either-let* (left-of-z))) => #t)
    (check (left-of-z?
            (either-let* ((x (right 2)) left-of-z (y (right 3)))
              (right (* x y))))
     => #t))
  ;; let* / bind identities.
  (let ((right-neg (lambda (b) (right (not b)))))
    (check (either= eqv?
                    (either-bind (right #t) right-neg)
                    (either-let* ((b (right #t))) (right-neg b))
                    (right #f))
     => #t)
    (check (either= eqv?
                    (either-bind (left #t) right-neg)
                    (either-let* ((b (left #t))) (right-neg b))
                    (left #t))
     => #t))

  (check (right? (either-let*-values ())) => #t)
  (check (right-of-z? (either-let*-values (((right 'z))))) => #t)
  (check (right-of-z? (either-let*-values (((x) (right 'z))))) => #t)
  (check (right-of-z?
          (either-let*-values (((either-bind (right #t) right)))
            (right 'z)))
   => #t)
  (check (left-of-z?
          (either-let*-values (((x) (right #t))
                               (y (left 'z)))
            (right x)))
   => #t)
  (check (either= eqv?
                 (either-let*-values (((x y) (right 2 3)))
                   (right (* x y)))
                 (right 6))
   => #t)
  (check (left-of-z?
          (either-let*-values (((x) (right 2))
                               ((either-swap (right 'z))))
            (right x)))
   => #t)
  (check (either= eqv?
                 (either-let*-values (((b) (right #t)) ((truth->either b)))
                   (right b))
                 (right #t))
   => #t)
  (check (right-of-z? (either-let*-values ((vals (right 'z #t)))
                      (right (car vals))))
   => #t)
  (check (right-of-z? (either-let*-values (((x . _) (right 'z #t)))
                      (right x)))
   => #t)
  (check (either= eqv?
                 (either-let*-values ((vals (right 'z #t))
                                     ((b c . _) (right #t 'y #f)))
                   (right (car vals) b))
                 (right 'z #t))
   => #t)
  ;; Behavior of bound-variable claws.
  (let ((right-of-z (right 'z)) (left-of-z (left 'z)))
    (check (right-of-z? (either-let*-values (right-of-z)
                          right-of-z))
     => #t)
    (check (either= eqv?
                    (either-let*-values (((x) (right 2))
                                        right-of-z
                                        ((y) (right 3)))
                      (right (* x y)))
                    (right 6))
     => #t)
    (check (right-of-z? (either-let*-values (right-of-z ((right 'x)))
                          right-of-z))
     => #t)
    (check (left-of-z? (either-let*-values (left-of-z))) => #t)
    (check (left-of-z? (either-let*-values (((x) (right 2))
                                            left-of-z
                                            ((y) (right 3)))
                       (right (* x y))))
     => #t))
  ;; let*-values / bind identities.
  (let ((right-neg-both (lambda (b c) (right (not b) (not c)))))
    (check (either= eqv?
                    (either-bind (right #t #t) right-neg-both)
                    (either-let*-values (((b c) (right #t #t)))
                      (right-neg-both b c))
                    (right #f #f))
     => #t)
    (check (either= eqv?
                    (either-bind (left #t #t) right-neg-both)
                    (either-let*-values (((b c) (left #t #t))) (right-neg b))
                    (left #t #t))
     => #t)))
