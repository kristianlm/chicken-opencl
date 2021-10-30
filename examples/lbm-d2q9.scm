(import srfi-4 opencl fmt clojurian.syntax test srfi-18 chicken.string chicken.port)

(begin
  (define Q 9)
  (define w 2)
  (define h 2)

  (define viscosity 0.045)
  (define dx 0.1) ;; grid size
  (define dt 1) ;; time step
  (define tau 0.6) ;; viscous relaxation parameter
  (define cVel (/ dx dt 1.0))
  (define omega (/ dt tau 1.0)))

(define device (car (values (flatten (map (cut platform-devices <> 'gpu) (platforms))))))
(print device)
(define context (context-create device))
(define cq (command-queue-create context device))
(define program (program-build (program-create context "#include \"lbm-d2q9.cl\"") device))

(define collide
  (let ((kernel (kernel-create program "collide")))
    (lambda (w h omega f)
      (kernel-arg-set! kernel 0 (u32vector w h))
      (kernel-arg-set! kernel 1 (f32vector omega))
      (kernel-arg-set! kernel 2 f)
      (kernel-enqueue kernel cq (list w h)))))

(define stream
  (let ((kernel (kernel-create program "stream")))
    (lambda (w h f f0)
      (kernel-arg-set! kernel 0 (u32vector w h))
      (kernel-arg-set! kernel 1 f)
      (kernel-arg-set! kernel 2 f0)
      (kernel-enqueue kernel cq (list w h)))))

(define rho
  (let ((kernel (kernel-create program "rho")))
    (lambda (w h f rho u)
      (kernel-arg-set! kernel 0 (u32vector w h))
      (kernel-arg-set! kernel 1 f)
      (kernel-arg-set! kernel 2 rho)
      (kernel-arg-set! kernel 3 u)
      (kernel-enqueue kernel cq (list w h)))))

(define speed
  (let ((kernel (kernel-create program "speed")))
    (lambda (w h f speed)
      (kernel-arg-set! kernel 0 (u32vector w h))
      (kernel-arg-set! kernel 1 f)
      (kernel-arg-set! kernel 2 speed)
      (kernel-enqueue kernel cq (list w h)))))

(define corner
  (let ((kernel (kernel-create program "corner")))
    (lambda (w h f)
      (kernel-arg-set! kernel 0 (u32vector w h))
      (kernel-arg-set! kernel 1 f)
      (kernel-enqueue kernel cq (list w h)))))

(define f (buffer-create
            (f32vector 1 2 3 4 5 6 7 8 9   10 20 30 40 50 60 70 80 90
                          -1 -2 -3 -4 -5 -6 -7 -8 -9   -10 -20 -30 -40 -50 -60 -70 -80 -90) cq))

(define f0 (buffer-create (f32vector 11 11 11 11 11 11 11 11 11    11 11 11 11 11 11 11 11 11
                                        11 11 11 11 11 11 11 11 11    11 11 11 11 11 11 11 11 11) cq))

(define (lattice ve x y)
  (define (get i) (f32vector-ref ve (+ (* 9 x) (* y w 9) i)))
  (list (list (get 5) (get 1) (get 6))
        (list (get 4) (get 0) (get 3))
        (list (get 7) (get 2) (get 8))))

(define (l b x y)
  (fmt-join (cut fmt-join dsp <>) (lattice b x y) nl))

(define (print-bit f)
  (fmt #t (fix 2 (f32vector->list (buffer-read f cq))) nl nl
       (decimal-align 4 (fix 2 (columnar (cat (l (buffer-read f cq) 0 0) "\n\n"
                                              (l (buffer-read f cq) 0 1))
                                         (cat (l (buffer-read f cq) 1 0) "\n\n"
                                              (l (buffer-read f cq) 1 1))))) nl))

;; (collide (/ 1 (+ (* 3 viscosity) 0.5)) f)
(print-bit f)
(test-group
 "stream"
 (define f (buffer-create (f32vector 1 2 3 4 5 6 7 8 9   10 20 30 40 50 60 70 80 90
                                     -1 -2 -3 -4 -5 -6 -7 -8 -9   -10 -20 -30 -40 -50 -60 -70 -80 -90)
                          cq))

 (define f0 (buffer-create (f32vector 11 11 11 11 11 11 11 11 11    11 11 11 11 11 11 11 11 11
                                         11 11 11 11 11 11 11 11 11    11 11 11 11 11 11 11 11 11) cq))
 (stream w h f f0)
 (test (f32vector 1   0  -2  50   0  0  0  70 -60        10   0 -20   5   4  0  0  -7  -6
                  -1   3   2 -50  40  0 80   0  60      -10  30  20  -5  -4  9  8   7   6)
       (buffer-read f0 cq)))

(test-group
 "collide"
 (define f
   (buffer-create
    (f32vector 16  4 4 4 4  1 1 1 1
                  16  4 4 4 4  1 1 1 1
                  16  4 4 4 4  1 1 1 1
                  16  4 4 4 4  1 1 1 1) cq))

 (collide w h .5 f)
 (test (f32vector  8  4 4 4 4  1 1 1 1
                   8  4 4 4 4  1 1 1 1
                   8  4 4 4 4  1 1 1 1
                   8  4 4 4 4  1 1 1 1)
       (buffer-read f cq)))


(define w 40)
(define h 30)

(define hostf
  (make-f32vector (* w h 9) 0.5))

;; (let ((x (/ w 2))
;;       (y (/ h 2)))
;;   (f32vector-set! hostf (+ (* 9 w y) (* 9 x) 1) 12))

(define f (buffer-create hostf cq))
(define f0 (buffer-create (make-f32vector (* w h 9) 1) cq))
(define r  (buffer-create (make-f32vector (* w h 1) 0) cq))
(define u  (buffer-create (make-f32vector (* w h 2) 0) cq))
(define s  (buffer-create (make-f32vector (* w h 1) 0) cq))

;; (fmt #t (fix 2 (fmt-join (cut fmt-join dsp <> (dsp " ")) (chop (f32vector->list (buffer-read u cq)) w) nl)))

(define (def x)
  (cond ((< x 0.01) ".")
        ((< x 0.10) ":")
        ((< x 0.50) "+")
        ((< x 1.00) "*")
        ((< x 4.00) "@")
        ((< x 6.00) "#")
        ((< x 7.00) "g")
        ((< x 9.00) "G")
        ((< x 10.00) "h")
        ((< x 12.00) "H")
        ((< x 14.00) "w")
        ((< x 16.00) "W")
        (else "%")))

(define (print-canvas v #!optional (ch def))
  (display
   (with-output-to-string
     (lambda ()
       (do ((y 0 (+ y 1)))
           ((>= y h))
         (do ((x 0 (+ x 1)))
             ((>= x w))
           (let ((x (f32vector-ref v (+ x (* w y)))))
             (display (ch x))))
         (newline))))))

(define (coldef x)
  (let ((r (inexact->exact (floor (/ (* 255 x) 32))))
        (g 0)
        (b 0))
   (conc
    "\x1b[48;2;" r ";" g ";" b "m" " "
    (cond ((< x 0.01) ".")
          ((< x 0.10) ":")
          ((< x 0.50) "+")
          ((< x 1.00) "*")
          ((< x 4.00) "@")
          ((< x 6.00) "#")
          ((< x 7.00) "g")
          ((< x 9.00) "G")
          ((< x 10.00) "h")
          ((< x 12.00) "H")
          ((< x 14.00) "w")
          ((< x 16.00) "W")
          (else "%"))
    "\x1b[0m")))

(let loop ((n 0))
  (define viscosity 0.01)
  (collide w h (/ 1 (+ (* 3 viscosity) 0.5)) f)
  (stream w h f f0) (let ((tmp f)) (set! f f0) (set! f0 tmp)) ;; swap
  (corner w h f)
  (rho w h f r u)
  (speed w h f s)
  (define r_ (buffer-read r cq))
  (define s_ (buffer-read s cq))
  (define rlst (f32vector->list r_))
  (define slst (f32vector->list s_))
  (fmt #t
       (fix 3 "frame " n
            " r" (list (foldl max -1e6 rlst) (foldl min 1e6 rlst))
            " s" (list (foldl max -1e6 slst) (foldl min 1e6 slst)))
       nl)
  (print-canvas r_ coldef)

  (thread-sleep! .2)
  (when (< n 400)
   (loop (+ n 1))))
