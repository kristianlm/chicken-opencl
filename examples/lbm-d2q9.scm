(import srfi-4 opencl fmt clojurian.syntax test srfi-18
        chicken.string
        chicken.port
        chicken.file.posix
        chicken.time
        chicken.condition
        chicken.process-context
        stb-image-write stb-image)

(define w     (string->number (car   (command-line-arguments))))
(define h     (string->number (cadr  (command-line-arguments))))
(define scale (string->number (caddr (command-line-arguments))))
(define frame 0)
(define viscosity 0.1)
(define sim-steps 1)
(define paused? #f)

(define device (cadr (values (flatten (map (cut platform-devices <> 'gpu) (platforms))))))
(print device)
(define context (context-create device))
(define cq (command-queue-create context device))

(define (ü lattice)

  (let* ((n (f32vector-ref lattice 0)) 
         (E (f32vector-ref lattice 1)) 
         (W (f32vector-ref lattice 2)) 
         (N (f32vector-ref lattice 3)) 
         (S (f32vector-ref lattice 4)) 
         (NE (f32vector-ref lattice 5))
         (SE (f32vector-ref lattice 6))
         (NW (f32vector-ref lattice 7))
         (SW (f32vector-ref lattice 8))
         (rho (+ E W N S NE SE NW SW)))
    (f32vector (/ (- (+ E NE SE) W NW SW) rho)
               (/ (- (+ S SE SW) N NE NW) rho))))

(define (load-kernel)

  (define program (program-build (program-create context "#include \"lbm-d2q9.cl\"") device))

  (set! collide
    (let ((kernel (kernel-create program "collide")))
      (lambda (w h Ω f b)
        (kernel-arg-set! kernel 0 (u32vector w h))
        (kernel-arg-set! kernel 1 (f32vector Ω))
        (kernel-arg-set! kernel 2 f)
        (kernel-arg-set! kernel 3 b)
        (kernel-enqueue kernel cq (list w h)))))

  (set! stream
    (let ((kernel (kernel-create program "stream")))
      (lambda (w h f f0 barrier)
        (kernel-arg-set! kernel 0 (u32vector w h))
        (kernel-arg-set! kernel 1 f)
        (kernel-arg-set! kernel 2 f0)
        (kernel-arg-set! kernel 3 barrier)
        (kernel-enqueue kernel cq (list w h)))))

  (set! rho
    (let ((kernel (kernel-create program "rho")))
      (lambda (w h f rho u)
        (kernel-arg-set! kernel 0 (u32vector w h))
        (kernel-arg-set! kernel 1 f)
        (kernel-arg-set! kernel 2 rho)
        (kernel-arg-set! kernel 3 u)
        (kernel-enqueue kernel cq (list w h)))))

  (set! speed
    (let ((kernel (kernel-create program "speed")))
      (lambda (w h f speed)
        (kernel-arg-set! kernel 0 (u32vector w h))
        (kernel-arg-set! kernel 1 f)
        (kernel-arg-set! kernel 2 speed)
        (kernel-enqueue kernel cq (list w h)))))

  (set! corner
    (let ((kernel (kernel-create program "corner")))
      (lambda (w h f)
        (kernel-arg-set! kernel 0 (u32vector w h))
        (kernel-arg-set! kernel 1 f)
        (kernel-enqueue kernel cq (list w h)))))

  (set! initialize
    (let ((kernel (kernel-create program "initialize")))
      (lambda (w h u rho f) ;; u is complex number for 2d velocity
        (kernel-arg-set! kernel 0 (u32vector w h))
        (kernel-arg-set! kernel 1 (f32vector (real-part u) (imag-part u)))
        (kernel-arg-set! kernel 2 (f32vector rho))
        (kernel-arg-set! kernel 3 f)
        (kernel-enqueue kernel cq (list w h)))))

  (set! visualize
    (let ((kernel (kernel-create program "visualize")))
      (lambda (w h rmin rmax sscale f barrier image)
        (kernel-arg-set! kernel 0 (u32vector w h))
        (kernel-arg-set! kernel 1 (f32vector rmin rmax))
        (kernel-arg-set! kernel 2 (f32vector sscale))
        (kernel-arg-set! kernel 3 f)
        (kernel-arg-set! kernel 4 barrier)
        (kernel-arg-set! kernel 5 image)
        (kernel-enqueue kernel cq (list w h))))))

(load-kernel)

(define bhost (make-u8vector (* w h) 0))

(define f  (buffer-create (make-f32vector (* w h 9)) cq))
(define f0 (buffer-create (make-f32vector (* w h 9) 1) cq))
(define b  (buffer-create bhost cq))
(define r  (buffer-create (make-f32vector (* w h 1) 0) cq))
(define u  (buffer-create (make-f32vector (* w h 2) 0) cq))
(define s  (buffer-create (make-f32vector (* w h 1) 0) cq))

(define (reset!)
  (initialize w h 0 1 f))

(reset!)

(define image (buffer-create (make-u32vector (* w h) 1) cq))

;; (fmt #t (fix 2 (fmt-join (cut fmt-join dsp <> (dsp " ")) (chop (f32vector->list (buffer-read u cq)) w) nl)))

(begin
  
  (define b  (buffer-create (make-u8vector (* w h) 0) cq))
  (let ((o (buffer-read b cq)))

    (do ((y (* h 3/8) (+ y 1)))
        ((>= y (* h 5/8)))
      (let ((x 100))
        (u8vector-set! o (+ x (* w y)) 1)))

    ;; (do ((x 55 (+ x 1)))
    ;;     ((>= x 300))
    ;;   (u8vector-set! o (+ x (* w (* 3/8 h))) 1)
    ;;   ;;(when (< x (* 5/8 w)) (u8vector-set! o (+ x (* w 100)) 1))
    ;;   (u8vector-set! o (+ x (* w (* 5/8 h))) 1))
    
    (buffer-write b cq o)))

(define (minmax buffer)
  (let* ((vec (buffer-read buffer cq ))
         (lst (f32vector->list vec))
         (mx (foldl max -1e6 lst))
         (mn (foldl min +1e6 lst)))
    (list mn mx)))

(begin
  (rho w h f r u)
  (minmax u))

(let ()
  (rho w h f r u)
  (speed w h f s)

  (let ((uix (minmax u))
        (rix (minmax r)))
   (fmt #t
        " float2 u_range = (float2)(" (fmt-join dsp uix ",") ");" nl
        " float2 rho_range = (float2)(" (fmt-join dsp rix ",") ");" nl)

   (set! rmn 0.9)
   (set! rmx 1.3)))

(fmt #t (fmt-join dsp '(1 2 3) ","))

(define (filename) (conc "barrier-" w "x" h ".png"))
;;(define (save-file) (with-output-to-file  (filename) (lambda () (write-u8vector (buffer-read b cq)))))
;;(define (load-file) (with-input-from-file (filename) (lambda () (buffer-write b cq (read-u8vector)))))

(define (save-file) (with-output-to-file (filename) (lambda () (write-png (buffer-read b cq) w h 1))))
(define (load-file)
  (with-input-from-file (filename)
    (lambda ()
      (receive (pixels W H C) (read-image)
        (unless (and (= W w) (= H h) (= C 1))
          (error
           (conc "error while reading" (filename) ": expected " (list w h 1)
                 "got " (list W H C))))
        (buffer-write b cq pixels)))))

(define maybe-load-kernels
  (let ((last-mod-time #f)
        (last-check #f))
    (lambda ()
      (unless (and last-check (< (- (current-milliseconds) last-check) 200))
        (set! last-check (current-milliseconds))
        (let ((now-mod-time (file-modification-time "lbm-d2q9.cl")))
          (unless (and last-mod-time (equal? now-mod-time last-mod-time))
            (set! last-mod-time now-mod-time)
            (print "kernels changed!")
            (load-kernel)))))))

(buffer-read f cq bytes: (* 4 9))

(define (game-loop)
  (maybe-load-kernels)

  (thread-sleep! 0.001)

  (define mx (min (- w 1) (max 0 (inexact->exact (floor (/ (- (mouse-x) 1) scale))))))
  (define my (min (- h 1) (max 0 (inexact->exact (floor (/ (- (mouse-y) 1) scale))))))

  (when (or (mouse-button-down? 0)
            (mouse-button-down? 1))
    (let ((data (buffer-read b cq)))
      
      (define (pset! x y v)
        (when (and (>= x 0) (>= y 0) (< x w) (< y h))
          (u8vector-set! data (+ x (* w y)) v)))

      (pset! (+ 0 mx) (+ 0 my) (if (mouse-button-down? 0) 255 0))
      (pset! (+ 0 mx) (+ 1 my) (if (mouse-button-down? 0) 255 0))
      (pset! (+ 1 mx) (+ 0 my) (if (mouse-button-down? 0) 255 0))
      (pset! (+ 1 mx) (+ 1 my) (if (mouse-button-down? 0) 255 0))
      (buffer-write b cq data)))

  (when (key-down? (char->integer #\R))
    (reset!))

  (when (key-pressed? (char->integer #\space))
    (set! paused? (not paused?)))

  (when (key-pressed? (char->integer #\S)) (save-file))
  (when (key-pressed? (char->integer #\L)) (load-file))

  (when (key-pressed? (char->integer #\.)) (set! sim-steps (+ sim-steps 1)))
  (when (key-pressed? (char->integer #\,)) (set! sim-steps (max 1 (- sim-steps 1))))

  ;; ==================== simulation ====================
  
  (define (maybe-external)
    (when (mouse-button-down? 2)
      (let ()
        (define f1 (buffer-read f cq bytes: (* 4 9) byte-offset: (* 4 9 (+ mx (* w my)))))
        (define rho (+ (foldl + 0 (f32vector->list f1)) 0.00001))
        (buffer-write f cq (equilibrium .5+0i rho)
                      offset: (* 9 4 (+ mx (* w my)))))))

  (maybe-external)
  (when (or (not paused?) (key-pressed? (char->integer #\O)))
    (let loop ((n (if paused? 1 sim-steps)))
      (set! frame (+ frame 1))
      ;;(unless (= n 0) (maybe-external))
      (collide w h 1.9 f b) ;; (/ 1 (+ (* 3 viscosity) 1))
      (let ((tmp f)) (stream w h f f0 b) (set! f f0) (set! f0 tmp))
      (when (> n 1) (loop (+ n -1)))))

  (define (draw-lattice p #!optional (x 80))
    (define (emit n) (fmt #f (fix 5 n)))
    (let ((>> 80) (<< -80) (ß scale))
      (draw-text (emit (f32vector-ref p 0)) (+ x  0) (+ (* ß h) (+ 50   0)) 10 #xffffffff)
      (draw-text (emit (f32vector-ref p 1)) (+ x >>) (+ (* ß h) (+ 50   0)) 10 #xff00ffff)
      (draw-text (emit (f32vector-ref p 2)) (+ x <<) (+ (* ß h) (+ 50   0)) 10 #xff00ffff)
      (draw-text (emit (f32vector-ref p 3)) (+ x  0) (+ (* ß h) (+ 50 -10)) 10 #xff00ffff)
      (draw-text (emit (f32vector-ref p 4)) (+ x  0) (+ (* ß h) (+ 50 +10)) 10 #xff00ffff)
      (draw-text (emit (f32vector-ref p 5)) (+ x >>) (+ (* ß h) (+ 50 -10)) 10 #xff00aaff)
      (draw-text (emit (f32vector-ref p 6)) (+ x >>) (+ (* ß h) (+ 50 +10)) 10 #xff00aaff)
      (draw-text (emit (f32vector-ref p 7)) (+ x <<) (+ (* ß h) (+ 50 -10)) 10 #xff00aaff)
      (draw-text (emit (f32vector-ref p 8)) (+ x <<) (+ (* ß h) (+ 50 +10)) 10 #xff00aaff)
      (draw-text (emit (foldl + 0 (f32vector->list p))) (+ x >> >>) (+ (* ß h) (+ 50 -5)) 20 #xff8080ff)))

  (let* ((bb  (buffer-read b  cq bytes: 1 byte-offset: (+ mx (* w my))))
         (ff  (buffer-read f  cq bytes: (* 4 9) byte-offset: (* 4 9 (+ mx (* w my)))))
         (ff0 (buffer-read f0 cq bytes: (* 4 9) byte-offset: (* 4 9 (+ mx (* w my))))))
    (draw-lattice ff0)    (draw-text (fmt #f "old" ) 90 (+ (* scale h) 30) 10 #xffffffff)
    (draw-lattice ff 420) (draw-text (fmt #f "new " (ü ff)) 430 (+ (* scale h) 30) 10 #xffffffff)
    (draw-text (conc "fps " (fps) " f" frame " " mx " " my " steps" sim-steps) 1 (* scale h) 20 #xff0000ff))

  (visualize w h rmn rmx 100 f b image)
  (let ((u32 (buffer-read image cq)))
    (if (< 250 (remainder (current-milliseconds) 500)) (u32vector-set! u32 (+ mx (* w my)) #xff00ffff))
    (mypixels w h u32)))

(begin
  (let ()
    (rho w h f r u)
    (speed w h f s)

    (let ((uix (minmax u))
          (rix (minmax r)))
      (fmt #t
           " float2 u_range = (float2)(" (fmt-join dsp uix ",") ");" nl
           " float2 rho_range = (float2)(" (fmt-join dsp rix ",") ");" nl)

      (let ((fn (conc "out-" w "x" h "f" frame "_min" uix "_max" rix ".png")))
        (with-output-to-file fn
          (lambda () (write-png (u32vector->blob/shared (buffer-read image cq)) w h 4)))
        (print "wrote " fn)))))

