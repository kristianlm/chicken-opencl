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
(define sim-steps 1)
(define paused? #f)

(define device (cadr (values (flatten (map (cut platform-devices <> 'gpu) (platforms))))))
(print device)
(define context (context-create device))
(define cq (command-queue-create context device))

(define (load-kernel)

  (define program (program-build (program-create context "#include \"lbm-d2q9.cl\"") device))

  (set! ripple
    (let ((kernel (kernel-create program "ripple")))
      (lambda (w h src dst)
        (kernel-arg-set! kernel 0 (u32vector w h))
        (kernel-arg-set! kernel 1 src)
        (kernel-arg-set! kernel 2 dst)
        (kernel-enqueue kernel cq (list w h)))))  

  (set! render
    (let ((kernel (kernel-create program "render")))
      (lambda (w h src image)
        (kernel-arg-set! kernel 0 (u32vector w h))
        (kernel-arg-set! kernel 1 src)
        (kernel-arg-set! kernel 2 image)
        (kernel-enqueue kernel cq (list w h)))))

  (set! reset
    (let ((kernel (kernel-create program "reset")))
      (lambda (w h v src)
        (kernel-arg-set! kernel 0 (u32vector w h))
        (kernel-arg-set! kernel 1 (f32vector v))
        (kernel-arg-set! kernel 2 src)
        (kernel-enqueue kernel cq (list w h))))))

(load-kernel)

(define bhost (make-u8vector (* w h) 0))

(define src (buffer-create (make-f32vector (* w h) 0) cq))
(define dst (buffer-create (make-f32vector (* w h) 0) cq))
(define image (buffer-create (make-u32vector (* w h) 1) cq))

(define (reset!)
  (reset w h 0 dst)
  (reset w h 0 src))

(reset!)

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

(define (game-loop)
  (maybe-load-kernels)

  (thread-sleep! 0.001)

  (define mx (min (- w 1) (max 0 (inexact->exact (floor (/ (- (mouse-x) 1) scale))))))
  (define my (min (- h 1) (max 0 (inexact->exact (floor (/ (- (mouse-y) 1) scale))))))

  (when (key-down? (char->integer #\R))
    (reset!))

  (when (key-pressed? (char->integer #\space))
    (set! paused? (not paused?)))

  ;; ==================== simulation ====================
  
  (define (maybe-external)
    (when (mouse-button-down? 0)
      (buffer-write src cq (f32vector 2) offset: (* 4 (+ mx (* w my))))))

  (maybe-external)

  (when (or (not paused?) (key-pressed? (char->integer #\O)))
    (let loop ((n (if paused? 1 sim-steps)))
      (set! frame (+ frame 1))
      (begin (ripple w h src dst) (let ((t src)) (set! src dst) (set! dst t)))
      (when (> n 1) (loop (+ n -1)))))

  (let* ((s (f32vector->list (buffer-read src cq bytes: 4 byte-offset: (* 4 (+ mx (* w my))))))
         (d (f32vector->list (buffer-read dst cq bytes: 4 byte-offset: (* 4 (+ mx (* w my)))))))
    (define (emit n) (fmt #f (fix 5 n)))
    (draw-text (fmt #f "fps " (fps) " f" frame " " mx " " my) 1 (* scale h) 20 #xff0000ff)
    (draw-text (fmt #f (fix 5 "s " s)) 1 (+ (* scale h) 20) 20 #xff00ff00)
    (draw-text (fmt #f (fix 5 "d " d)) 1 (+ (* scale h) 40) 20 #xff00ff00))

  (render w h src image)
  (let ((u32 (buffer-read image cq)))
    (if (< 250 (remainder (current-milliseconds) 500)) (u32vector-set! u32 (+ mx (* w my)) #xff00ffff))
    (mypixels w h u32)))


