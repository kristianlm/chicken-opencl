(import srfi-4 opencl chicken.file.posix srfi-18 chicken.condition
        stb-image-write)

(define device (car (reverse (flatten (map (cut platform-devices <> 'gpu) (platforms))))))
(define context (context-create device))
(define cq (command-queue-create context device))
(define w 3840)
(define h 2160)
(define out (buffer-create (* 1 w h) cq))

(let ((kernel (kernel-create
               (program-build
                (program-create context "#include \"mandelbrot.cl\"")
                device)
               "mandelbrot")))
  (kernel-arg-set! kernel 0 (f64vector -2.0))   ;; x0
  (kernel-arg-set! kernel 1 (f64vector  1.0))   ;; y0
  (kernel-arg-set! kernel 2 (f64vector  0.001)) ;; stepSize
  (kernel-arg-set! kernel 3 (u32vector 256))    ;; maxIterations
  (kernel-arg-set! kernel 4 out)
  (kernel-arg-set! kernel 5 (s32vector w)) ;; windowWidth
  (kernel-enqueue kernel cq (list w h))
  (with-output-to-file "mandelbrot.png"
    (lambda ()
      (write-png (buffer-read out cq) w h 1)))
  (print "done, see mandelbrot.png"))
