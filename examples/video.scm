(import srfi-4 stb-image-write chicken.port)

(define (mypixels w h v32)
  (write-u8vector (blob->u8vector/shared (u32vector->blob/shared v32))))

(define stdout (current-output-port))
(current-output-port (current-error-port))

(define equilibrium           (constantly #f))
(define fps                   (constantly #f))
(define mouse-x               (constantly 0))
(define mouse-y               (constantly 0))
(define mouse-button-pressed? (constantly #f))
(define mouse-button-down?    (constantly #f))
(define draw-text             (constantly #f))
(define key-down?             (constantly #f))
(define key-pressed?          (constantly #f))

(include "lbm-d2q9.scm")

(load-file)
(current-output-port stdout)

(let loop ()
  (game-loop)
  (loop))
