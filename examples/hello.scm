(import srfi-4 opencl)

(define device (car (flatten (map platform-devices (platforms)))))
(define context (context-create device))
(define cq (command-queue-create context device))
(define buffer (buffer-create (f32vector 1 2 3 4 5) cq))
(define program (program-build (program-create context "
__kernel void plus(__global float *array, float term) {
  array[get_global_id(0)] += term;
}
__kernel void times(__global float *array, float factor) {
  array[get_global_id(0)] *= factor;
}
") device))

(define plus!
  (let ((kernel (kernel-create program "plus")))
    (lambda (array term)
      (kernel-arg-set! kernel 0 array)
      (kernel-arg-set! kernel 1 (f32vector term))
      (kernel-enqueue kernel cq (list (/ (buffer-size array) 4)))
      array)))

(define times!
  (let ((kernel (kernel-create program "times")))
    (lambda (array factor)
      (kernel-arg-set! kernel 0 array)
      (kernel-arg-set! kernel 1 (f32vector factor)) ;;       ,-- bytes
      (kernel-enqueue kernel cq (list (/ (buffer-size array) 4)))
      array)))

;; note this works because cq is in-order. that is, plus will wait for
;; times to complete, and buffer-read will in turn wait for plus to
;; complete.

(print (buffer-read (plus! (times! buffer 10) 1) cq))
;; should output #f32(11.0 21.0 31.0 41.0 51.0)
