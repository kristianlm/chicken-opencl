(import opencl srfi-4 test chicken.string chicken.gc)

(define (test-platform platform)

  (define device (car (platform-devices platform)))
  (define context (context-create device))
  (define cq (command-queue context device))

  (define data (u32vector 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
  ;;                                 ,-- for type and size information only
  (define A (buffer-create context data))
  (buffer-write A cq data)
  (test "read-back after buffer-write"
        (u32vector 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
        (buffer-read A cq))

  (define kernel (kernel-create (program-build (program-create context "
__kernel void tt(__global uint *A) {
  uint gx = get_global_id(0) - get_global_offset(0);
  uint gy = get_global_id(1) - get_global_offset(1);
  uint lx = get_local_id(0);
  uint ly = get_local_id(1);
  uint ox = get_global_offset(0);
  uint oy = get_global_offset(1);
  uint pos = (gx + (gy * get_global_size(1)));
  A[pos] = 1000000000 + ox*10000000 + oy*1000000 + ly*10000 + 1000*lx + gy*10 + gx;
}
") device) "tt"))
  (kernel-arg-set! kernel 0 A)

  (kernel-enqueue kernel cq (list 4 4) local-work-sizes: (list 1 1))
  (test "simple kernel 4x4 1x1"
        (u32vector 1000000000 1000000001 1000000002 1000000003
                   1000000010 1000000011 1000000012 1000000013
                   1000000020 1000000021 1000000022 1000000023
                   1000000030 1000000031 1000000032 1000000033)
        (buffer-read A cq))

  (kernel-enqueue kernel cq (list 4 4) local-work-sizes: (list 2 2))
  (test "simple kernel 4x4 2x2"
        (u32vector 1000000000 1000001001 1000000002 1000001003
                   1000010010 1000011011 1000010012 1000011013
                   1000000020 1000001021 1000000022 1000001023
                   1000010030 1000011031 1000010032 1000011033)
        (buffer-read A cq))

  (kernel-enqueue kernel cq (list 4 4) local-work-sizes: (list 1 1) global-work-offsets: (list 1 3))
  (test "simple kernel 4x4 1x1 with 3x3 offset"
        ;;      oo ll gg   oo ll gg   oo ll gg   oo ll gg
        (u32vector 1013000000 1013000001 1013000002 1013000003
                   1013000010 1013000011 1013000012 1013000013
                   1013000020 1013000021 1013000022 1013000023
                   1013000030 1013000031 1013000032 1013000033)
        (buffer-read A cq))

  (test-group
   "datatype conversion"

   (test "buffer without explicit type" 'blob (buffer-type (buffer-create context 32)))
   (test "buffer by srfi4 vector type" 'f32 (buffer-type (buffer-create context (f32vector 1 2))))
   (test "buffer by srfi4 vector type" 'f64 (buffer-type (buffer-create context (f64vector 1 2))))
   (test "buffer by srfi4 vector type" 'u64 (buffer-type (buffer-create context (u64vector 1 2))))

   ;;                                                 ,-- bytes
   (define b (buffer-write (buffer-create context (* 4 2) type: 's32) cq
                           (u32vector 1 2)))
   (test "buffer-type match" 's32 (buffer-type b))

   (test "implicit buffer-type" (s32vector 1 2) (buffer-read b cq))
   (set! (buffer-type b) 'u32) ;; you're not really meant to modify this
   (test "explicit buffer type" (u32vector 1 2) (buffer-read b cq))
   (test "explicit buffer-read type" (u32vector->blob (u32vector 1 2)) (buffer-read b cq type: 'blob))

   (define b2 (buffer-create context b))
   (test "buffer type is copied" 'u32 (buffer-type b2))
   (test "buffer type is copied" (* 4 2) (buffer-size b2)))

  (test-group
   "kernel argument types"

   (define program
     (program-build (program-create context "

__kernel void test_char (char a1,  char4 a4,  __global char  *out) { *out = a1 + a4.r + a4.g + a4.b + a4.a; }
__kernel void test_short(short a1, short4 a4, __global short *out) { *out = a1 + a4.r + a4.g + a4.b + a4.a; }
__kernel void test_int  (int a1,   int4 a4,   __global int   *out) { *out = a1 + a4.r + a4.g + a4.b + a4.a; }
__kernel void test_float(float a1, float4 a4, __global float *out) { *out = a1 + a4.r + a4.g + a4.b + a4.a; }
// long and double not supported by all platforms

") device))
   (define test_char  (kernel-create program "test_char"))
   (define test_short (kernel-create program "test_short"))
   (define test_int   (kernel-create program "test_int"))
   (define test_float (kernel-create program "test_float"))
   (define out (buffer-create context 4)) ;; <-- plenty of room for everybody

   (begin
     (kernel-arg-set! test_char 0 (s8vector 11))
     (kernel-arg-set! test_char 1 (s8vector 1 2 3 4))
     (kernel-arg-set! test_char 2 out)
     (kernel-enqueue test_char cq (list 1))
     (test "kernel argument type char"  (+ 11 1 2 3 4)  (s8vector-ref (buffer-read out cq type: 's8) 0)))
   (begin
     (kernel-arg-set! test_short 0 (s16vector 11))
     (kernel-arg-set! test_short 1 (s16vector 1 2 3 4))
     (kernel-arg-set! test_short 2 out)
     (kernel-enqueue test_short cq (list 1))
     (test "kernel argument type short"  (+ 11 1 2 3 4)  (s16vector-ref (buffer-read out cq type: 's16) 0)))
   (begin
     (kernel-arg-set! test_int 0 (s32vector 11))
     (kernel-arg-set! test_int 1 (s32vector 1 2 3 4))
     (kernel-arg-set! test_int 2 out)
     (kernel-enqueue test_int cq (list 1))
     (test "kernel argument type int"  (+ 11 1 2 3 4)  (s32vector-ref (buffer-read out cq type: 's32) 0)))
   (begin
     (kernel-arg-set! test_float 0 (f32vector 11))
     (kernel-arg-set! test_float 1 (f32vector 1 2 3 4))
     (kernel-arg-set! test_float 2 out)
     (kernel-enqueue test_float cq (list 1))
     (test "kernel argument type float"  (+ 11.0 1 2 3 4)  (f32vector-ref (buffer-read out cq type: 'f32) 0)))))

(for-each (lambda (platform)
            (test-group
             (conc "platform " (platform-name platform))
             (test-platform platform)))
          (platforms))

(test-exit)
