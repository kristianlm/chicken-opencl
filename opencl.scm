(import chicken.base
        chicken.foreign
        chicken.pretty-print
        (only chicken.gc set-finalizer!)
        (only chicken.string conc)
        (only chicken.blob make-blob blob? blob->string blob-size)
        srfi-4
        srfi-1)

(foreign-declare "
// TODO: don't hardcode this
#define CL_TARGET_OPENCL_VERSION 120

#ifdef __APPLE__
#include <OpenCL/cl.h>
#else
 #include <CL/cl.h>
#endif


void chicken_opencl_notify_cb(const char *errinfo, const void *private_info, size_t cb, void *user_data) {
  fprintf(stderr, \"chicken-opencl: %s\\\n\", errinfo);
}
")

;; ==================== util ====================
(define (u8vector-split v groups groupsize)
  (unless (= (/ (u8vector-length v) groups) groupsize)
    (error "internal error: cannot split u8vector into equal sized parts" groups groupsize))

  (list-tabulate groups (lambda (idx) (subu8vector v ;;  ,-- from
                                                   (* groupsize idx) ;; ,-- to
                                                   (+ groupsize (* groupsize idx))))))

;; ==================== errors ====================

(define cl-errors
  `(
    (,(foreign-value "CL_BUILD_PROGRAM_FAILURE" int) .           build-program-failure)
    (,(foreign-value "CL_COMPILER_NOT_AVAILABLE" int) .          compiler-not-available)
    (,(foreign-value "CL_DEVICE_NOT_AVAILABLE" int) .            device-not-available)
    (,(foreign-value "CL_DEVICE_NOT_FOUND" int) .                device-not-found)
    (,(foreign-value "CL_IMAGE_FORMAT_MISMATCH" int) .           image-format-mismatch)
    (,(foreign-value "CL_IMAGE_FORMAT_NOT_SUPPORTED" int) .      image-format-not-supported)
    (,(foreign-value "CL_INVALID_ARG_INDEX" int) .               invalid-arg-index)
    (,(foreign-value "CL_INVALID_ARG_SIZE" int) .                invalid-arg-size)
    (,(foreign-value "CL_INVALID_ARG_VALUE" int) .               invalid-arg-value)
    (,(foreign-value "CL_INVALID_BINARY" int) .                  invalid-binary)
    (,(foreign-value "CL_INVALID_BUFFER_SIZE" int) .             invalid-buffer-size)
    (,(foreign-value "CL_INVALID_BUILD_OPTIONS" int) .           invalid-build-options)
    (,(foreign-value "CL_INVALID_COMMAND_QUEUE" int) .           invalid-command-queue)
    (,(foreign-value "CL_INVALID_CONTEXT" int) .                 invalid-context)
    (,(foreign-value "CL_INVALID_DEVICE" int) .                  invalid-device)
    (,(foreign-value "CL_INVALID_DEVICE_TYPE" int) .             invalid-device-type)
    (,(foreign-value "CL_INVALID_EVENT" int) .                   invalid-event)
    (,(foreign-value "CL_INVALID_EVENT_WAIT_LIST" int) .         invalid-event-wait-list)
    (,(foreign-value "CL_INVALID_GLOBAL_OFFSET" int) .           invalid-global-offset)
    (,(foreign-value "CL_INVALID_GLOBAL_WORK_SIZE" int) .        invalid-global-work-size)
    (,(foreign-value "CL_INVALID_GL_OBJECT" int) .               invalid-gl-object)
    (,(foreign-value "CL_INVALID_HOST_PTR" int) .                invalid-host-ptr)
    (,(foreign-value "CL_INVALID_IMAGE_FORMAT_DESCRIPTOR" int) . invalid-image-format-descriptor)
    (,(foreign-value "CL_INVALID_IMAGE_SIZE" int) .              invalid-image-size)
    (,(foreign-value "CL_INVALID_KERNEL_ARGS" int) .             invalid-kernel-args)
    (,(foreign-value "CL_INVALID_KERNEL_DEFINITION" int) .       invalid-kernel-definition)
    (,(foreign-value "CL_INVALID_KERNEL" int) .                  invalid-kernel)
    (,(foreign-value "CL_INVALID_KERNEL_NAME" int) .             invalid-kernel-name)
    (,(foreign-value "CL_INVALID_MEM_OBJECT" int) .              invalid-mem-object)
    (,(foreign-value "CL_INVALID_MIP_LEVEL" int) .               invalid-mip-level)
    (,(foreign-value "CL_INVALID_OPERATION" int) .               invalid-operation)
    (,(foreign-value "CL_INVALID_PLATFORM" int) .                invalid-platform)
    (,(foreign-value "CL_INVALID_PROGRAM_EXECUTABLE" int) .      invalid-program-executable)
    (,(foreign-value "CL_INVALID_PROGRAM" int) .                 invalid-program)
    (,(foreign-value "CL_INVALID_QUEUE_PROPERTIES" int) .        invalid-queue-properties)
    (,(foreign-value "CL_INVALID_SAMPLER" int) .                 invalid-sampler)
    (,(foreign-value "CL_INVALID_VALUE" int) .                   invalid-value)
    (,(foreign-value "CL_INVALID_WORK_DIMENSION" int) .          invalid-work-dimension)
    (,(foreign-value "CL_INVALID_WORK_GROUP_SIZE" int) .         invalid-work-group-size)
    (,(foreign-value "CL_INVALID_WORK_ITEM_SIZE" int) .          invalid-work-item-size)
    (,(foreign-value "CL_MAP_FAILURE" int) .                     map-failure)
    (,(foreign-value "CL_MEM_COPY_OVERLAP" int) .                mem-copy-overlap)
    (,(foreign-value "CL_MEM_OBJECT_ALLOCATION_FAILURE" int) .   mem-object-allocation-failure)
    (,(foreign-value "CL_OUT_OF_HOST_MEMORY" int) .              out-of-host_memory)
    (,(foreign-value "CL_OUT_OF_RESOURCES" int) .                out-of-resources)
    (,(foreign-value "CL_PROFILING_INFO_NOT_AVAILABLE" int) .    profiling-info-not-available)
    ))

;; ==================== platforms ====================
(define (platforms%)
  ((foreign-lambda* unsigned-int () "
 cl_uint num_platforms;
 clGetPlatformIDs(0, NULL, &num_platforms);
 return (num_platforms);
")))

(define-record cl_platform blob)
(define-record-printer cl_platform
  (lambda (x op)
    (display "#<cl_platform " op)
    (write (platform-name x) op)
    (display ">" op)))
(define-foreign-type cl_platform_id (c-pointer "cl_platform_id")
  (lambda (x) (location (cl_platform-blob x)))
  (lambda (x) (error "internal error: cannot return cl_platform_id by value")))

(define size_cl_platform_id (foreign-value "sizeof(cl_platform_id *)" int))

(define (status-check ret #!optional msg location)
  (unless (= ret (foreign-value "CL_SUCCESS" int))
    (error location msg (cond ((assoc ret cl-errors) => cdr)
                                      (else ret)))))

(define (platforms)
  (let* ((num-platforms (platforms%))
         (blob (make-u8vector (* num-platforms size_cl_platform_id))))

    ;; fill blob with all platforms as demanded by the C API
    (status-check
     ((foreign-lambda* int ((int num_platforms) (u8vector dest))
                       "return (clGetPlatformIDs(num_platforms, (cl_platform_id *)dest, NULL));")
      num-platforms blob)
     "clGetPlatformIDs" 'platforms)

    ;; go from one big blob into one blob per platform
    (map make-cl_platform (u8vector-split blob num-platforms size_cl_platform_id))))

(define-foreign-type cl_platform_info int)

(define (platform-info/string platform name)
  ;;                               ,-- should be overwritten
  (let-location ((status int (foreign-value "CL_INVALID_VALUE" int)))
    (let ((value ((foreign-lambda* c-string* ((cl_platform_id platform)
                                              (cl_platform_info name)
                                              ((c-pointer int) status))
                                   "size_t value_size = 0;"
                                   "clGetPlatformInfo(*platform, name, 0, NULL, &value_size);"
                                   "char *dest = malloc(value_size);"
                                   "*status = clGetPlatformInfo(*platform, name, value_size, dest, NULL);"
                                   "return(dest); // <-- freed by c-string* foreign type")
                  platform name (location status))))
      (status-check status (conc "clGetPlatformInfo name=" name) 'platform-info)
      value)))

(define (platform-profile p)    (platform-info/string p (foreign-value "CL_PLATFORM_PROFILE"    cl_platform_info)))
(define (platform-version p)    (platform-info/string p (foreign-value "CL_PLATFORM_VERSION"    cl_platform_info)))
(define (platform-name p)       (platform-info/string p (foreign-value "CL_PLATFORM_NAME"       cl_platform_info)))
(define (platform-vendor p)     (platform-info/string p (foreign-value "CL_PLATFORM_VENDOR"     cl_platform_info)))
(define (platform-extensions p) (platform-info/string p (foreign-value "CL_PLATFORM_EXTENSIONS" cl_platform_info)))

;; ============================= devices ====================

(define-record cl_device blob)
(define-record-printer cl_device
  (lambda (x op)
    (display "#<cl_device " op)
    (write (device-name x) op)
    (display ">" op)))
(define-foreign-type cl_device_id (c-pointer "cl_device_id")
  (lambda (x) (location (cl_device-blob x)))
  (lambda (x) (error "internal error: cannot return cl_device_id by value")))

(define (platform-devices platform)
  (define num-devices
    ((foreign-lambda* size_t ((cl_platform_id platform))
                      "cl_uint num_devices = 0;"
                      "clGetDeviceIDs(*platform, CL_DEVICE_TYPE_ALL, 0, NULL, &num_devices);"
                      "return(num_devices);")
     platform))

  (define size_cl_device_id (foreign-value "sizeof(cl_device_id)" size_t))

  (let ((blob (make-u8vector (* size_cl_device_id num-devices))))
    ((foreign-lambda* size_t ((cl_platform_id platform)
                              (u8vector blob)
                              (size_t num_devices))
                      "clGetDeviceIDs(*platform, CL_DEVICE_TYPE_ALL, num_devices, (cl_device_id*)blob, 0);"
                      "return(num_devices);")
     platform blob num-devices)
    (map make-cl_device (u8vector-split blob num-devices size_cl_device_id))))

(define-foreign-type cl_device_info int)

(define (device-info/string name)
  (lambda (device)
   (let-location ((status int (foreign-value "CL_INVALID_VALUE" int)))
     (let ((value ((foreign-lambda* c-string* ((cl_device_id device)
                                               (cl_device_info name)
                                               ((c-pointer int) status))
                                    "size_t value_size = 0;"
                                    "clGetDeviceInfo(*device, name, 0, NULL, &value_size);"
                                    "char *dest = malloc(value_size);"
                                    "*status = clGetDeviceInfo(*device, name, value_size, dest, NULL);"
                                    "return(dest); // <-- freed by c-string* foreign type")
                   device name (location status))))
       (status-check status (conc "clGetDeviceInfo name=" name) 'device-info/string)
       value))))

(define device-extensions       (device-info/string (foreign-value "CL_DEVICE_EXTENSIONS" cl_device_info)))
(define device-name             (device-info/string (foreign-value "CL_DEVICE_NAME" cl_device_info)))
(define device-opencl-c-version (device-info/string (foreign-value "CL_DEVICE_OPENCL_C_VERSION" cl_device_info)))
(define device-profile          (device-info/string (foreign-value "CL_DEVICE_PROFILE" cl_device_info)))
(define device-vendor           (device-info/string (foreign-value "CL_DEVICE_VENDOR" cl_device_info)))
(define device-version          (device-info/string (foreign-value "CL_DEVICE_VERSION" cl_device_info)))
(define driver-version          (device-info/string (foreign-value "CL_DRIVER_VERSION" cl_device_info)))

(define (device-info/size_t name)
  (lambda (device)
   (let-location ((value size_t 0))
     (status-check
      ((foreign-lambda* int ((cl_device_id device)
                             (cl_device_info name)
                             ((c-pointer int) dest))
                        "return(clGetDeviceInfo(*device, name, sizeof(dest), dest, NULL));")
       device name (location value))
      (conc "clGetDeviceInfo name=" name) 'device-info/size_t)
     value)))

(define device-max-work-group-size        (device-info/size_t (foreign-value "CL_DEVICE_MAX_WORK_GROUP_SIZE" int)))
(define device-max-parameter-size         (device-info/size_t (foreign-value "CL_DEVICE_MAX_PARAMETER_SIZE" int)))
(define device-profiling-timer-resolution (device-info/size_t (foreign-value "CL_DEVICE_PROFILING_TIMER_RESOLUTION" int)))
(define device-image2d-max-height         (device-info/size_t (foreign-value "CL_DEVICE_IMAGE2D_MAX_HEIGHT" int)))
(define device-image2d-max-width          (device-info/size_t (foreign-value "CL_DEVICE_IMAGE2D_MAX_WIDTH" int)))
(define device-image3d-max-depth          (device-info/size_t (foreign-value "CL_DEVICE_IMAGE3D_MAX_DEPTH" int)))
(define device-image3d-max-height         (device-info/size_t (foreign-value "CL_DEVICE_IMAGE3D_MAX_HEIGHT" int)))
(define device-image3d-max-width          (device-info/size_t (foreign-value "CL_DEVICE_IMAGE3D_MAX_WIDTH" int)))

(define (device-info/uint name)
  (lambda (device)
   (let-location ((value unsigned-int 0))
     (status-check
      ((foreign-lambda* int ((cl_device_id device)
                             (cl_device_info name)
                             ((c-pointer unsigned-int) dest))
                        "return(clGetDeviceInfo(*device, name, sizeof(dest), dest, NULL));")
       device name (location value))
      (conc "clGetDeviceInfo name=" name) 'device-info/uint)
     value)))

(define device-address-bits                  (device-info/uint (foreign-value "CL_DEVICE_ADDRESS_BITS" int)))
(define device-global-mem-cacheline-size     (device-info/uint (foreign-value "CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE" int)))
(define device-max-clock-frequency           (device-info/uint (foreign-value "CL_DEVICE_MAX_CLOCK_FREQUENCY" int)))
(define device-max-compute-units             (device-info/uint (foreign-value "CL_DEVICE_MAX_COMPUTE_UNITS" int)))
(define device-max-constant-args             (device-info/uint (foreign-value "CL_DEVICE_MAX_CONSTANT_ARGS" int)))
(define device-max-read-image-args           (device-info/uint (foreign-value "CL_DEVICE_MAX_READ_IMAGE_ARGS" int)))
(define device-max-samplers                  (device-info/uint (foreign-value "CL_DEVICE_MAX_SAMPLERS" int)))
(define device-max-work-item-dimensions      (device-info/uint (foreign-value "CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS" int)))
(define device-max-write-image-args          (device-info/uint (foreign-value "CL_DEVICE_MAX_WRITE_IMAGE_ARGS" int)))
(define device-mem-base-addr-align           (device-info/uint (foreign-value "CL_DEVICE_MEM_BASE_ADDR_ALIGN" int)))
(define device-min-data-type-align-size      (device-info/uint (foreign-value "CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE" int)))
(define device-native-vector-width-char      (device-info/uint (foreign-value "CL_DEVICE_NATIVE_VECTOR_WIDTH_CHAR" int)))
(define device-native-vector-width-short     (device-info/uint (foreign-value "CL_DEVICE_NATIVE_VECTOR_WIDTH_SHORT" int)))
(define device-native-vector-width-int       (device-info/uint (foreign-value "CL_DEVICE_NATIVE_VECTOR_WIDTH_INT" int)))
(define device-native-vector-width-long      (device-info/uint (foreign-value "CL_DEVICE_NATIVE_VECTOR_WIDTH_LONG" int)))
(define device-native-vector-width-float     (device-info/uint (foreign-value "CL_DEVICE_NATIVE_VECTOR_WIDTH_FLOAT" int)))
(define device-native-vector-width-double    (device-info/uint (foreign-value "CL_DEVICE_NATIVE_VECTOR_WIDTH_DOUBLE" int)))
(define device-native-vector-width-half      (device-info/uint (foreign-value "CL_DEVICE_NATIVE_VECTOR_WIDTH_HALF" int)))
(define device-preferred-vector-width-char   (device-info/uint (foreign-value "CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR" int)))
(define device-preferred-vector-width-short  (device-info/uint (foreign-value "CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT" int)))
(define device-preferred-vector-width-int    (device-info/uint (foreign-value "CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT" int)))
(define device-preferred-vector-width-long   (device-info/uint (foreign-value "CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG" int)))
(define device-preferred-vector-width-float  (device-info/uint (foreign-value "CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT" int)))
(define device-preferred-vector-width-double (device-info/uint (foreign-value "CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE" int)))
(define device-preferred-vector-width-half   (device-info/uint (foreign-value "CL_DEVICE_PREFERRED_VECTOR_WIDTH_HALF" int)))
(define device-vendor-id                     (device-info/uint (foreign-value "CL_DEVICE_VENDOR_ID" int)))

(define (device-info device)
  (map
   (lambda (pair)
     (cons (car pair) ((cdr pair) device)))
   `((device-address-bits . ,device-address-bits)
     (device-extensions . ,device-extensions)
     (device-global-mem-cacheline-size . ,device-global-mem-cacheline-size)
     (device-image2d-max-height . ,device-image2d-max-height)
     (device-image2d-max-width . ,device-image2d-max-width)
     (device-image3d-max-depth . ,device-image3d-max-depth)
     (device-image3d-max-height . ,device-image3d-max-height)
     (device-image3d-max-width . ,device-image3d-max-width)
     (device-max-clock-frequency . ,device-max-clock-frequency)
     (device-max-compute-units . ,device-max-compute-units)
     (device-max-constant-args . ,device-max-constant-args)
     (device-max-parameter-size . ,device-max-parameter-size)
     (device-max-read-image-args . ,device-max-read-image-args)
     (device-max-samplers . ,device-max-samplers)
     (device-max-work-group-size . ,device-max-work-group-size)
     (device-max-work-item-dimensions . ,device-max-work-item-dimensions)
     (device-max-write-image-args . ,device-max-write-image-args)
     (device-mem-base-addr-align . ,device-mem-base-addr-align)
     (device-min-data-type-align-size . ,device-min-data-type-align-size)
     (device-name . ,device-name)
     (device-native-vector-width-char . ,device-native-vector-width-char)
     (device-native-vector-width-double . ,device-native-vector-width-double)
     (device-native-vector-width-float . ,device-native-vector-width-float)
     (device-native-vector-width-half . ,device-native-vector-width-half)
     (device-native-vector-width-int . ,device-native-vector-width-int)
     (device-native-vector-width-long . ,device-native-vector-width-long)
     (device-native-vector-width-short . ,device-native-vector-width-short)
     (device-opencl-c-version . ,device-opencl-c-version)
     (device-preferred-vector-width-char . ,device-preferred-vector-width-char)
     (device-preferred-vector-width-double . ,device-preferred-vector-width-double)
     (device-preferred-vector-width-float . ,device-preferred-vector-width-float)
     (device-preferred-vector-width-half . ,device-preferred-vector-width-half)
     (device-preferred-vector-width-int . ,device-preferred-vector-width-int)
     (device-preferred-vector-width-long . ,device-preferred-vector-width-long)
     (device-preferred-vector-width-short . ,device-preferred-vector-width-short)
     (device-profile . ,device-profile)
     (device-profiling-timer-resolution . ,device-profiling-timer-resolution)
     (device-vendor . ,device-vendor)
     (device-vendor-id . ,device-vendor-id)
     (device-version . ,device-version)
     (driver-version . ,driver-version))))

;; ==================== context ====================

(define-record cl_context blob)
(define-foreign-type cl_context (c-pointer "cl_context")
  (lambda (x) (location (cl_context-blob x)))
  (lambda (x) (error "internal error: cannot return cl_context by value")))

(define (context-release! context)
  (print "RELEASEING " context)
  (status-check ((foreign-lambda* int ((cl_context context)) "return(clReleaseContext(*context));") context)
                "clReleaseContext" 'context-release!))

(define (context-create devices #!key (finalizer (lambda (x) (set-finalizer! x context-release!))))
  (let ((devices (if (pair? devices) devices (list devices)))) ;; list is optional
    (let* ((concatenated (list->u8vector (append-map (o u8vector->list cl_device-blob) devices)))
           (context (make-cl_context (make-u8vector (foreign-value "sizeof(cl_context)" int)))))
      (status-check
       ((foreign-lambda* int ((cl_context context) (u8vector device_list) (size_t num_devices))
                         "int status;"
                         "*context = clCreateContext(NULL, num_devices, (cl_device_id*)device_list, chicken_opencl_notify_cb, NULL, &status);"
                         "return(status);")
        context
        concatenated
        (length devices))
       "clCreateContext" 'context-create*)
      (finalizer context))))

;; ==================== command queue ====================

(define-record cl_command_queue blob)
(define-foreign-type cl_command_queue (c-pointer "cl_command_queue")
  (lambda (x) (location (cl_command_queue-blob x)))
  (lambda (x) (error "internal error: cannot return cl_command_queue by value")))


(define (command-queue-release! cq)
  (print "RELEASEING " cq)
  (status-check ((foreign-lambda* int ((cl_command_queue cq)) "return(clReleaseCommandQueue(*cq));") cq)
                "clReleaseCommandQueue" 'command-queue-release!))

(define (command-queue context device #!key (finalizer (lambda (x) (set-finalizer! x command-queue-release!)))) ;; TODO: properties
  (let* ((blob (make-u8vector (foreign-value "sizeof(cl_command_queue)" int)))
         (cq (make-cl_command_queue blob)))
    (status-check
     ((foreign-lambda* int ((cl_context context) (cl_device_id device) (cl_command_queue cq))
                       "int status;"
                       "*cq = clCreateCommandQueue(*context, *device, 0, &status);"
                       "return(status);")
      context device cq)
     "clCreateCommandQueue" 'command-queue)
    (finalizer cq)))

;; ==================== buffer ====================

(define-record cl_mem blob type)
(define-record-printer cl_mem
  (lambda (x op)
    (display "#<cl_mem " op)
    (write (mem-type x) op)
    (display " " op)
    (write (mem-size x) op)
    (display ">" op)))
(define-foreign-type cl_mem (c-pointer "cl_mem")
  (lambda (x) (location (cl_mem-blob x)))
  (lambda (x) (error "internal error: cannot return cl_mem by value")))

(define-foreign-type cl_mem_info int)

(define (mem-type/int->symbol type)
  (string->symbol
   ((foreign-lambda* c-string ((unsigned-int type)) "
if(type == CL_MEM_OBJECT_BUFFER)         return (\"buffer\");
if(type == CL_MEM_OBJECT_IMAGE2D)        return (\"image2d\");
if(type == CL_MEM_OBJECT_IMAGE3D)        return (\"image3d\");
#ifdef CL_VERSION_1_2
if(type == CL_MEM_OBJECT_IMAGE2D_ARRAY)  return (\"image2d-array\");
if(type == CL_MEM_OBJECT_IMAGE1D)        return (\"image1d\");
if(type == CL_MEM_OBJECT_IMAGE1D_ARRAY)  return (\"image1d-array\");
if(type == CL_MEM_OBJECT_IMAGE1D_BUFFER) return (\"image1d-buffer\");
#endif
#ifdef CL_VERSION_2_0
if(type == CL_MEM_OBJECT_PIPE)           return (\"pipe\");
#endif
") type)))

(define (mem-info/cl_uint mem name)
  (let-location ((type unsigned-int 0))
    (status-check ((foreign-lambda* int ((cl_mem mem)
                                         (cl_mem_info name)
                                         ((c-pointer unsigned-int) type))
                                    "return(clGetMemObjectInfo(*mem, name, sizeof(type), type, NULL));")
                   mem name (location type))
                  (conc "clGetMemObjectInfo name=" name) 'mem-info/cl_uint)
    type))

(define (mem-info/size_t mem name)
  (let-location ((size size_t 0))
    (status-check ((foreign-lambda* int ((cl_mem mem)
                                         (cl_mem_info name)
                                         ((c-pointer size_t) size))
                                    "return(clGetMemObjectInfo(*mem, name, sizeof(size), size, NULL));")
                   mem name (location size)))
    size))

;;(define (mem-type mem)   (mem-type/int->symbol (mem-info/cl_uint mem (foreign-value "CL_MEM_TYPE" int))))
(define mem-type cl_mem-type)
(define (mem-size mem)   (mem-info/size_t mem (foreign-value "CL_MEM_SIZE" int)))
(define (mem-offset mem) (mem-info/size_t mem (foreign-value "CL_MEM_OFFSET" int)))

;; size in bytes
(define (object-size x)
  (cond ((blob? x) (blob-size x))
        (( u8vector? x) (* 1 ( u8vector-length x)))
        (( s8vector? x) (* 1 ( s8vector-length x)))
        ((u16vector? x) (* 2 (u16vector-length x)))
        ((s16vector? x) (* 2 (s16vector-length x)))
        ((u32vector? x) (* 4 (u32vector-length x)))
        ((s32vector? x) (* 4 (s32vector-length x)))
        ((u64vector? x) (* 8 (u64vector-length x)))
        ((s64vector? x) (* 8 (s64vector-length x)))
        ((f32vector? x) (* 4 (f32vector-length x)))
        ((f64vector? x) (* 8 (f64vector-length x)))
        ((cl_mem?    x) (mem-size x))
        (else (error "cannot determine object size" x))))

(define (srfi4-vector-blob x) ;; any type that works with the foreign type scheme-pointer
  (cond ((blob? x) x)
        (( u8vector? x) ( u8vector->blob/shared x))
        (( s8vector? x) ( s8vector->blob/shared x))
        ((u16vector? x) (u16vector->blob/shared x))
        ((s16vector? x) (s16vector->blob/shared x))
        ((u32vector? x) (u32vector->blob/shared x))
        ((s32vector? x) (s32vector->blob/shared x))
        ((u64vector? x) (u64vector->blob/shared x))
        ((s64vector? x) (s64vector->blob/shared x))
        ((f32vector? x) (f32vector->blob/shared x))
        ((f64vector? x) (f64vector->blob/shared x))
        (else (error "cannot get blob from" x))))

(define (object-type x)
  (cond ((blob? x) 'blob)
        (( u8vector? x) 'u8)
        (( s8vector? x) 's8)
        ((u16vector? x) 'u16)
        ((s16vector? x) 's16)
        ((u32vector? x) 'u32)
        ((s32vector? x) 's32)
        ((u64vector? x) 'u64)
        ((s64vector? x) 's64)
        ((f32vector? x) 'f32)
        ((f64vector? x) 'f64)
        ((cl_mem?    x) (cl_mem-type x))
        (else (error "cannot determine type from" x))))

;; ((blob->type-converter 'f32) (f32vector->blob/shared (f32vector 1 2)))
(define (blob->type-converter x) ;; <-- a symbol like 'f32
  (case x
    ((blob) (lambda (x) x))
    (( u8) blob->u8vector/shared)
    (( s8) blob->s8vector/shared)
    ((u16) blob->u16vector/shared)
    ((s16) blob->s16vector/shared)
    ((u32) blob->u32vector/shared)
    ((s32) blob->s32vector/shared)
    ((u64) blob->u64vector/shared)
    ((s64) blob->s64vector/shared)
    ((f32) blob->f32vector/shared)
    ((f64) blob->f64vector/shared)
    (else (if (procedure? x) x
              (error "dont know how to convert type" x)))))

(define (mem-release! mem)
  (print "RELEASEING " mem)
  (status-check ((foreign-lambda* int ((cl_mem mem)) "return(clReleaseMemObject(*mem));") mem)
                "clReleaseMem" 'mem-release!))

(define (buffer-create context source/size #!key (flags 0) (type #f) (finalizer (lambda (x) (set-finalizer! x mem-release!))))
  (let* ((size (if (number? source/size) source/size
                   (object-size source/size)))
         (type (or type (if (number? source/size) 'blob
                            (object-type source/size))))
         (mem (make-cl_mem (make-u8vector (foreign-value "sizeof(cl_mem)" int)) type)))
    (status-check
     ((foreign-lambda* int ((cl_context context) (unsigned-long flags) (size_t size) (cl_mem mem))
                       "int status;"
                       "*mem = clCreateBuffer(*context, flags, size, NULL, &status);"
                       "return(status);")
      context flags size mem)
     "clCreateBuffer" 'buffer-create)
    (finalizer mem)))

(define buffer-size mem-size)
(define buffer-type
  (getter-with-setter cl_mem-type
                      (lambda (mem value) (cl_mem-type-set! mem value))))

(define (buffer-write buffer cq src #!key (offset 0))
  (status-check
   ((foreign-lambda* int ((cl_command_queue cq) (cl_mem buffer)
                          (scheme-pointer src) (size_t offset) (size_t size))
                     "return(clEnqueueWriteBuffer(*cq, *buffer, CL_TRUE, offset, size, src, 0, NULL, NULL));")
    cq buffer (srfi4-vector-blob src) offset (object-size src)))
  buffer)

(define (buffer-read buffer cq #!key (type (buffer-type buffer)) (dst #f) (offset 0) (size #f))
  (let* ((size (or size (if dst
                            (min (mem-size buffer) (object-size dst))
                            (mem-size buffer))))
         (dst  (or dst (make-blob size)))) ;; TODO: clear
   (status-check
    ((foreign-lambda* int ((cl_command_queue cq) (cl_mem buffer)
                           (scheme-pointer dst) (size_t offset) (size_t size))
                      "return(clEnqueueReadBuffer(*cq, *buffer, CL_TRUE, offset, size, dst, 0, NULL, NULL));")
     cq buffer (srfi4-vector-blob dst) offset (object-size dst)))

   ((blob->type-converter type) dst)))

;; ==================== program ====================

(define-record cl_program blob)
(define-record-printer program
  (lambda (x op)
    (display "#<program" op)
    ;; (write (program-size x) op)
    (display ">" op)))
(define-foreign-type cl_program (c-pointer "cl_program")
  (lambda (x) (location (cl_program-blob x)))
  (lambda (x) (error "internal error: cannot return cl_program by value")))

(define (program-release! program)
  (print "RELEASEING " program)
  (status-check ((foreign-lambda* int ((cl_program program)) "return(clReleaseProgram(*program));") program)
                "clReleaseProgram" 'program-release!))

(define (program-create context source #!key (finalizer (lambda (x) (set-finalizer! x program-release!))))
  (let ((program (make-cl_program (make-u8vector (foreign-value "sizeof(cl_program)" int)))))
    (status-check
     ((foreign-lambda* int ((cl_context context) (c-string source) (cl_program program))
                       "int status;"
                       "*program = clCreateProgramWithSource(*context, 1, (const char **)&source, NULL, &status);"
                       "return(status);")
      context source program)
     "clCreateProgramWithSource" 'program-create)
    (finalizer program)))

(define-foreign-type cl_program_build_info int)

(define (program-build-info/string name)
  (lambda (program device)
    ;;                               ,-- should be overwritten
    (let-location ((status int (foreign-value "CL_INVALID_VALUE" int)))
      (let ((value ((foreign-lambda* c-string* ((cl_program program)
                                                (cl_device_id device)
                                                (cl_program_build_info name)
                                                ((c-pointer int) status))
                                     "size_t value_size = 0;"
                                     "clGetProgramBuildInfo(*program, *device, name, 0, NULL, &value_size);"
                                     "char *dest = malloc(value_size);"
                                     "*status = clGetProgramBuildInfo(*program, *device, name, value_size, dest, NULL);"
                                     "return(dest); // <-- freed by c-string* foreign type")
                    program device name (location status))))
        (status-check status (conc "clGetPlatformInfo name=" name) 'program-build-info/string)
        value))))

(define program-build-log (program-build-info/string (foreign-value "CL_PROGRAM_BUILD_LOG" int)))
(define program-build-options (program-build-info/string (foreign-value "CL_PROGRAM_BUILD_OPTIONS" int)))

(define (program-build program devices)
  (if (pair? devices) (error "TODO: support multiple devices"))
  (let* ((device devices)
         (status ((foreign-lambda* int ((cl_program program) (u8vector devices))
                                   "return(clBuildProgram(*program, 1, (cl_device_id*)devices, NULL, NULL, NULL));")
                  program (cl_device-blob device))))

    (if (= status (foreign-value "CL_BUILD_PROGRAM_FAILURE" int))
        (error (conc "program-build: build-program-failure\n"
                     (program-build-log program device))))
    (status-check status "clBuildProgram" 'program-build)
    program))


;; ==================== kernel ====================

(define-record cl_kernel blob)
(define-foreign-type cl_kernel (c-pointer "cl_kernel")
  (lambda (x) (location (cl_kernel-blob x)))
  (lambda (x) (error "internal error: cannot return cl_kernel by value")))


(define (kernel-release! kernel)
  (print "RELEASEING " kernel)
  (status-check ((foreign-lambda* int ((cl_kernel kernel)) "return(clReleaseKernel(*kernel));") kernel)
                "clReleaseKernel" 'kernel-release!))

(define (kernel-create program name #!key (finalizer (lambda (x) (set-finalizer! x kernel-release!))))
  (let ((kernel (make-cl_kernel (make-u8vector (foreign-value "sizeof(cl_kernel)" int)))))
    (status-check
     ((foreign-lambda* int ((cl_program program) (c-string name) (cl_kernel kernel))
                       "int status;"
                       "*kernel = clCreateKernel(*program, name, &status);"
                       "return(status);")
      program name kernel)
     "clCreateKernel" 'program-build)
    (finalizer kernel)))

(define (kernel-arg-set! kernel idx value)
  ;; TODO: support non-mem types
  (status-check
   ((foreign-lambda* int ((cl_kernel kernel) (unsigned-byte idx) (size_t size) (cl_mem mem))
                     "return(clSetKernelArg(*kernel, idx, size, mem));")
    kernel idx (foreign-value "sizeof(cl_mem)" int) value)
   "clSetKernelArg" 'kernel-arg))

(define (kernel-enqueue kernel cq global-work-sizes #!key global-work-offsets local-work-sizes)

  (define (->size_t-vector lst)
    (cond ((equal? lst #f) #f) ;; NULL is ok
          ((list? lst)
           (cond ((= 8 (foreign-value "sizeof(size_t)" int)) (list->u64vector lst))
                 ((= 4 (foreign-value "sizeof(size_t)" int)) (list->u32vector lst))
                 (else (error "only 4 or 8 bytes supported for size_t, got:" (foreign-value "sizeof(size_t)" int)))))
          (error "expecting proper list of dimension, got " lst)))

  (if (and local-work-sizes (not (= (length global-work-sizes)
                                    (length local-work-sizes))))
      (error "local-work-sizes dimensions must match global-work-sizes" local-work-sizes))

  (if (and global-work-offsets (not (= (length global-work-sizes)
                                       (length global-work-offsets))))
      (error "global-work-offsets dimensions must match global-work-sizes" global-work-sizes))

  (status-check
   ((foreign-lambda* int ((cl_command_queue cq) (cl_kernel kernel)
                          (int work_dim) (u64vector gwo) (u64vector gws) (u64vector lws))
                     "return(clEnqueueNDRangeKernel(*cq, *kernel, work_dim, gwo, gws, lws, 0, NULL, NULL));")
    cq kernel
    (length global-work-sizes) ;; dimensions
    (->size_t-vector global-work-offsets)
    (->size_t-vector global-work-sizes)
    (->size_t-vector local-work-sizes))
   "clEnqueueNDRangeKernel" 'kernel-enqueue))

;; ========================================================================================

