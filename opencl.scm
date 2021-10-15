(import chicken.base
        chicken.foreign
        chicken.pretty-print
        (only chicken.gc set-finalizer!)
        (only chicken.string conc)
        (only chicken.blob make-blob)
        (only chicken.memory.representation number-of-bytes)
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
    (,(foreign-value "CL_INVALID_VALUE" int) .                   invalid_value)
    (,(foreign-value "CL_INVALID_DEVICE_TYPE" int) .             invalid_device_type)
    (,(foreign-value "CL_INVALID_PLATFORM" int) .                invalid_platform)
    (,(foreign-value "CL_INVALID_DEVICE" int) .                  invalid_device)
    (,(foreign-value "CL_INVALID_CONTEXT" int) .                 invalid_context)
    (,(foreign-value "CL_INVALID_QUEUE_PROPERTIES" int) .        invalid_queue_properties)
    (,(foreign-value "CL_INVALID_COMMAND_QUEUE" int) .           invalid_command_queue)
    (,(foreign-value "CL_INVALID_HOST_PTR" int) .                invalid_host_ptr)
    (,(foreign-value "CL_INVALID_MEM_OBJECT" int) .              invalid_mem_object)
    (,(foreign-value "CL_INVALID_IMAGE_FORMAT_DESCRIPTOR" int) . invalid_image_format_descriptor)
    (,(foreign-value "CL_INVALID_IMAGE_SIZE" int) .              invalid_image_size)
    (,(foreign-value "CL_INVALID_SAMPLER" int) .                 invalid_sampler)
    (,(foreign-value "CL_INVALID_BINARY" int) .                  invalid_binary)
    (,(foreign-value "CL_INVALID_BUILD_OPTIONS" int) .           invalid_build_options)
    (,(foreign-value "CL_INVALID_PROGRAM" int) .                 invalid_program)
    (,(foreign-value "CL_INVALID_PROGRAM_EXECUTABLE" int) .      invalid_program_executable)
    (,(foreign-value "CL_INVALID_KERNEL_NAME" int) .             invalid_kernel_name)
    (,(foreign-value "CL_INVALID_KERNEL_DEFINITION" int) .       invalid_kernel_definition)
    (,(foreign-value "CL_INVALID_KERNEL" int) .                  invalid_kernel)
    (,(foreign-value "CL_INVALID_ARG_INDEX" int) .               invalid_arg_index)
    (,(foreign-value "CL_INVALID_ARG_VALUE" int) .               invalid_arg_value)
    (,(foreign-value "CL_INVALID_ARG_SIZE" int) .                invalid_arg_size)
    (,(foreign-value "CL_INVALID_KERNEL_ARGS" int) .             invalid_kernel_args)
    (,(foreign-value "CL_INVALID_WORK_DIMENSION" int) .          invalid_work_dimension)
    (,(foreign-value "CL_INVALID_WORK_GROUP_SIZE" int) .         invalid_work_group_size)
    (,(foreign-value "CL_INVALID_WORK_ITEM_SIZE" int) .          invalid_work_item_size)
    (,(foreign-value "CL_INVALID_GLOBAL_OFFSET" int) .           invalid_global_offset)
    (,(foreign-value "CL_INVALID_EVENT_WAIT_LIST" int) .         invalid_event_wait_list)
    (,(foreign-value "CL_INVALID_EVENT" int) .                   invalid_event)
    (,(foreign-value "CL_INVALID_OPERATION" int) .               invalid_operation)
    (,(foreign-value "CL_INVALID_GL_OBJECT" int) .               invalid_gl_object)
    (,(foreign-value "CL_INVALID_BUFFER_SIZE" int) .             invalid_buffer_size)
    (,(foreign-value "CL_INVALID_MIP_LEVEL" int) .               invalid_mip_level)
    (,(foreign-value "CL_INVALID_GLOBAL_WORK_SIZE" int) .        invalid_global_work_size)
    ))

;; ==================== platforms ====================
(define (platforms%)
  ((foreign-lambda* unsigned-int () "
 cl_uint num_platforms;
 clGetPlatformIDs(0, NULL, &num_platforms);
 return (num_platforms);
")))

(define-record platform blob)
(define-record-printer platform
  (lambda (x op)
    (display "#<platform " op)
    (write (platform-name x) op)
    (display ">" op)))
(define-foreign-type cl_platform_id (c-pointer "cl_platform_id")
  (lambda (x) (location (platform-blob x)))
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
    (map make-platform (u8vector-split blob num-platforms size_cl_platform_id))))

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

(define-record device blob)
(define-record-printer device
  (lambda (x op)
    (display "#<device " op)
    (write (device-name x) op)
    (display ">" op)))
(define-foreign-type cl_device_id (c-pointer "cl_device_id")
  (lambda (x) (location (device-blob x)))
  (lambda (x) (error "internal error: cannot return cl_device_id by value")))

(define (devices platform)
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
    (map make-device (u8vector-split blob num-devices size_cl_device_id))))

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

(define-record context blob)
(define-record-printer context
  (lambda (x op)
    (display "#<context" op)
    ;; (write (context-name x) op)
    (display ">" op)))
(define-foreign-type cl_context (c-pointer "cl_context")
  (lambda (x) (location (context-blob x)))
  (lambda (x) (error "internal error: cannot return cl_context by value")))

(define (context-release! context)
  (print "RELEASEING " context)
  (status-check ((foreign-lambda* int ((cl_context context)) "return(clReleaseContext(*context));") context)
                "clReleaseContext" 'context-release!))

(define (context-create devices #!key (finalizer (lambda (x) (set-finalizer! x context-release!))))
  (let ((devices (if (pair? devices) devices (list devices)))) ;; list is optional
    (let* ((concatenated (list->u8vector (append-map (o u8vector->list device-blob) devices)))
           (blob (make-u8vector (foreign-value "sizeof(cl_context)" int)))
           (context (make-context blob)))
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

(define-record command-queue blob)
(define-record-printer command-queue
  (lambda (x op)
    (display "#<command-queue" op)
    ;; (write (command-queue-name x) op)
    (display ">" op)))
(define-foreign-type cl_command_queue (c-pointer "cl_command_queue")
  (lambda (x) (location (command-queue-blob x)))
  (lambda (x) (error "internal error: cannot return cl_command_queue by value")))


(define (command-queue-release! cq)
  (print "RELEASEING " cq)
  (status-check ((foreign-lambda* int ((cl_command_queue cq)) "return(clReleaseCommandQueue(*cq));") cq)
                "clReleaseCommandQueue" 'command-queue-release!))

(define (command-queue context device #!key (finalizer (lambda (x) (set-finalizer! x command-queue-release!)))) ;; TODO: properties
  (let* ((blob (make-u8vector (foreign-value "sizeof(cl_command_queue)" int)))
         (cq (make-command-queue blob)))
    (status-check
     ((foreign-lambda* int ((cl_context context) (cl_device_id device) (cl_command_queue cq))
                       "int status;"
                       "*cq = clCreateCommandQueue(*context, *device, 0, &status);"
                       "return(status);")
      context device cq)
     "clCreateCommandQueue" 'command-queue)
    (finalizer cq)))

;; ==================== buffer ====================

(define-record mem blob)
(define-record-printer mem
  (lambda (x op)
    (display "#<mem " op)
    (write (mem-type x) op)
    (display " " op)
    (write (mem-size x) op)
    (display ">" op)))
(define-foreign-type cl_mem (c-pointer "cl_mem")
  (lambda (x) (location (mem-blob x)))
  (lambda (x) (error "internal error: cannot return cl_mem by value")))

(define (mem-release! mem)
  (print "RELEASEING " mem)
  (status-check ((foreign-lambda* int ((cl_mem mem)) "return(clReleaseMemObject(*mem));") mem)
                "clReleaseMem" 'mem-release!))

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

(define (mem-type mem)   (mem-type/int->symbol (mem-info/cl_uint mem (foreign-value "CL_MEM_TYPE" int))))
(define (mem-size mem)   (mem-info/size_t mem (foreign-value "CL_MEM_SIZE" int)))
(define (mem-offset mem) (mem-info/size_t mem (foreign-value "CL_MEM_OFFSET" int)))

;; size in bytes
(define (srfi4-vector-bytes v)
  (u8vector-length v))

(define (buffer-create context flags source/size #!key (finalizer (lambda (x) (set-finalizer! x mem-release!))))
  (let* ((size (if (number? source/size) source/size
                   (srfi4-vector-bytes source/size)))
         (blob (make-u8vector (foreign-value "sizeof(cl_mem)" int)))
         (mem (make-mem blob)))
    (status-check
     ((foreign-lambda* int ((cl_context context) (unsigned-long flags) (size_t size) (cl_mem mem))
                       "int status;"
                       "*mem = clCreateBuffer(*context, flags, size, NULL, &status);"
                       "return(status);")
      context flags size mem)
     "clCreateBuffer" 'buffer-create)
    (finalizer mem)))

(define (enqueue-buffer-write cq buffer src #!key (blocking? #t) (offset 0))
  (status-check
   ((foreign-lambda* int ((cl_command_queue cq) (cl_mem buffer)
                          (scheme-pointer src) (size_t offset) (size_t size)
                          (bool blocking))
                     "return(clEnqueueWriteBuffer(*cq, *buffer, blocking ? CL_TRUE : CL_FALSE, offset, size, src, 0, NULL, NULL));")
    cq buffer src offset
    (number-of-bytes src)
    blocking?)))

(define (enqueue-buffer-read cq buffer #!key (dst #f) (blocking? #t) (offset 0) (size #f))
  (let* ((size (or size (if dst
                            (min (mem-size buffer) (srfi4-vector-bytes dst))
                            (mem-size buffer))))
         (dst  (or dst  (make-blob size)))) ;; TODO: clear
   (status-check
    ((foreign-lambda* int ((cl_command_queue cq) (cl_mem buffer)
                           (scheme-pointer dst) (size_t offset) (size_t size)
                           (bool blocking))
                      "return(clEnqueueReadBuffer(*cq, *buffer, blocking ? CL_TRUE : CL_FALSE, offset, size, dst, 0, NULL, NULL));")
     cq buffer dst offset
     (number-of-bytes dst)
     blocking?))
   dst))

;; ==================== program ====================

(define-record program blob)
(define-record-printer program
  (lambda (x op)
    (display "#<program" op)
    ;; (write (program-size x) op)
    (display ">" op)))
(define-foreign-type cl_program (c-pointer "cl_program")
  (lambda (x) (location (program-blob x)))
  (lambda (x) (error "internal error: cannot return cl_program by value")))


(define (program-release! program)
  (print "RELEASEING " program)
  (status-check ((foreign-lambda* int ((cl_program program)) "return(clReleaseProgram(*program));") program)
                "clReleaseProgram" 'program-release!))

(define (program-create context source #!key (finalizer (lambda (x) (set-finalizer! x program-release!))))
  (let ((program (make-program (make-u8vector (foreign-value "sizeof(cl_program)" int)))))
    (status-check
     ((foreign-lambda* int ((cl_context context) (c-string source) (cl_program program))
                       "int status;"
                       "*program = clCreateProgramWithSource(*context, 1, (const char **)&source, NULL, &status);"
                       "return(status);")
      context source program)
     "clCreateProgramWithSource" 'program-create)
    (finalizer program)))

(define (program-build program devices)
  (if (pair? devices) (error "TODO: support multiple devices"))
  (let ((device devices))
    (status-check
     ((foreign-lambda* int ((cl_program program) (u8vector devices))
                       "return(clBuildProgram(*program, 1, (cl_device_id*)devices, NULL, NULL, NULL));")
      program (device-blob device))
     "clBuildProgram" 'program-build)))

;; ==================== kernel ====================

(define-record kernel blob)
(define-record-printer kernel
  (lambda (x op)
    (display "#<kernel" op)
    ;; (write (kernel-size x) op)
    (display ">" op)))
(define-foreign-type cl_kernel (c-pointer "cl_kernel")
  (lambda (x) (location (kernel-blob x)))
  (lambda (x) (error "internal error: cannot return cl_kernel by value")))


(define (kernel-release! kernel)
  (print "RELEASEING " kernel)
  (status-check ((foreign-lambda* int ((cl_kernel kernel)) "return(clReleaseKernel(*kernel));") kernel)
                "clReleaseKernel" 'kernel-release!))

(define (kernel-create program name #!key (finalizer (lambda (x) (set-finalizer! x kernel-release!))))
  (let ((kernel (make-kernel (make-u8vector (foreign-value "sizeof(cl_kernel)" int)))))
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


;; cl_int clEnqueueNDRangeKernel (	cl_command_queue command_queue,
;;  	cl_kernel kernel,
;;  	cl_uint work_dim,
;;  	const size_t *global_work_offset,
;;  	const size_t *global_work_size,
;;  	const size_t *local_work_size,
;;  	cl_uint num_events_in_wait_list,
;;  	const cl_event *event_wait_list,
;;  	cl_event *event)
(define (kernel-enqueue kernel cq #!key global-work-offsets global-work-sizes local-work-sizes)
  (unless (= 8 (foreign-value "sizeof(size_t)" int))
    (error "TODO: implement non-8-byte size_t:" (foreign-value "sizeof(size_t)" int) ))
  (status-check
   ((foreign-lambda* int ((cl_command_queue cq) (cl_kernel kernel)
                          (int work_dim) (u64vector gwo) (u64vector gws) (u64vector lws))
                     "return(clEnqueueNDRangeKernel(*cq, *kernel, work_dim, gwo, gws, lws, 0, NULL, NULL));")
    cq kernel
    (u64vector-length global-work-sizes) ;; dimensions
    global-work-offsets
    global-work-sizes
    local-work-sizes)
   "clEnqueueNDRangeKernel" 'kernel-enqueue))

;; ========================================================================================
(print "all devices:")
(pp (map device-info (append-map devices (platforms))))

(define device (car (devices (cadr (platforms)))))
(print "got device: " device)
(define context (context-create device))
(print "got context: " context)

(define cq (command-queue context device))
(print "cq: " cq)

(define A (buffer-create context 0 (* 8)))
(print "mem " A)

(enqueue-buffer-write cq A #${1 2 3 4 10 20 30 40})
(print "enqueued")
(print "read-back: " (blob->u8vector/shared (enqueue-buffer-read cq A)))

(define program-source "
__kernel void tst(__global uchar *A) {
    int index = get_global_id(0);
    A[index] *= 2;
}")
(print "program source: " program-source)

(define program (program-create context program-source))
(print "program: " program)

(program-build program device)

(define kernel (kernel-create program "tst"))
(print "kernel: " kernel)
(kernel-arg-set! kernel 0 A)

(kernel-enqueue kernel cq global-work-sizes: (u64vector 8))

(print "read-back: " (blob->u8vector/shared (enqueue-buffer-read cq A)))

(set! A #f)
(set! kernel #f)
(set! program #f)
(set! cq #f)
(set! context #f)
