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
#define CL_TARGET_OPENCL_VERSION 110

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
  (unless (= (* groupsize groups) (u8vector-length v))
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

(define (platform-devices platform #!optional (device-type 'all))

  (define device_type (case device-type
                        ((gpu) (foreign-value "CL_DEVICE_TYPE_GPU" int))
                        ((cpu) (foreign-value "CL_DEVICE_TYPE_CPU" int))
                        ((all) (foreign-value "CL_DEVICE_TYPE_ALL" int))
                        ((default) (foreign-value "CL_DEVICE_TYPE_DEFAULT" int))
                        ((accelerator) (foreign-value "CL_DEVICE_TYPE_ACCELERATOR" int))
                        (else (error "unknown device-type" device-type))))

  (define num-devices
    ((foreign-lambda* size_t ((cl_platform_id platform) (int device_type))
                      "cl_uint num_devices = 0;"
                      "clGetDeviceIDs(*platform, device_type, 0, NULL, &num_devices);"
                      "return(num_devices);")
     platform device_type))

  (define size_cl_device_id (foreign-value "sizeof(cl_device_id)" size_t))

  (let ((blob (make-u8vector (* size_cl_device_id num-devices))))
    ((foreign-lambda* size_t ((cl_platform_id platform)
                              (u8vector blob)
                              (int device_type)
                              (size_t num_devices))
                      "clGetDeviceIDs(*platform, device_type, num_devices, (cl_device_id*)blob, 0);"
                      "return(num_devices);")
     platform blob device_type num-devices)
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
  (when (cl_context-blob context)
    (status-check ((foreign-lambda* int ((cl_context context))
                                    "return(clReleaseContext(*context));")
                   context)
                  "clReleaseContext" 'context-release!)
    (cl_context-blob-set! context #f)))

(define (context-allocate)
  (make-cl_context (make-u8vector (foreign-value "sizeof(cl_context)" int))))

(define (context-create devices #!key (finalizer (lambda (x) (set-finalizer! x context-release!))))
  (let ((devices (if (pair? devices) devices (list devices)))) ;; list is optional
    (let* ((concatenated (list->u8vector (append-map (o u8vector->list cl_device-blob) devices)))
           (context (context-allocate)))
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

;; ==================== events ====================

(define-record cl_event blob)
(define-record-printer cl_event
  (lambda (x op)
    (display "#<cl_event " op)
    (write (event-command-type x) op)
    (display " " op)
    (write (event-status x) op)
    (display ">" op)))
(define-foreign-type cl_event (c-pointer "cl_event")
  (lambda (x) (and x (location (cl_event-blob x))))
  (lambda (x) (error "internal error: cannot return cl_event by value")))


(define (event-release! event)
  (when (cl_event-blob event)
    (status-check ((foreign-lambda* int ((cl_event event)) "return(clReleaseEvent(*event));") event)
                  "clReleaseEvent" 'event-release!)
    (cl_event-blob-set! event #f)))

(define (event-allocate)
  (make-cl_event (make-string (foreign-value "sizeof(cl_event)" int) #\null)))

(define (event-create context)
  (let ((event (event-allocate)))
    ((foreign-lambda* int ((cl_context context) (cl_event event))
                      "cl_int status;"
                      "*event = clCreateUserEvent(*context, &status);"
                      "return(status);")
    context event)
   event))

(define (event-info/int name)
  (lambda (event)
    (let-location ((value int 0))
      (status-check
       ((foreign-lambda* int ((cl_event event) (int name) ((c-pointer int) value))
                         "return(clGetEventInfo(*event, name, sizeof(int), value, NULL));")
        event name (location value))
       "clGetEventInfo" 'event-info/int)
      value)))

(define event-command-type* (event-info/int (foreign-value "CL_EVENT_COMMAND_TYPE" int)))
(define event-status* (event-info/int (foreign-value "CL_EVENT_COMMAND_EXECUTION_STATUS" int)))

(define (event-command-type event)
  (let ((s (event-command-type* event)))
    (cond ((= s (foreign-value "CL_COMMAND_NDRANGE_KERNEL" int))       'ndrange-kernel)
          ((= s (foreign-value "CL_COMMAND_TASK" int))                 'task)
          ((= s (foreign-value "CL_COMMAND_NATIVE_KERNEL" int))        'native-kernel)
          ((= s (foreign-value "CL_COMMAND_READ_BUFFER" int))          'read-buffer)
          ((= s (foreign-value "CL_COMMAND_WRITE_BUFFER" int))         'write-buffer)
          ((= s (foreign-value "CL_COMMAND_COPY_BUFFER" int))          'copy-buffer)
          ((= s (foreign-value "CL_COMMAND_READ_IMAGE" int))           'read-image)
          ((= s (foreign-value "CL_COMMAND_WRITE_IMAGE" int))          'write-image)
          ((= s (foreign-value "CL_COMMAND_COPY_IMAGE" int))           'copy-image)
          ((= s (foreign-value "CL_COMMAND_COPY_BUFFER_TO_IMAGE" int)) 'copy-buffer-to-image)
          ((= s (foreign-value "CL_COMMAND_COPY_IMAGE_TO_BUFFER" int)) 'copy-image-to-buffer)
          ((= s (foreign-value "CL_COMMAND_MAP_BUFFER" int))           'map-buffer)
          ((= s (foreign-value "CL_COMMAND_MAP_IMAGE" int))            'map-image)
          ((= s (foreign-value "CL_COMMAND_UNMAP_MEM_OBJECT" int))     'unmap-mem-object)
          ((= s (foreign-value "CL_COMMAND_MARKER" int))               'marker)
          ((= s (foreign-value "CL_COMMAND_ACQUIRE_GL_OBJECTS" int))   'acquire-gl-objects)
          ((= s (foreign-value "CL_COMMAND_RELEASE_GL_OBJECTS" int))   'release-gl-objects)
          ((= s (foreign-value "CL_COMMAND_READ_BUFFER_RECT" int))     'read-buffer-rect)
          ((= s (foreign-value "CL_COMMAND_WRITE_BUFFER_RECT" int))    'write-buffer-rect)
          ((= s (foreign-value "CL_COMMAND_COPY_BUFFER_RECT" int))     'copy-buffer-rect)
          ((= s (foreign-value "CL_COMMAND_USER" int))                 'user)
          (else s))))

(define (event-status event)
  (let ((s (event-status* event)))
    (cond ((= s (foreign-value "CL_QUEUED" int))    'queued)
          ((= s (foreign-value "CL_SUBMITTED" int)) 'submitted)
          ((= s (foreign-value "CL_RUNNING" int))   'running)
          ((= s (foreign-value "CL_COMPLETE" int))  'complete)
          (else s))))

;; complete, or a negative integer to cancel all jobs waiting for this user event
(define (event-complete! event #!optional (status (foreign-value "CL_COMPLETE" int)))
  (status-check
   ((foreign-lambda* int ((cl_event event) (int status))
                     "return(clSetUserEventStatus(*event, status));")
    event status)
   "clSetUserEventStatus" 'event-complete!))

;; ==================== command queue ====================

(define-record cl_command_queue blob context)
(define-foreign-type cl_command_queue (c-pointer "cl_command_queue")
  (lambda (x) (location (cl_command_queue-blob x)))
  (lambda (x) (error "internal error: cannot return cl_command_queue by value")))


(define (command-queue-release! cq)
  (when (cl_command_queue-blob cq)
    (status-check ((foreign-lambda* int ((cl_command_queue cq)) "return(clReleaseCommandQueue(*cq));") cq)
                  "clReleaseCommandQueue" 'command-queue-release!)
    (cl_command_queue-blob-set! cq #f)))

(define (command-queue-create context device
                       #!key out-of-order profile
                       (finalizer (lambda (x) (set-finalizer! x command-queue-release!))))
  (let* ((properties (+ (if out-of-order (foreign-value "CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE" int) 0)
                        (if profile (foreign-value "CL_QUEUE_PROFILING_ENABLE" int) 0)))
         (blob (make-u8vector (foreign-value "sizeof(cl_command_queue)" int)))
         (cq (make-cl_command_queue blob context)))
    (status-check
     ((foreign-lambda* int ((cl_context context) (cl_device_id device) (int properties) (cl_command_queue cq))
                       "int status;"
                       "*cq = clCreateCommandQueue(*context, *device, properties, &status);"
                       "return(status);")
      context device properties cq)
     "clCreateCommandQueue" 'command-queue)
    (finalizer cq)))

;; we could use clGetCommandQueueInfo here, but that would make things
;; complicated with the GC and finalizers. it'd have to return a new
;; context with a finalizer, and probaly increasing the OpenCL context
;; refcount. this is much simpler.
(define (command-queue-context cq) (cl_command_queue-context cq))

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
  (when (cl_mem-blob mem)
    (status-check ((foreign-lambda* int ((cl_mem mem)) "return(clReleaseMemObject(*mem));") mem)
                  "clReleaseMem" 'mem-release!)
    (cl_mem-blob-set! mem #f)))

(define (mem-allocate type)
  (make-cl_mem (make-u8vector (foreign-value "sizeof(cl_mem)" int)) type))

(define (buffer-create* context size type flags finalizer)
  (let* ((mem (mem-allocate type)))
    (status-check
     ((foreign-lambda* int ((cl_context context) (unsigned-long flags) (size_t size) (cl_mem mem))
                       "int status;"
                       "*mem = clCreateBuffer(*context, flags, size, NULL, &status);"
                       "return(status);")
      context flags size mem)
     "clCreateBuffer" 'buffer-create)
    (finalizer mem)))

(define (buffer-create cq/context source/size
                       #!key (flags 0) (type #f)
                       (finalizer (lambda (x) (set-finalizer! x mem-release!))))
  (if (cl_command_queue? cq/context)
      (let* ((source source/size)
             (cq cq/context)
             (context (command-queue-context cq))
             (buffer (buffer-create* context
                                     (if (integer? source)
                                         source
                                         (object-size source))
                                     (or type
                                         (if (integer? source)
                                             'blob
                                             (object-type source)))
                                     flags finalizer)))
        (cond ((cl_mem? source) (error 'buffer-create "TODO: buffer-copy on source" source))
              ((integer? source) buffer) ;; you could just supply a context for this
              (else (buffer-write buffer cq source))))
      (let ()
        (unless (integer? source/size)
          (error 'buffer-create "invalid buffer size (try giving command-queue instead of context)" source/size))
        (buffer-create* cq/context source/size (or type 'blob) flags finalizer))))

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
  (when (cl_program-blob program)
    (status-check ((foreign-lambda* int ((cl_program program))
                                    "return(clReleaseProgram(*program));")
                   program)
                  "clReleaseProgram" 'program-release!)
    (cl_program-blob-set! program #f)))

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
  (when (cl_kernel-blob kernel)
    (status-check ((foreign-lambda* int ((cl_kernel kernel)) "return(clReleaseKernel(*kernel));") kernel)
                  "clReleaseKernel" 'kernel-release!)
    (cl_kernel-blob-set! kernel #f)))

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

;; value can be buffers or a srfi-4 vector of 1 element (so we can
;; distinguish between uchar and uint, for example)
(define (kernel-arg-set! kernel idx value)
  ;; TODO: support size_t kernel arguments (32 or 64 bit, depending on system)?
  (status-check
   (let-syntax ((fl (er-macro-transformer
                     (lambda (x r t)
                       (let ((foreign-type (cadr x))
                             (c-type       (caddr x))
                             (elements     (cadddr x)))
                         `((foreign-lambda* int ((cl_kernel kernel)
                                                 (unsigned-integer idx)
                                                 (byte elements)
                                                 (,foreign-type value))
                                            "return(clSetKernelArg(*kernel, idx, elements*sizeof(" ,c-type "), value));")
                           kernel idx ,elements value))))))
     (cond ((cl_mem?    value) (fl    cl_mem "cl_mem"               1            ))
           (( u8vector? value) (fl  u8vector "cl_uchar"  ( u8vector-length value)))
           (( s8vector? value) (fl  s8vector "cl_char"   ( s8vector-length value)))
           ((u16vector? value) (fl u16vector "cl_short"  (u16vector-length value)))
           ((s16vector? value) (fl s16vector "cl_short"  (s16vector-length value)))
           ((u32vector? value) (fl u32vector "cl_int"    (u32vector-length value)))
           ((s32vector? value) (fl s32vector "cl_int"    (s32vector-length value)))
           ((u64vector? value) (fl u64vector "cl_long"   (u64vector-length value)))
           ((s64vector? value) (fl s64vector "cl_long"   (s64vector-length value)))
           ((f32vector? value) (fl f32vector "cl_float"  (f32vector-length value)))
           ((f64vector? value) (fl f64vector "cl_double" (f64vector-length value)))
           (else (error "unsupported kernel argument type (try srfi4 vectors or buffers)" value))))
   "clSetKernelArg" 'kernel-arg))

(define (kernel-enqueue kernel cq global-work-sizes #!key event wait global-work-offsets local-work-sizes)

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

  (let ((event (if (equal? event #t) ;; #f for no events, #t to create one, (event-allocate) to reuse existing
                   (event-allocate)
                   event)))
    (status-check
     ((foreign-lambda* int ((cl_command_queue cq) (cl_kernel kernel)
                            (int work_dim) (u64vector gwo) (u64vector gws) (u64vector lws)
                            (unsigned-int num_waitlist) (scheme-pointer waitlist)
                            (cl_event event))
                       "return(clEnqueueNDRangeKernel(*cq, *kernel, work_dim, gwo, gws, lws,
                                                      num_waitlist, (cl_event*)waitlist, event));")
      cq kernel
      (length global-work-sizes) ;; dimensions
      (->size_t-vector global-work-offsets)
      (->size_t-vector global-work-sizes)
      (->size_t-vector local-work-sizes)
      (if wait (length wait) 0) (and wait (apply conc (map cl_event-blob wait)))
      event)
     "clEnqueueNDRangeKernel" 'kernel-enqueue)
    event))
