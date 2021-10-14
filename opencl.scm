(import chicken.base
        chicken.foreign
        chicken.pretty-print
        chicken.memory
        (only chicken.string conc)
        srfi-4
        srfi-1)

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

(foreign-declare "
// TODO: don't hardcode this
#define CL_TARGET_OPENCL_VERSION 120

#ifdef __APPLE__
#include <OpenCL/cl.h>
#else
 #include <CL/cl.h>
#endif
")

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
    (display (platform-name x) op)
    (display ">" op)))
(define-foreign-type cl_platform_id (c-pointer "cl_platform_id")
  (lambda (x) (location (platform-blob x)))
  (lambda (x) (error "internal error: cannot return cl_platform_id by value")))

(define-foreign-type cl_platform_info int)

(define size_cl_platform_id (foreign-value "sizeof(cl_platform_id *)" int))

(define (status-check ret #!optional msg location)
  (unless (= ret (foreign-value "CL_SUCCESS" int))
    (error location msg (cond ((assoc ret cl-errors) => cdr)
                                      (else ret)))))

(define (platforms)
  (let* ((count (platforms%))
         (blob (make-u8vector (* count size_cl_platform_id) 0)))

    ;; fill blob with all platforms
    (status-check
     ((foreign-lambda* int ((int num_platforms) (u8vector dest))
                       "return (clGetPlatformIDs(num_platforms, (cl_platform_id *)dest, NULL));")
      count blob)
     "clGetPlatformIDs" 'platforms)

    ;; go from one big blob into one platform per blob
    (list-tabulate count
                   (lambda (idx)
                     (let ((blob1 (make-u8vector size_cl_platform_id)))
                       (move-memory! blob blob1 size_cl_platform_id (* size_cl_platform_id idx))
                       (make-platform blob1))))))

(define (platform-info platform name)
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

(define (platform-profile p)    (platform-info p (foreign-value "CL_PLATFORM_PROFILE"    cl_platform_info)))
(define (platform-version p)    (platform-info p (foreign-value "CL_PLATFORM_VERSION"    cl_platform_info)))
(define (platform-name p)       (platform-info p (foreign-value "CL_PLATFORM_NAME"       cl_platform_info)))
(define (platform-vendor p)     (platform-info p (foreign-value "CL_PLATFORM_VENDOR"     cl_platform_info)))
(define (platform-extensions p) (platform-info p (foreign-value "CL_PLATFORM_EXTENSIONS" cl_platform_info)))

(pp (platforms))
