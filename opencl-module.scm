(module opencl (
                buffer-create buffer-release!
                buffer-read buffer-size buffer-type buffer-write

                cl_command-queue cl_command-queue?
                cl_command-queue-blob cl_command-queue-blob-set!
                cl_command-queue-context cl_command-queue-context-set!

                cl_context      cl_context?
                cl_context-blob cl_context-blob-set!

                cl_device      cl_device?
                cl_device-blob cl_device-blob-set!

                cl-errors

                cl_event      cl_event?
                cl_event-blob cl_event-blob-set!

                cl_kernel      cl_kernel?
                cl_kernel-blob cl_kernel-blob-set!

                cl_mem      cl_mem?
                cl_mem-blob cl_mem-blob-set!
                cl_mem-type cl_mem-type-set!

                cl_platform cl_platform?
                cl_platform-blob cl_platform-blob-set!
                cl_program cl_program?
                cl_program-blob cl_program-blob-set!

                command-queue-context
                command-queue-create command-queue-release!

                ;; context-allocate
                context-create context-release!

                ;; device info
                device-info device-info/size_t device-info/string device-info/uint
                device-address-bits
                device-extensions                   device-global-mem-cacheline-size
                device-image2d-max-height           device-image2d-max-width
                device-image3d-max-depth            device-image3d-max-height
                device-image3d-max-width            device-max-clock-frequency
                device-max-compute-units            device-max-constant-args
                device-max-parameter-size           device-max-read-image-args
                device-max-samplers                 device-max-work-group-size
                device-max-work-item-dimensions     device-max-write-image-args
                device-mem-base-addr-align          device-min-data-type-align-size
                device-name                         device-native-vector-width-char
                device-native-vector-width-double   device-native-vector-width-float
                device-native-vector-width-half     device-native-vector-width-int
                device-native-vector-width-long     device-native-vector-width-short
                device-opencl-c-version
                device-preferred-vector-width-char  device-preferred-vector-width-double
                device-preferred-vector-width-float device-preferred-vector-width-half
                device-preferred-vector-width-int   device-preferred-vector-width-long
                device-preferred-vector-width-short
                device-profile                      device-profiling-timer-resolution
                device-vendor                       device-vendor-id
                device-version                      driver-version

                ;; event-allocate
                event-command-type event-complete! event-status
                event-create event-release!

                kernel-create kernel-enqueue kernel-arg-set! kernel-release!

                ;; mem-allocate
                mem-offset
                mem-size mem-type

                platform-devices platform-extensions
                platform-name platform-profile platforms
                platform-vendor platform-version

                program-build program-build-log program-build-options
                program-create program-release!)

(import scheme chicken.base chicken.foreign)
(include "opencl.scm"))
