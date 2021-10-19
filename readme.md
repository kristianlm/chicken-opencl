
  [Chicken Scheme]: http://call-cc.org
  [OpenCL]: https://www.khronos.org/opencl/

# [OpenCL] bindings for [Chicken Scheme]

These bindings target [OpenCL version
1.1](https://www.khronos.org/registry/OpenCL/sdk/1.1/docs/man/xhtml/)
for wide support. It should be fairly easy to add support to newer
OpenCL APIs.

## Source Code

Hosted [here](https://github.com/kristianlm/chicken-opencl).

## General conventions

Most Scheme procedures map 1-1 to OpenCL C procedures, with some
naming conventions changed. All procedures start with the type of
object they work on. For example, `clCreateBuffer` becomes
`buffer-create`.

All OpenCL objects are wrapped in a record structure. All `finalizer`
keyword arguments default to using `set-finalizer!` to call the
appropriate `*-release!` procedure when the object is freed by the
CHICKEN GC. This automatic memory management should be sufficient for
many use cases.

For a large number of OpenCL objects it may be a good idea to call the
`*-release!` procedures explicitly. All the `*-release!` procedures
are idempotent, so it is even safe to use them on objects that have
finalizers attached. Also, `(gc #t)` can sometimes be used to free
ObjecCL objects no longer needed by the application, freeing OpenCL
resources sooner.

## Example

There is unfortunately a bit boilerplate to get started, but luckily
it's less than the equivalent C code. This code snippet might be
useful:

```scheme
(import srfi-4 opencl)

(define device (car (flatten (map platform-devices (platforms)))))
(define context (context-create device))
(define cq (command-queue-create context device))
(define buffer (buffer-create cq (f32vector -1 -1 -1 -1 -1)))
(define kernel (kernel-create (program-build (program-create context "
__kernel void foo(__global float *out) {
  out[get_global_id(0)] = get_global_id(0) * 10;
}
") device) "foo"))
(kernel-arg-set! kernel 0 buffer)
(kernel-enqueue kernel cq (list 4))
(print (buffer-read buffer cq))
```

When run, it should output `#f32(0.0 10.0 20.0 30.0 -1.0)`. The last
`-1` is leftovers from buffer creation.

## API

    [procedure] (platforms) => (list-of cl_platform)

List all the OpenCL platforms on the system.

    [procedure] (platform-extensions platform) => string
    [procedure] (platform-name platform) => string
    [procedure] (platform-profile platform) => string
    [procedure] (platform-vendor platform) => string
    [procedure] (platform-version platform) => string

Get various information for a given platform. See
[clGetPlatformIDs](https://www.khronos.org/registry/OpenCL/sdk/1.1/docs/man/xhtml/clGetPlatformInfo.html)

    [procedure] (platform-devices platform #!optional device-type) => (list-of cl_device)

List the available devices for `platform`. `device-type` can
optionally be supplied as a filter, and symbol among `cpu` `gpu` `all`
`accelerator` `default`.

    [procedure] (device-info device) => alist
    [procedure] (device-address-bits device) => number
    [procedure] (device-name device) => string
    [procedure] (device-vendor device) => string
    [procedure] (device-version device) => string

Get various information of `device`. See [this
page](https://www.khronos.org/registry/OpenCL/sdk/1.1/docs/man/xhtml/clGetDeviceInfo.html).

    [procedure] (context-create devices #!key (finalizer #t)) => cl_context
    [procedure] (context-release!) => void

Create or release an OpenCL context. See [this
page](https://www.khronos.org/registry/OpenCL/sdk/1.1/docs/man/xhtml/clCreateContext.html). Unfurtunately,
currently `devices` must be a single `cl_device`. In the future, a
list of `cl_device`s might be implemented.

    [procedure] (command-queue-create context device #!key out-of-order profile (finalizer #t)) => cl_command_queue
    [procedure] (command-queue-release! command_queue) => void

Create or release a `cl_command-queue`. `out-of-order` is a boolean
flag and is `#f` by default. See [this
page](https://www.khronos.org/registry/OpenCL/sdk/1.1/docs/man/xhtml/clCreateCommandQueue.html).

    [procedure] (command-queue-context context) => cl_context

Get back the `cl_context` passed into `command-queue-create`.

    [procedure] (buffer-create c s #!key (type #f) (flags 0) (finalizer #t)) => cl_mem
    [procedure] (buffer-release! buffer) => void

Create or release an OpenCL buffer object, a memory chunk that is
usually stored on the target device. See [this
page](https://www.khronos.org/registry/OpenCL/sdk/1.1/docs/man/xhtml/clCreateBuffer.html).

If `c` is a `cl_context`, `s` must be an integer representing the
buffer size in bytes.

If `c` is a `cl_command-queue`, `s` must be a srfi-4 vector or a
blob. The content of `s` is copied to the newly created buffer
`buffer` using `buffer-write`. The size in bytes of this buffer and
the size in bytes of `s` will be equal.

`type` defaults to the type of `s` for srfi-4 vectors, or `blob` if
`s` is an integer. If `s` is an integer, `type` can be explicitly
supplied. See `buffer-type` for valid values.

Support for `flags` is currently limited, and must be a number
corresponding to the C API. The default is `CL_MEM_READ_WRITE`.

    [procedure] (buffer-write buffer cq src #!key (offset 0)) => cl_mem

Copy the content of `src` to `buffer`. See [this
page](https://www.khronos.org/registry/OpenCL/sdk/1.1/docs/man/xhtml/clEnqueueWriteBuffer.html). Only
blocking writes are supported.

`cq` must be a valid `cl_command-queue`. `src` must be a srfi-4 vector
or a blob. `offset` is the offset in bytes in the buffer object to
write to.

    [procedure] (buffer-read buffer cq #!key type (dst #f) (byte-offset 0) (bytes #f)) => srfi-4-vector

Copy the content of `buffer` into host memory. The returned object is
`dst` if supplied, otherwise a newly allocated srfi-4 vector or
blob. Which vector type depends on `type`, which defaults to
`(buffer-type buffer)`. If supplied, `dst` must be a srfi-4 vector or
blob and its content will be overwritten with data from `buffer`.

If supplied, `bytes` must be the number of bytes to copy. If supplied,
`byte-offset` must be the number of bytes to skip in `buffer`.

    [procedure] (buffer-size buffer) => integer

Return the size of `buffer` in bytes.

    [procedure] (buffer-type buffer) => symbol

Return the type tag symbol. This is stored in the `cl_mem` Scheme
record, not in the OpenCL `cl_mem` object. It is used by `buffer-read`
to construct a suitable srfi-4 vector. Valid types are `u8` `s8` `u16`
`s16` `u32` `s32` `u64` `s64` `f32` `f64`.

    [procedure] (program-create context source #!key (finalizer #t)) => cl_program
    [procedure] (program-release! program) => void

Create or release a OpenCL with
[clCreateProgramWithSource](https://www.khronos.org/registry/OpenCL/sdk/1.1/docs/man/xhtml/clCreateProgramWithSource.html).

`context` must be a valid `cl_context`. `source` must be OpenCL C
source code as a string. Note that the returned `program` must be
built before it can be used to create kernels.

    [procedure] (program-build program devices) => program

`devices` must be a valid `cl_device`. Unfortunately, only a single
device is currently supported. If the build process fails, an error is
signaled with the content of `(program-build-log program)` which will
hopefully contain useful compiler errors.

    [procedure] (program-build-log program) => string
    [procedure] (program-build-options program) => string

Retrieve various information about `program`, see [this
page](https://www.khronos.org/registry/OpenCL/sdk/1.1/docs/man/xhtml/clGetProgramInfo.html).

    [procedure] (kernel-create program name #!key (finalizer #t)) => cl_kernel
    [procedure] (kernel-release! kernel) => void

Create or release a `cl_kernel`. See [this page](https://www.khronos.org/registry/OpenCL/sdk/1.1/docs/man/xhtml/clCreateKernel.html).

`name` must be a string, the name of a `__kernel` function inside
`program`.

    [procedure] (kernel-arg-set! kernel idx value) => void

Calls
[`clSetKernelArg`](https://www.khronos.org/registry/OpenCL/sdk/1.1/docs/man/xhtml/clSetKernelArg.html). `idx`
must be an integer. `value` must be a srfi-4 vector or a `cl_buffer`.

The type of the kernel argument (ie. `float`) must match the srfi-4
vector type (ie. `(f32vector 1)`). It is unfortunately not possible to
enforce checks for this. The srfi-4 vector's length must match the
kernel argument. For example, a kernel argument `float4` must be
passed a `f32vector` of length 4.

    [procedure] (kernel-enqueue kernel cq global-work-sizes #!key event wait global-work-offsets local-work-sizes) => (or cl_event #f)

Enqueues `kernel` for execution on `cq` as described
[here](https://www.khronos.org/registry/OpenCL/sdk/1.1/docs/man/xhtml/clEnqueueNDRangeKernel.html).

`global-work-sizes` must be a list of integers, each representing a
work size for each dimension. On most platforms, the maximum number of
dimensions (and thus the length of `global-work-sizes`) is 3. The
minimum is always 1.

The numbers supplied here will be reflected inside `kernel` as
[`get_global_size(n)`](https://www.khronos.org/registry/OpenCL/sdk/1.1/docs/man/xhtml/get_global_size.html). Note that total number of worker-items executed should be `(foldl * 1
global-work-sizes)`.

Unlike most of the other procedures, `kernel-enqueue` is non-blocking
and may return before `kernel` is finished executing. If `event` is
supplied and is `#t` or a `cl_event`, the returned `event` can be used
to query the kernel execution status. If `event` is not supplied,
`kernel-enqueue` returnes `#f`.

### Events

The event API is a bit experimental and difficult to test.

    [procedure] (event-create context) => cl_event
    [procedure] (event-release!) => void

Create or release a user-event `cl_event`, see [this
page](https://www.khronos.org/registry/OpenCL/sdk/1.1/docs/man/xhtml/clCreateUserEvent.html).

    [procedure] (event-complete! event #!optional (status 'complete)) => cl_event

Set the execution status of `event`, see
[clSetUserEventStatus](https://www.khronos.org/registry/OpenCL/sdk/1.1/docs/man/xhtml/clSetUserEventStatus.html).

    [procedure] (event-status event) => symbol

Retrieve `event` status, one of: `queued` `submitted` `running`
`complete`.

### Record predicates

    [procedure] (cl_command-queue? x) => bool
    [procedure] (cl_context? x) => bool
    [procedure] (cl_device? x) => bool
    [procedure] (cl_event? x) => bool
    [procedure] (cl_kernel? x) => bool
    [procedure] (cl_mem? x) => bool
    [procedure] (cl_platform? x) => bool
    [procedure] (cl_program? x) => bool

Predicates for record types. `buffer` object are `cl_mem?` records.

## Development status

- [x] automatic memory management of OpenCL objects
- [x] list platforms and devices
- [x] create context and command queue
- [x] create, compile and run programs from source-code
- [ ] create, compile and run programs from IR/Binary
- [x] launch kernels (global-size, local-size and global-offset)
- [ ] kernel name
- [ ] `clCreateKernelsInProgram`
- [x] create, read and write buffer objects
- [ ] create, read and write image objects
- [ ] copy buffers
- [ ] copy images
- [ ] samplers
- [ ] sub-buffers
- [ ] non-blocking API
- [x] event support for kernel and user (test fails on my Clover platform)
- [ ] event support for buffer commands
- [ ] event callbacks
- [ ] event blocking (`clWaitForEvents`)

So far everything is blocking. Implementing the non-blocking API would
complicate things as the Chicken GC might get in the way.
