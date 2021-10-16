
  [Chicken Scheme]: http://call-cc.org
  [OpenCL]: https://www.khronos.org/opencl/

# [OpenCL] bindings for [Chicken Scheme]

These are very alpha-stage. These bindings target [OpenCL version
1.1](https://www.khronos.org/registry/OpenCL/sdk/1.1/docs/man/xhtml/)
to have wide support. It should be fairly easy to add newer OpenCL
APIs.

```
$ chicken-install -s -test
```

# Development status

- [x] automatic memory management of OpenCL objects
- [x] list platforms and devices
- [x] create context and command queue
- [x] create, compile and run programs from source-code
- [ ] create, compile and run programs from IR/Binary
- [x] launch kernels (global-size, local-size and global-offset)
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
