;;; using preprocessor to safely get all error codes.
;;; turned out to not be such a good idea after all. very complicated
;;; and we're set on 1.1 for now anyway.
;;;
;;; test like this:
;;; csc opencl-errors.scm ; and ./opencl-errors
(foreign-declare "
#define CL_TARGET_OPENCL_VERSION 120
#include <CL/cl.h>")

;; returns foreign fixnum if condition is met, otherwise #f
(define-syntax maybe-foreign-int
  (syntax-rules ()
    ((_ variable condition) ;; preprocessor condition, eg "#if 1"
     ((foreign-lambda* scheme-object ()
                       condition "
  return(C_fix(" variable "));
#else
  return(C_mk_bool(0));
#endif")))))

(define cl-errors
  (let ((lst '()))
    (let-syntax
        ((adding
          (er-macro-transformer
           (lambda (x r t)

             (import (only chicken.string conc))
             (define (symbol->cl-var sym)
               (conc "CL_" (list->string
                            (map (lambda (char)
                                   (if (eq? char #\-)
                                       #\_
                                       (char-upcase char)))
                                 (string->list (symbol->string sym))))))

             (let ((cvar (cadr x))
                   (preprocessor-var (and (pair? (cddr x)) (caddr x))))
               `(let ((value (maybe-foreign-int
                              ,(symbol->cl-var cvar)
                              ,(if preprocessor-var
                                   (conc "#ifdef " preprocessor-var)
                                   "#if 1"))))
                  (set! lst (cons (cons value ',cvar)  lst))))))))
      (adding build-program-failure                                     )
      (adding compile-program-failure                     CL_VERSION_1_2)
      (adding compiler-not-available                                    )
      (adding device-not-available                                      )
      (adding device-not-found                                          )
      (adding device-partition-failed                     CL_VERSION_1_2)
      (adding exec-status-error-for-events-in-wait-list   CL_VERSION_1_1)
      (adding image-format-mismatch                                     )
      (adding image-format-not-supported                                )
      (adding invalid-arg-index                                         )
      (adding invalid-arg-size                                          )
      (adding invalid-arg-value                                         )
      (adding invalid-binary                                            )
      (adding invalid-buffer-size                                       )
      (adding invalid-build-options                                     )
      (adding invalid-command-queue                                     )
      (adding invalid-compiler-options                    CL_VERSION_1_2)
      (adding invalid-context                                           )
      (adding invalid-device                                            )
      (adding invalid-device-partition-count              CL_VERSION_1_2)
      (adding invalid-device-queue                        CL_VERSION_2_0)
      (adding invalid-device-type                                       )
      (adding invalid-event                                             )
      (adding invalid-event-wait-list                                   )
      (adding invalid-global-offset                                     )
      (adding invalid-global-work-size                                  )
      (adding invalid-gl-object                                         )
      (adding invalid-host-ptr                                          )
      (adding invalid-image-descriptor                    CL_VERSION_1_2)
      (adding invalid-image-format-descriptor                           )
      (adding invalid-image-size                                        )
      (adding invalid-kernel                                            )
      (adding invalid-kernel-args                                       )
      (adding invalid-kernel-definition                                 )
      (adding invalid-kernel-name                                       )
      (adding invalid-linker-options                      CL_VERSION_1_2)
      (adding invalid-mem-object                                        )
      (adding invalid-mip-level                                         )
      (adding invalid-operation                                         )
      (adding invalid-pipe-size                           CL_VERSION_2_0)
      (adding invalid-platform                                          )
      (adding invalid-program                                           )
      (adding invalid-program-executable                                )
      (adding invalid-property                            CL_VERSION_1_1)
      (adding invalid-queue-properties                                  )
      (adding invalid-sampler                                           )
      (adding invalid-spec-id                             CL_VERSION_2_2)
      (adding invalid-value                                             )
      (adding invalid-work-dimension                                    )
      (adding invalid-work-group-size                                   )
      (adding invalid-work-item-size                                    )
      (adding kernel-arg-info-not-available               CL_VERSION_1_2)
      (adding linker-not-available                        CL_VERSION_1_2)
      (adding link-program-failure                        CL_VERSION_1_2)
      (adding map-failure                                               )
      (adding max-size-restriction-exceeded               CL_VERSION_2_2)
      (adding mem-copy-overlap                                          )
      (adding mem-object-allocation-failure                             )
      (adding misaligned-sub-buffer-offset                CL_VERSION_1_1)
      (adding out-of-host-memory                                        )
      (adding out-of-resources                                          )
      (adding profiling-info-not-available                              )
      (adding success                                                   ))
    lst))

(for-each print cl-errors)
