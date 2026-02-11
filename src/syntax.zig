const std = @import("std");

// (define-syntax syntax-rules
//   (lambda (x)
//     (syntax-case x ()
//       [(_ (k ...) [(_ . p) f ... t] ...)
//        #'(lambda (x)
//            (syntax-case x (k ...)
//              [(_ . p) f ... #'t] ...))])))
