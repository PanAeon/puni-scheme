const std = @import("std");
const NodePtr = @import("Node.zig").NodePtr;

pub const PatternTag = enum {
    underscore,
    ellipsis,
    list,
    vector,
    keyword,
    variable,
    constant,
};
pub const Pattern = union(PatternTag) {
    underscore,
    ellipsis: *Pattern,
    list: []Pattern,
    vector: []Pattern,
    keyword: []u8,
    variable: []u8,
    constant: NodePtr,


    pub fn compile(expr: NodePtr, allocator: std.mem.Allocator) anyerror!*Pattern {
        _ = allocator;
        switch (expr.getId()) {
        }
    }
};
// (define-syntax syntax-rules
//   (lambda (x)
//     (syntax-case x ()
//       [(_ (k ...) [(_ . p) f ... t] ...)
//        #'(lambda (x)
//            (syntax-case x (k ...)
//              [(_ . p) f ... #'t] ...))])))
