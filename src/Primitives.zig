const std = @import("std");
const VM = @import("main.zig").VM;
const AstNode = @import("Parser.zig").AstNode;
const _void = @import("Node.zig")._void;
const _true = @import("Node.zig")._true;
const _false = @import("Node.zig")._false;
const NodePtr = @import("Node.zig").NodePtr;
const Node = @import("Node.zig").Node;
const AstBuilder = @import("Parser.zig").Builder;


const Primitives = @This();

pub const Macro = struct {
    exec: *const fn (AstBuilder, []*AstNode) anyerror!*AstNode,
    name: []const u8,
};

pub const Prim = struct { 
    exec: *const fn (*VM) anyerror!void,
    numArgs: usize = 0,
    varargs: bool = false,
    name: []const u8,
};

pub fn hasFloats(vm: *VM, n: i48) bool {
    for (0..@intCast(n)) |i| {
        const j = vm.stack.size - 1 - i;
        if (j < 0) {
            @panic("return stack underflow");
        }
        if (vm.stack.items[j].getId() == .floatNumber) {
            return true;
        }
    }
    return false;
}


pub fn assertArityEq(expected: i48, actual: i48) anyerror!void {
    if (expected != actual) {
        return error.ArityMismatch;
    }
}
pub fn assertArityGreaterOrEq(expected: i48, actual: i48) anyerror!void {
    if (actual < expected) {
        return error.ArityMismatch;
    }
}

// need to add mark if it is varargs?
pub fn add(vm: *VM) anyerror!void {
    const n = (try vm.stack.pop()).getIntValue();
    if (hasFloats(vm, n)) {
        var res: f32 = 0.0;
        for (0..@intCast(n)) |_| {
            res += try (try vm.stack.pop()).convertToFloat();
        }
        try vm.bldr.newFloatNumber(res);
    } else {
        var res: i48 = 0;
        for (0..@intCast(n)) |_| {
            res += (try (try vm.stack.pop()).tryGetIntValue());
        }
        try vm.bldr.newIntNumber(res);
    }
}

pub fn _isZero(vm: *VM) anyerror!void {
    const n = try vm.stack.pop();
    var isZero = false;
    if (n.getId() == .intNumber) {
        isZero = (try n.tryGetIntValue()) == 0;
    } else if (n.getId() == .floatNumber) {
        isZero = (try n.tryGetFloatValue()) == 0.0;
    } else {
        return error.IllegalArgument;
    }
    try vm.bldr.newBool(isZero);
}

pub fn sub(vm: *VM) anyerror!void {
    const n = (try vm.stack.pop()).getIntValue();
    try assertArityGreaterOrEq(1, n);
    try vm.stack.reverseInPlace(@intCast(n));
    if (hasFloats(vm, n)) {
        var res: f32 = (try (try vm.stack.pop()).convertToFloat());
        if (n == 1) {
            try vm.bldr.newFloatNumber(-res);
            return;
        }
        for (1..@intCast(n)) |_| {
            res -= (try (try vm.stack.pop()).convertToFloat());
        }
        try vm.bldr.newFloatNumber(res);
    } else {
        var res: i48 = (try (try vm.stack.pop()).tryGetIntValue());
        if (n == 1) {
            try vm.bldr.newIntNumber(-res);
            return;
        }
        for (1..@intCast(n)) |_| {
            res -= (try (try vm.stack.pop()).tryGetIntValue());
        }
        try vm.bldr.newIntNumber(res);
    }
}



// pub fn apply(vm: *VM) anyerror!void {
//     const n = (try vm.stack.pop()).getIntValue();
//     try assertArityGreaterOrEq(2, n);
//     // const f = try vm.stack.pop();
//     if (n > 2) {
//         for (0..@intCast(n-2)) |_| {
//             const arg = try vm.stack.pop();
//             vm.protect(arg);
//         }
//     }
//     const args = (try vm.stack.pop()); // last ones
//     if (!(args.getId() == .pair or args.getId() == .nil)) {
//         return error.ExpectedList;
//     }
//     // const m = args.len();
//     // try vm.pushStackFrame(vm.builtins.__ap, vm.env, m+n-2, .{ .isReturn = true });
//
//     try vm.stack.push(args);
//     // try vm._evalList();
//     for (0..@intCast(n-2)) |_| {
//         const x = try vm.protectStack.pop();
//         // try vm.pushStackFrame(x, vm.env, 0, .{});
//     }
//     // try vm.pushStackFrame(f, vm.env, args.len(), .{});
// }


// pub fn @"define-macro"(vm: *VM) anyerror!void {
//     const n = (try vm.stack.pop()).getIntValue();
//     try assertArityGreaterOrEq(2, n);
//     const _p = try vm.stack.pop();
//     vm.protect(_p);
//     const p = try _p.tryCast(.pair);
//     const name = p.fst;
//     const args = p.snd;
//
//     try vm.bldr.newList();
//     for (0..@intCast(n - 1)) |_| {
//         try vm.appendToListRev();
//     }
//     try vm.reverseList();
//     try vm.bldr.newAtom("begin");
//     try vm.appendToList();
//
//     const body = try vm.stack.pop();
//     vm.protect(body);
//
//     try vm.stack.push(name);
//     try vm.stack.push(vm.env);
//     try vm.stack.push(body);
//     try vm.stack.push(args);
//     try vm.bldr.newProc(true);
//     const proc = try vm.stack.head();
//     try vm.appendToLastEnv();
//     proc.cast(.procedure).env = vm.env;
//     try vm.stack.push(_void);
// }

// pub fn __define_macro(vm: *VM) anyerror!void {
//     const numArgs = (try vm.stack.pop()).getIntValue();
//     try assertArityGreaterOrEq(2, numArgs);
//     const arg0 = try vm.stack.pop();
//     vm.protect(arg0);
//
//     if (arg0.getId() == .atom) {
//        try assertArityEq(2, numArgs);
//        const expr = try vm.stack.pop();
//        vm.protect(expr);
//
//         try vm.bldr.newList();
//         try vm.bldr.newAtom("put!");
//         try vm.appendToList();
//         try vm.stack.push(arg0);
//         try vm.appendToList();
//         try vm.stack.push(expr);
//         try vm.appendToList();
//         try vm.reverseList();
//     } else if (arg0.getId() == .pair) {
//
//         const name = arg0.head();
//         const params = arg0.tail();
//         for (1..@intCast(numArgs)) |_| {
//             vm.protect(try vm.stack.pop()); // pop body expr
//         }
//         try vm.protectStack.reverseInPlace(@intCast(numArgs - 1));
//         try vm.bldr.newList();
//         try vm.bldr.newAtom("put!");
//         try vm.appendToList();
//         try vm.stack.push(name);
//         try vm.appendToList();
//
//             try vm.bldr.newList(); // our lambda
//             try vm.bldr.newAtom("lambda");
//             try vm.appendToList();
//             try vm.stack.push(params);
//             try vm.appendToList();
//
//             for (1..@intCast(numArgs)) |_| {
//                 try vm.stack.push(try vm.protectStack.pop());
//                 try vm.appendToList();
//             }
//             try vm.reverseList();
//
//         try vm.appendToList();
//         try vm.reverseList();
//         // (try vm.stack.head()).debugprint("**result**");
//
//     } else {
//         std.debug.print("can't define not atom, got: {}\n", .{arg0.getId()});
//         return error.IllegalArgument;
//     }
//
//
// }





// quotient     remainder     modulo
// max          min           abs
// numerator    denominator   gcd
// lcm          floor         ceiling
// truncate     round         rationalize
// expt
//
// procedure:  (< x1 x2 x3 ...)
// procedure:  (> x1 x2 x3 ...)
// procedure:  (<= x1 x2 x3 ...)
// procedure:  (>= x1 x2 x3 ...)
// These procedures return #t if their arguments are (respectively): equal, monotonically increasing, monotonically decreasing, monotonically nondecreasing, or monotonically nonincreasing.
//

//
// library procedure:  (max x1 x2 ...)
// library procedure:  (min x1 x2 ...)
//  (abs x)
//  procedure:  (floor x)
// procedure:  (ceiling x)
// procedure:  (truncate x)
// procedure:  (round x)
// procedure:  (exp z)
// procedure:  (log z)
// procedure:  (sin z)
// procedure:  (cos z)
// procedure:  (tan z)
// procedure:  (asin z)
// procedure:  (acos z)
// procedure:  (atan z)
// procedure:  (atan y x)
// procedure:  (sqrt z)
// procedure:  (number->string z)
// procedure:  (number->string z radix)
// procedure:  (string->number string)
// procedure:  (string->number string radix)
//

pub fn @"="(vm: *VM) anyerror!void {
    const n = (try vm.stack.pop()).getIntValue();
    try assertArityGreaterOrEq(1, n);
    var res = true;
    if (hasFloats(vm, n)) {
        const first: f32 = try (try vm.stack.pop()).convertToFloat();
        for (1..@intCast(n)) |_| {
            res &= (first == try (try vm.stack.pop()).convertToFloat());
        }
    } else {
        const first = (try (try vm.stack.pop()).tryGetIntValue());
        for (1..@intCast(n)) |_| {
            res &= first == (try (try vm.stack.pop()).tryGetIntValue());
        }
    }
    try vm.bldr.newBool(res);
}
pub fn isPositive(vm: *VM) anyerror!void {
    const n = try vm.stack.pop();
    var positive = false;
    if (n.getId() == .intNumber) {
        positive = (try n.tryGetIntValue()) > 0;
    } else if (n.getId() == .floatNumber) {
        positive = (try n.tryGetFloatValue()) > 0.0;
    } else {
        return error.IllegalArgument;
    }
    try vm.bldr.newBool(positive);
}


pub fn isBoolean(vm: *VM) anyerror!void {
    const n = try vm.stack.pop();
    try vm.bldr.newBool(n.getId() == .bool);
}

pub fn _not(vm: *VM) anyerror!void {
    const numArgs = (try vm.stack.pop()).getIntValue();
    try assertArityEq(1, numArgs);
    const n = try vm.stack.pop();
    try vm.bldr.newBool(((n.getId() == .bool and n.getBoolValue() == false)));
}

// list ops -----------------------------------------------------------------------------
pub fn _cons(vm: *VM) anyerror!void {
    const numArgs = (try vm.stack.pop()).getIntValue();
    try assertArityEq(2, numArgs);
    // const a = try vm.stack.pop();
    // const b = try vm.stack.pop();
    try vm.bldr.newPair();
}
pub fn _car(vm: *VM) anyerror!void {
    const numArgs = (try vm.stack.pop()).getIntValue();
    try assertArityEq(1, numArgs);
    const a = try vm.stack.pop();
    try vm.stack.push((try a.tryCast(.pair)).fst);
}
pub fn _cdr(vm: *VM) anyerror!void {
    const numArgs = (try vm.stack.pop()).getIntValue();
    try assertArityEq(1, numArgs);
    const a = try vm.stack.pop();
    try vm.stack.push((try a.tryCast(.pair)).snd);
}
pub fn isNull(vm: *VM) anyerror!void {
    const n = try vm.stack.pop();
    try vm.bldr.newBool(n.getId() == .nil);
}
pub fn isPair(vm: *VM) anyerror!void {
    const n = try vm.stack.pop();
    try vm.bldr.newBool(n.getId() == .pair);
}
pub fn isList(vm: *VM) anyerror!void {
    const n = try vm.stack.pop();
    try vm.bldr.newBool(n.getId() == .nil or n.getId() == .pair);
}
pub fn @"set-car!"(vm: *VM) anyerror!void {
    const numArgs = (try vm.stack.pop()).getIntValue();
    try assertArityEq(2, numArgs);
    const a = try vm.stack.pop();
    const xs = try a.tryCast(.pair);
    const b = try vm.stack.pop();
    xs.fst = b;
    try vm.stack.push(_void);
}
pub fn @"set-cdr!"(vm: *VM) anyerror!void {
    const numArgs = (try vm.stack.pop()).getIntValue();
    try assertArityEq(2, numArgs);
    const a = try vm.stack.pop();
    const xs = try a.tryCast(.pair);
    const b = try vm.stack.pop();
    xs.snd = b;
    try vm.stack.push(_void);
}

pub fn @"number?"(vm: *VM) anyerror!void {
    const numArgs = (try vm.stack.pop()).getIntValue();
    try assertArityEq(1, numArgs);
    const n = try vm.stack.pop();
    try vm.bldr.newBool(n.getId() == .intNumber or n.getId() == .floatNumber);
}
pub fn @"symbol?"(vm: *VM) anyerror!void {
    const numArgs = (try vm.stack.pop()).getIntValue();
    try assertArityEq(1, numArgs);
    const n = try vm.stack.pop();
    try vm.bldr.newBool(n.getId() == .atom);
}
// pub fn @"macro?"(vm: *VM) anyerror!void {
//     const numArgs = (try vm.stack.pop()).getIntValue();
//     try assertArityEq(1, numArgs);
//     const x = try vm.stack.pop();
//     if (x.getId() == .atom) {
//         if (getEnv(vm.env, x.cast(.atom).name)) | value | {
//             if ((value.getId() == .procedure and value.cast(.procedure).isMacro) or 
//                 (value.getId() == .primitive and value.cast(.primitive).isMacro)) {
//                 try vm.bldr.newBool(true);
//                 return;
//             }
//         }
//     }
//     try vm.bldr.newBool(false);
// }

pub fn @"integer?"(vm: *VM) anyerror!void {
    const numArgs = (try vm.stack.pop()).getIntValue();
    try assertArityEq(1, numArgs);
    const n = try vm.stack.pop();
    try vm.bldr.newBool(n.getId() == .intNumber);
}

pub fn @"string?"(vm: *VM) anyerror!void {
    const numArgs = (try vm.stack.pop()).getIntValue();
    try assertArityEq(1, numArgs);
    const n = try vm.stack.pop();
    try vm.bldr.newBool(n.getId() == .string);
}

pub fn @"real?"(vm: *VM) anyerror!void {
    const numArgs = (try vm.stack.pop()).getIntValue();
    try assertArityEq(1, numArgs);
    const n = try vm.stack.pop();
    try vm.bldr.newBool(n.getId() == .floatNumber);
}

pub fn __add(vm: *VM) anyerror!void {
    const n = (try vm.stack.pop()).getIntValue();
    if (hasFloats(vm, n)) {
        var res: f32 = 0.0;
        for (0..@intCast(n)) |_| {
            res += try (try vm.stack.pop()).convertToFloat();
        }
        try vm.bldr.newFloatNumber(res);
    } else {
        var res: i48 = 0;
        for (0..@intCast(n)) |_| {
            res += (try (try vm.stack.pop()).tryGetIntValue());
        }
        try vm.bldr.newIntNumber(res);
    }
}
pub fn @"string-append"(vm: *VM) anyerror!void {
    const n = (try vm.stack.pop()).getIntValue();
    if (n == 0) {
        try vm.bldr.newString("");
    } else if (n > 512) {
        return error.ArgsTooLong;
    } else {
        for (0..@intCast(n)) |i| {
            if (!((try vm.stack.nth(i)).getId() == .string)) {
                return error.IllegalArgument;
            }
        }
        var buff: [512][]const u8 = undefined;
        for (0..@intCast(n)) |i| {
            const _s = try vm.stack.pop();
            vm.protect(_s);
            buff[i] = _s.cast(.string).s;
        }

        const res = try std.mem.concat(vm.allocator, u8, buff[0..@intCast(n)]);
        defer vm.allocator.free(res);
        try vm.bldr.newString(res);
    }
}
pub fn @"string->atom"(vm: *VM) anyerror!void { // TODO: radix..
    const n = (try vm.stack.pop()).getIntValue();
    try assertArityEq(1, n);
    const string = try vm.stack.pop();
    vm.protect(string);
    if (string.getId() == .string) {
        try vm.bldr.newAtom(string.cast(.string).s);
    } else {
        return error.IllegalArgument;
    }
}
pub fn @"number->string"(vm: *VM) anyerror!void { // TODO: radix..
    const n = (try vm.stack.pop()).getIntValue();
    try assertArityGreaterOrEq(1, n);
    const number = try vm.stack.pop();
    if (number.getId() == .intNumber) {
        const s = try std.fmt.allocPrint(vm.allocator, "{d}", .{number.getIntValue()});
        defer vm.allocator.free(s);
        try vm.bldr.newString(s);
    } else if (number.getId() == .floatNumber) {
        const s = try std.fmt.allocPrint(vm.allocator, "{d}", .{number.getFloatValue()});
        defer vm.allocator.free(s);
        try vm.bldr.newString(s);
    } else {
        return error.IllegalArgument;
    }
}
pub fn _mul(vm: *VM) anyerror!void {
    const n = (try vm.stack.pop()).getIntValue();
    if (hasFloats(vm, n)) {
        var res: f32 = 1.0;
        for (0..@intCast(n)) |_| {
            res *= try (try vm.stack.pop()).convertToFloat();
        }
        try vm.bldr.newFloatNumber(res);
    } else {
        var res: i48 = 1;
        for (0..@intCast(n)) |_| {
            res *= (try (try vm.stack.pop()).tryGetIntValue());
        }
        try vm.bldr.newIntNumber(res);
    }
}
pub fn _div(vm: *VM) anyerror!void {
    const n = (try vm.stack.pop()).getIntValue();
    if (n != 1 and n != 2) {
        return error.ArityMismatch;
    }
    if (hasFloats(vm, n)) {
        var res: f32 = try (try vm.stack.pop()).convertToFloat();
        if (n == 1) {
            try vm.bldr.newFloatNumber(1.0 / res);
            return;
        }
        res /= try (try vm.stack.pop()).convertToFloat();
        try vm.bldr.newFloatNumber(res);
    } else {
        var res: i48 = (try (try vm.stack.pop()).tryGetIntValue());
        if (n == 1) {
            if (res == 0) {
                return error.IllegalArgument;
            }
            try vm.bldr.newIntNumber(0);
            return;
        }
        const b = (try (try vm.stack.pop()).tryGetIntValue());
        if (b == 0) {
            return error.IllegalArgument;
        }
        res = @divExact(res, b);
        try vm.bldr.newIntNumber(res);
    }
}
pub fn _display(vm: *VM) anyerror!void {
    const n = (try vm.stack.pop()).getIntValue();
    try assertArityEq(1, n);
    const a = try vm.stack.pop();
    a.print(); // todo: prettyprint..
    try vm.stack.push(_void);
}
pub fn _displaynl(vm: *VM) anyerror!void {
    const n = (try vm.stack.pop()).getIntValue();
    try assertArityEq(1, n);
    const a = try vm.stack.pop();
    a.print(); // todo: prettyprint..
    std.debug.print("\n", .{});
    try vm.stack.push(_void);
}
pub fn _newline(vm: *VM) anyerror!void {
    const n = (try vm.stack.pop()).getIntValue();
    try assertArityEq(0, n);
    std.debug.print("\n", .{});
    try vm.stack.push(_void);
}
pub fn _print(vm: *VM) anyerror!void {
    const n = (try vm.stack.pop()).getIntValue();
    try assertArityEq(1, n);
    const a = try vm.stack.pop();
    a.print(); // todo: prettyprint..
    try vm.stack.push(_void);
}



pub fn _timeStart(vm: *VM) anyerror!void { // for simplicity nested time not allowed.
    vm.stats.startTime = try std.time.Instant.now();
    vm.stats.numAllocs = 0;
    vm.stats.numAllocConses = 0;
    vm.stats.gcTime = 0;
    try vm.stack.push(_void);
}
pub fn _timeStop(vm: *VM) anyerror!void {
    const now = try std.time.Instant.now();
    const elapsed = now.since(vm.stats.startTime) / 1000_000;
    std.debug.print(
        \\ * Elapsed: {d}ms
        \\ * Total allocs: {d}
        \\ * Cons Allocs: {d}
        \\ * GC Time: {d}ms
        \\
    , .{ elapsed, vm.stats.numAllocs, vm.stats.numAllocConses, vm.stats.gcTime / 1000_000 });
    // pass result as is
}
// pub fn _eval(vm: *VM) anyerror!void {
//     const n = (try vm.stack.pop()).getIntValue();
//     try assertArityEq(1, n);
//     const a = try vm.stack.pop();
//     try vm.pushStackFrame(a, vm.env, 0, .{});
// }
// pub fn _eval1(vm: *VM) anyerror!void {
//     const n = (try vm.stack.pop()).getIntValue();
//     try assertArityEq(2, n);
//     const a = try vm.stack.pop();
//     const env = try vm.stack.pop();
//     try vm.pushStackFrame(a, env, 0, .{});
// }

pub fn @"eq?"(vm: *VM) anyerror!void {
    const n = (try vm.stack.pop()).getIntValue();
    try assertArityEq(2, n);
    const a = try vm.stack.pop();
    const b = try vm.stack.pop();
    try vm.bldr.newBool(a.equal(&b));
}

pub fn @"eqv?"(vm: *VM) anyerror!void {
    const n = (try vm.stack.pop()).getIntValue();
    try assertArityEq(2, n);
    const a = try vm.stack.pop();
    const b = try vm.stack.pop();
    if (a.getId() == .intNumber and b.getId() == .intNumber) {
        try vm.bldr.newBool(a.getIntValue() == b.getIntValue());
        return;
    } else if (a.getId() == .floatNumber and b.getId() == .floatNumber) {
        try vm.bldr.newBool(a.getFloatValue() == b.getFloatValue());
        return;
    }
    try vm.bldr.newBool(a.equal(&b));
}
pub fn equal(a: *const NodePtr, b: *const NodePtr) bool {
    if (a.getId() != b.getId()) {
        return false;
    }
    if (a.getId() == .intNumber) {
        return (a.getIntValue() == b.getIntValue());
    } else if (a.getId() == .floatNumber) {
        return (a.getFloatValue() == b.getFloatValue());
    } else if (a.getId() == .string) {
        return (std.mem.eql(u8, a.cast(.string).s, b.cast(.string).s));
    } else if (a.getId() == .atom) {
        return (std.mem.eql(u8, a.cast(.atom).name, b.cast(.atom).name));
    } else if (a.getId() == .pair) {
        return (listEqual(a.cast(.pair), b.cast(.pair)));
    }
    return a == b;
}
pub fn listEqual(a: *Node.Pair, b: *Node.Pair) bool {
    if (!equal(&a.fst, &b.fst)) {
        return false;
    }
    if (a.snd.equal(&b.snd)) {
        return true;
    }
    if (a.snd.getId() != .pair or b.snd.getId() != .pair) {
        return equal(&a.snd, &b.snd);
    }
    return listEqual(a.snd.cast(.pair), b.snd.cast(.pair));
}
pub fn @"equal?"(vm: *VM) anyerror!void {
    const n = (try vm.stack.pop()).getIntValue();
    try assertArityEq(2, n);
    const a = try vm.stack.pop();
    const b = try vm.stack.pop();
    if (a.getId() != b.getId()) {
        try vm.bldr.newBool(false);
        return;
    }
    if (a.equal(&b)) {
        try vm.bldr.newBool(true);
        return;
    }
    if (a.getId() == .intNumber or a.getId() == .floatNumber or a.getId() == .string or a.getId() == .atom or a.getId() == .pair) {
        try vm.bldr.newBool(equal(&a, &b));
        return;
    }
    try vm.bldr.newBool(false);
}

// If no exprs are provided, then result is #t.
//
// If a single expr is provided, then it is in tail position, so the results of the and expression
// are the results of the expr.
//
// Otherwise, the first expr is evaluated. If it produces #f, the result of the and expression is #f.
// Otherwise, the result is the same as an and expression with the remaining exprs in tail position with
// respect to the original and form.
// pub fn __and(vm: *VM) anyerror!void {
//     const n = (try vm.stack.pop()).getIntValue();
//     if (n == 0) {
//         try vm.bldr.newBool(true);
//     } else if (n == 1) {
//         const expr = try vm.stack.pop();
//         try vm.pushStackFrame(expr, vm.env, 0, .{});
//     } else {
//         const expr = try vm.stack.pop();
//         try vm.pushStackFrame(vm.getBuiltin("__and1"), vm.env, n - 1, .{ .isReturn = true });
//         try vm.pushStackFrame(expr, vm.env, 0, .{});
//     }
// }
// pub fn __and1(vm: *VM) anyerror!void {
//     const n = (try vm.stack.pop()).getIntValue();
//     const res = try vm.stack.pop();
//     if (res.getId() == .bool and res.equal(&@"#f")) {
//         for (0..@intCast(n)) |_| try vm.stack.drop1();
//         try vm.stack.push(res);
//     } else {
//         try vm.pushStackFrame(vm.getBuiltin("and"), vm.env, n, .{ .isReturn = true });
//     }
// }

// pub fn __or(vm: *VM) anyerror!void {
//     const n = (try vm.stack.pop()).getIntValue();
//     if (n == 0) {
//         try vm.bldr.newBool(false);
//     } else if (n == 1) {
//         const expr = try vm.stack.pop();
//         try vm.pushStackFrame(expr, vm.env, 0, .{});
//     } else {
//         const expr = try vm.stack.pop();
//         try vm.pushStackFrame(vm.getBuiltin("__or1"), vm.env, n - 1, .{ .isReturn = true });
//         try vm.pushStackFrame(expr, vm.env, 0, .{});
//     }
// }
// pub fn __or1(vm: *VM) anyerror!void {
//     const n = (try vm.stack.pop()).getIntValue();
//     const res = try vm.stack.pop();
//     if (res.getId() != .bool or (res.getId() == .bool and !(res.equal(&@"#f")))) {
//         for (0..@intCast(n)) |_| try vm.stack.drop1();
//         try vm.stack.push(res);
//     } else {
//         try vm.pushStackFrame(vm.getBuiltin("or"), vm.env, n, .{ .isReturn = true });
//     }
// }

// pub fn __cond(vm: *VM) anyerror!void {
//     const n = (try vm.stack.pop()).getIntValue();
//     if (n == 0) {
//         try vm.stack.push(_void);
//     } else {
//         const expr = try (try vm.stack.pop()).tryCast(.pair);
//         const cond = expr.fst;
//         if (cond.getId() == .atom and std.mem.eql(u8, "else", cond.cast(.atom).name)) {
//             if (expr.snd.getId() == .nil) {
//                 try vm.stack.push(_void);
//             } else {
//                 const body = expr.snd.head();
//                 try vm.stack.push(body);
//             }
//             try vm.pushStackFrame(vm.getBuiltin("__cond1"), vm.env, n - 1, .{ .isReturn = true });
//             try vm.bldr.newBool(true);
//             return;
//         }
//         if (expr.snd.getId() == .nil) {
//             try vm.stack.push(cond);
//         } else {
//             const body = expr.snd.head();
//             try vm.stack.push(body);
//         }
//         try vm.pushStackFrame(vm.getBuiltin("__cond1"), vm.env, n - 1, .{ .isReturn = true });
//         try vm.pushStackFrame(cond, vm.env, 0, .{});
//     }
// }
// pub fn __cond1(vm: *VM) anyerror!void {
//     const n = (try vm.stack.pop()).getIntValue();
//     const res = try vm.stack.pop();
//     if (res.getId() != .bool or (res.getId() == .bool and res.equal(&@"#t"))) {
//         const body = try vm.stack.pop();
//         for (0..@intCast(n)) |_| try vm.stack.drop1();
//         try vm.pushStackFrame(body, vm.env, 0, .{});
//     } else {
//         _ = try vm.stack.pop();
//         try vm.pushStackFrame(vm.getBuiltin("cond"), vm.env, n, .{ .isReturn = true });
//     }
// }
// pub fn __case(vm: *VM) anyerror!void {
//     const _n = try vm.stack.pop();
//     const n = _n.getIntValue();
//     try assertArityGreaterOrEq(1, n);
//     if (n == 1) {
//         try vm.stack.push(_void);
//     } else {
//         const expr = try vm.stack.pop();
//         try vm.pushStackFrame(vm.getBuiltin("__case1"), vm.env, n - 1, .{ .isReturn = true });
//         try vm.pushStackFrame(expr, vm.env, 0, .{});
//     }
// }
// pub fn contains(xs: *Node.Pair, x: *const NodePtr) anyerror!bool { // FIXME: replace with iteration
//     if (equal(&xs.fst, x)) {
//         return true;
//     }
//     if (xs.snd.getId() == .nil) {
//         return false;
//     }
//     return contains(try xs.snd.tryCast(.pair), x);
// }
// pub fn __case1(vm: *VM) anyerror!void {
//     const _n = try vm.stack.pop();
//     const n = _n.getIntValue();
//     if (n == 0) {
//         _ = try vm.stack.pop();
//         try vm.stack.push(_void);
//         return;
//     }
//     const value = try vm.stack.pop();
//     vm.protect(value);
//     const _clause = try vm.stack.pop();
//     vm.protect(_clause);
//     const clause = try _clause.tryCast(.pair);
//     if ((clause.fst.getId() == .atom and std.mem.eql(u8, "else", clause.fst.cast(.atom).name))) {
//         for (0..@intCast(n - 1)) |_| try vm.stack.drop1();
//         try vm.pushStackFrame(clause.snd.head(), vm.env, 0, .{});
//         return;
//     }
//     const xs = (try clause.fst.tryCast(.pair));
//     if (try contains(xs, &value)) {
//         for (0..@intCast(n - 1)) |_| try vm.stack.drop1();
//         try vm.pushStackFrame(clause.snd.head(), vm.env, 0, .{});
//         return;
//     }
//     try vm.stack.push(value);
//     try vm.pushStackFrame(vm.getBuiltin("__case1"), vm.env, n - 1, .{ .isReturn = true });
// }


pub fn _error(vm: *VM) anyerror!void {
    const n = (try vm.stack.pop()).getIntValue();
    try assertArityGreaterOrEq(1, n);
    for (0..@intCast(n)) |_| {
        const msg = try vm.stack.pop();
        msg.debugprint("");
    }
    return error.UserError;
}


pub const @"+": Prim = .{.name = "+", .exec = add, .varargs = true};
pub const @"-": Prim = .{.name = "-", .exec = sub, .varargs = true};
pub const @"/": Prim = .{.name = "/", .exec = _div, .varargs = true};
pub const @"*": Prim = .{.name = "*", .exec = _mul, .varargs = true};

pub const @"zero?": Prim = .{.name = "zero?", .exec = _isZero, .numArgs = 1};
pub const @"pair?": Prim = .{.name = "pair?", .exec = isPair,  .numArgs = 1};
pub const @"null?": Prim = .{.name = "null?", .exec = isNull,  .numArgs = 1};
pub const @"list?": Prim = .{.name = "list?", .exec = isList,  .numArgs = 1};
pub const @"positive?": Prim = .{.name = "positive?", .exec = isPositive,  .numArgs = 1};
pub const @"boolean?": Prim = .{.name = "boolean?", .exec = isBoolean,  .numArgs = 1};

pub const timeStart: Prim = .{.name = "timeStart", .exec = _timeStart, .numArgs = 0};
pub const timeStop: Prim = .{.name = "timeStop", .exec = _timeStop, .numArgs = 1};


    //         // .{ "boolean?", @"boolean?" },
    //         // .{ "number?", @"number?" },
    //         // .{ "symbol?", @"symbol?" },
    //         // .{ "macro?", @"macro?" },
    //         // .{ "integer?", @"integer?" },
    //         // .{ "string?", @"integer?" },
    //         // .{ "real?", @"real?" },
// ------------------------------ macros --------------------------------------

pub fn newList(arena: std.mem.Allocator, xs: []const *AstNode) !*AstNode {
       const list = try arena.create(AstNode.List);
        
       list.* = .{
            .xs = try arena.dupe(*AstNode, xs),
       };
        return &list.base;
}
pub fn newAtom(arena: std.mem.Allocator, name: []const u8) !*AstNode {
        const atom = try arena.create(AstNode.Atom);
        atom.* = .{
            .name = name,
        };
    return &atom.base;
}

pub fn _time(bldr: AstBuilder, params: []*AstNode) anyerror!*AstNode {
    if (params.len != 1) {
        return error.ArityMismatch;
    }
    return bldr.newList(&.{
        try bldr.newAtom("begin"),
          try bldr.newList(&.{try bldr.newAtom("timeStart")}),
          try bldr.newList(&.{try bldr.newAtom("timeStop"), params[0]}),
      });
}
pub fn _define(bldr: AstBuilder, params: []*AstNode) anyerror!*AstNode {
    if (params.len < 2) {
        return error.ArityMismatch;
    }

    if (params[0].id == .atom) {
        if (params.len != 2) {
            return error.ArityMismatch;
        }

        return bldr.newList(&.{try bldr.newAtom("set!"), params[0], params[1]});
    } else if (params[0].id == .list) {
        const args = params[0].cast(.list).xs;
        const name = args[0];
        const lambdaParams = try bldr.newList(args[1..]);
        var lambda = try bldr.emptyList(params.len + 1);
        lambda.cast(.list).xs[0] = try bldr.newAtom("lambda");
        lambda.cast(.list).xs[1] = lambdaParams;
        for (1..params.len) |i| {
            lambda.cast(.list).xs[i+1] = params[i];
        }

        return bldr.newList(&.{try bldr.newAtom("set!"), name, lambda});

    } else if (params[0].id == .improperList) {
        @panic("not implemented");
    } else {
        std.debug.print("can't define not atom, got: {}\n", .{params[0].id});
        return error.IllegalArgument;
    }


}

pub const define: Macro = .{.name = "define", .exec = _define };
pub const time: Macro = .{.name = "time", .exec = _time };
    // pub fn createInitialEnv(self: *VM) !void {
    //     _ = &self;
    // //     const xs = [_]struct { []const u8, *const fn (*VM) anyerror!void }{
    //         // .{ "number->string", @"number->string" },
    //         // .{ "string->atom", @"string->atom" },
    //         // .{ "string-append", @"string-append" },
    //         // .{ "__returnLast", __returnLast },
    //         // .{ "__discard", __discard },
    //         // .{ "__retrieve", __retrieve },
    //         // .{ "__retrieveArg", __retrieveArg },
    //         // .{ "__ap", __ap },
    //         // .{ "__ap0", __ap0 },
    //         // .{ "__if1", __if1 },
    //         // .{ "zero?", @"zero?" },
    //         // .{ "__set1", __set1 },
    //         // .{ "apply", apply },
    //         // .{ "=", @"=" },
    //         // .{ "__and1", __and1 },
    //         // .{ "__or1", __or1 },
    //         // .{ "__cond1", __cond1 },
    //         // .{ "_define1", _define1 },
    //         // .{ "eq?", @"eq?" },
    //         // .{ "eqv?", @"eqv?" },
    //         // .{ "equal?", @"equal?" },
    //         // .{ "not", _not },
    //         // .{ "cons", _cons },
    //         // .{ "car", _car },
    //         // .{ "cdr", _cdr },
    //         // .{ "null?", @"null?" },
    //         // .{ "pair?", @"pair?" },
    //         // .{ "list?", @"list?" },
    //         // .{ "set-car!", @"set-car!" },
    //         // .{ "set-cdr!", @"set-cdr!" },
    //         // .{ "display", _display },
    //         // .{ "displaynl", _displaynl },
    //         // .{ "newline", _newline },
    //         // .{ "print", _print },
    //         // .{ "eval", __eval },
    //         // .{ "eval1", __eval1 },
    //         // .{ "__time1", __time1 },
    //         // .{ "__exec", __exec },
    //         // .{ "printLastEnv", __printLastEnv },
    //         // .{ "error", __error },
    //         // .{ "expand", expand },
    //         // .{ "body", __body },
    //         // .{ "args", __args },
    //         // .{ "set-body", @"set-body"},
    //         // .{ "get-env-ref", @"get-env-ref" },
    //         // .{ "__case1", __case1 },
    //         // .{ "addToEnv", __addToEnv },
    //         // .{ "callerEnv", __callerEnv },
    //         // .{ "__put1", __put1 },
    //         // .{ "getAllProcs", __getAllProcs },
    //     // };
    //     // const dontEval = [_]struct { []const u8, *const fn (*VM) anyerror!void }{
    //         // .{ "if", __if },
    //         // .{ "and", __and },
    //         // .{ "or", __or },
    //         // .{ "cond", __cond },
    //         //
    //         // // .{ "define", _define },
    //         // .{ "time", __time },
    //         // .{ "case", __case },
    //         // .{ "set!", @"set!" },
    //         // .{ "put!", @"put!" },
    //         // .{ "define-macro", @"define-macro" },
    //     // };
    //     // const macros = [_]struct { []const u8, *const fn (*VM) anyerror!void }{
    //     //     // .{ "macrofoobar", __macrofoobar },
    //     //     // .{ "define", __define_macro},
    //     // };
    //     // try self.newList();
    //     // for (xs) |x| {
    //     //     try self.newAtom(x.@"0");
    //     //     try self.appendToList();
    //     // }
    //     // for (dontEval) |x| {
    //     //     try self.newAtom(x.@"0");
    //     //     try self.appendToList();
    //     // }
    //     // for (macros) |x| {
    //     //     try self.newAtom(x.@"0");
    //     //     try self.appendToList();
    //     // }
    //     //
    //     // try self.newList();
    //     // for (xs) |x| {
    //     //     // f.cons(f.builtin())
        //     try self.newBuiltin(x.@"1", x.@"0", false, false);
        //     try self.appendToList();
        // }
        // for (dontEval) |x| {
        //     try self.newBuiltin(x.@"1", x.@"0", true, false);
        //     try self.appendToList();
        // }
        // for (macros) |x| {
        //     try self.newBuiltin(x.@"1", x.@"0", true, true);
        //     try self.appendToList();
        // }
        // try self.newEnv();
    // }

