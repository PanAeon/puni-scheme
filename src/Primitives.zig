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



pub fn _apply(vm: *VM) anyerror!void { // can we make this tail call friendly?
    var args = try vm.stack.pop(); // last ones
    const _f = try vm.stack.pop();
    if (!(args.getId() == .pair or args.getId() == .nil)) {
        return error.ExpectedList;
    }

    try vm.stack.push(vm.env);
    try vm.bldr.newIntNumber(@intCast(vm.ip + 1)); // push return value to the stack

    var numArgs:u32 = 0;
    while (args.getId() != .nil) {
        try vm.stack.push(args.cast(.pair).fst);
        numArgs += 1;
        args = args.cast(.pair).snd;
    }

    // unfortunately have to inline this here
                    const f = try _f.tryCast(.procedure);
                    if (f.varargs) {
                        if (numArgs + 1 < f.numArgs) {
                            return error.ArityMismatch;
                        }
                        const numLast = numArgs + 1 - f.numArgs ;
                        try vm.bldr.newList();
                        for (0..numLast) |_| {
                            try vm.bldr.appendToListRev();
                        }
                    } else {
                        if (f.numArgs != numArgs) {
                            return error.ArityMismatch;
                        }
                    }
                    vm.env = f.env;
                    vm.ip = @as(i64, @intCast(f.code)) - 1;
}








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

pub fn numEq(vm: *VM) anyerror!void {
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
    const n = try vm.stack.pop();
    try vm.bldr.newBool(((n.getId() == .bool and n.getBoolValue() == false)));
}

// list ops -----------------------------------------------------------------------------
pub fn _cons(vm: *VM) anyerror!void {
    try vm.stack.reverseInPlace(2);
    try vm.bldr.newPair();
}
pub fn _car(vm: *VM) anyerror!void {
    const a = try vm.stack.pop();
    try vm.stack.push((try a.tryCast(.pair)).fst);
}
pub fn _cdr(vm: *VM) anyerror!void {
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
pub fn setCar(vm: *VM) anyerror!void {
    const b = try vm.stack.pop();
    const a = try vm.stack.pop();
    const xs = try a.tryCast(.pair);
    xs.fst = b;
    try vm.stack.push(_void);
}
pub fn setCdr(vm: *VM) anyerror!void {
    const b = try vm.stack.pop();
    const a = try vm.stack.pop();
    const xs = try a.tryCast(.pair);
    xs.snd = b;
    try vm.stack.push(_void);
}

pub fn isNumber(vm: *VM) anyerror!void {
    const n = try vm.stack.pop();
    try vm.bldr.newBool(n.getId() == .intNumber or n.getId() == .floatNumber);
}
pub fn isSymbol(vm: *VM) anyerror!void {
    const n = try vm.stack.pop();
    try vm.bldr.newBool(n.getId() == .atom);
}

pub fn isInteger(vm: *VM) anyerror!void {
    const n = try vm.stack.pop();
    try vm.bldr.newBool(n.getId() == .intNumber);
}

pub fn isString(vm: *VM) anyerror!void {
    const n = try vm.stack.pop();
    try vm.bldr.newBool(n.getId() == .string);
}

pub fn isReal(vm: *VM) anyerror!void {
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
pub fn stringAppend(vm: *VM) anyerror!void {
    const n = (try vm.stack.pop()).getIntValue();
    if (n == 0) {
        try vm.bldr.newString("");
    } else if (n > 512) {
        return error.ArgsTooLong;
    } else {
        try vm.stack.reverseInPlace(@intCast(n));
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
pub fn stringToAtom(vm: *VM) anyerror!void { // TODO: radix..
    const string = try vm.stack.pop();
    vm.protect(string);
    if (string.getId() == .string) {
        try vm.bldr.newAtom(string.cast(.string).s);
    } else {
        return error.IllegalArgument;
    }
}
pub fn numberToString(vm: *VM) anyerror!void { // TODO: radix..
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
    try vm.stack.reverseInPlace(@intCast(n));
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
        res = @divTrunc(res, b);
        try vm.bldr.newIntNumber(res);
    }
}
pub fn _display(vm: *VM) anyerror!void {
    const a = try vm.stack.pop();
    a.print(); // todo: prettyprint..
    try vm.stack.push(_void);
}
pub fn _displaynl(vm: *VM) anyerror!void {
    const a = try vm.stack.pop();
    a.print(); // todo: prettyprint..
    std.debug.print("\n", .{});
    try vm.stack.push(_void);
}
pub fn _newline(vm: *VM) anyerror!void {
    std.debug.print("\n", .{});
    try vm.stack.push(_void);
}
pub fn _print(vm: *VM) anyerror!void {
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

pub fn isEq(vm: *VM) anyerror!void {
    const a = try vm.stack.pop();
    const b = try vm.stack.pop();
    try vm.bldr.newBool(a.equal(&b));
}

pub fn isEqv(vm: *VM) anyerror!void {
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
pub fn isEqual(vm: *VM) anyerror!void {
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
pub fn _inspect(vm: *VM) anyerror!void {
    const x = try vm.stack.pop();
    if (x.getId() != .procedure) {
        return error.IllegalArgument;
    }
    const p = x.cast(.procedure);
    std.debug.print("Procedure {} {} \n", .{p.numArgs, p.varargs});
    var i = p.code;
    while (true) :(i+=1) {
        const instr = vm.code.items[i];
        std.debug.print("{d}:\t ", .{i});
        instr.print();
        if (instr == .Return) {
            break;
        }
    }

    try vm.stack.push(_void);
}

pub const newline: Prim = .{.name = "newline", .exec = _newline,  .numArgs = 0};

pub const @"+": Prim = .{.name = "+", .exec = add,  .varargs = true};
pub const @"-": Prim = .{.name = "-", .exec = sub,  .varargs = true};
pub const @"/": Prim = .{.name = "/", .exec = _div, .varargs = true};
pub const @"*": Prim = .{.name = "*", .exec = _mul, .varargs = true};
pub const @"=": Prim = .{.name = "=", .exec = numEq,.varargs = true};

pub const @"error": Prim = .{.name = "error", .exec = _error,  .varargs = true};

pub const @"zero?": Prim = .{.name = "zero?", .exec = _isZero, .numArgs = 1};
pub const @"pair?": Prim = .{.name = "pair?", .exec = isPair,  .numArgs = 1};
pub const @"null?": Prim = .{.name = "null?", .exec = isNull,  .numArgs = 1};
pub const @"list?": Prim = .{.name = "list?", .exec = isList,  .numArgs = 1};
pub const @"positive?": Prim = .{.name = "positive?", .exec = isPositive,  .numArgs = 1};
pub const @"boolean?": Prim = .{.name = "boolean?", .exec = isBoolean,  .numArgs = 1};
pub const @"number?": Prim = .{.name = "number?", .exec = isNumber,  .numArgs = 1};
pub const @"symbol?": Prim = .{.name = "symbol?", .exec = isSymbol,  .numArgs = 1};
pub const @"integer?": Prim = .{.name = "integer?", .exec = isInteger,  .numArgs = 1};
pub const @"string?": Prim = .{.name = "string?", .exec = isString,  .numArgs = 1};
pub const @"real?": Prim = .{.name = "real?", .exec = isReal,  .numArgs = 1};
pub const not: Prim = .{.name = "not", .exec = _not,  .numArgs = 1};
pub const inspect: Prim = .{.name = "inspect", .exec = _inspect,  .numArgs = 1};


pub const display: Prim = .{.name = "display", .exec = _display,  .numArgs = 1};
pub const displaynl: Prim = .{.name = "displaynl", .exec = _displaynl,  .numArgs = 1};
pub const print: Prim = .{.name = "print", .exec = _print,  .numArgs = 1};


pub const car: Prim = .{.name = "car", .exec = _car,  .numArgs = 1};
pub const cdr: Prim = .{.name = "cdr", .exec = _cdr,  .numArgs = 1};

pub const @"number->string": Prim = .{.name = "number->string", .exec = numberToString,  .numArgs = 1};
pub const @"string->atom": Prim = .{.name = "string->atom", .exec = stringToAtom,  .numArgs = 1};
pub const @"string-append": Prim =    .{.name = "string-append",    .exec = stringAppend,     .varargs = true};
//         // .{ "string-append", @"string-append" },

pub const @"eq?": Prim =    .{.name = "eq?",    .exec = isEq,     .numArgs = 2};
pub const @"eqv?": Prim =   .{.name = "eqv?",   .exec = isEqv,    .numArgs = 2};
pub const @"equal?": Prim = .{.name = "equal?", .exec = isEqual,  .numArgs = 2};


pub const cons: Prim =    .{.name = "cons",    .exec = _cons,     .numArgs = 2};
pub const @"set-car!": Prim =    .{.name = "set-car!",    .exec = setCar,     .numArgs = 2};
pub const @"set-cdr!": Prim =    .{.name = "set-cdr!",    .exec = setCdr,     .numArgs = 2};



pub const @"apply": Prim = .{.name = "apply", .exec = _apply,  .numArgs = 2};

pub const timeStart: Prim = .{.name = "timeStart", .exec = _timeStart, .numArgs = 0};
pub const timeStop: Prim = .{.name = "timeStop", .exec = _timeStop, .numArgs = 1};


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
        bldr.newAtom("begin"),
          bldr.newList(&.{bldr.newAtom("timeStart")}),
          bldr.newList(&.{bldr.newAtom("timeStop"), params[0]}),
      });
}
pub fn defineMacro(bldr: AstBuilder, params: []*AstNode) anyerror!*AstNode {
    // TODO: check that it is run only in toplevel
    if (params.len < 2) {
        return error.ArityMismatch;
    }

    if (params[0].id == .list) {
        const args = params[0].cast(.list).xs;
        const name = args[0];
        const lambdaParams = bldr.newList(args[1..]);
        var lambda = bldr.emptyList(params.len + 1);
        lambda.cast(.list).xs[0] = bldr.newAtom("lambda");
        lambda.cast(.list).xs[1] = lambdaParams;
        for (1..params.len) |i| {
            lambda.cast(.list).xs[i+1] = params[i];
        }

        return bldr.newList(&.{bldr.newAtom("set-macro!"), name, lambda});

    } else if (params[0].id == .improperList) {
        const l = params[0].cast(.improperList);
        const args = l.xs;
        const name = args[0];
        const lambdaParams = bldr.newImproperList(args[1..], l.last);
        var lambda = bldr.emptyList(params.len + 1);
        lambda.cast(.list).xs[0] = bldr.newAtom("lambda");
        lambda.cast(.list).xs[1] = lambdaParams;
        for (1..params.len) |i| {
            lambda.cast(.list).xs[i+1] = params[i];
        }

        return bldr.newList(&.{bldr.newAtom("set-macro!"), name, lambda});
    } else {
        std.debug.print("can't define not atom, got: {}\n", .{params[0].id});
        return error.IllegalArgument;
    }
}
pub fn _define(bldr: AstBuilder, params: []*AstNode) anyerror!*AstNode {
    if (params.len < 2) {
        return error.ArityMismatch;
    }

    if (params[0].id == .atom) {
        if (params.len != 2) {
            return error.ArityMismatch;
        }

        return bldr.newList(&.{bldr.newAtom("set!"), params[0], params[1]});
    } else if (params[0].id == .list) {
        const args = params[0].cast(.list).xs;
        const name = args[0];
        const lambdaParams = bldr.newList(args[1..]);
        var lambda = bldr.emptyList(params.len + 1);
        lambda.cast(.list).xs[0] = bldr.newAtom("lambda");
        lambda.cast(.list).xs[1] = lambdaParams;
        for (1..params.len) |i| {
            lambda.cast(.list).xs[i+1] = params[i];
        }

        return bldr.newList(&.{bldr.newAtom("set!"), name, lambda});

    } else if (params[0].id == .improperList) {
        const l = params[0].cast(.improperList);
        const args = l.xs;
        const name = args[0];
        const lambdaParams = bldr.newImproperList(args[1..], l.last);
        var lambda = bldr.emptyList(params.len + 1);
        lambda.cast(.list).xs[0] = bldr.newAtom("lambda");
        lambda.cast(.list).xs[1] = lambdaParams;
        for (1..params.len) |i| {
            lambda.cast(.list).xs[i+1] = params[i];
        }

        return bldr.newList(&.{bldr.newAtom("set!"), name, lambda});
    } else {
        std.debug.print("can't define not atom, got: {}\n", .{params[0].id});
        return error.IllegalArgument;
    }


}


pub fn genCond(bldr: AstBuilder, params: []*AstNode) anyerror!*AstNode {
    if (params.len == 0) {
        return bldr.newList(&.{bldr.newAtom("begin")});
    }
    if (params[0].id != .list) {
        return error.BadSyntax;
    }
    const clause = params[0].cast(.list).xs;
    if (clause[0].id == .atom and std.mem.eql(u8, "else", clause[0].cast(.atom).name)) {
        if (clause.len == 2) {
            return clause[1];
        } else {
            @panic("not implemented");
        }
    }
    if (clause.len != 2) {
            @panic("not implemented");
    }
    return bldr.newList(&.{bldr.newAtom("if"), clause[0], clause[1], try genCond(bldr, params[1..])});
}
pub fn _cond(bldr: AstBuilder, params: []*AstNode) anyerror!*AstNode {
    // if (params.len == 0) {
    //     return bldr.newList(&.{bldr.newAtom("begin")});
    // }
    return genCond(bldr, params);
}

pub const define: Macro = .{.name = "define", .exec = _define };
pub const cond: Macro = .{.name = "cond", .exec = _cond };
pub const @"define-macro": Macro = .{.name = "define-macro", .exec = defineMacro };
pub const time: Macro = .{.name = "time", .exec = _time };
    // pub fn createInitialEnv(self: *VM) !void {
    //     _ = &self;
    // //     const xs = [_]struct { []const u8, *const fn (*VM) anyerror!void }{
    //         // .{ "apply", apply },
    //         // .{ "eval", __eval },
    //     // };
    //     // const dontEval = [_]struct { []const u8, *const fn (*VM) anyerror!void }{
    //         // .{ "and", __and },
    //         // .{ "or", __or },
    //         // .{ "cond", __cond },
    //         //
    //         // .{ "case", __case },
    //         // .{ "set!", @"set!" },
    //         // .{ "put!", @"put!" },
    //         // .{ "define-macro", @"define-macro" },
    //     // };
