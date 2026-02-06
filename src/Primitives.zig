const std = @import("std");
const VM = @import("main.zig").VM;
const _void = @import("Node.zig")._void;
const _true = @import("Node.zig")._true;
const _false = @import("Node.zig")._false;
const NodePtr = @import("Node.zig").NodePtr;
const Node = @import("Node.zig").Node;
const LexicalContext = @import("Compiler.zig").LexicalCtx;
const getEnv = @import("Env.zig").getEnv;


const Primitives = @This();

pub const Macro = struct {
    exec: *const fn (*VM, *LexicalContext, NodePtr) anyerror!NodePtr,
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


pub fn _apply(vm: *VM) anyerror!void {
    var args = try vm.stack.pop();
    const _f = try vm.stack.pop();
    vm.protect(args);
    vm.protect(_f);
    if (!(args.getId() == .pair or args.getId() == .nil)) {
        return error.ExpectedList;
    }

    try vm.save(1);

    var numArgs:u32 = 0;
    while (args.getId() != .nil) {
        try vm.stack.push(args.cast(.pair).fst);
        numArgs += 1;
        args = args.cast(.pair).snd;
    }
    try vm.stack.push(_f);
    try vm.jcall(numArgs);
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
// TODO: write macro?
pub fn _less(vm: *VM) anyerror!void {
    const n = (try vm.stack.pop()).getIntValue();
    try assertArityGreaterOrEq(1, n);
    try vm.stack.reverseInPlace(@intCast(n));
    var res = true;
    if (hasFloats(vm, n)) {
        const first: f32 = try (try vm.stack.pop()).convertToFloat();
        for (1..@intCast(n)) |_| {
            res &= (first < try (try vm.stack.pop()).convertToFloat());
        }
    } else {
        const first = (try (try vm.stack.pop()).tryGetIntValue());
        for (1..@intCast(n)) |_| {
            res &= first < (try (try vm.stack.pop()).tryGetIntValue());
        }
    }
    try vm.bldr.newBool(res);
}
pub fn _lessEq(vm: *VM) anyerror!void {
    const n = (try vm.stack.pop()).getIntValue();
    try assertArityGreaterOrEq(1, n);
    try vm.stack.reverseInPlace(@intCast(n));
    var res = true;
    if (hasFloats(vm, n)) {
        const first: f32 = try (try vm.stack.pop()).convertToFloat();
        for (1..@intCast(n)) |_| {
            res &= (first <= try (try vm.stack.pop()).convertToFloat());
        }
    } else {
        const first = (try (try vm.stack.pop()).tryGetIntValue());
        for (1..@intCast(n)) |_| {
            res &= first <= (try (try vm.stack.pop()).tryGetIntValue());
        }
    }
    try vm.bldr.newBool(res);
}
pub fn _greater(vm: *VM) anyerror!void {
    const n = (try vm.stack.pop()).getIntValue();
    try assertArityGreaterOrEq(1, n);
    try vm.stack.reverseInPlace(@intCast(n));
    var res = true;
    if (hasFloats(vm, n)) {
        const first: f32 = try (try vm.stack.pop()).convertToFloat();
        for (1..@intCast(n)) |_| {
            res &= (first > try (try vm.stack.pop()).convertToFloat());
        }
    } else {
        const first = (try (try vm.stack.pop()).tryGetIntValue());
        for (1..@intCast(n)) |_| {
            res &= first > (try (try vm.stack.pop()).tryGetIntValue());
        }
    }
    try vm.bldr.newBool(res);
}
pub fn _greaterEq(vm: *VM) anyerror!void {
    const n = (try vm.stack.pop()).getIntValue();
    try assertArityGreaterOrEq(1, n);
    try vm.stack.reverseInPlace(@intCast(n));
    var res = true;
    if (hasFloats(vm, n)) {
        const first: f32 = try (try vm.stack.pop()).convertToFloat();
        for (1..@intCast(n)) |_| {
            res &= (first >= try (try vm.stack.pop()).convertToFloat());
        }
    } else {
        const first = (try (try vm.stack.pop()).tryGetIntValue());
        for (1..@intCast(n)) |_| {
            res &= first >= (try (try vm.stack.pop()).tryGetIntValue());
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
pub fn isVector(vm: *VM) anyerror!void {
    const n = try vm.stack.pop();
    try vm.bldr.newBool(n.getId() == .vector);
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
    if (string.getId() == .string) {
        try vm.bldr.newAtom(string.cast(.string).s);
    } else {
        return error.IllegalArgument;
    }
}
pub fn atomToString(vm: *VM) anyerror!void { // TODO: radix..
    const atom = try vm.stack.pop();
    if (atom.getId() == .atom) {
        try vm.bldr.newString(atom.cast(.atom).name);
    } else {
        return error.IllegalArgument;
    }
}
pub fn numberToString(vm: *VM) anyerror!void { // TODO: radix..
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

pub fn mkVector(vm: *VM) anyerror!void {
    const n = (try vm.stack.pop()).getIntValue();
    var el: NodePtr = undefined;
    if (n != 1 and n != 2) {
        return error.ArityMismatch;
    }
    const populate = n == 2;
    if (populate) {
        el = try vm.stack.pop();
    }
    const size = (try vm.stack.pop()).getIntValue();
    if (size < 0) {
        return error.IllegalArgument;
    }
    if (populate) {
        for (0..@intCast(size)) |_| {
            try vm.stack.push(el);
        }
    }
    try vm.bldr.newVector(@intCast(size), populate);
}
pub fn _vector(vm: *VM) anyerror!void {
    const n = (try vm.stack.pop()).getIntValue();
    try vm.bldr.newVector(@intCast(n), true);
}
pub fn _vectorLength(vm: *VM) anyerror!void {
    const v = try vm.stack.pop();
    const vec = try v.tryCast(.vector);
    try vm.bldr.newIntNumber(@intCast(vec.xs.len));
}
pub fn _vectorRef(vm: *VM) anyerror!void {
    const n = try vm.stack.pop();
    const v = try vm.stack.pop();
    const vec = try v.tryCast(.vector);
    const idx = try n.tryGetIntValue();
    if (idx < 0) {
        return error.IllegalArgument;
    }
    if (idx >= vec.xs.len) {
        return error.IndexTooLarge;
    }
    try vm.stack.push(vec.xs[@intCast(idx)]);
}

pub fn _vectorSet(vm: *VM) anyerror!void {
    const obj = try vm.stack.pop();
    const n = try vm.stack.pop();
    const v = try vm.stack.pop();
    const vec = try v.tryCast(.vector);
    const idx = try n.tryGetIntValue();
    if (idx < 0) {
        return error.IllegalArgument;
    }
    if (idx >= vec.xs.len) {
        return error.IndexTooLarge;
    }
    vec.xs[@intCast(idx)] = obj;
    try vm.stack.push(_void);
}



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
// pub fn retrieveArg(vm: *VM) anyerror!void {
//     const pos = (try vm.stack.pop()).getIntValue();
//     const level = (try vm.stack.pop()).getIntValue();
//     const value = getEnv(vm.env, level, pos);
//     if (value) |v| {
//         try vm.stack.push(v);
//     } else {
//         return error.UnknownName;
//     }
// }

// regular closure that returns to(?) continuation
pub fn _cc(vm: *VM) anyerror!void {
    _ = &vm;
}

// return closure that when called returns stack to this position..
pub fn _callcc(vm: *VM) anyerror!void {
    const returnAddr = vm.ip + 1;
    const f = try vm.stack.pop();
    vm.protect(f);

    const stackLen = vm.stack.size;
    try vm.bldr.newList();
    try vm.bldr.newVector(stackLen, false);
    const vec = (try vm.stack.head()).cast(.vector);
    for (0..stackLen) |i| {
        vec.xs[i] = vm.stack.items[i];
    }
    try vm.bldr.appendToList();
    try vm.stack.push(vm.closure);
    try vm.bldr.appendToList();
    try vm.bldr.newIntNumber(@intCast(vm.frame));
    try vm.bldr.appendToList();
    try vm.bldr.newIntNumber(@intCast(returnAddr));
    try vm.bldr.appendToList();


    try vm.bldr.newEnv(1, true);
    try vm.bldr.newProc(vm.ccCodeLoc, false, 1);
    const cc = try vm.stack.pop();
    vm.protect(cc);
    try vm.save(1);
    try vm.stack.push(cc);

    try vm.stack.push(f);
    try vm.jcall(1);
}

pub const newline: Prim = .{.name = "newline", .exec = _newline,  .numArgs = 0};

pub const @"+": Prim = .{.name = "+", .exec = add,  .varargs = true};
pub const @"-": Prim = .{.name = "-", .exec = sub,  .varargs = true};
pub const @"/": Prim = .{.name = "/", .exec = _div, .varargs = true};
pub const @"*": Prim = .{.name = "*", .exec = _mul, .varargs = true};
pub const @"=": Prim = .{.name = "=", .exec = numEq,.varargs = true};

pub const @"<": Prim = .{.name = "<", .exec = _less,.varargs = true};
pub const @"<=": Prim = .{.name = "<=", .exec = _lessEq,.varargs = true};
pub const @">": Prim = .{.name = ">", .exec = _greater,.varargs = true};
pub const @">=": Prim = .{.name = ">=", .exec = _greaterEq,.varargs = true};

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

pub const @"call/cc": Prim = .{.name = "call/cc", .exec = _callcc,  .numArgs = 1};


pub const display: Prim = .{.name = "display", .exec = _display,  .numArgs = 1};
pub const displaynl: Prim = .{.name = "displaynl", .exec = _displaynl,  .numArgs = 1};
pub const print: Prim = .{.name = "print", .exec = _print,  .numArgs = 1};


pub const car: Prim = .{.name = "car", .exec = _car,  .numArgs = 1};
pub const cdr: Prim = .{.name = "cdr", .exec = _cdr,  .numArgs = 1};

pub const @"number->string": Prim = .{.name = "number->string", .exec = numberToString,  .numArgs = 1};
pub const @"string->symbol": Prim = .{.name = "string->symbol", .exec = stringToAtom,  .numArgs = 1};
pub const @"symbol->string": Prim = .{.name = "symbol->string", .exec = atomToString,  .numArgs = 1};
pub const @"string-append": Prim =    .{.name = "string-append",    .exec = stringAppend,     .varargs = true};

pub const @"eq?": Prim =    .{.name = "eq?",    .exec = isEq,     .numArgs = 2};
pub const @"eqv?": Prim =   .{.name = "eqv?",   .exec = isEqv,    .numArgs = 2};
pub const @"equal?": Prim = .{.name = "equal?", .exec = isEqual,  .numArgs = 2};


pub const cons: Prim =    .{.name = "cons",    .exec = _cons,     .numArgs = 2};
pub const @"set-car!": Prim =    .{.name = "set-car!",    .exec = setCar,     .numArgs = 2};
pub const @"set-cdr!": Prim =    .{.name = "set-cdr!",    .exec = setCdr,     .numArgs = 2};



pub const @"apply": Prim = .{.name = "apply", .exec = _apply,  .numArgs = 2};

pub const timeStart: Prim = .{.name = "timeStart", .exec = _timeStart, .numArgs = 0};
pub const timeStop: Prim = .{.name = "timeStop", .exec = _timeStop, .numArgs = 1};


pub const @"vector?": Prim = .{.name = "vector?", .exec = isVector,  .numArgs = 1};
pub const @"make-vector": Prim =    .{.name = "make-vector",    .exec = mkVector,     .varargs = true};
pub const @"vector": Prim =    .{.name = "vector",    .exec = _vector,     .varargs = true};
pub const @"vector-length": Prim =    .{.name = "vector-length",    .exec = _vectorLength, .numArgs = 1};
pub const @"vector-ref": Prim =    .{.name = "vector-length",    .exec = _vectorRef, .numArgs = 2};
pub const @"vector-set!": Prim =    .{.name = "vector-set!",    .exec = _vectorSet, .numArgs = 3};

// ------------------------------ macros --------------------------------------


pub fn _time(vm: *VM, _:*LexicalContext, params: NodePtr) anyerror!NodePtr {
    // if (params.len != 1) {
    //     return error.ArityMismatch;
    // }
    try vm.bldr.newList();
        try vm.bldr.newList();
        try vm.stack.push(try params.tryHead());
        try vm.bldr.appendToList();
        try vm.bldr.newAtom("timeStop");
        try vm.bldr.appendToList();
    try vm.bldr.appendToList();
        try vm.bldr.newList();
        try vm.bldr.newAtom("timeStart");
        try vm.bldr.appendToList();
    try vm.bldr.appendToList();
    try vm.bldr.newAtom("begin");
    try vm.bldr.appendToList();
    const res =  try vm.stack.pop();
    try vm.protectCompile.push(res);
    return res;
}

pub fn defineMacro(vm: *VM, lc: *LexicalContext, params: NodePtr) anyerror!NodePtr {
    // if (params.len < 2) {
    //     return error.ArityMismatch;
    // }
    if (lc.rib != null) {
        return error.IllegalDefine;
    }

    const pair = try params.tryCast(.pair);

    if (pair.fst.getId() == .pair) {
        var args = pair.fst;
        const name = (try args.tryCast(.pair)).fst;
        const lambdaParams = (try args.tryCast(.pair)).snd;

        try vm.bldr.newList();
            try vm.stack.push(pair.snd);
            try vm.stack.push(lambdaParams);
            try vm.bldr.appendToList();
            try vm.bldr.newAtom("lambda");
            try vm.bldr.appendToList();
        try vm.bldr.appendToList();

        try vm.stack.push(name);
        try vm.bldr.appendToList();
        try vm.bldr.newAtom("set-macro!");
        try vm.bldr.appendToList();

        const res = try vm.stack.pop();
        // res.debugprint("result::: ");
        try vm.protectCompile.push(res);
        return res;

    } else {
        std.debug.print("can't define not atom, got: {}\n", .{pair.fst.getId()});
        return error.IllegalArgument;
    }
}
pub fn _define(vm: *VM, lc: *LexicalContext, params: NodePtr) anyerror!NodePtr {
    // if (params.len < 2) {
    //     return error.ArityMismatch;
    // }
    if (lc.rib != null) {
        return error.IllegalDefine;
    }

    const pair = try params.tryCast(.pair);
    if (pair.fst.getId() == .atom) {
        // if (params.len != 2) {
        //     return error.ArityMismatch;
        // }
        try vm.bldr.newList();
        try vm.stack.push(try params.second());
        try vm.bldr.appendToList();
        try vm.stack.push(try params.tryHead());
        try vm.bldr.appendToList();
        try vm.bldr.newAtom("set!");
        try vm.bldr.appendToList();
        const res = try vm.stack.pop();
        try vm.protectCompile.push(res);
        return res;

    } else if (pair.fst.getId() == .pair) {
        var args = pair.fst;
        const name = (try args.tryCast(.pair)).fst;
        const lambdaParams = (try args.tryCast(.pair)).snd;

        try vm.bldr.newList();
            try vm.stack.push(pair.snd);
            try vm.stack.push(lambdaParams);
            try vm.bldr.appendToList();
            try vm.bldr.newAtom("lambda");
            try vm.bldr.appendToList();
        try vm.bldr.appendToList();

        try vm.stack.push(name);
        try vm.bldr.appendToList();
        try vm.bldr.newAtom("set!");
        try vm.bldr.appendToList();

        const res = try vm.stack.pop();
        try vm.protectCompile.push(res);
        return res;

    } else {
        std.debug.print("can't define not atom, got: {}\n", .{pair.fst.getId()});
        return error.IllegalArgument;
    }


}


pub fn genCond(vm: *VM, params: NodePtr) anyerror!void {
    if (params.getId() == .nil) {
        try vm.bldr.newList();
        try vm.bldr.newAtom("begin");
        try vm.bldr.appendToList();
        return;
    }
    if ((try params.tryCast(.pair)).fst.getId() != .pair) {
        return error.BadSyntax;
    }
    const clause = (try params.tryHead());
    const condition = try clause.tryHead();
    if (condition.getId() == .atom and std.mem.eql(u8, "else", condition.cast(.atom).name)) {
        try vm.stack.push(try clause.second()); // or snd? TODO: need to append begin?
        return;
    }
    try vm.bldr.newList();
    try genCond(vm, params.tail());
    try vm.bldr.appendToList();
    try vm.stack.push(try clause.tryTail());
    try vm.bldr.newAtom("begin");
    try vm.bldr.appendToList();

    try vm.bldr.appendToList();
    try vm.stack.push(condition);
    try vm.bldr.appendToList();
    try vm.bldr.newAtom("if");
    try vm.bldr.appendToList();
}
pub fn _cond(vm: *VM, _: *LexicalContext, params: NodePtr) anyerror!NodePtr {
    try genCond(vm, params);

    const res = try vm.stack.pop();
    // res.debugprint("result:");
    try vm.protectCompile.push(res);
    return res;
}

pub fn qqExpandList(vm: *VM, xs: NodePtr,  depth: usize) anyerror!void {
    switch (xs.getId()) {
        .atom, .nil => { 
            try vm.bldr.newList();
                try vm.bldr.newList();
                try vm.stack.push(xs);
                try vm.bldr.appendToList();
                try vm.bldr.newAtom("quote");
                try vm.bldr.appendToList();
            try vm.bldr.appendToList();
            try vm.bldr.newAtom("list");
            try vm.bldr.appendToList();

        },
        .intNumber,
        .floatNumber,
        .string,
        .bool => {
            try vm.bldr.newList();
            try vm.stack.push(xs);
            try vm.bldr.appendToList();
            try vm.bldr.newAtom("list");
            try vm.bldr.appendToList();

        },
        // .nil => {
        //     try vm.stack.push(x);
        // },
        .pair => {
            const head = xs.cast(.pair).fst;

            if (head.getId() == .atom) {
                if (std.mem.eql(u8, "quasiquote", head.cast(.atom).name)) {
                    try vm.bldr.newList();
                        try vm.bldr.newList();
                        try qqExpand(vm, xs.tail(), depth + 1);
                        try vm.bldr.appendToList();
                            try vm.bldr.newList();
                            try vm.bldr.newAtom("quasiquote");
                            try vm.bldr.appendToList();
                            try vm.bldr.newAtom("quote");
                            try vm.bldr.appendToList();
                        try vm.bldr.appendToList();

                        try vm.bldr.newAtom("cons");
                        try vm.bldr.appendToList();
                    try vm.bldr.appendToList();
                    try vm.bldr.newAtom("list");
                    try vm.bldr.appendToList();
                    return;
                } else if ( std.mem.eql(u8, "unquote", head.cast(.atom).name)
                        or (std.mem.eql(u8, "unquote-splicing", head.cast(.atom).name))) {
                    if (depth == 0) {
                        if (std.mem.eql(u8, "unquote", head.cast(.atom).name)) {
                            try vm.stack.push(xs.tail());
                            try vm.bldr.newAtom("list");
                            try vm.bldr.appendToList();
                           return;
                        } else {
                            // try vm.bldr.newList();
                            try vm.stack.push(xs.tail());
                            try vm.bldr.newAtom("append");
                            try vm.bldr.appendToList();
                            return;
                        }
                    } else {
                        try vm.bldr.newList();
                            try vm.bldr.newList();
                            try qqExpand(vm, xs.tail(), depth - 1);
                            try vm.bldr.appendToList();

                                try vm.bldr.newList();
                                try vm.stack.push(xs.head());
                                try vm.bldr.appendToList();
                                try vm.bldr.newAtom("quote");
                                try vm.bldr.appendToList();
                            try vm.bldr.appendToList();

                            try vm.bldr.newAtom("cons");
                            try vm.bldr.appendToList();
                        try vm.bldr.appendToList();
                        try vm.bldr.newAtom("list");
                        try vm.bldr.appendToList();
                        return;

                    }
            }
            }

            try vm.bldr.newList();

                try vm.bldr.newList();
                try qqExpand(vm, xs.tail(), depth);
                try vm.bldr.appendToList();
                try qqExpandList(vm, xs.head(), depth);
                try vm.bldr.appendToList();
                try vm.bldr.newAtom("append");
                try vm.bldr.appendToList();

            try vm.bldr.appendToList();


            try vm.bldr.newAtom("list");
            try vm.bldr.appendToList();
        },
        else => {
            @panic("not implemented");
        }
    }

}

// direct translation of Quasiquotation in Lisp (Bawden).pdf (appendix B)
pub fn qqExpand(vm: *VM, x: NodePtr, depth: usize) anyerror!void {
    switch (x.getId()) {
        .atom, .nil => { 
            // try vm.bldr.newList();
                try vm.bldr.newList();
                try vm.stack.push(x);
                try vm.bldr.appendToList();
                try vm.bldr.newAtom("quote");
                try vm.bldr.appendToList();
            // try vm.bldr.appendToList();
            // try vm.bldr.newAtom("list");
            // try vm.bldr.appendToList();
        },
        .intNumber,
        .floatNumber,
        .string,
        .bool => {
            // try vm.bldr.newList();
            try vm.stack.push(x);
            // try vm.bldr.appendToList();
            // try vm.bldr.newAtom("list");
            // try vm.bldr.appendToList();
            // try vm.stack.push(x);
        },
        // .nil => {
        //     try vm.stack.push(x);
        // },
        .pair => {
            const head = x.cast(.pair).fst;
            if (head.getId() == .atom) {
                if (std.mem.eql(u8, "quasiquote", head.cast(.atom).name)) {
                    try vm.bldr.newList();
                    try qqExpand(vm, x.tail(), depth + 1);
                    try vm.bldr.appendToList();
                        try vm.bldr.newList();
                        try vm.bldr.newAtom("quasiquote");
                        try vm.bldr.appendToList();
                        try vm.bldr.newAtom("quote");
                        try vm.bldr.appendToList();
                    try vm.bldr.appendToList();

                    try vm.bldr.newAtom("cons");
                    try vm.bldr.appendToList();
                    return;
                } else if ( std.mem.eql(u8, "unquote", head.cast(.atom).name)
                        or (std.mem.eql(u8, "unquote-splicing", head.cast(.atom).name))) {
                    if (depth == 0) {
                        if (std.mem.eql(u8, "unquote", head.cast(.atom).name)
                            and (x.tail().getId() != .nil)
                            and (x.tail().tail().getId() == .nil)) {
                            try vm.stack.push(x.tail().head());
                            return;
                        } else {
                            return error.IllegalArgument;
                        }
                    } else {
                        try vm.bldr.newList();
                        try qqExpand(vm, x.tail(), depth - 1);
                        try vm.bldr.appendToList();

                            try vm.bldr.newList();
                            try vm.stack.push(x.head());
                            try vm.bldr.appendToList();
                            try vm.bldr.newAtom("quote");
                            try vm.bldr.appendToList();
                        try vm.bldr.appendToList();

                        try vm.bldr.newAtom("cons");
                        try vm.bldr.appendToList();
                        return;

                    }
                } 
            }
            try vm.bldr.newList();
            try qqExpand(vm, x.tail(), depth);
            try vm.bldr.appendToList();
            try qqExpandList(vm, x.head(), depth);
            try vm.bldr.appendToList();
            try vm.bldr.newAtom("append");
            try vm.bldr.appendToList();
        },
        else => { 
            @panic("not implemented");
        },
    }
}


pub fn _quasiquote(vm: *VM, _: *LexicalContext, params: NodePtr) anyerror!NodePtr {
    if (params.len() != 1) {
        return error.BadSyntax;
    }
    // params[0].debugprint("quasiquote: ");
    try qqExpand(vm, params.head(), 0);
    // r.@"0".debugprint("quasiquote expansion");

    const res = try vm.stack.pop();
    // res.debugprint("res>>> ");
    try vm.protectCompile.push(res);
    return res;
}


pub const define: Macro = .{.name = "define", .exec = _define };
pub const cond: Macro = .{.name = "cond", .exec = _cond };
pub const quasiquote: Macro = .{.name = "quasiquote", .exec = _quasiquote };

pub const @"define-macro": Macro = .{.name = "define-macro", .exec = defineMacro };
pub const time: Macro = .{.name = "time", .exec = _time };
