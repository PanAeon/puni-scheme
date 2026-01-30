const std = @import("std");
const VM = @import("main.zig").VM;
const AstNode = @import("Parser.zig").AstNode;


const Primitives = @This();

pub const Macro = struct {
    exec: *const fn (std.mem.Allocator, []*AstNode) anyerror!*AstNode,
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

pub const @"+": Prim = .{.name = "+", .exec = add, .varargs = true};
pub const @"-": Prim = .{.name = "-", .exec = sub, .varargs = true};
pub const @"zero?": Prim = .{.name = "zero?", .exec = _isZero,  .numArgs = 1};

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

pub fn _define(arena: std.mem.Allocator, params: []*AstNode) anyerror!*AstNode {
    if (params.len < 2) {
        return error.ArityMismatch;
    }

    if (params[0].id == .atom) {
        if (params.len != 2) {
            return error.ArityMismatch;
        }

        return newList(arena, &.{try newAtom(arena, "set!"), params[0], params[1]});
    } else if (params[0].id == .list) {
        const args = params[0].cast(.list).xs;
        const name = args[0];
        const lambdaParams = try newList(arena, args[1..]);
        var xs = try arena.alloc(*AstNode, params.len + 1);
        xs[0] = try newAtom(arena, "lambda");
        xs[1] = lambdaParams;
        for (1..params.len) |i| {
            xs[i+1] = params[i];
        }

        const lambda = try newList(arena,xs);
        return newList(arena, &.{try newAtom(arena, "set!"), name, lambda});

    } else if (params[0].id == .improperList) {
        @panic("not implemented");
    } else {
        std.debug.print("can't define not atom, got: {}\n", .{params[0].id});
        return error.IllegalArgument;
    }


}

pub const define: Macro = .{.name = "define", .exec = _define };
    // pub fn createInitialEnv(self: *VM) !void {
    //     _ = &self;
    // //     const xs = [_]struct { []const u8, *const fn (*VM) anyerror!void }{
    //         // .{ "+", __add },
    //         // .{ "-", __sub },
    //         // .{ "/", __div },
    //         // .{ "*", __mul },
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
    //         // .{ "positive?", @"positive?" },
    //         // .{ "__set1", __set1 },
    //         // .{ "apply", apply },
    //         // .{ "boolean?", @"boolean?" },
    //         // .{ "number?", @"number?" },
    //         // .{ "symbol?", @"symbol?" },
    //         // .{ "macro?", @"macro?" },
    //         // .{ "integer?", @"integer?" },
    //         // .{ "string?", @"integer?" },
    //         // .{ "real?", @"real?" },
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

