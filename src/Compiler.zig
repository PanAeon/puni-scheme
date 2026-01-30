const std = @import("std");
const AstNode = @import("Parser.zig").AstNode;
const VM = @import("main.zig").VM;
const Instruction = @import("main.zig").Instruction;
const Primitives = @import("Primitives.zig");
const NodePtr = @import("Node.zig").NodePtr;
const _true = @import("Node.zig")._true;
const _false = @import("Node.zig")._false;
const _void = @import("Node.zig")._void;

const Compiler = @This();

pub const CompileError = error {
   EmptyApplication,
   NotAProcedure,
   WrongSyntax,
   ArityMismatch,
};


pub const LexicalCtx = struct {
    rib: ?*Rib = null,

    const Rib = struct {
       next: ?*Rib = null,
       params: [][]const u8,
    };

    pub fn push(self: *LexicalCtx, arena: std.mem.Allocator, params: []const []const u8) void {
        const rib = arena.create(Rib) catch @panic("oom");
        rib.* = .{
            .next = self.rib,
            .params = arena.dupe([]const u8, params) catch @panic("oom"),
        };
        self.rib = rib;
    }
    pub fn pop(self: *LexicalCtx) void {
        if (self.rib) |r| {
            self.rib = r.next;
        } else {
            @panic("stack underflow");
        }
    }

    pub fn findArg(self: LexicalCtx, name: []const u8) ?struct{u16,u16} {
        var rib = self.rib;
        var i:u16 = 0;
        while (rib) |r| {
            for (r.params, 0..) |p,j| {
                if (std.mem.eql(u8, p, name)) {
                    return .{i, @intCast(j)};
                }
            }
            rib = r.next;
            i += 1;
        }
        return null;
    }
};


arena: std.mem.Allocator,
allocator: std.mem.Allocator,
vm: *VM,
codePoint: usize = 0,
primitives: std.StringHashMap(*const Primitives.Prim),
macros: std.StringHashMap(*const Primitives.Macro),

pub fn init(arena: std.mem.Allocator, allocator: std.mem.Allocator, vm: *VM) Compiler {
    return .{ .arena = arena, .vm = vm, .primitives = std.StringHashMap(*const Primitives.Prim).init(allocator),
              .macros = std.StringHashMap(*const Primitives.Macro).init(allocator),
     .allocator = allocator };
}

pub fn deinit(self: *Compiler) void {
    self.primitives.deinit();
    self.macros.deinit();
}


pub fn getPrims(self: *Compiler) void {
    inline for (@typeInfo(Primitives).@"struct".decls) |decl| {
        const value = @field(Primitives, decl.name);
        if (@TypeOf(value) == Primitives.Prim) {
            self.primitives.put(decl.name, &value) catch @panic("oom..");
            // std.debug.print("got {s}\n", .{decl.name});

        } else if (@TypeOf(value) == Primitives.Macro) {
            self.macros.put(decl.name, &value) catch @panic("oom..");
        }
    }
}

// push code => vm.code and constants => vm.constants
pub fn compile(self: *Compiler, xs: []*AstNode) anyerror!i64 {
    var buff = try std.ArrayList(Instruction).initCapacity(self.arena, 256);
    var lexicalCtx: LexicalCtx = .{};
    const codeRestore = self.vm.code.items.len;
    const dataRestore = self.vm.data.items.len;
    for (xs) |x| {
        self.genExpr(x, &buff, &lexicalCtx) catch |e| {
            self.vm.code.shrinkRetainingCapacity(codeRestore);
            self.vm.data.shrinkRetainingCapacity(dataRestore);
            return e;
        };
    }
    const start = self.vm.code.items.len;
    try self.vm.code.appendSlice(self.allocator, buff.items);
    try self.vm.code.append(self.allocator, .Halt);
    return @intCast(start);
}
pub fn genExpr(self: *Compiler, ast: *AstNode, buffer: *std.ArrayList(Instruction), lexicalCtx: *LexicalCtx) anyerror!void {
    switch(ast.id) {
        .list => {
            const xs = ast.cast(.list).xs;
            if (xs.len == 0) {
                return error.EmptyApplication;
            }
            if (xs[0].id == .atom) { // FIXME: ((something returns prim?) 3)
                const name = xs[0].cast(.atom).name;
                if (std.mem.eql(u8, "set!", name)) { // TODO: set is not allowed to create new global vars
                    if (xs.len != 3) {
                        return error.ArityMismatch;
                    }
                    try self.genSet(xs[1], xs[2], buffer, lexicalCtx);
                    return;
                } else if (std.mem.eql(u8, "begin", name)) {
                    try self.genBegin(xs[1..], buffer, lexicalCtx);
                    return;
                } else if (std.mem.eql(u8, "if", name)) { // TODO: if could be (if #t body)
                    if (xs.len != 4) {
                        return error.WrongSyntax;
                    }
                    try self.genIf(xs[1], xs[2], xs[3], buffer, lexicalCtx);
                    return;
                } else if (std.mem.eql(u8, "quote", name)) {
                    if (xs.len != 2) {
                        return error.WrongSyntax;
                    }
                    try self.genQuote(xs[1], buffer);
                    return;
                } else if (std.mem.eql(u8, "lambda", name)) {
                    if (xs.len < 3) {
                        return error.WrongSyntax;
                    }
                    try self.genLambda(xs[1], xs[2..], buffer, lexicalCtx);
                    return;
                } else if (self.primitives.get(name)) |prim| {
                    try self.genPrim(prim, ast.cast(.list).xs[1..], buffer, lexicalCtx);
                    return;
                } else if (self.macros.get(name)) |macro| {
                    const transformed = try macro.exec(self.arena, xs[1..]);
                    try self.genExpr(transformed, buffer, lexicalCtx);
                    return;
                }
            }
            try self.genAp(ast.cast(.list).xs, buffer, lexicalCtx);
        },
        .intNumber => {
            const n = ast.cast(.intNumber).value;
            try buffer.append(self.arena, .{.Const = NodePtr.initU64(@as(u48, @bitCast(n)), .{ .id = .intNumber }) });
        },
        .floatNumber => {
            const n = ast.cast(.floatNumber).value;
            try buffer.append(self.arena, .{.Const = NodePtr.initU64(@as(u32, @bitCast(n)), .{ .id = .floatNumber }) });
        },
        .bool => {
            const b = ast.cast(.bool).value;
            const v = if (b) _true else _false;
            try buffer.append(self.arena, .{.Const = v  });
        },
        .string => {
            const s = ast.cast(.string).s;
            try self.vm.bldr.newString(s);
            const node = try self.vm.stack.pop();
            try self.vm.data.append(self.allocator, node);
            try buffer.append(self.arena, .{ .Const = node } );

        },
        .atom => {
            const name = ast.cast(.atom).name;
            if (lexicalCtx.findArg(name)) |pos| {
                try buffer.append(self.arena, .{ .LVar = pos } );
                return;
            } else {
                try self.vm.bldr.newAtom(name);
                const node = try self.vm.stack.pop();
                try self.vm.data.append(self.allocator, node);
                try buffer.append(self.arena, .{ .GVar = node });
            }
            // try self.vm.bldr.newAtom(name);
            // const node = try self.vm.stack.pop();
            // try self.vm.data.append(self.allocator, node);
            // try buffer.append(self.arena, .{ .Const = node } );

        },
        .quote => {
            const x = ast.cast(.quote).value;
            try self.genQuote(x, buffer);
        },
        else => @panic("not implemented"),
        // improperList,
        // quote,
        // quasiquote,
        // unquote,
        // unquoteSplicing,
        // vector,
    }
}


pub fn genLambda(self: *Compiler, params: *AstNode, bodies: []*AstNode, buffer: *std.ArrayList(Instruction), lexicalCtx: *LexicalCtx) anyerror!void {
    var lambdaBuff = try std.ArrayList(Instruction).initCapacity(self.arena, 256);
    var numParams: usize = 0;
    var varargs: bool = false;
    if (params.id == .list) {
        const xs = params.cast(.list).xs;
        var buff = try self.arena.alloc([]const u8, xs.len);
        for (xs, 0..) |x,i| {
            if (x.id != .atom) {
                return error.WrongSyntax;
            }
            buff[i] = x.cast(.atom).name;
        }
        lexicalCtx.push(self.arena, buff[0..]);
        numParams = xs.len; 
    } else if (params.id == .improperList) {
        const l = params.cast(.improperList);
        numParams = l.xs.len + 1;
        varargs = true;
        var buff = try self.arena.alloc([]const u8, numParams);
        for (l.xs, 0..) |x,i| {
            if (x.id != .atom) {
                return error.WrongSyntax;
            }
            buff[i] = x.cast(.atom).name;
        }
        if (l.last.id != .atom) {
            return error.WrongSyntax;
        }
        buff[numParams - 1] = l.last.cast(.atom).name;
        lexicalCtx.push(self.arena, buff[0..]);
    } else if (params.id == .atom) {
        lexicalCtx.push(self.arena, &.{params.cast(.atom).name});
        numParams = 1;
        varargs = true;
    } else {
        return error.WrongSyntax;
    }
    try lambdaBuff.append(self.arena, .{ .Args = numParams } );
    for (0..bodies.len - 1) |i| {
        try self.genExpr(bodies[i], &lambdaBuff, lexicalCtx);
        try lambdaBuff.append(self.arena, .Pop );
    }
    try self.genExpr(bodies[bodies.len - 1], &lambdaBuff, lexicalCtx);
    try lambdaBuff.append(self.arena, .Return );


    const offset = self.vm.code.items.len;
    try self.vm.code.appendSlice(self.allocator, lambdaBuff.items);

    // try self.vm.bldr.newEnv(numParams); // the environment is wrong, must create procedure in code..
    // try self.vm.bldr.newProc(@intCast(offset), varargs);
    // const proc = try self.vm.stack.pop();
    // try self.vm.data.append(self.allocator, proc);
    try buffer.append(self.arena, .{ .Fn = .{ .code = @intCast(offset), .varargs = varargs, .numArgs = @intCast(numParams) }} );
    lexicalCtx.pop();
    // return const lambda I suppose..
}

pub fn genIf(self: *Compiler, cond: *AstNode, branch0: *AstNode, branch1: *AstNode, buffer: *std.ArrayList(Instruction), lexicalCtx: *LexicalCtx) anyerror!void {
    try self.genExpr(cond, buffer, lexicalCtx);
    try buffer.append(self.arena, .{ .FJump = 0 });
    const fJumpIdx = buffer.items.len-1;
    try self.genExpr(branch0, buffer, lexicalCtx);
    try buffer.append(self.arena, .{ .Jump = 0 });
    const endJumpIdx = buffer.items.len-1;
    const fIdx = buffer.items.len;
    try self.genExpr(branch1, buffer, lexicalCtx);
    const endIdx = buffer.items.len;
    // now we can set jumps as we know the length..
    _ = &fJumpIdx;
    _ = &endJumpIdx;
    buffer.items[endJumpIdx].Jump = @intCast(endIdx - endJumpIdx);
    buffer.items[fJumpIdx].FJump = @intCast(fIdx - fJumpIdx);
}
pub fn genPrim(self: *Compiler, prim: *const Primitives.Prim, params: []*AstNode, buffer: *std.ArrayList(Instruction), lexicalCtx: *LexicalCtx) anyerror!void {
    for (params) |p| {
        try self.genExpr(p, buffer, lexicalCtx);
    }
    if (prim.varargs) {
        const n: i48 = @intCast(params.len);
        try buffer.append(self.arena, .{.Const = NodePtr.initU64(@as(u48, @bitCast(n)), .{ .id = .intNumber }) });
    } else {
        if (prim.numArgs != params.len) {
            return error.ArityMismatch;
        }
    }
    try buffer.append(self.arena, .{.Primitive = prim });
}
pub fn genAp(self: *Compiler, xs: []*AstNode, buffer: *std.ArrayList(Instruction), lexicalCtx: *LexicalCtx) anyerror!void {
    const f = xs[0];

    const params = xs[1..];

    for (params) |p| {
        try self.genExpr(p, buffer, lexicalCtx);
    }

    try self.genExpr(f, buffer, lexicalCtx);

    try buffer.append(self.arena, .{ .Call = @intCast(params.len) });


    // if (f.id == .atom) {
    // } else if (f.id == .list) {
    //     try self.genAp(f.cast(.list).xs, buffer);
    // } else {
    //     return error.NotAProcedure;
    // }
    // if f is atom
    // if f is primitive gen primitive application
    // otherwise it is 
}

// can't we do (set! (expr ..) (expr)) ???
pub fn genSet(self: *Compiler, nameNode: *AstNode, expr: *AstNode, buffer: *std.ArrayList(Instruction), lexicalCtx: *LexicalCtx) anyerror!void {
    if (nameNode.id != .atom) {
        return error.WrongSyntax;
    }
    const name = nameNode.cast(.atom).name;
    try self.genExpr(expr, buffer, lexicalCtx);

    if (lexicalCtx.findArg(name)) |pos| {
        try buffer.append(self.arena, .{ .LSet = pos } );
        return;
    } else {
        try self.vm.bldr.newAtom(name);
        const node = try self.vm.stack.pop();
        try self.vm.data.append(self.allocator, node);
        try buffer.append(self.arena, .{ .GSet = node });
        try buffer.append(self.arena, .{ .Const = _void });
    }


}
pub fn genBegin(self: *Compiler, bodies: []*AstNode, buffer: *std.ArrayList(Instruction), lexicalCtx: *LexicalCtx) anyerror!void {
    if (bodies.len == 0) {
        try buffer.append(self.arena, .{ .Const = _void });
        return;
    }
    for (0..bodies.len - 1) |i| {
        try self.genExpr(bodies[i], buffer, lexicalCtx);
        try buffer.append(self.arena, .Pop );
    }
    try self.genExpr(bodies[bodies.len - 1], buffer, lexicalCtx);
    try buffer.append(self.arena, .Return );
}

pub fn genQuoteInner(self: *Compiler, x: *AstNode) anyerror!void {
    const bldr = &self.vm.bldr;
    switch (x.id) {
        .atom => try bldr.newAtom(x.cast(.atom).name),
        .intNumber => try bldr.newIntNumber(x.cast(.intNumber).value),
        .floatNumber => try bldr.newIntNumber(x.cast(.intNumber).value),
        .string => try  bldr.newString(x.cast(.string).s),
        .bool => try bldr.newBool(x.cast(.bool).value),
        .list => {
            const xs = x.cast(.list).xs;
            try bldr.newList();
            for (0..xs.len) |i| {
                try self.genQuoteInner(xs[xs.len - 1 - i]);
                try bldr.appendToList();
            }
        },
        .improperList => {
            const l = x.cast(.improperList);
            try self.genQuoteInner(l.last);
            const xs = l.xs;
            for (0..xs.len) |i| {
                try self.genQuoteInner(xs[xs.len - 1 - i]);
                try bldr.appendToList();
            }
        },
        .vector => @panic("not implemented"),
        .quote => { 
            try bldr.newList();
            const expr = x.cast(.quote).value;
            try self.genQuoteInner(expr);
            try bldr.appendToList();
            try bldr.newAtom("quote");
            try bldr.appendToList();
        },
        .quasiquote =>try  bldr.newAtom("quasiquote"),
        .unquote => try  bldr.newAtom("unquote"),
        .unquoteSplicing => try  bldr.newAtom("unquote-splicing"),
    }
}
pub fn genQuote(self: *Compiler, x: *AstNode, buffer: *std.ArrayList(Instruction)) anyerror!void {
    try self.genQuoteInner(x);
    const c = try self.vm.stack.pop();
     
    try self.vm.data.append(self.allocator, c);
    try buffer.append(self.arena, .{.Const = c });
}
