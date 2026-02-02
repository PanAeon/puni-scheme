const std = @import("std");
const VM = @import("main.zig").VM;
const Instruction = @import("main.zig").Instruction;
const Primitives = @import("Primitives.zig");
const NodePtr = @import("Node.zig").NodePtr;
const Node = @import("Node.zig").Node;
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
pub fn compile(self: *Compiler) anyerror!i64 {
    var buff = try std.ArrayList(Instruction).initCapacity(self.arena, 256);
    var lexicalCtx: LexicalCtx = .{};
    const codeRestore = self.vm.code.items.len;
    const dataRestore = self.vm.data.items.len;
    const node = try self.vm.stack.pop();
    try self.vm.protectCompile.push(node);
    defer self.vm.protectCompile.clear();
    node.debugprint("compiling>> ");

        self.genExpr(node, &buff, &lexicalCtx, false) catch |e| {
            self.vm.code.shrinkRetainingCapacity(codeRestore);
            self.vm.data.shrinkRetainingCapacity(dataRestore);
            return e;
        };
    const start = self.vm.code.items.len;
    try self.vm.code.appendSlice(self.allocator, buff.items);
    try self.vm.code.append(self.allocator, .Halt);
    return @intCast(start);
}
pub fn genExpr(self: *Compiler, node: NodePtr, buffer: *std.ArrayList(Instruction), lexicalCtx: *LexicalCtx, isTailCall: bool) anyerror!void {
    switch(node.getId()) {
        .pair => {
            const pair = node.cast(.pair);
            if (pair.fst.getId() == .atom) {
                const name = pair.fst.cast(.atom).name;
                if (std.mem.eql(u8, "set!", name)) { // TODO: set is not allowed to create new global vars
                    // if (xs.len != 3) {
                    //     return error.ArityMismatch;
                    // }
                    try self.genSet(try node.second(), try node.third(), buffer, lexicalCtx);
                    return;
                } else if (std.mem.eql(u8, "set-macro!", name)) { // TODO: set is not allowed to create new global vars
                    // if (xs.len != 3) {
                    //     return error.ArityMismatch;
                    // }
                    if (lexicalCtx.rib != null) {
                        return error.IllegalMacro;
                    }
                    try self.genSetMacro(try node.second(), try node.third(), buffer, lexicalCtx);
                    return;
                } else if (std.mem.eql(u8, "begin", name)) {
                    try self.genBegin(node.tail(), buffer, lexicalCtx, isTailCall);
                    return;
                } else if (std.mem.eql(u8, "if", name)) { // TODO: if could be (if #t body)
                    const l = node.len();
                    if (l == 4) {
                        try self.genIf(try node.second(), try node.third(), try node.fourth(), buffer, lexicalCtx, isTailCall);
                        return;
                    } else if (l == 3) {
                        try self.vm.bldr.newList();
                        try self.vm.bldr.newString("Missing else branch");
                        try self.vm.bldr.appendToList();
                        try self.vm.bldr.newAtom("error");
                        try self.vm.bldr.appendToList();
                        const _missingFalseBranch = try self.vm.stack.pop();
                        try self.vm.protectCompile.push(_missingFalseBranch);
                        defer self.vm.protectCompile.popOrDie();
                        try self.genIf(try node.second(), try node.third(), _missingFalseBranch, buffer, lexicalCtx, isTailCall);
                        return;
                    }
                    else {
                        return error.WrongSyntax;
                    }
                } else if (std.mem.eql(u8, "quote", name)) {
                    // if (xs.len != 2) {
                    //     return error.WrongSyntax;
                    // }
                    try self.genQuote(try node.second(), buffer);
                    return;
                // } else if (std.mem.eql(u8, "quasiquote", name)) {
                    // if (xs.len != 2) {
                    //     return error.WrongSyntax;
                    // }

                    // try self.genExpr(try self.genQuasiQuote(xs[1]), buffer, lexicalCtx, false);
                    // return;
                } else if (std.mem.eql(u8, "lambda", name)) {
                    // if (xs.len < 3) {
                    //     return error.WrongSyntax;
                    // }
                    try self.genLambda(try node.second(), try node.ttail(), buffer, lexicalCtx);
                    return;
                } else if (self.primitives.get(name)) |prim| {
                    try self.genPrim(prim, node.tail(), buffer, lexicalCtx);
                    return;
                } else if (self.macros.get(name)) |macro| {
                    var p = pair.snd;
                    if (!std.mem.eql(u8, "quasiquote", name)) {
                        while (p.getId() != .nil) {
                            const _p = try p.tryCast(.pair);
                            _p.fst = try self.expandMacros(_p.fst);
                            p = try p.tryTail();
                        }
                    }
                    const transformed = try macro.exec(self.vm, pair.snd);
                    // std.debug.print("builtin macro: {s}\n", .{name});
                    try self.genExpr(transformed, buffer, lexicalCtx, isTailCall);
                    // transformed.debugprint("... ");
                    return;
                } else if (self.vm.macroMap.get(name)) |usermacro| {
                        var p = pair.snd;
                    if (!std.mem.eql(u8, "quasiquote", name)) {
                        while (p.getId() != .nil) {
                            const _p = try p.tryCast(.pair);
                            _p.fst = try self.expandMacros(_p.fst);
                            p = try p.tryTail();
                        }
                    }

                    const transformed = try self.runUserMacro(usermacro,  pair.snd);
                    // transformed.debugprint("... ");
                    try self.genExpr(transformed, buffer, lexicalCtx, isTailCall);
                    return;
                }
            }
            try self.genAp(node, buffer, lexicalCtx, isTailCall);
        },
        .nil => {
                return error.EmptyApplication;
        },
        .intNumber, .floatNumber, .bool, .string, .vector => {
            try buffer.append(self.arena, .{.Const = node});
        },
        .atom => {
            const name = node.cast(.atom).name;
            if (lexicalCtx.findArg(name)) |pos| {
                try buffer.append(self.arena, .{ .LVar = pos } );
                return;
            } else {
                try buffer.append(self.arena, .{ .GVar = node });
            }
        },
        .void, .procedure, .currentCont => {
            @panic("unreachable");
        }
    }
}


pub fn genLambda(self: *Compiler, params: NodePtr, bodies: NodePtr, buffer: *std.ArrayList(Instruction), lexicalCtx: *LexicalCtx) anyerror!void {
    var lambdaBuff = try std.ArrayList(Instruction).initCapacity(self.arena, 256);
    var numParams: usize = 0;
    var varargs: bool = false;
    const isImproper = params.isImproperList();
    if (params.getId() == .nil) {
        lexicalCtx.push(self.arena, &.{});
        numParams = 0;
    } else if (params.getId() == .pair) {
        if (!isImproper) {
            var xs = params;
            var i: usize = 0;
            numParams = @intCast(xs.len());
            var buff = try self.arena.alloc([]const u8, numParams);
            while (xs.getId() != .nil) {
                buff[i] = (try (try xs.tryHead()).tryCast(.atom)).name;
                i += 1;
                xs = try xs.tryTail();
            }
            lexicalCtx.push(self.arena, buff[0..]);
        } else {
            var xs = params;
            numParams = @intCast(xs.len());
            varargs = true;
            var i: usize = 0;
            var buff = try self.arena.alloc([]const u8, numParams);
            while (xs.getId() == .pair) {
                buff[i] = (try (try xs.tryHead()).tryCast(.atom)).name;
                i += 1;
                xs = try xs.tryTail();
            }
            buff[numParams - 1] = (try xs.tryCast(.atom)).name;
            lexicalCtx.push(self.arena, buff[0..]);
        }
    } else if (params.getId() == .atom) {
        lexicalCtx.push(self.arena, &.{params.cast(.atom).name});
        numParams = 1;
        varargs = true;
    } else {
        return error.WrongSyntax;
    }
    try lambdaBuff.append(self.arena, .{ .Args = numParams } );

    var b = try bodies.tryCast(.pair);
    while (b.snd.getId() != .nil) {
        try self.genExpr(b.fst, &lambdaBuff, lexicalCtx, false);
        try lambdaBuff.append(self.arena, .Pop );
        b = try b.snd.tryCast(.pair);
    }
    try self.genExpr(b.fst, &lambdaBuff, lexicalCtx, true);
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

pub fn genIf(self: *Compiler, cond: NodePtr, branch0: NodePtr, branch1: NodePtr, buffer: *std.ArrayList(Instruction), lexicalCtx: *LexicalCtx, isTailCall: bool) anyerror!void {
    try self.genExpr(cond, buffer, lexicalCtx, false);
    try buffer.append(self.arena, .{ .FJump = 0 });
    const fJumpIdx = buffer.items.len-1;
    try self.genExpr(branch0, buffer, lexicalCtx, isTailCall);
    try buffer.append(self.arena, .{ .Jump = 0 });
    const endJumpIdx = buffer.items.len-1;
    const fIdx = buffer.items.len;
    try self.genExpr(branch1, buffer, lexicalCtx, isTailCall);
    const endIdx = buffer.items.len;
    // now we can set jumps as we know the length..
    _ = &fJumpIdx;
    _ = &endJumpIdx;
    buffer.items[endJumpIdx].Jump = @intCast(endIdx - endJumpIdx);
    buffer.items[fJumpIdx].FJump = @intCast(fIdx - fJumpIdx);
}
pub fn genPrim(self: *Compiler, prim: *const Primitives.Prim, params: NodePtr, buffer: *std.ArrayList(Instruction), lexicalCtx: *LexicalCtx) anyerror!void {

    var b = params;
    var l:i48 = 0;
    while (b.getId() != .nil) {
        try self.genExpr(try b.tryHead(), buffer, lexicalCtx, false);
        l += 1;
        b = b.tail();
    }
    if (prim.varargs) {
        const n: i48 = l;
        try buffer.append(self.arena, .{.Const = NodePtr.initU64(@as(u48, @bitCast(n)), .{ .id = .intNumber }) });
    } else {
        if (prim.numArgs != l) {
            return error.ArityMismatch;
        }
    }
    try buffer.append(self.arena, .{.Primitive = prim });
}
pub fn genAp(self: *Compiler, xs: NodePtr, buffer: *std.ArrayList(Instruction), lexicalCtx: *LexicalCtx, isTailCall: bool) anyerror!void {
    const savePos = buffer.items.len;
    if (!isTailCall) {
       try buffer.append(self.arena, .{ .Save = 0});
    }
    const f = xs.head();

    const params = xs.tail();

    var b = params;
    var l:u32 = 0;

    while (b.getId() != .nil) {
        try self.genExpr(try b.tryHead(), buffer, lexicalCtx, false);
        b = b.tail();
        l += 1;
    }

    try self.genExpr(f, buffer, lexicalCtx, false);

    try buffer.append(self.arena, .{ .JCall = l });
    if (!isTailCall) {
       buffer.items[savePos] = .{ .Save = @intCast(buffer.items.len-savePos) };
    }


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

// can't we do (set! (expr ..) (expr)) ??? no
pub fn genSet(self: *Compiler, nameNode: NodePtr, expr: NodePtr, buffer: *std.ArrayList(Instruction), lexicalCtx: *LexicalCtx) anyerror!void {
    if (nameNode.getId() != .atom) {
        return error.WrongSyntax;
    }
    const name = nameNode.cast(.atom).name;
    try self.genExpr(expr, buffer, lexicalCtx, false);

    if (lexicalCtx.findArg(name)) |pos| {
        try buffer.append(self.arena, .{ .LSet = pos } );
        return;
    } else {
        try buffer.append(self.arena, .{ .GSet = nameNode });
        try buffer.append(self.arena, .{ .Const = _void });
    }
}

pub fn genSetMacro(self: *Compiler, nameNode: NodePtr, expr: NodePtr, buffer: *std.ArrayList(Instruction), lexicalCtx: *LexicalCtx) anyerror!void {
    if (nameNode.getId() != .atom) {
        return error.WrongSyntax;
    }
    try self.genExpr(expr, buffer, lexicalCtx, false);

    try buffer.append(self.arena, .{ .MSet = nameNode });
    try buffer.append(self.arena, .{ .Const = _void });
}

pub fn genBegin(self: *Compiler, bodies: NodePtr, buffer: *std.ArrayList(Instruction), lexicalCtx: *LexicalCtx, isTailCall: bool) anyerror!void {
    if (bodies.getId() == .nil) {
        try buffer.append(self.arena, .{ .Const = _void });
        return;
    }
    var b = try bodies.tryCast(.pair);
    while (b.snd.getId() != .nil) {
        try self.genExpr(b.fst, buffer, lexicalCtx, false);
        try buffer.append(self.arena, .Pop );
        b = try b.snd.tryCast(.pair);
    }
    try self.genExpr(b.fst, buffer, lexicalCtx, isTailCall);
}



pub fn genQuote(self: *Compiler, c: NodePtr,  buffer: *std.ArrayList(Instruction)) anyerror!void {
    try self.vm.data.append(self.allocator, c);
    try buffer.append(self.arena, .{.Const = c });
}





// pub fn expandMacros(self: *Compiler, ast: *AstNode) anyerror!*AstNode {
pub fn expandMacros(self: *Compiler, ast: NodePtr) anyerror!NodePtr {
    // std.debug.print("in expand macros", .{});
   switch(ast.getId()) {
        .pair => {
            const pair = ast.cast(.pair);
            if (pair.fst.getId() == .atom) {
                const name = pair.fst.cast(.atom).name;
                if (self.macros.get(name)) |macro| {
                    if (!std.mem.eql(u8, "quasiquote", name)) {
                        var p = pair.snd;
                        while (p.getId() != .nil) {
                            const _p = try p.tryCast(.pair);
                            _p.fst = try self.expandMacros(_p.fst);
                            p = try p.tryTail();
                        }
                    }
                    return macro.exec(self.vm, pair.snd);
                } else if (self.vm.macroMap.get(name)) |usermacro| {
                    if (!std.mem.eql(u8, "quasiquote", name)) {
                        var p = pair.snd;
                        while (p.getId() != .nil) {
                            const _p = try p.tryCast(.pair);
                            _p.fst = try self.expandMacros(_p.fst);
                            p = try p.tryTail();
                        }
                    }

                    return try self.runUserMacro(usermacro,  pair.snd);
                }

            }
            return ast;
        },
        else => return ast,
    }
}

pub fn runUserMacro(self: *Compiler, usermacro: NodePtr, params: NodePtr) anyerror!NodePtr {
    // const m = usermacro.cast(.procedure);
    const codeRestore = self.vm.code.items.len;
    try self.vm.bldr.newList();
    try self.vm.bldr.newIntNumber(@intCast(codeRestore + 1));
    var p = params;

    var l : u32 = 0;
    while (p.getId() != .nil) {
        try self.vm.stack.push(p.cast(.pair).fst);
        p = p.cast(.pair).snd;
        l += 1;
    }
    try self.vm.stack.push(usermacro);

    // try self.vm.code.append(self.arena, .{ .Save = @intCast(2) });
    try self.vm.code.append(self.allocator, .{ .JCall = l });
    try self.vm.code.append(self.allocator, .Halt );

    self.vm.ip = @intCast(codeRestore);
    try self.vm.run();

    self.vm.code.shrinkRetainingCapacity(codeRestore);
    const res = try self.vm.stack.pop();
    try self.vm.protectCompile.push(res);
    return res;
}
