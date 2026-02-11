const std = @import("std");
const VM = @import("main.zig").VM;
const Instruction = @import("main.zig").Instruction;
const Primitives = @import("Primitives.zig");
const NodePtr = @import("Node.zig").NodePtr;
const Node = @import("Node.zig").Node;
const _true = @import("Node.zig")._true;
const _false = @import("Node.zig")._false;
const _void = @import("Node.zig")._void;
const _nil = @import("Node.zig")._nil;

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
       params: []Param,
    };

    const Param = struct {
        name: []const u8,
        isBoxed: bool = false,
    };

    pub fn push(self: *LexicalCtx, arena: std.mem.Allocator, params: []const Param) void {
        const rib = arena.create(Rib) catch @panic("oom");
        rib.* = .{
            .next = self.rib,
            .params = arena.dupe(Param, params) catch @panic("oom"),
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
                if (std.mem.eql(u8, p.name, name)) {
                    return .{i, @intCast(j)};
                }
            }
            rib = r.next;
            i += 1;
        }
        return null;
    }

    pub fn setBoxed(self: LexicalCtx, name: []const u8) void {
        var rib = self.rib;
        var i:u16 = 0;
        while (rib) |r| {
            for (r.params) |*p| {
                if (std.mem.eql(u8, p.name, name)) {
                    p.isBoxed = true;
                    return;
                }
            }
            rib = r.next;
            i += 1;
        }
        return;
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
    var node = try self.vm.stack.pop();
    try self.vm.protectCompile.push(node);
    defer self.vm.protectCompile.clear();
    // node.debugprint("compiling>> ");

    if (self.vm.globalEnv.get("*syntax-expand*")) |expand| {

        node = try self.preprocessDefines(node, &lexicalCtx);
        try self.vm.bldr.newList();
        try self.vm.stack.push(node);
        try self.vm.bldr.appendToList();
        node = try self.vm.stack.pop();
        node = try self.runUserMacro(expand,  node);
    }

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
                if (std.mem.eql(u8, "put!", name)) {
                    return self.genPut(try node.second(), try node.third(), buffer, lexicalCtx);
                } else if (std.mem.eql(u8, "set!", name)) {
                    // if (xs.len != 3) {
                    //     return error.ArityMismatch;
                    // }
                    try self.genSet(try node.second(), try node.third(), buffer, lexicalCtx);
                    return;
                } else if (std.mem.eql(u8, "set-macro!", name)) { // TODO: set is not allowed to create new global vars
                    if (lexicalCtx.rib != null) {
                        return error.IllegalMacro;
                    }
                    try self.genSetMacro(try node.second(), try node.third(), buffer, lexicalCtx);
                    return;
                } else if (std.mem.eql(u8, "set-lmacro!", name)) { // TODO: set is not allowed to create new global vars
                    if (lexicalCtx.rib != null) {
                        return error.IllegalMacro;
                    }
                    try self.genSetLMacro(try node.second(), try node.third(), buffer, lexicalCtx);
                    return;
                } else if (std.mem.eql(u8, "begin", name)) {
                    // TODO: make sure this is true, maybe makes sense to put them one level up?
                    const bodies = try self.transformInnerDefines(node.tail());
                    try self.genBegin(bodies, buffer, lexicalCtx, isTailCall);
                    return;
                } else if (std.mem.eql(u8, "if", name)) { // TODO: if could be (if #t body)
                    const l = node.len();
                    if (l == 4) {
                        try self.genIf(try node.second(), try node.third(), try node.fourth(), buffer, lexicalCtx, isTailCall);
                        return;
                    } else if (l == 3) {
                        try self.vm.bldr.newList();
                        // try self.vm.bldr.newString("Missing else branch");
                        // try self.vm.bldr.appendToList();
                        try self.vm.bldr.newAtom("begin");
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
                } else if (std.mem.eql(u8, "case-lambda", name) or std.mem.eql(u8, "case-λ", name)) {
                    try self.genCaseLambda(node.tail(), buffer, lexicalCtx);
                    return;
                } else if (std.mem.eql(u8, "lambda", name) or std.mem.eql(u8, "λ", name)) {
                    // if (xs.len < 3) {
                    //     return error.WrongSyntax;
                    // }
                    const bodies = try self.transformInnerDefines(try node.ttail());
                    try self.genLambda(try node.second(), bodies, buffer, lexicalCtx);
                    return;
                } else if (self.primitives.get(name)) |prim| {
                    try self.genPrim(prim, node.tail(), buffer, lexicalCtx);
                    return;
                } else if (self.vm.lexMacroMap.get(name)) |lexMacro| {
                      const transformed = try self.runLexMacro(lexMacro,  node);
                      const expanded = try self.maybeSyntaxExpand(transformed);
                      try self.genExpr(expanded, buffer, lexicalCtx, isTailCall);
                      return;

                } else if (self.macros.get(name)) |macro| {
                    const transformed = try macro.exec(self.vm, lexicalCtx, self.arena, pair.snd);
                    // if (std.mem.eql(u8, "cond", name) ) {
                    //     transformed.debugprint("expanded: ");
                    // }
                    // std.debug.print("builtin macro: {s}\n", .{name});
                    const expanded = try self.maybeSyntaxExpand(transformed);
                    try self.genExpr(expanded, buffer, lexicalCtx, isTailCall);
                    // transformed.debugprint("... ");
                    return;
                } else if (self.vm.macroMap.get(name)) |usermacro| {
                    const transformed = try self.runUserMacro(usermacro,  pair.snd);
                    const expanded = try self.maybeSyntaxExpand(transformed);

                    try self.genExpr(expanded, buffer, lexicalCtx, isTailCall);
                    return;
                } 
                // else if (std.mem.eql(u8, "eval", name)) {
                //     const result = try self.runUserMacro(pair.snd.head(), _nil); // TODO: '() not tail..
                //     try buffer.append(self.arena, .{.Const = result});
                //     return;
                // }
            }
            try self.genAp(node, buffer, lexicalCtx, isTailCall);
        },
        .nil => {
                return error.EmptyApplication;
        },
        .intNumber, .floatNumber, .bool, .char => { 
            try buffer.append(self.arena, .{.Const = node});
        },
        .string, .vector => {
            try buffer.append(self.arena, .{.Const = node});
            try self.vm.data.append(self.allocator, node);
        },
        .atom => {
            const name = node.cast(.atom).name;
            if (lexicalCtx.findArg(name)) |pos| {
                if (pos.@"0" == 0) {
                   try buffer.append(self.arena, .{ .LocalVar = pos.@"1" } );
                } else {
                   try buffer.append(self.arena, .{ .FreeVar = pos } );
                }
                return;
            } else {
                try buffer.append(self.arena, .{ .GVar = node });
            }
        },
        .void, .procedure, .resource => {
            @panic("unreachable");
        }
    }
}

pub fn maybeSyntaxExpand(self: *Compiler, node: NodePtr) anyerror!NodePtr {
    if (self.vm.globalEnv.get("*syntax-expand*")) |expand| {
        try self.vm.bldr.newList();
        try self.vm.stack.push(node);
        try self.vm.bldr.appendToList();
        const l = try self.vm.stack.pop();
        return self.runUserMacro(expand,  l);
    } else {
        return node;
    }
}

pub fn preprocessDefines(self: *Compiler, node: NodePtr, lexicalCtx: *LexicalCtx) anyerror!NodePtr {
    switch(node.getId()) {
        .pair => {
            const pair = node.cast(.pair);
            if (pair.fst.getId() == .atom) {
                const name = pair.fst.cast(.atom).name;
                if (std.mem.eql(u8, "define", name)) {
                    const macro = self.macros.get("define").?;
                    const transformed = try macro.exec(self.vm, lexicalCtx, self.arena, pair.snd);
                    return transformed;
                } else if (std.mem.eql(u8, "define-syntax", name)) {
                    const macro = self.macros.get("define-syntax").?;
                    const transformed = try macro.exec(self.vm, lexicalCtx, self.arena, pair.snd);
                    return transformed;
                }
            }
            return node;
        },
        else => { return node; },
    }
}

pub fn genCaseLambda(self: *Compiler, _clauses: NodePtr, buffer: *std.ArrayList(Instruction), lexicalCtx: *LexicalCtx) anyerror!void {
    const numClauses = _clauses.len();
    var params = try self.arena.alloc(Instruction, @intCast(numClauses));
    const parentNumArgs = if (lexicalCtx.rib) |r| r.params.len else 0;
    var clauses = _clauses;
    var i:usize = 0;
    while (clauses.getId() != .nil) {
        const clause = clauses.head();
        const bodies = try self.transformInnerDefines(clause.tail());
        try self.genLambda(clause.head(), bodies, buffer, lexicalCtx);
        clauses = clauses.tail();
        params[i] = buffer.getLast();
        buffer.shrinkRetainingCapacity(buffer.items.len - 2);
        i += 1;
    }

    try buffer.append(self.arena, .{ .Fn = .{ 
        .numParams = @intCast(numClauses),
        .parentNumArgs = @intCast(parentNumArgs) }} );
    for (params) |p| {
        try buffer.append(self.allocator, p);
    }
}

pub fn genLambda(self: *Compiler, params: NodePtr, bodies: NodePtr, buffer: *std.ArrayList(Instruction), lexicalCtx: *LexicalCtx) anyerror!void {
    var lambdaBuff = try std.ArrayList(Instruction).initCapacity(self.arena, 256);
    var numParams: usize = 0;
    var varargs: bool = false;
    const isImproper = params.isImproperList();
    const parentNumArgs = if (lexicalCtx.rib) |r| r.params.len else 0;
    if (params.getId() == .nil) {
        lexicalCtx.push(self.arena, &.{});
        numParams = 0;
    } else if (params.getId() == .pair) {
        if (!isImproper) {
            var xs = params;
            var i: usize = 0;
            numParams = @intCast(xs.len());
            var buff = try self.arena.alloc(LexicalCtx.Param, numParams);
            while (xs.getId() != .nil) {
                buff[i].name = (try (try xs.tryHead()).tryCast(.atom)).name;
                i += 1;
                xs = try xs.tryTail();
            }
            lexicalCtx.push(self.arena, buff[0..]);
        } else {
            var xs = params;
            numParams = @intCast(xs.len());
            varargs = true;
            var i: usize = 0;
            var buff = try self.arena.alloc(LexicalCtx.Param, numParams);
            while (xs.getId() == .pair) {
                buff[i].name = (try (try xs.tryHead()).tryCast(.atom)).name;
                i += 1;
                xs = try xs.tryTail();
            }
            buff[numParams - 1].name = (try xs.tryCast(.atom)).name;
            lexicalCtx.push(self.arena, buff[0..]);
        }
    } else if (params.getId() == .atom) {
        lexicalCtx.push(self.arena, &.{.{. name = params.cast(.atom).name}});
        numParams = 1;
        varargs = true;
    } else {
        return error.WrongSyntax;
    }
    // try lambdaBuff.append(self.arena, .{ .Args = numParams } );

    var b = try bodies.tryCast(.pair);
    while (b.snd.getId() != .nil) {
        try self.genExpr(b.fst, &lambdaBuff, lexicalCtx, false);
        try lambdaBuff.append(self.arena, .{ .Pop = 1 } );
        b = try b.snd.tryCast(.pair);
    }
    try self.genExpr(b.fst, &lambdaBuff, lexicalCtx, true);
    try lambdaBuff.append(self.arena, .{ .Return = @intCast(numParams) } );

    // ok, it's good time to insert box commands... hmm, what about unbox?
    // now I need to expand macros again beforehand?
    // can't be bothered .. let's fix boxes afterwards
    for (lexicalCtx.rib.?.params, 0..) |p,i| {
        if (p.isBoxed) {
            try lambdaBuff.insert(self.arena, 0, .{ .Box = @intCast(i) });
        }
    }


    const offset = self.vm.code.items.len;
    try self.vm.code.appendSlice(self.allocator, lambdaBuff.items);


    for (lexicalCtx.rib.?.params, 0..) |p,i| {
        if (p.isBoxed) {
            self.fixUpLambda(offset, 0, i);
        }
    }

    try buffer.append(self.arena, .{ .Fn = .{ 
        .numParams = 1,
        .parentNumArgs = @intCast(parentNumArgs) }} );
    try buffer.append(self.allocator, .{ .FnParams = .{
        .code = @intCast(offset),
        .numArgs = @intCast(numParams),
        .isVarargs = varargs,
    } });
    lexicalCtx.pop();
    // return const lambda I suppose..
}

pub fn fixUpLambda(self: *Compiler, codeOffset:usize, level: usize, idx: usize) void {
    var i = codeOffset;
    loop: while (true) {
        const instr = self.vm.code.items[i];
        switch (instr) {
            .LocalVar => |_idx| {
                if (level == 0 and _idx == idx) {
                    self.vm.code.items[i] = .{.LocalVarIndirect = _idx };
                }
            },
            .FreeVar  => |offset| {
                if (offset.@"0" == level and offset.@"1" == idx) {
                    self.vm.code.items[i] = .{.FreeVarIndirect = offset };
                }
            },
            .Fn  => |f| {
                for (0..f.numParams) |j| {
                    const param = self.vm.code.items[i+j+1]; 
                    switch (param) {
                        .FnParams => |p| {
                            self.fixUpLambda(p.code, level + 1, idx);
                        },
                        else => { @panic("not reachable"); }
                    }
                }
            },
            .Return => {
                break :loop;
            },
            else => {},
        }
        i+=1;
    }
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
            std.debug.print("primitive: {s}, required: {d}, provided: {d}\n", .{prim.name, prim.numArgs, l});
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

    try self.genExpr(f, buffer, lexicalCtx, false); // here we shifting too early for f to create closure?

    if (isTailCall) {
       const parentNumArgs = if (lexicalCtx.rib) |r| r.params.len else 0;
       try buffer.append(self.arena, .{ .Shift = .{l, @intCast(parentNumArgs)} });
    }

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
        if (pos.@"0" == 0) {
           try buffer.append(self.arena, .{ .LocalSet = pos.@"1" } );
           try buffer.append(self.arena, .{ .Const = _void });
           // try buffer.append(self.arena, .{ .LocalVar = pos.@"1" } );
        } else {
           try buffer.append(self.arena, .{ .FreeSet = pos } );
           try buffer.append(self.arena, .{ .Const = _void });
           // try buffer.append(self.arena, .{ .FreeVar = pos } );
        }
        lexicalCtx.setBoxed(name);
    } else {
        try buffer.append(self.arena, .{ .GSet = nameNode });
        try buffer.append(self.arena, .{ .Const = _void });
    }
}
pub fn genPut(self: *Compiler, nameNode: NodePtr, expr: NodePtr, buffer: *std.ArrayList(Instruction), lexicalCtx: *LexicalCtx) anyerror!void {
    if (nameNode.getId() != .atom) {
        return error.WrongSyntax;
    }
    if (lexicalCtx.rib != null) {
        return error.IllegalDefine;
    }
    // const name = nameNode.cast(.atom).name;
    try self.genExpr(expr, buffer, lexicalCtx, false);


        try buffer.append(self.arena, .{ .GPut = nameNode });
        try buffer.append(self.arena, .{ .Const = _void });
}

pub fn genSetMacro(self: *Compiler, nameNode: NodePtr, expr: NodePtr, buffer: *std.ArrayList(Instruction), lexicalCtx: *LexicalCtx) anyerror!void {
    if (nameNode.getId() != .atom) {
        return error.WrongSyntax;
    }
    try self.genExpr(expr, buffer, lexicalCtx, false);

    try buffer.append(self.arena, .{ .MSet = nameNode });
    try buffer.append(self.arena, .{ .Const = _void });
}
pub fn genSetLMacro(self: *Compiler, nameNode: NodePtr, expr: NodePtr, buffer: *std.ArrayList(Instruction), lexicalCtx: *LexicalCtx) anyerror!void {
    if (nameNode.getId() != .atom) {
        return error.WrongSyntax;
    }
    try self.genExpr(expr, buffer, lexicalCtx, false);

    try buffer.append(self.arena, .{ .LMSet = nameNode });
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
        try buffer.append(self.arena, .{ .Pop = 1 } );
        b = try b.snd.tryCast(.pair);
    }
    try self.genExpr(b.fst, buffer, lexicalCtx, isTailCall);
}

//
//     see 5.2.2  Internal definitions
//
// Definitions may occur at the beginning of a <body> 
// (that is, the body of a lambda, let, let*, letrec, let-syntax, or letrec-syntax expression or 
//  that of a definition of an appropriate form). Such definitions are known as internal definitions as opposed to the top level definitions described above. The variable defined by an internal definition is local to the <body>. That is, <variable> is bound rather than assigned, and the region of the binding is the entire <body>.
pub fn transformInnerDefines(self: *Compiler, bodies: NodePtr) anyerror!NodePtr {
    var hasDefines = false;
    var b = bodies;
    while (b.getId() != .nil) {
        if ((try b.tryCast(.pair)).fst.getId() == .pair) {
            const d = b.head().cast(.pair).fst;
            if (d.getId() == .atom and std.mem.eql(u8, d.cast(.atom).name, "define")) {
                hasDefines = true;
                break;
            }
        }
        b = b.cast(.pair).snd;

    }
    if (!hasDefines) {
        return bodies;
    }
    const totalLen = bodies.len();
    var regularBodies = try std.ArrayList(NodePtr).initCapacity(self.arena, @intCast(totalLen));
    var defines = try std.ArrayList(NodePtr).initCapacity(self.arena, @intCast(totalLen));
    b = bodies;
    loop: while (b.getId() != .nil) {
        if ((try b.tryCast(.pair)).fst.getId() == .pair) {
            const d = b.head().cast(.pair).fst;
                if (d.getId() == .atom and std.mem.eql(u8, d.cast(.atom).name, "define")) {
                    defines.appendAssumeCapacity(b.head().cast(.pair).snd);
                    b = b.tail();
                    continue :loop;
                }
            }
        regularBodies.appendAssumeCapacity(b.head());
        b = b.tail();
    }
    if (regularBodies.items.len == 0) {
        return error.WrongSyntax;
    }
    // std.debug.print("bodies len: {d}, defines len: {d}\n", .{regularBodies.items.len, defines.items.len});

    try self.vm.bldr.newList();
    try self.vm.bldr.newList();
    var n = regularBodies.items.len;
    for (0..n) |i| {
        try self.vm.stack.push(regularBodies.items[n - 1 - i]);
        try self.vm.bldr.appendToList();
    }
        n = defines.items.len;
        try self.vm.bldr.newList();
        for (0..n) |i| {
            const d = defines.items[n - 1 - i];
            if ( (try d.tryHead()).getId() == .atom) {
                try self.vm.stack.push(d);
                try self.vm.bldr.appendToList();
            } else if ( d.head().getId() == .pair) {
                const p = d.head();
                try self.vm.bldr.newList();
                try self.vm.stack.push(p.head());
                try self.vm.bldr.appendToList();
                   try self.vm.stack.push(d.tail());
                   try self.vm.stack.push(p.tail());
                   try self.vm.bldr.appendToList();
                   try self.vm.bldr.newAtom("lambda");
                   try self.vm.bldr.appendToList();
                   
                try self.vm.bldr.appendToList();
                try self.vm.stack.push(p.head());
                try self.vm.bldr.appendToList();
                try self.vm.bldr.appendToList();
            } else {
            return error.WrongSyntax;
          }
        }
    try self.vm.bldr.appendToList();
    try self.vm.bldr.newAtom("letrec*");
    try self.vm.bldr.appendToList();
    try self.vm.bldr.appendToList();
    const res = try self.vm.stack.pop();
    // res.debugprint(">>> result: ");
    try self.vm.protectCompile.push(res);
    return res;

}



pub fn genQuote(self: *Compiler, c: NodePtr,  buffer: *std.ArrayList(Instruction)) anyerror!void {
    try self.vm.data.append(self.allocator, c);
    try buffer.append(self.arena, .{.Const = c });
}






pub fn runUserMacro(self: *Compiler, usermacro: NodePtr, params: NodePtr) anyerror!NodePtr {
    // const m = usermacro.cast(.procedure);
    // const stackBefore = self.vm.stack.size;
    const codeRestore = self.vm.code.items.len;
    try self.vm.bldr.newIntNumber(0);
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
    self.vm.run() catch |e| {
        usermacro.debugprint("while compiling: ");
        return e;
    };

    self.vm.code.shrinkRetainingCapacity(codeRestore);
    const res = try self.vm.stack.pop();

    // const stackAfter = self.vm.stack.size;
    // std.debug.print("user macro, stack before {d}, after {d} \n" , .{stackBefore, stackAfter});
    // _ = try self.vm.stack.pop();
    try self.vm.protectCompile.push(res);
    return res;
}
pub fn runLexMacro(self: *Compiler, usermacro: NodePtr, params: NodePtr) anyerror!NodePtr {
    // const m = usermacro.cast(.procedure);
    // const stackBefore = self.vm.stack.size;
    const codeRestore = self.vm.code.items.len;
    try self.vm.bldr.newIntNumber(0);
    try self.vm.bldr.newList();
    try self.vm.bldr.newIntNumber(@intCast(codeRestore + 1));

    try self.vm.stack.push(params);
    try self.vm.stack.push(usermacro);

    // try self.vm.code.append(self.arena, .{ .Save = @intCast(2) });
    try self.vm.code.append(self.allocator, .{ .JCall = 1 });
    try self.vm.code.append(self.allocator, .Halt );

    self.vm.ip = @intCast(codeRestore);
    self.vm.run() catch |e| {
        usermacro.debugprint("while compiling: ");
        return e;
    };

    self.vm.code.shrinkRetainingCapacity(codeRestore);
    const res = try self.vm.stack.pop();

    // const stackAfter = self.vm.stack.size;
    // std.debug.print("user macro, stack before {d}, after {d} \n" , .{stackBefore, stackAfter});
    // _ = try self.vm.stack.pop();
    try self.vm.protectCompile.push(res);
    return res;
}
