const std = @import("std");
const Stack = @import("Stack.zig").Stack;
const lex = @import("lexer.zig");
const linenoise = @import("linenoise.zig");
// const ConsAllocator = @import("ConsAllocator.zig");
const ConsPage = @import("ConsAllocator.zig").Page;
const NodePtr = @import("Node.zig").NodePtr;
const Node = @import("Node.zig").Node;
const _nil = @import("Node.zig")._nil;
const _true = @import("Node.zig")._true;
const _false = @import("Node.zig")._false;
const Parser = @import("Parser.zig").Parser;
const ParserError = @import("Parser.zig").ParserError;
const getEnv = @import("Env.zig").getEnv;
const getEnvRef = @import("Env.zig").getEnvRef;
const NodeBuilder = @import("Node.zig").NodeBuilder;
const Primitives = @import("Primitives.zig");
const Prim = Primitives.Prim;
const Compiler = @import("Compiler.zig");
const VMError = error{ ExpectedList, StackOverflow, StackUnderflow, UnknownName, IllegalDefine, MultipleExprsAfterId, NotAList, NotCallable, ArityMismatch, NotExhastiveMatch, IllegalArgument, AssignmentDisallowed, InvalidSyntaxDefinition, NotImplemented, UserError, ArgsTooLong, MissingElseBranch };

pub const MaxStack: usize = 10024;
pub const MaxProtectStack: usize = 1024;
pub const InitialGCThreshold: usize = 8; // 8


const InstructionTag = enum { 
    Const,
    LocalVar, // search in the current frame,
    LocalVarIndirect, // access boxed value
    LocalSet,
    GVar,
    GSet,
    GPut,
    FreeVar, // search in the current closure
    FreeVarIndirect,
    FreeSet,
    Box,
    Pop,
    TJump,
    FJump,
    Jump,
    Return,
    Shift,
    JCall,
    Save,
    Fn,
    FnParams,
    Primitive,
    MSet,
    LMSet,
    CC,
    Halt
};
pub const Instruction = union(InstructionTag) {
    Const: NodePtr,
    LocalVar: u32,
    LocalVarIndirect: u32,
    LocalSet: u32,
    GVar: NodePtr,
    GSet: NodePtr,
    GPut: NodePtr,
    FreeVar: struct {u32, u32},
    FreeVarIndirect: struct {u32, u32},
    FreeSet: struct{u32, u32},
    Box: u32,
    Pop: i64,
    TJump :i64,
    FJump: i64,
    Jump: i64,
    Return: i64,
    Shift: struct{u32, u32},
    JCall: u32, // num of arguments
    Save: i64, // return address and closure
    Fn: Function, // create closure from arg and current env, push result on the stack
    FnParams: FunctionParams,
    Primitive: *const Prim,
    MSet: NodePtr,
    LMSet: NodePtr,
    CC,
    Halt,

    const Function = struct {
        numParams: u15,
        parentNumArgs:u15,
    };
    const FunctionParams = struct {
        code: u32,
        numArgs: u16,
        isVarargs: bool,
    };

    pub fn print(self: Instruction) void {
        switch (self) {
                .Halt => std.debug.print("Halt\n", .{}),
                .Jump => |offset| std.debug.print("Jump {d}\n", .{offset}),
                .TJump =>  |offset| std.debug.print("TJump {d}\n", .{offset}),
                .FJump =>  |offset| std.debug.print("FJump {d}\n", .{offset}),
                .Const => |ptr|  { 
                     ptr.debugprint("Const ");
                 },
                .Primitive => |p| std.debug.print("Primitive {s}\n", .{p.name}),
                .LocalVar => |idx|   std.debug.print("Localvar {d}\n", .{idx}),
                .LocalVarIndirect => |idx|   std.debug.print("LocalvarIndirect {d}\n", .{idx}),
                .LocalSet => |idx|  std.debug.print("LocalSet {d}\n", .{idx}),
                .GVar => |p|  std.debug.print("GVar {s}\n", .{p.cast(.atom).name}),
                .GSet => |p|  std.debug.print("GSet {s}\n", .{p.cast(.atom).name}),
                .GPut => |p|  std.debug.print("GPut {s}\n", .{p.cast(.atom).name}),
                .FreeVar => |offset|   std.debug.print("FreeVar {d} {d}\n", .{offset.@"0", offset.@"1"}),
                .FreeVarIndirect => |offset|   std.debug.print("FreeVarIndirect {d} {d}\n", .{offset.@"0", offset.@"1"}),
                .FreeSet => |offset|  std.debug.print("FreeSet {d} {d}\n", .{offset.@"0", offset.@"1"}),
                .Box => |idx|   std.debug.print("Box {d}\n", .{idx}),
                .Shift => |x| std.debug.print("Shift n:{d} m:{d}\n", .{x.@"0", x.@"1"}),
                .Fn   => |x|  std.debug.print("Fn {any}\n", .{x}),
                .FnParams   => |p|  std.debug.print("FnParams {any}\n", .{p}),
                .Save => |addr|  std.debug.print("Save {d}\n", .{addr}),
                .JCall => |numArgs|  std.debug.print("JCall {d}\n", .{numArgs}),
                .Pop => |n| std.debug.print("Pop {d}\n", .{n}),
                .MSet => |p|  std.debug.print("MSet {any}\n", .{p}),
                .LMSet => |p|  std.debug.print("LMSet {any}\n", .{p}),
                .CC =>  std.debug.print("CC", .{}),
                .Return => |n| std.debug.print("Return {d}\n", .{n}),
        }
    }
};

// comptime {
//     std.debug.assert(@sizeOf(Instruction) == 16);
// }

pub const VM = struct {
    allocator: std.mem.Allocator,
    data: std.ArrayList(NodePtr),
    code: std.ArrayList(Instruction),
    ip: i64 = 0,
    globalEnv: std.StringHashMap(NodePtr),
    symbolMap: std.StringHashMap(NodePtr),
    macroMap: std.StringHashMap(NodePtr),
    lexMacroMap: std.StringHashMap(NodePtr),
    consAlloctor: *ConsPage,
    // env: NodePtr = _nil,
    frame: i64 = 0, // current frame idx in the stack
    closure: NodePtr = _nil,
    lastNode: ?NodePtr = null,
    numObjects: usize = 0,
    maxObjects: usize = InitialGCThreshold,
    stack: Stack(NodePtr) = undefined,
    protectStack: Stack(NodePtr) = undefined,
    protectCompile: Stack(NodePtr) = undefined,
    argsBuffer: [1024]NodePtr = undefined, // TODO: remove it..
    disableGC: bool = false,
    verboseGC: bool = true,
    stats: Stats = .{},
    bldr: NodeBuilder,
    ccCodeLoc: u32 = 0,
    prng: std.Random.DefaultPrng = undefined,
    io: std.Io,



    
    pub const Stats = struct {
        numAllocs: u64 = 0,
        numAllocConses: u64 = 0,
        startTime: std.time.Instant = undefined,
        gcTime: u64 = 0,
    };

    pub fn createCC(self:*VM) anyerror!void {
        try self.code.append(self.allocator,  .{ .FreeVar = .{1, 0} }  );
        try self.code.append(self.allocator,  .{ .LocalVar = 0 }  );
        try self.code.append(self.allocator,  .CC  );
        try self.code.append(self.allocator, .{ .Return = 1 });
    }

    pub fn init(allocator: std.mem.Allocator, io: std.Io, verboseGC: bool) !*VM {
        const vm = try allocator.create(VM);
        vm.* = .{
            .allocator = allocator,
            .stack = try Stack(NodePtr).init(allocator, MaxStack),
            .protectStack = try Stack(NodePtr).init(allocator, MaxProtectStack),
            .protectCompile = try Stack(NodePtr).init(allocator, MaxProtectStack),
            .code = try std.ArrayList(Instruction).initCapacity(allocator, 16 * 1024),
            .data = try std.ArrayList(NodePtr).initCapacity(allocator, 16 * 1024),
            .globalEnv = std.StringHashMap(NodePtr).init(allocator),
            .symbolMap = std.StringHashMap(NodePtr).init(allocator),
            .macroMap = std.StringHashMap(NodePtr).init(allocator),
            .lexMacroMap = std.StringHashMap(NodePtr).init(allocator),
            .verboseGC = verboseGC,
            .consAlloctor = try ConsPage.create(),
            .bldr = NodeBuilder.init(vm, allocator),
            .io = io
        };
        vm.stats.startTime = try std.time.Instant.now();
        vm.createCC() catch @panic("can't create cc");
        vm.prng = .init(blk: {
            var seed: u64 = undefined;
            const res = std.os.linux.getrandom(std.mem.asBytes(&seed), 8, 0);
            if (res != 8) {
                @panic("can't get enough entropy\n");
            }
            // std.debug.print("res: {d}", .{res});
            break :blk seed;
        });
        return vm;
    }
    pub fn destroy(self: *VM) void {
        self.protectStack.clear();
        self.protectCompile.clear();
        self.stack.clear();
        self.data.shrinkRetainingCapacity(0);
        self.globalEnv.clearRetainingCapacity();
        self.macroMap.clearRetainingCapacity();
        self.lexMacroMap.clearRetainingCapacity();
        self.disableGC = false;
        self.closure = _nil;
        self.gc();
        self.code.deinit(self.allocator);
        self.data.deinit(self.allocator);
        self.stack.deinit(self.allocator);
        self.globalEnv.deinit();
        self.symbolMap.deinit();
        self.macroMap.deinit();
        self.lexMacroMap.deinit();
        self.protectStack.deinit(self.allocator);
        self.protectCompile.deinit(self.allocator);
        self.consAlloctor.destroy();
        self.allocator.destroy(self);
    }
    pub fn protect(self: *VM, n: NodePtr) void {
        self.protectStack.push(n) catch {
            @panic("protected stack overflow");
        };
    }
    pub fn clearProtected(self: *VM) void {
        self.protectStack.clear();
    }
    pub fn markAll(self: *VM) void {
        mark(self.closure);
        
        for (0..self.stack.size) |i| {
            mark(self.stack.items[i]);
        }
        for (0..self.protectStack.size) |i| {
            mark(self.protectStack.items[i]);
        }
        for (0..self.protectCompile.size) |i| {
            mark(self.protectCompile.items[i]);
        }
        for (self.data.items) |d| {
            mark(d);
        }
        var iter = self.globalEnv.valueIterator();
        while (iter.next()) |x| {
            mark(x.*); // will break if mark is moved to Ptr itself..
        }
        iter = self.macroMap.valueIterator();
        while (iter.next()) |x| {
            mark(x.*); // will break if mark is moved to Ptr itself..
        }
        iter = self.lexMacroMap.valueIterator();
        while (iter.next()) |x| {
            mark(x.*); // will break if mark is moved to Ptr itself..
        }
    }
    pub fn mark(node: NodePtr) void {
        const id = node.getId();
        if (id == .intNumber or id == .floatNumber or id == .bool or id == .char or id == .nil or id == .void or node.raw().marked) {
            return;
        }
        node.raw().marked = true;
        switch (id) {
            .bool, .string, .atom, .intNumber, .floatNumber, .char, .nil, .void => {},
            .procedure => {
                const p = node.cast(.procedure);
                // mark(p.args);
                // mark(p.body);
                mark(p.env);
                // if (p.optimized) |o| {
                //     mark(o);
                // }
            },
            .vector => {
                const l = node.cast(.vector);
                for (l.xs) |x| {
                    mark(x);
                }
            },
            .pair => {
                const p = node.cast(.pair);
                mark(p.fst);
                mark(p.snd);
            },
            .resource => {
                const c = node.cast(.resource);
                mark(c.metadata);
            },
        }
    }
    pub fn cleanUpGlobalEnv(self: *VM) !void {
        var toBeDeleted = try std.ArrayList(*[]const u8).initCapacity(self.allocator, 1024);
        defer toBeDeleted.deinit(self.allocator);

        var it = self.globalEnv.iterator();
        while (it.next()) |x| {
            if (x.value_ptr.isBoxed() and !x.value_ptr.raw().marked) {
                try toBeDeleted.append(self.allocator, x.key_ptr);
                // need to remove the key..
            }
        }
        for (toBeDeleted.items) |key_ptr| {
            self.globalEnv.removeByPtr(key_ptr);
        }
    }
    pub fn cleanUpSymbolMap(self: *VM) !void {
        var toBeDeleted = try std.ArrayList(*[]const u8).initCapacity(self.allocator, 1024);
        defer toBeDeleted.deinit(self.allocator);

        var it = self.symbolMap.iterator();
        while (it.next()) |x| {
            if (x.value_ptr.isBoxed() and !x.value_ptr.raw().marked) {
                try toBeDeleted.append(self.allocator, x.key_ptr);
                // need to remove the key..
            }
        }
        for (toBeDeleted.items) |key_ptr| {
            self.symbolMap.removeByPtr(key_ptr);
        }
    }
    pub fn sweep(self: *VM) void {
        // self.consAlloctor.gc();
        var object: *?NodePtr = &self.lastNode;
        while (object.* != null) {
            const raw = object.*.?.raw();
            if (!raw.marked) {
                const unreached = object.*.?;
                object.* = raw.next;
                const id = unreached.getId();
                // std.debug.print("removing {any}\n", .{id});
                switch (id) {
                    .string => {
                        self.allocator.free(unreached.cast(.string).s);
                        self.allocator.destroy(unreached.cast(.string));
                    },
                    .atom => {
                        const a = unreached.cast(.atom);
                        self.allocator.free(a.name);
                        self.allocator.destroy(a);
                    },
                    .pair => {
                        const p = unreached.cast(.pair);
                        self.consAlloctor.dealloc(p);
                    },
                    .vector => {
                        const l = unreached.cast(.vector);
                        self.allocator.free(l.xs);
                        self.allocator.destroy(l);
                    },
                    .resource => {
                        const c = unreached.cast(.resource);
                        c.finalize(self.allocator);
                        self.allocator.destroy(c);
                    },
                    .procedure => {
                        const p = unreached.cast(.procedure);
                        self.allocator.free(p.params);
                        self.allocator.destroy(p);
                    },
                    .nil, .void, .intNumber, .floatNumber, .bool, .char => {},
                }
                self.numObjects -= 1;
            } else {
                raw.marked = false;
                object = &(raw.next);
            }
        }
    }
    pub fn gc(self: *VM) void {
        if (self.disableGC) {
            return;
        }
        const before = std.time.Instant.now() catch @panic("time failure");
        const numObjects = self.numObjects;
        self.markAll();
        self.cleanUpSymbolMap() catch { @panic("something went wrong"); };
        // self.cleanUpGlobalEnv() catch { @panic("something went wrong"); };
        self.sweep();
        self.maxObjects = if (self.numObjects <  InitialGCThreshold) InitialGCThreshold else self.numObjects * 2;
        const after = std.time.Instant.now() catch @panic("again");
        self.stats.gcTime += after.since(before);
        if (self.verboseGC) {
            std.debug.print("Collected {d} objects, {d} remaining.\n", .{ numObjects - self.numObjects, self.numObjects });
        }
    }
    pub fn printLastEnv(self: *VM) void {
        const top = self.env.cast(.pair).fst;
        top.debugprint("lastenv:>");
    }

    pub fn run(self: *VM) anyerror!void {
        while (true) : (self.ip += 1) {
            self.clearProtected();
            const instr = self.code.items[@intCast(self.ip)];
            switch (instr) {
                .Halt => return,
                .Jump => |offset| self.ip += offset - 1,
                .TJump =>  |offset| { 
                    const cond = try self.stack.pop();
                    if (!cond.equal(&_false)) {
                        self.ip += offset - 1;
                    }
                },
                .FJump =>  |offset| { 
                    const cond = try self.stack.pop();
                    if (cond.equal(&_false)) {
                        self.ip += offset - 1;
                    }
                },
                .Const => |ptr| {
                    try self.stack.push(ptr);
                },
                .Primitive => |p| {
                    try p.exec(self);
                },
                .Pop => |n| { 
                    for (0..@intCast(n)) |_| {
                       _ = try self.stack.pop();
                    }
                },
                .Box => |idx| {
                    const value = self.stack.items[@intCast(self.frame + idx + 3)];
                    try self.bldr.newList();
                    try self.stack.push(value);
                    try self.bldr.newPair();
                    const box = try self.stack.pop();
                    self.stack.items[@intCast(self.frame + idx + 3)] = box;
                },
                .LocalVar => |idx| {
                    const value = self.stack.items[@intCast(self.frame + idx + 3)];
                    try self.stack.push(value);
                },
                .LocalVarIndirect => |idx| {
                    const box = self.stack.items[@intCast(self.frame + idx + 3)];
                    const value = box.head();
                    try self.stack.push(value);
                },
                .LocalSet => |idx| {
                    var box = self.stack.items[@intCast(self.frame + idx + 3)];
                    box.cast(.pair).fst = try self.stack.pop();
                },
                .GVar => |p| {
                    const name = (try p.tryCast(.atom)).name;
                    const value = self.globalEnv.get(name);
                    if (value) |v| {
                        try self.stack.push(v);
                    } else {
                        std.debug.print("name: {s}\n", .{name});
                        return error.UnknownName;
                    }
                },
                .FreeVar => |offset| {
                    const v = getEnv(self.closure, offset.@"0" - 1, offset.@"1").?;
                    try self.stack.push(v);
                },
                .FreeVarIndirect => |offset| {
                    const box = getEnv(self.closure, offset.@"0" - 1, offset.@"1").?;
                    const v = box.head();
                    try self.stack.push(v);
                },
                .FreeSet => |offset| {
                    var box = getEnv(self.closure, offset.@"0" - 1, offset.@"1").?;
                    box.cast(.pair).fst = try self.stack.pop();
                },
                .GSet => |p| {
                    const name = (try p.tryCast(.atom)).name;
                    if (self.globalEnv.contains(name)) {
                       try self.globalEnv.put(name, try self.stack.pop());
                    } else {
                        std.debug.print("not allowed to set {s} which is undefined\n", .{name});
                        return error.IllegalSet;
                    }
                },
                .GPut => |p| {
                    const name = (try p.tryCast(.atom)).name;
                    try self.globalEnv.put(name, try self.stack.pop());
                },
                .Shift => |x| { // moves top n elements m places down the stack
                    const last = try self.stack.pop(); // TODO: think of a better solution
                    try self.stack.shift(x.@"0",x.@"1");
                    try self.stack.push(last);
                },
                .Fn   => |x| {
                    try self.createProcedure(x);
                },
                .FnParams => {
                    @panic("not reachable");
                },
                .Save => |offset| {
                    try self.save(offset);
                },
                .JCall => |numArgs| {
                    try self.jcall(numArgs);
                },

                .MSet => |p| {
                    const name = (try p.tryCast(.atom)).name;
                    try self.macroMap.put(name, try self.stack.pop());
                },
                .LMSet => |p| {
                    const name = (try p.tryCast(.atom)).name;
                    try self.lexMacroMap.put(name, try self.stack.pop());
                },
                .CC => {
                    const v = try self.stack.pop();
                    const c = try self.stack.pop();
                    const ret = c.head();
                    const frame = try c.second();
                    const closure = try c.third();
                    const st = (try c.fourth()).cast(.vector).xs;
                    self.stack.size = st.len;
                    for (0..st.len) |i| {
                        self.stack.items[i] = st[i];
                    }
                    try self.stack.push(frame);
                    try self.stack.push(closure);
                    try self.stack.push(ret);
                    try self.stack.push(c); // 1 arg to drop;
                    try self.stack.push(v);
                },
                .Return => |n| {
                    const value = try self.stack.pop();
                    // value.debugprint("return value");

                    for (0..@intCast(n)) |_| { // drop N args..
                       _ = try self.stack.pop();
                    }
                    const addr  = try self.stack.pop();
                    self.closure = try self.stack.pop();
                    self.frame   = try (try self.stack.pop()).tryGetIntValue();
                    self.ip = (try addr.tryGetIntValue()) - 1;
                    try self.stack.push(value);
                },
            }
        }
    }

    pub fn save(self: *VM, offset: i64) anyerror!void {
        try self.bldr.newIntNumber(@intCast(self.frame));
        try self.stack.push(self.closure);
        try self.bldr.newIntNumber(@intCast(self.ip + offset)); // push return value to the stack
    }

    pub fn jcall(self:*VM, numArgs: u32) anyerror!void {
                    const _f = try self.stack.pop();
                    self.protect(_f);
                    const f = try _f.tryCast(.procedure);
                    // std.debug.print("stack size: {d}\n", .{self.stack.size});
                    self.frame = @intCast(self.stack.size - numArgs - 3);
                    var code: u32 = std.math.maxInt(u32);
                    for (f.params) |p| {
                       if (p.varargs and numArgs + 1 >= p.numArgs) {
                          code = p.code;
                          const numLast = numArgs + 1 - p.numArgs ;
                          try self.bldr.newList();
                          for (0..numLast) |_| {
                            try self.bldr.appendToListRev();
                          }
                          break;
                       } else if (p.numArgs == numArgs) {
                          code = p.code;
                          break;
                       }
                    
                    }
                    if (code == std.math.maxInt(u32)) {
                        var iter = self.globalEnv.iterator();
                        while (iter.next()) |p| {
                          if (p.value_ptr.*.equal(&_f)) {
                                std.debug.print("in {s}\n", .{p.key_ptr.*});
                                return error.ArityMismatch;
                          }
                        }
                        std.debug.print("in anonymous lambda\n", .{});
                        return error.ArityMismatch;
                    }
                    // if (f.varargs) { // TODO: think about varargs..
                    //     if (numArgs + 1 < f.numArgs) {
                    //         return error.ArityMismatch;
                    //     }
                    //     const numLast = numArgs + 1 - f.numArgs ;
                    //     try self.bldr.newList();
                    //     for (0..numLast) |_| {
                    //         try self.bldr.appendToListRev();
                    //     }
                    // } else {
                    //     if (f.numArgs != numArgs) {
                    //         return error.ArityMismatch;
                    //     }
                    // }
                    self.closure = f.env;
                    self.ip = @as(i64, @intCast(code)) - 1;
    }

    pub fn createProcedure(self: *VM, f: Instruction.Function) anyerror!void {
        // try self.stack.push(self.closure);
        for (0..f.parentNumArgs) |i| {
            try self.stack.push(self.stack.items[@intCast(self.frame + @as(i64, @intCast(i)) + 3)]);
        }
        try self.bldr.newEnv(f.parentNumArgs, true);
        var params = try self.allocator.alloc(Node.Procedure.Params, f.numParams);
        defer self.allocator.free(params);
        for (0..f.numParams) |i| {
            self.ip += 1;
            const param = self.code.items[@intCast(self.ip)]; 
            switch (param) {
                .FnParams => |p| {
                    params[i].code = p.code;
                    params[i].numArgs = p.numArgs;
                    params[i].varargs = p.isVarargs;
                },
                else => { @panic("not reachable"); }
            }
        }
        // try self.bldr.newProcFromParams(f.params);
        try self.bldr.newProc(params);
        // self.lastProc = try self.stack.head();
    }

    pub fn printStack(self: *VM) void {
        std.debug.print("stack: \n", .{});
        for (0..self.stack.size) |i| {
            NodePtr.debugprint(&self.stack.items[i], ">> ");
        }
        std.debug.print("--- \n", .{});
    }
    pub fn restore(self: *VM, cont: *Node.CurrentCont) anyerror!void {
        self.callStack.size = cont.callStack.len;
        for (0..self.callStack.size) |i| self.callStack.items[i] = cont.callStack[i];
        self.stack.size = cont.stack.len;
        for (0..self.stack.size) |i| self.stack.items[i] = cont.stack[i];
    }

};
// Closures and Scope: When a lambda expression is evaluated, it "remembers" the environment in which it was created.
// This forms a closure, allowing the created procedure to access variables that were in scope at the time it was defined,
// even when called from a different scope later.
pub fn repl(gpa: std.mem.Allocator, vm: *VM, parser: *Parser) !void {
    _ = linenoise.linenoiseHistoryLoad("/tmp/puni-scheme.hist");
    defer _ = linenoise.linenoiseHistorySave("/tmp/puni-scheme.hist");
    var threaded: std.Io.Threaded = .init_single_threaded;
    // var stdin_buffer: [2048]u8 = undefined;
    var stdout_buffer: [2048]u8 = undefined;
    var stdout_writer = std.Io.File.stdout().writer(threaded.io(), &stdout_buffer);
    // var stdin_reader = std.Io.File.stdin().reader(threaded.io(), &stdin_buffer);
    // const stdin = &stdin_reader.interface;
    const stdout = &stdout_writer.interface;

    var lineBuffer = try std.ArrayList(u8).initCapacity(gpa, 4096);
    defer lineBuffer.deinit(gpa);

    try stdout.print(" ** Puni scheme 0.01 **\n", .{});
    try stdout.flush();
    _ = linenoise.linenoiseHistorySetMaxLen(256);
    linenoise.linenoiseSetMultiLine(1);
    var prompt = "==> ";
    repl: while (linenoise.linenoise(prompt)) |line| {

        var arena = std.heap.ArenaAllocator.init(gpa);
        defer arena.deinit();

        var compiler = Compiler.init(arena.allocator(), gpa, vm);
        defer compiler.deinit();
        compiler.getPrims();
        const lastLineLength = std.mem.len(line);
        try lineBuffer.appendSlice(gpa, std.mem.span(line));
        _ = linenoise.linenoiseHistoryAdd(line);
        linenoise.linenoiseFree(line);

        try lineBuffer.append(gpa, 0);
        parser.parse(lineBuffer.items[0 .. lineBuffer.items.len - 1 :0], arena.allocator()) catch |e| {
            switch (e) {
                ParserError.UnmatchedParen, lex.LexerError.UnclosedString => {
                    _ = lineBuffer.pop();
                    if (lastLineLength != 0) {
                        try lineBuffer.append(gpa, '\n');
                        vm.stack.clear();
                        // vm.returnStack.clear();
                        prompt = "    ";
                        continue :repl;
                    }
                },
                else => {},
            }
            lineBuffer.clearRetainingCapacity();
            vm.stack.clear();
            try stdout.print("parse error: {any}\n", .{e});
            try stdout.flush();
            prompt = "==> ";
            continue :repl;
        };
        while (vm.stack.size > 0) {
            const start = compiler.compile() catch |e| {
                lineBuffer.clearRetainingCapacity();
                vm.stack.clear();
                prompt = "==> ";
                try stdout.print("compile error: {any}\n", .{e});
                try stdout.flush();
                continue :repl;
            };

            vm.closure = _nil;
            vm.frame = 0;
            vm.ip = start;
            vm.run() catch |e| {
                lineBuffer.clearRetainingCapacity();
                vm.stack.clear();
                prompt = "==> ";
                try stdout.print("error: {any}\n", .{e});
                try stdout.flush();
                continue :repl;
            };
           
            const item = try vm.stack.pop();
            if (item.getId() != .void) {
                item.debugprint("");
            }
        }
        lineBuffer.clearRetainingCapacity();
        // vm.printReturnStack();
        vm.stack.clear();
        prompt = "==> ";

        // try stdout.print(">> ", .{});
        // try stdout.flush();
    }
}
// const initialBuffer = @embedFile("./init.scm");
pub fn main() !void {
    var threaded: std.Io.Threaded = .init_single_threaded;
     { // Change current working directory to where the executable is located.
        var buffer: [1024]u8 = undefined;
        // std.fs.path.
        const n = std.process.executableDirPath(threaded.io(), buffer[0..]) catch @panic("can't get executable path");
        try std.Io.Threaded.chdir(buffer[0..n]);
    }
    // std.debug.print("size of instruction: {d}\n", .{@sizeOf(Instruction)}); // 16 byte instr..
    // const k = @sizeOf(NodePtr);
    // const m = @sizeOf(Node);
    // const n = @sizeOf(Node.Pair);
    // std.debug.print("size of cons: {d}; size of node base: {d}; nodept?: {d}\n", .{n, m, k});
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    var vm = try VM.init(allocator, threaded.io(), true);
    defer vm.destroy();
    

    // for (0..7) |_| {
    //     try vm.bldr.newIntNumber(0);
    //     // try vm.stack.push(NodePtr.
    // }
    // try vm.bldr.newIntNumber(1);
    // try vm.bldr.newIntNumber(2);
    // try vm.bldr.newIntNumber(3);
    //
    // vm.printStack();
    //
    // try vm.stack.shift(3, 7);
    //
    // vm.printStack();
    //
    // if (true) {
    //     return;
    // }

    var parser = Parser.init(vm);
    // vm.disableGC = true;
    {
        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();
        const file = try std.Io.Dir.cwd().openFile(threaded.io(), "scheme/init.scm", .{});
        defer file.close(threaded.io());
        var buffer: [1024*1024]u8 = undefined;
        const len = try std.Io.File.readPositionalAll(file, threaded.io(), &buffer, 0);
        buffer[len] = 0;
        try parser.parse(buffer[0..len :0], arena.allocator());

        var compiler = Compiler.init(arena.allocator(), allocator, vm);
        defer compiler.deinit();

        compiler.getPrims();

        if (vm.stack.size > 0) {
            try vm.stack.reverseInPlace(vm.stack.size);
        }

        while (vm.stack.size > 0)  {
           const start = try compiler.compile();
           vm.ip = start;
           try vm.run();
                const item = try vm.stack.pop();
                if (item.getId() != .void) {
                    item.debugprint("");
                }
        }

        // for (vm.code.items, 0..) |*instr, i| {
        //     std.debug.print("{d}:\t ", .{i});
        //     instr.print();
        // }


    }
    // defer std.debug.print("...\n", .{});

    // try vm.pushExprsToCallstack();

    // std.debug.print("{*}", .{vm});


    try repl(allocator, vm, &parser);
    std.debug.print("\n", .{});
}
