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












// pub fn __ap0(vm: *VM) anyerror!void {
//     const n = (try vm.returnStack.pop()).getIntValue();
//     const f = try vm.returnStack.pop();
//     vm.protect(f);
//     if (f.getId() == .procedure or f.getId() == .currentCont or f.getId() == .primitive) {
//         if (f.dontEvalArgs()) {
//             try vm.pushStackFrame(vm.builtins.__ap, vm.env, n, .{ .isReturn = true });
//             var args = (try vm.returnStack.pop());
//             try vm.returnStack.push(f);
//             while (args.getId() != .nil) {
//                 try vm.returnStack.push(args.head());
//                 args = args.tail();
//             }
//         } else {
//             try vm.pushStackFrame(vm.builtins.__ap, vm.env, n, .{ .isReturn = true });
//             try vm._evalList();
//             try vm.returnStack.push(f);
//         }
//     } else {
//         return error.NotCallable;
//     }
// }
// pub fn __ap(vm: *VM) anyerror!void {
//     // vm.printReturnStack();
//     const _n = try vm.returnStack.pop();
//     const n = _n.getIntValue();
//     if (n > vm.argsBuffer.len) {
//         return error.ArgsTooLong;
//     }
//     var arr = std.ArrayList(NodePtr).initBuffer(&vm.argsBuffer);
//     for (0..@intCast(n)) |_| {
//         const node = try vm.returnStack.pop();
//         vm.protect(node);
//         arr.appendAssumeCapacity(node);
//     }
//     const f = try vm.returnStack.pop();
//     if (f.getId() == .primitive) {
//         for (0..@intCast(n)) |i| {
//             try vm.returnStack.push(arr.items[i]);
//         }
//         try vm.returnStack.push(_n);
//         const prim = f.cast(.primitive);
//         try prim.body(vm);
//
//         if (prim.isMacro) {
//             if (prim.expandMacro) {
//                 prim.expandMacro = false; // yeah, won't work on recursive macros..
//             } else {
//               try vm.pushStackFrame(vm.builtins.__exec, vm.env, 0, .{ .isReturn = true });
//             }
//         }
    // } else if (f.getId() == .procedure) {
    //     // try self.returnStack.push(f);
    //     // std.debug.print("procedure>>\n", .{});
    //     // Node.debugprint(f);
    //     const p = f.cast(.procedure);
    //     // Node.debugprint(try self.head());
    //     // try vm._evalList(); // eval args first
    //     // Node.debugprint(p.env, "fact env:");
    //     const varargs = p.args.isImproperList();
    //     const m = p.args.len();
    //
    //     try vm.newList();
    //
    //     if (varargs) {
    //         // std.debug.print("{d}, {d}", .{n, m});
    //         try vm.newList();
    //         if(n + 1 < m) {
    //             return error.ArityMismatch;
    //         }
    //         for (@intCast(0)..@intCast(n + 1 - m)) |i| {
    //             try vm.returnStack.push(arr.items[i]);
    //             try vm.appendToList();
    //         }
    //         try vm.appendToList();
    //         for (@intCast(n - m + 1)..@intCast(n)) |i| {
    //             try vm.returnStack.push(arr.items[i]);
    //             try vm.appendToList();
    //         }
    //     } else {
    //         if(n != m) {
    //             if (getEnvReverse(vm.env, f)) |name| {
    //                 std.debug.print("in {s} ; expected: {d} actual: {d} \n", .{name, m, n});
    //             } else {
    //                 f.debugprint("<<error somewhere here");
    //                 std.debug.print("expected: {d} actual: {d} \n", .{m, n});
    //             }
    //             return error.ArityMismatch;
    //         }
    //         for (0..@intCast(n)) |i| {
        //         try vm.returnStack.push(arr.items[i]);
        //         try vm.appendToList();
        //     }
        // }
        // const args = try vm.returnStack.pop();
        // vm.protect(args);
        // // try self.returnStack.push(self.env);
        // // try self.push(self.env);
        // const prevEnv = vm.env;
        // // if (!p.isMacro) { // really bad idea
        //     vm.env = p.env;
        // // }
        // // p.args.debugprint("names, we pushing to env"); !!
        // try vm.returnStack.push(p.args); // first names;
        // if (varargs) {
        //     try vm.mkProperList();
        // }
        //
        // try vm.returnStack.push(args); // then data?
        // try vm.newEnv();
        //
        // try vm.newList();
        // try vm.newList();
        // try vm.newEnv(); // add extra env;
        // if (p.isMacro) {
        //     if (p.expandMacro) {
        //         p.expandMacro = false; // yeah, won't work on recursive macros..
        //     } else {
        //       try vm.pushStackFrame(vm.builtins.__exec, prevEnv, 0, .{ .isReturn = true });
        //     }
        //
        //     try vm.pushStackFrame(p.body, vm.env, 0, .{});
        // } else {
        //     // if (p.optimized) |opt| {
        //     //     try vm.pushStackFrame(opt, vm.env, 0, .{});
        //     // } else {
        //     try vm.pushStackFrame(p.body, vm.env, 0, .{});
        //     // }
        // }
//     } else if (f.getId() == .currentCont) {
//         try vm.restore(f.cast(.currentCont));
//         for (0..@intCast(n)) |i| {
//             try vm.returnStack.push(arr.items[i]);
//         }
//         // vm.printReturnStack();
//         // vm.printStack();
//     } else {
//         std.debug.print("can't apply: {any}\n", .{f.getId()});
//         return error.NotCallable;
//     }
// }
pub const MaxStack: usize = 1024;
pub const MaxReturnStack: usize = 1024;
pub const MaxProtectStack: usize = 1024;
pub const InitialGCThreshold: usize = 8; // 8


const InstructionTag = enum { 
    Const,
    LVar,
    GVar,
    LSet,
    GSet,
    Pop,
    TJump,
    FJump,
    Jump,
    Return,
    Args,
    JCall,
    Save,
    Fn,
    Primitive,
    Halt
};
pub const Instruction = union(InstructionTag) {
    Const: NodePtr,
    LVar: struct {u32, u32},
    GVar: NodePtr,
    LSet: struct{u32, u32},
    GSet: NodePtr,
    Pop,
    TJump :i64,
    FJump: i64,
    Jump: i64,
    Return,
    Args: usize,
    JCall: u32, // num of arguments
    Save: i64, // return address
    Fn: Function, // create closure from arg and current env, push result on the stack
    Primitive: *const Prim,
    Halt,

    const Function = struct {
        code: u32,
        numArgs: u16,
        varargs: bool,
    };

    pub fn print(self: Instruction) void {
        switch (self) {
                .Halt => std.debug.print("Halt\n", .{}),
                .Jump => |offset| std.debug.print("Jump {d}\n", .{offset}),
                .TJump =>  |offset| std.debug.print("TJump {d}\n", .{offset}),
                .FJump =>  |offset| std.debug.print("FJump {d}\n", .{offset}),
                .Const => |ptr|  { 
                     std.debug.print("Const ", .{});
                     ptr.print();
                     std.debug.print("\n", .{});
                 },
                .Primitive => |p| std.debug.print("Primitive {s}\n", .{p.name}),
                .LVar => |offset|   std.debug.print("Lvar {d} {d}\n", .{offset.@"0", offset.@"1"}),
                .GVar => |p|  std.debug.print("GVar {any}\n", .{p}),
                .LSet => |offset|  std.debug.print("Lset {d} {d}\n", .{offset.@"0", offset.@"1"}),
                .GSet => |p|  std.debug.print("GSet {any}\n", .{p}),
                .Args => |n| std.debug.print("Args {d}\n", .{n}),
                .Fn   => |x|  std.debug.print("Fn {any}\n", .{x}),
                .Save => |addr|  std.debug.print("Save {d}\n", .{addr}),
                .JCall => |numArgs|  std.debug.print("JCall {d}\n", .{numArgs}),
                .Pop => std.debug.print("Pop\n", .{}),
                .Return => std.debug.print("Return\n", .{}),
        }
    }
};

comptime {
    std.debug.assert(@sizeOf(Instruction) == 16);
}

pub const VM = struct {
    allocator: std.mem.Allocator,
    data: std.ArrayList(NodePtr),
    code: std.ArrayList(Instruction),
    ip: i64 = 0,
    globalEnv: std.StringHashMap(NodePtr),
    consAlloctor: *ConsPage,
    env: NodePtr = _nil,
    lastNode: ?NodePtr = null,
    numObjects: usize = 0,
    maxObjects: usize = InitialGCThreshold,
    stack: Stack(NodePtr) = undefined,
    protectStack: Stack(NodePtr) = undefined,
    argsBuffer: [1024]NodePtr = undefined, // TODO: remove it..
    disableGC: bool = false,
    verboseGC: bool = true,
    stats: Stats = .{},
    bldr: NodeBuilder,


    
    pub const Stats = struct {
        numAllocs: u64 = 0,
        numAllocConses: u64 = 0,
        startTime: std.time.Instant = undefined,
        gcTime: u64 = 0,
    };

    pub fn init(allocator: std.mem.Allocator, verboseGC: bool) !*VM {
        const vm = try allocator.create(VM);
        vm.* = .{
            .allocator = allocator,
            .stack = try Stack(NodePtr).init(allocator, MaxStack),
            .protectStack = try Stack(NodePtr).init(allocator, MaxProtectStack),
            .code = try std.ArrayList(Instruction).initCapacity(allocator, 16 * 1024),
            .data = try std.ArrayList(NodePtr).initCapacity(allocator, 16 * 1024),
            .globalEnv = std.StringHashMap(NodePtr).init(allocator),
            .verboseGC = verboseGC,
            .consAlloctor = try ConsPage.create(),
            .bldr = NodeBuilder.init(vm, allocator),
        };
        vm.stats.startTime = try std.time.Instant.now();
        return vm;
    }
    pub fn destroy(self: *VM) void {
        self.protectStack.clear();
        self.stack.clear();
        self.data.shrinkRetainingCapacity(0);
        self.globalEnv.clearRetainingCapacity();
        self.disableGC = false;
        self.env = _nil;
        self.gc();
        self.code.deinit(self.allocator);
        self.data.deinit(self.allocator);
        self.stack.deinit(self.allocator);
        self.globalEnv.deinit();
        self.protectStack.deinit(self.allocator);
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
        mark(self.env);
        
        for (0..self.stack.size) |i| {
            mark(self.stack.items[i]);
        }
        for (0..self.protectStack.size) |i| {
            mark(self.protectStack.items[i]);
        }
        for (self.data.items) |d| {
            mark(d);
        }
        var iter = self.globalEnv.valueIterator();
        while (iter.next()) |x| {
            mark(x.*); // will break if mark is moved to Ptr itself..
        }
    }
    pub fn mark(node: NodePtr) void {
        const id = node.getId();
        if (id == .intNumber or id == .floatNumber or id == .bool or id == .nil or id == .void or node.raw().marked) {
            return;
        }
        node.raw().marked = true;
        switch (id) {
            .bool, .string, .atom, .intNumber, .floatNumber, .nil, .void => {},
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
            .currentCont => {
                const c = node.cast(.currentCont);
                for (c.stack) |x| {
                    mark(x);
                }
                // mark(c.node);
            },
        }
    }
    // ok, so what to do with prims in globalEnv? good question
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
    pub fn sweep(self: *VM) void {
        // self.consAlloctor.gc();
        var object: *?NodePtr = &self.lastNode;
        while (object.* != null) {
            const raw = object.*.?.raw();
            if (!raw.marked) {
                const unreached = object.*.?;
                object.* = raw.next;
                const id = unreached.getId();
                switch (id) {
                    .bool => {
                        self.allocator.destroy(unreached.cast(.bool));
                    },
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
                    .currentCont => {
                        const c = unreached.cast(.currentCont);
                        self.allocator.free(c.stack);
                        self.allocator.destroy(c);
                    },
                    .procedure => {
                        const p = unreached.cast(.procedure);
                        self.allocator.destroy(p);
                    },
                    .nil, .void, .intNumber, .floatNumber => {},
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
        self.cleanUpGlobalEnv() catch { @panic("something went wrong"); };
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
                .Pop => { _ = try self.stack.pop();},
                .LVar => |offset| {
                    const value = getEnv(self.env, offset.@"0", offset.@"1");
                    if (value) |v| {
                        try self.stack.push(v);
                    } else {
                        return error.UnknownName;
                    }
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
                .LSet => |offset| {
                    const maybeRef = getEnvRef(self.env, offset.@"0", offset.@"1");
                    if (maybeRef) |ref| {
                        ref.* = try self.stack.pop();
                    } else {
                        return error.UnknownName;
                    }
                },
                .GSet => |p| {
                    const name = (try p.tryCast(.atom)).name;
                    try self.globalEnv.put(name, try self.stack.pop());
                },
                .Args => |n| {
                    const v = self.env.head().cast(.vector);

                    // const ret = try self.stack.pop();
                    for (0..n) |i| {
                        v.xs[n - i - 1] = try self.stack.pop();
                    }
                    // try self.stack.push(ret);
                    // try self.bldr.newEnv(n);
                },
                .Fn   => |x| {
                    try self.createProcedure(x);
                    // try self.bldr.newProc(x.@"0");
                },
                // .Call => |numArgs| {
                //     _ = &numArgs;
                //     const f = try (try self.stack.pop()).tryCast(.procedure);
                //     self.env = f.env;
                //     try self.bldr.newIntNumber(@intCast(self.ip + 1)); // push return value to the stack
                //     self.ip = @as(i64, @intCast(f.code)) - 1;
                // },
                .Save => |offset| {
                    try self.bldr.newIntNumber(@intCast(self.ip + offset)); // push return value to the stack
                },
                .JCall => |numArgs| { // FIXME: now i need to rotate params
                    _ = &numArgs;
                    // std.debug.print("num args? {d}\n", .{numArgs});
                    // self.printStack();
                    // @panic("stop");
                    const f = try (try self.stack.pop()).tryCast(.procedure);
                    if (f.varargs) {
                        if (numArgs < f.numArgs) {
                            return error.ArityMismatch;
                        }
                        const numLast = numArgs - f.numArgs + 1;
                        try self.bldr.newList();
                        for (0..numLast) |_| {
                            try self.bldr.appendToListRev();
                        }
                    } else {
                        if (f.numArgs != numArgs) {
                            return error.ArityMismatch;
                        }
                    }
                    // self.printStack();
                    // try self.stack.shift(numArgs + 1);
                    // self.printStack();
                    // _ = &f;
                    // @panic("stop");
                    self.env = f.env;
                    self.ip = @as(i64, @intCast(f.code)) - 1;
                },
                .Return => {
                    const value = try self.stack.pop();
                    const addr  = try self.stack.pop();
                    self.ip = (try addr.tryGetIntValue()) - 1;
                    try self.stack.push(value);
                },
            }
        }
    }

    pub fn createProcedure(self: *VM, f: Instruction.Function) anyerror!void {
        try self.bldr.newEnv(f.numArgs, false);
        try self.bldr.newProc(@intCast(f.code), f.varargs, f.numArgs);
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
        const nodes = parser.parse(lineBuffer.items[0 .. lineBuffer.items.len - 1 :0], arena.allocator()) catch |e| {
            switch (e) {
                ParserError.UnmatchedParen, lex.LexerError.UnclosedString => {
                    _ = lineBuffer.pop();
                    if (lastLineLength != 0) {
                        try lineBuffer.append(gpa, '\n');
                        // vm.returnStack.clear();
                        prompt = "    ";
                        continue :repl;
                    }
                },
                else => {},
            }
            lineBuffer.clearRetainingCapacity();
            // vm.returnStack.clear();
            try stdout.print("parse error: {any}\n", .{e});
            try stdout.flush();
            prompt = "==> ";
            continue :repl;
        };
        const start = compiler.compile(nodes) catch |e| {
            lineBuffer.clearRetainingCapacity();
            try stdout.print("compile error: {any}\n", .{e});
            try stdout.flush();
            continue :repl;
        };

    for (vm.code.items, 0..) |*instr, i| {
        std.debug.print("{d}:\t ", .{i});
        instr.print();
    }
        vm.env = _nil;
        vm.ip = start;
        vm.run() catch |e| {
            try stdout.print("error: {any}\n", .{e});
            try stdout.flush();
        };
       
        const st = if (vm.stack.size < 8) 0 else vm.stack.size - 8;
        if (vm.stack.size >= 8) {
            std.debug.print("...\n", .{});
        }
        for (st..vm.stack.size) |i| {
            const item = vm.stack.items[i];
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
    var vm = try VM.init(allocator, true);
    defer vm.destroy();
    var parser = Parser.init();
    // vm.disableGC = true;
    {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const file = try std.Io.Dir.cwd().openFile(threaded.io(), "scheme/init.scm", .{});
    var buffer: [128*1024]u8 = undefined;
    const len = try std.Io.File.readPositionalAll(file, threaded.io(), &buffer, 0);
    buffer[len] = 0;
    const nodes = try parser.parse(buffer[0..len :0], arena.allocator());

    var compiler = Compiler.init(arena.allocator(), allocator, vm);
    defer compiler.deinit();

    compiler.getPrims();


    const start = try compiler.compile(nodes);

    for (vm.code.items, 0..) |*instr, i| {
        std.debug.print("{d}:\t ", .{i});
        instr.print();
    }
    vm.ip = start;

    try vm.run();
    vm.printStack();
    }
    defer std.debug.print("...\n", .{});

    // try vm.pushExprsToCallstack();

    // std.debug.print("{*}", .{vm});


    try repl(allocator, vm, &parser);
    std.debug.print("\n", .{});
}
