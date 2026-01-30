const std = @import("std");
const builtin = @import("builtin");
const VM = @import("main.zig").VM;

pub const _nil: NodePtr = NodePtr.initU64(0, .{ .id = .nil });
pub const _void: NodePtr = NodePtr.initU64(1, .{ .id = .void });
pub const _true: NodePtr = NodePtr.initU64(2, .{ .id = .bool });
pub const _false: NodePtr = NodePtr.initU64(3, .{ .id = .bool });


// 0x0000 0048  1030 8C90 0200
//   └───┬───┘  └─────┬──────┘
//   Data Bits  48-bit Address
//              7fff 7d55 16f0
const CanonicalAddressSize: u64 = 48;
const PtrMask: u64 = 0x0000FFFFFFFFFFFF;
const FloatMask: u64 = 0x00000000FFFFFFFF;
const IntMask: u64 = 0x0000FFFFFFFFFFFF;



//credit https://vectrx.substack.com/p/pointer-tagging-in-c-the-art-of-packing
pub const NodePtr = struct {
    ptr: u64,

    pub const Tag = packed struct(u16) {
        id: NodePtr.Id,
        _: u11 = 0,
    };

    pub const Id = enum(u5) {
        atom,
        intNumber,
        floatNumber,
        string,
        bool,
        // primitive,
        procedure,
        pair,
        nil,
        vector,
        void,
        currentCont,

        pub fn Type(comptime id: Id) type {
            return switch (id) {
                .atom => Node.Atom,
                .intNumber => @panic("error"),
                .floatNumber => @panic("error"),
                .string => Node.String,
                .bool => Node.String,
                // .primitive => Node.Primitive,
                .procedure => Node.Procedure,
                .vector => Node.Vector,
                .pair => Node.Pair,
                .nil => Node.Nil,
                .void => Node,
                .currentCont => Node.CurrentCont,
            };
        }
    };

    pub fn initU64(p: u64, tag: Tag) NodePtr {
        var _p: NodePtr = .{ .ptr = p };
        _p.packTag(tag);
        return _p;
    }

    pub fn init(p: *const Node, tag: Tag) NodePtr {
        var _p: NodePtr = .{ .ptr = @as(u64, @intFromPtr(p)) };
        _p.packTag(tag);
        return _p;
    }

    pub fn packTag(self: *NodePtr, data: Tag) void {
        const packedData = @as(u64, @as(u16, @bitCast(data))) << CanonicalAddressSize;
        const maskedPtr = self.ptr & PtrMask;
        self.ptr = packedData | maskedPtr;
    }

    pub fn extractTag(self: *const NodePtr) Tag {
        return @bitCast(@as(u16, @intCast(self.ptr >> CanonicalAddressSize)));
    }

    pub fn raw(self: *const NodePtr) *Node {
        var masked = self.ptr & PtrMask;
        if (builtin.cpu.arch == .x86_64) {
            const signBit = @as(u64, 1) << @as(u64, 47);
            const signExtensionBits: u64 = 0xFFFF000000000000;
            if ((masked & signBit) != 0) {
                masked |= signExtensionBits;
            }
        }
        return @ptrFromInt(masked);
    }
    pub fn setRawPointer(self: *NodePtr, p: *Node) void {
        const data = self.extractTag();
        self.ptr = @intFromPtr(p);
        self.packTag(data);
    }
    // -------------------------- Node API -------------------------------------

    pub fn getId(self: *const NodePtr) Id {
        return self.extractTag().id;
    }

    pub fn isBoxed(self: *const NodePtr) bool {
        const id = self.extractTag().id;
        return id != .bool and id != .intNumber and id != .floatNumber and id != .nil;
    }
    pub fn getBoolValue(self: *const NodePtr) bool {
        if (self.getId() != .bool) {
            std.debug.panic("can't get bool value from: {}", .{self.getId()});
        }
        return self.equal(&_true);
    }

    pub fn getIntValue(self: *const NodePtr) i48 {
        if (self.getId() != .intNumber) {
            std.debug.panic("can't get intvalue from: {}", .{self.getId()});
        }
        return @bitCast(@as(u48, @truncate(self.ptr & IntMask)));
    }
    pub fn tryGetIntValue(self: *const NodePtr) anyerror!i48 {
        if (self.getId() != .intNumber) {
            std.debug.print("is not int: {}", .{self.getId()});
            return error.IllegalArgument;
        }
        return @bitCast(@as(u48, @truncate(self.ptr & IntMask)));
    }

    pub fn getFloatValue(self: *const NodePtr) f32 {
        if (self.getId() != .floatNumber) {
            std.debug.panic("can't get float value from: {}", .{self.getId()});
        }
        return @bitCast(@as(u32, @truncate(self.ptr & FloatMask)));
    }
    pub fn tryGetFloatValue(self: *const NodePtr) anyerror!f32 {
        if (self.getId() != .floatNumber) {
            std.debug.panic("can't get float value from: {}", .{self.getId()});
        }
        return @bitCast(@as(u32, @truncate(self.ptr & FloatMask)));
    }

    pub fn equal(a: *const NodePtr, b: *const NodePtr) bool {
        return ((a.ptr == b.ptr) and (a.extractTag() == b.extractTag()));
    }

    pub fn cast(self: *const NodePtr, comptime id: Id) *id.Type() {
        if (self.getId() != id) {
            std.debug.panic("wrong cast expected: {}, got: {}", .{ id, self.getId() });
        }
        return @alignCast(@fieldParentPtr("base", self.raw()));
    }

    pub fn tryCast(self: *const NodePtr, comptime id: Id) anyerror!*id.Type() {
        if (self.getId() != id) {
            std.debug.print("{} is not  {}\n", .{ self.getId(), id });
            self.debugprint("<actual value>");
            // @panic("gotcha!");
            return error.IllegalArgument;
        }
        return @alignCast(@fieldParentPtr("base", self.raw()));
    }

    pub fn convertToFloat(self: *const NodePtr) anyerror!f32 {
        if (self.getId() == .floatNumber) {
            return self.getFloatValue();
        } else if (self.getId() == .intNumber) {
            return @floatFromInt(self.getIntValue());
        } else {
            return error.IllegalArgument;
        }
    }

    pub fn dontEvalArgs(self: *const NodePtr) bool {
        if (self.getId() == .procedure) {
            return self.cast(.procedure).isMacro;
        } else if (self.getId() == .primitive) {
            return self.cast(.primitive).dontEvalArgs;
        }
        return false;
    }
    // pub fn isMacro(self: *const NodePtr) bool {
    //     if (self.getId() == .procedure) {
    //         return self.cast(.procedure).isMacro;
    //     } else if (self.getId() == .primitive) {
    //         return self.cast(.primitive).isMacro;
    //     }
    //     return false;
    // }

    pub fn debugprint(self: *const NodePtr, msg: []const u8) void {
        std.debug.print("{s} ", .{msg});
        std.debug.print("'", .{});
        self.print();
        std.debug.print("\n", .{});
    }
    pub fn printList(n: *const NodePtr) void {
        const p = n.cast(.pair);
        std.debug.print("(", .{});
        print(&p.fst);
        var x = p.snd;
        while (x.getId() == .pair) {
            var p1 = x.cast(.pair);
            print(&p1.fst);
            x = p1.snd;
        }
        // std.debug.print(" . ", .{});
        if (x.getId() != .nil) {
            std.debug.print(". ", .{});
            print(&x);
        }
        std.debug.print(")", .{});
    }
    pub fn print(n: *const NodePtr) void {
        switch (n.getId()) {
            .atom => {
                const b = n.cast(.atom);
                std.debug.print("{s} ", .{b.name});
            },
            .pair => {
                const p = n.cast(.pair);
                std.debug.print("(", .{});
                print(&p.fst);
                var x = p.snd;
                while (x.getId() == .pair) {
                    var p1 = x.cast(.pair);
                    print(&p1.fst);
                    x = p1.snd;
                }
                // std.debug.print(" . ", .{});
                if (x.getId() != .nil) {
                    std.debug.print(". ", .{});
                    print(&x);
                }
                std.debug.print(") ", .{});
            },
            .nil => {
                std.debug.print("() ", .{});
            },
            .void => {},
            .currentCont => {
                std.debug.print("<continuation>", .{});
            },
            .intNumber => {
                std.debug.print("{d} ", .{n.getIntValue()});
            },
            .floatNumber => {
                std.debug.print("{d} ", .{n.getFloatValue()});
            },
            // .primitive => {
            //     const p = n.cast(.primitive);
            //     if (p.isMacro) {
            //         std.debug.print("<macro {s}> ", .{p.name});
            //     } else {
            //         std.debug.print("<{s}> ", .{p.name});
            //     }
            // },
            .vector => {
                const v = n.cast(.vector);
                std.debug.print("#(", .{});
                for (v.xs) |x| {
                    print(&x);
                }
                std.debug.print(")", .{});
            },
            .procedure => {
                
                // const p = n.cast(.procedure);
                // if (p.isMacro) {
                //     std.debug.print("( <macro>", .{});
                // } else {
                //     std.debug.print("( ...", .{});
                // }
                // p.args.print();
                // p.body.printList();
                std.debug.print("proc", .{});
            },
            .string => {
                const b = n.cast(.string);
                std.debug.print("\"{s}\" ", .{b.s});
            },
            .bool => {
                if (n.getBoolValue()) {
                    std.debug.print("#t ", .{});
                } else {
                    std.debug.print("#f ", .{});
                }
            },
        }
    }

    pub fn getName(self: *const NodePtr) []const u8 {
        if (self.getId() != .atom) {
            std.debug.panic("can't getName on {any}", .{self.getId()});
        }
        return self.cast(.atom).name;
    }

    pub fn head(self: *const NodePtr) NodePtr {
        if (self.getId() != .pair) {
            std.debug.panic("can't head on {any}", .{self.getId()});
        }
        return self.cast(.pair).fst;
    }
    pub fn tryHead(self: *const NodePtr) anyerror!NodePtr {
        if (self.getId() != .pair) {
            return error.IllegalArgument;
        }
        return self.cast(.pair).fst;
    }
    pub fn tail(self: *const NodePtr) NodePtr {
        if (self.getId() != .pair) {
            std.debug.panic("can't head on {any}", .{self.getId()});
        }
        return self.cast(.pair).snd;
    }

    pub fn len(self: *const NodePtr) i48 {
        if (self.getId() == .nil) {
            return 0;
        } else if (self.getId() == .pair) {
            return 1 + self.tail().len();
        } else {
            return 1;
        }
    }
    pub fn isImproperList(self: *const NodePtr) bool { // TODO: replace with iteration
        if (self.getId() == .nil) {
            return false;
        } else if (self.getId() == .pair) {
            return isImproperList(&self.tail());
        } else {
            return true;
        }
    }
};

pub fn intNumberFromPtr(ptr: *NodePtr) i48 {
    const maskedPtr: u64 = @intFromPtr(ptr) & PtrMask;
    const truncated: u48 = @truncate(maskedPtr);
    return @bitCast(truncated);
}

pub fn ptrFromIntNumber(num: i48) *NodePtr {
    var masked: u64 = @as(u48, @bitCast(num));
    if (builtin.cpu.arch == .x86_64) {
        const signBit = @as(u64, 1) << @as(u64, 47);
        const signExtensionBits: u64 = 0xFFFF000000000000;
        if ((masked & signBit) != 0) {
            masked |= signExtensionBits;
        }
    }
    return @ptrFromInt(masked);
}



pub const Node = struct {
    next: ?NodePtr = null,
    marked: bool = false,


    pub const Procedure = struct { 
        base: Node = .{},
        code: u32,
        // optimized: ?NodePtr = null,
        env: NodePtr,
        varargs: bool,
        // isMacro: bool = false,
        // expandMacro: bool = false // hacky, but what to do?
    };

    pub const Atom = struct { base: Node = .{}, name: []const u8 };
    pub const Vector = struct {
        base: Node = .{},
        xs: []NodePtr,
    };
    pub const Pair = struct {
        base: Node = .{},
        fst: NodePtr,
        snd: NodePtr,
    };
    pub const String = struct {
        base: Node = .{},
        s: []u8,
    };
    pub const Bool = struct { base: Node = .{}, value: bool };

    pub const Nil = struct {
        base: Node = .{},
    };

    pub const CurrentCont = struct {
        base: Node = .{},
        stack: []NodePtr,
    };
};


pub const NodeBuilder = struct {
    vm: *VM,
    allocator: std.mem.Allocator,

    pub fn init(vm: *VM, allocator: std.mem.Allocator) NodeBuilder {
        return .{ .vm = vm, .allocator = allocator };
    }

    pub fn appendToList(self: *NodeBuilder) !void {
        try self.newPair();
    }
    pub fn appendToListRev(self: *NodeBuilder) !void {
        const a = try self.vm.stack.pop();
        const b = try self.vm.stack.pop();
        try self.vm.stack.push(a);
        try self.vm.stack.push(b);
        try self.newPair();
    }
    pub fn newList(self: *NodeBuilder) !void {
        try self.vm.stack.push(_nil);
    }


    pub fn newProc(self: *NodeBuilder, offset: u32, varargs: bool) !void {
        if (self.vm.numObjects >= self.vm.maxObjects) {
            self.vm.gc();
        }
        self.vm.stats.numAllocs +%= 1;
        const l = try self.allocator.create(Node.Procedure);
        l.* = .{ .code = offset,  .env =  try self.vm.stack.pop(), .varargs = varargs };
        l.base.next = self.vm.lastNode;
        const ptr = NodePtr.init(&l.base, .{ .id = .procedure });
        self.vm.lastNode = ptr;
        self.vm.numObjects += 1;

        try self.vm.stack.push(ptr);
    }
    pub fn newString(self: *NodeBuilder, xs: []const u8) !void {
        if (self.vm.numObjects >= self.vm.maxObjects) {
            self.vm.gc();
        }
        self.vm.stats.numAllocs +%= 1;
        const l = try self.allocator.create(Node.String);
        l.* = .{
            .s = try self.allocator.dupe(u8, xs),
        };
        l.base.next = self.vm.lastNode;
        const ptr = NodePtr.init(&l.base, .{ .id = .string });
        self.vm.lastNode = ptr;
        self.vm.numObjects += 1;
        try self.vm.stack.push(ptr);
    }
    pub fn newAtom(self: *NodeBuilder, xs: []const u8) !void {
        if (self.vm.numObjects >= self.vm.maxObjects) {
            self.vm.gc();
        }
        self.vm.stats.numAllocs +%= 1;
        const l = try self.allocator.create(Node.Atom);
        l.* = .{
            .name = try self.allocator.dupe(u8, xs),
        };
        l.base.next = self.vm.lastNode;
        const ptr = NodePtr.init(&l.base, .{ .id = .atom });
        self.vm.lastNode = ptr;
        self.vm.numObjects += 1;
        try self.vm.stack.push(ptr);
    }
    pub fn newPair(self: *NodeBuilder) !void {
        if (self.vm.numObjects >= self.vm.maxObjects) {
            self.vm.gc();
        }
        self.vm.stats.numAllocs +%= 1;
        self.vm.stats.numAllocConses +%= 1;
        const p = self.vm.consAlloctor.alloc().?;
        // const p = try self.allocator.create(Node.Pair);
        const fst = try self.vm.stack.pop();
        const snd = try self.vm.stack.pop();

        p.* = .{
            .fst = fst,
            .snd = snd,
        };
        p.base.next = self.vm.lastNode;
        const ptr = NodePtr.init(&p.base, .{ .id = .pair });
        self.vm.lastNode = ptr;
        self.vm.numObjects += 1;
        try self.vm.stack.push(ptr);
    }
    pub fn newIntNumber(self: *NodeBuilder, n: i48) !void {
        try self.vm.stack.push(NodePtr.initU64(@as(u48, @bitCast(n)), .{ .id = .intNumber }));
    }
    pub fn newFloatNumber(self: *NodeBuilder, n: f32) !void {
        try self.vm.stack.push(NodePtr.initU64(@as(u32, @bitCast(n)), .{ .id = .floatNumber }));
    }
    pub fn newBool(self: *NodeBuilder, b: bool) !void {
        if (b) {
            try self.vm.stack.push(_true);
        } else {
            try self.vm.stack.push(_false);
        }
    }
    pub fn newBuiltin(self: *NodeBuilder, f: *const fn (*VM) anyerror!void, name: []const u8, dontEvalArgs: bool, isMacro: bool) !void {
        if (self.vm.numObjects >= self.vm.maxObjects) {
            self.vm.gc();
        }
        self.vm.stats.numAllocs +%= 1;
        const func = try self.allocator.create(Node.Primitive);
        func.* = .{ .body = f, .name = name, .dontEvalArgs = dontEvalArgs, .isMacro = isMacro };
        func.base.next = self.vm.lastNode;
        const ptr = NodePtr.init(&func.base, .{ .id = .primitive });
        self.vm.lastNode = ptr;
        self.vm.numObjects += 1;
        try self.vm.stack.push(ptr);
    }

    // last item on the top of the stack
    pub fn newVector(self: *NodeBuilder, n: usize, populate: bool) !void {
        if (self.vm.numObjects >= self.vm.maxObjects) {
            self.vm.gc();
        }
        self.vm.stats.numAllocs +%= 1;
        const l = try self.allocator.create(Node.Vector);
        l.* = .{
            .xs = try self.allocator.alloc(NodePtr, n),
        };
        if (populate) {
            for (0..n) |i| {
                l.xs[n - i - 1] = try self.vm.stack.pop();
            }
        } else {
            for (0..n) |i| {
               l.xs[i] = _void;
            }
        }
        l.base.next = self.vm.lastNode;
        const ptr = NodePtr.init(&l.base, .{ .id = .vector });
        self.vm.lastNode = ptr;
        self.vm.numObjects += 1;
        try self.vm.stack.push(ptr);
    }
    // move n items from stack to env (creating new frame)
    // last item is the top of the stack
    pub fn newEnv(self: *NodeBuilder, n: usize, populate: bool) !void {
        // try self.newList();
        // try self.vm.stack.reverseInPlace(n);
        try self.newVector(n, populate);

        try self.vm.stack.push(self.vm.env);
        try self.appendToListRev();
        // self.vm.env = try self.vm.stack.pop();
    }
    // pub fn appendToLastEnv(self: *NodeBuilder) !void {
    //     const name = (try self.vm.stack.nth(1));
    //     // const values = (try self.stack.nth(0));
    //     if (name.getId() != .atom) {
    //         std.debug.print("appendToLast  names broken", .{});
    //         return error.IllegalArgument;
    //     }
    //     // if (values.getId() != .pair and values.getId() != .nil) {
    //     //     std.debug.print("appendToLast values broken", .{});
    //     //     return error.IllegalArgument;
    //     // }
    //     // (try self.stack.nth(1)).debugprint("names");
    //     // (try self.stack.nth(0)).debugprint("values");
    //     const lastEnv = self.vm.env.head(); // this is two lists ((names...)(data...))
    //     try self.vm.stack.push(lastEnv.tail().head());
    //     try self.appendToListRev();
    //     lastEnv.tail().cast(.pair).fst = try self.vm.stack.pop();
    //
    //     try self.vm.stack.push(lastEnv.head());
    //     try self.appendToListRev();
    //     lastEnv.cast(.pair).fst = try self.vm.stack.pop();
    // }


    pub fn mkProperList(self: *NodeBuilder) !void {
        var improper = try self.vm.stack.pop();
        self.vm.protect(improper);
        defer self.vm.protectStack.drop1() catch @panic("oops");
        try self.newList();
        while (improper.getId() == .pair) {
            try self.vm.stack.push(improper.head());
            try self.appendToList();
            improper = improper.tail();
        }
        try self.vm.stack.push(improper);
        try self.appendToList();
        try self.reverseList();
    }

    pub fn newCurrentCont(self: *NodeBuilder) anyerror!void {
        if (self.vm.numObjects >= self.vm.maxObjects) {
            self.vm.gc();
        }
        self.vm.stats.numAllocs +%= 1;
        const cc = try self.allocator.create(Node.CurrentCont);
        const stack = try self.allocator.alloc(NodePtr, self.vm.stack.size);
        for (0..self.vm.stack.size) |i| stack[i] = self.vm.stack.items[i];
        cc.* = .{
            .stack = stack,
        };
        cc.base.next = self.vm.lastNode;
        const ptr = NodePtr.init(&cc.base, .{ .id = .currentCont });
        self.vm.lastNode = ptr;
        self.vm.numObjects += 1;
        try self.vm.stack.push(ptr);
    }

    pub fn reverseList(self: *NodeBuilder) anyerror!void {
        var n: usize = 0;
        var ys = try self.vm.stack.pop();
        while (ys.getId() == .pair) {
            self.vm.protect(ys.head());
            ys = ys.tail();
            n += 1;
        }
        try self.newList();
        for (0..n) |i| {
            const x = try self.vm.protectStack.nth(n - 1 - i);
            try self.vm.stack.push(x);
            try self.vm.appendToList();
        }
        try self.vm.protectStack.dropN(n);
    }

};
