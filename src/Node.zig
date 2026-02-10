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
        procedure,
        pair,
        nil,
        vector,
        void,
        // currentCont,
        char,
        resource,

        pub fn Type(comptime id: Id) type {
            return switch (id) {
                .atom => Node.Atom,
                .intNumber => @panic("error"),
                .floatNumber => @panic("error"),
                .string => Node.String,
                .bool => @panic("error"),
                .procedure => Node.Procedure,
                .vector => Node.Vector,
                .pair => Node.Pair,
                .nil => Node.Nil,
                .void => Node,
                // .currentCont => Node.CurrentCont,
                .resource => Node.Resource,
                .char => @panic("error"),
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
    pub fn getCharValue(self: *const NodePtr) u8 {
        if (self.getId() != .char) {
            std.debug.panic("can't get char value from: {}", .{self.getId()});
        }
        return @bitCast(@as(u8, @truncate(self.ptr & IntMask)));
    }
    pub fn tryGetCharValue(self: *const NodePtr) anyerror!u8 {
        if (self.getId() != .char) {
            std.debug.print("is not char: {}", .{self.getId()});
            return error.IllegalArgument;
        }
        return @bitCast(@as(u8, @truncate(self.ptr & IntMask)));
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

        var threaded: std.Io.Threaded = .init_single_threaded;
        var stdout_buffer: [2048]u8 = undefined;
        var stderr_writer = std.Io.File.stderr().writer(threaded.io(), &stdout_buffer);
        const stderr = &stderr_writer.interface;

        std.debug.print("{s} ", .{msg});
        self.print(stderr) catch { }; // ignore?
        stderr.flush() catch {}; //ignore
        std.debug.print("\n", .{});
        
    }
    pub fn print(n: *const NodePtr, writer: *std.Io.Writer) anyerror!void {
        switch (n.getId()) {
            .pair, .vector, .atom, .nil => {
                try writer.print("'", .{});
            },
            else => {},
        }
        return n.print0(writer);
    }

    pub fn print0(n: *const NodePtr, writer: *std.Io.Writer) anyerror!void {
        switch (n.getId()) {
            .atom => {
                const b = n.cast(.atom);
                try writer.print("{s} ", .{b.name});
            },
            .pair => {
                if (n.cast(.pair).fst.getId() == .atom) {
                    const name = n.cast(.pair).fst.cast(.atom).name;
                    if (std.mem.eql(u8, "quote", name)) {
                        try writer.print("'", .{});
                        const snd = n.second() catch { try writer.print("<<bare quote>>", .{}); return; };
                        try snd.print0(writer);
                        return;
                    } else if (std.mem.eql(u8, "quasiquote", name)) {
                        try writer.print("`", .{});
                        const snd = n.second()  catch { try writer.print("<<bare >>", .{}); return; };
                        try snd.print0(writer);
                        return;
                    } else if (std.mem.eql(u8, "unquote", name)) {
                        try writer.print(",", .{});
                        const snd = n.second()  catch { try writer.print("<<bare >>", .{}); return; };
                        try snd.print0(writer);
                        return;
                    } else if (std.mem.eql(u8, "unquote-splicing", name)) {
                        try writer.print(",@", .{});
                        const snd = n.second()  catch { try writer.print("<<bare >>", .{}); return; };
                        try snd.print0(writer);
                        return;
                    }
                }
                const p = n.cast(.pair);
                try writer.print("(", .{});
                try print0(&p.fst, writer);
                var x = p.snd;
                while (x.getId() == .pair) {
                    var p1 = x.cast(.pair);
                    try print0(&p1.fst, writer);
                    x = p1.snd;
                }
                // try writer.print(" . ", .{});
                if (x.getId() != .nil) {
                    try writer.print(". ", .{});
                    try print0(&x, writer);
                }
                try writer.print(") ", .{});
            },
            .nil => {
                try writer.print("() ", .{});
            },
            .void => {},
            .resource => {
                try writer.print("resource\n", .{});
            },
            // .currentCont => {
            //     try writer.print("<continuation>", .{});
            // },
            .intNumber => {
                try writer.print("{d} ", .{n.getIntValue()});
            },
            .floatNumber => {
                try writer.print("{d} ", .{n.getFloatValue()});
            },
            .char => {
                const v = n.getCharValue();
                if (v == 10) {
                   try writer.print("#\\newline ", .{});
                } else if (v == 32) {
                   try writer.print("#\\space ", .{});
                } else {
                   try writer.print("{c} ", .{v});
                }
            },
            // .primitive => {
            //     const p = n.cast(.primitive);
            //     if (p.isMacro) {
            //         try writer.print("<macro {s}> ", .{p.name});
            //     } else {
            //         try writer.print("<{s}> ", .{p.name});
            //     }
            // },
            .vector => {
                const v = n.cast(.vector);
                try writer.print("#(", .{});
                for (v.xs) |x| {
                    try print0(&x, writer);
                }
                try writer.print(")", .{});
            },
            .procedure => {
                
                // const p = n.cast(.procedure);
                // if (p.isMacro) {
                //     try writer.print("( <macro>", .{});
                // } else {
                //     try writer.print("( ...", .{});
                // }
                // p.args.print();
                // p.body.printList();
                try writer.print("proc", .{});
            },
            .string => {
                const b = n.cast(.string);
                try writer.print("\"{s}\" ", .{b.s});
            },
            .bool => {
                if (n.getBoolValue()) {
                    try writer.print("#t ", .{});
                } else {
                    try writer.print("#f ", .{});
                }
            },
        }
    }

    pub fn display(n: *const NodePtr, writer: *std.Io.Writer) anyerror!void {
        switch (n.getId()) {
            .atom => {
                const b = n.cast(.atom);
                try writer.print("{s} ", .{b.name});
            },
            .pair => {
                if (n.cast(.pair).fst.getId() == .atom) {
                    const name = n.cast(.pair).fst.cast(.atom).name;
                    if (std.mem.eql(u8, "quote", name)) {
                        try writer.print("'", .{});
                        const snd = n.second() catch { try writer.print("<<bare quote>>", .{}); return; };
                        try snd.display(writer);
                        return;
                    } else if (std.mem.eql(u8, "quasiquote", name)) {
                        try writer.print("`", .{});
                        const snd = n.second()  catch { try writer.print("<<bare >>", .{}); return; };
                        try snd.display(writer);
                        return;
                    } else if (std.mem.eql(u8, "unquote", name)) {
                        try writer.print(",", .{});
                        const snd = n.second()  catch { try writer.print("<<bare >>", .{}); return; };
                        try snd.display(writer);
                        return;
                    } else if (std.mem.eql(u8, "unquote-splicing", name)) {
                        try writer.print(",@", .{});
                        const snd = n.second()  catch { try writer.print("<<bare >>", .{}); return; };
                        try snd.display(writer);
                        return;
                    }
                }
                const p = n.cast(.pair);
                try writer.print("(", .{});
                try display(&p.fst, writer);
                var x = p.snd;
                while (x.getId() == .pair) {
                    var p1 = x.cast(.pair);
                    try display(&p1.fst, writer);
                    x = p1.snd;
                }
                // try writer.print(" . ", .{});
                if (x.getId() != .nil) {
                    try writer.print(". ", .{});
                    try display(&x, writer);
                }
                try writer.print(") ", .{});
            },
            .nil => {
                try writer.print("() ", .{});
            },
            .void => {},
            .resource => {
                try writer.print("resource\n", .{});
            },
            // .currentCont => {
            //     try writer.print("<continuation>", .{});
            // },
            .intNumber => {
                try writer.print("{d} ", .{n.getIntValue()});
            },
            .floatNumber => {
                try writer.print("{d} ", .{n.getFloatValue()});
            },
            .char => {
                const v = n.getCharValue();
                if (v == 10) {
                   try writer.print("\n", .{});
                } else if (v == 32) {
                   try writer.print(" ", .{});
                } else {
                   try writer.print("{c} ", .{v});
                }
            },
            // .primitive => {
            //     const p = n.cast(.primitive);
            //     if (p.isMacro) {
            //         try writer.print("<macro {s}> ", .{p.name});
            //     } else {
            //         try writer.print("<{s}> ", .{p.name});
            //     }
            // },
            .vector => {
                const v = n.cast(.vector);
                try writer.print("#(", .{});
                for (v.xs) |x| {
                    try display(&x, writer);
                }
                try writer.print(")", .{});
            },
            .procedure => {
                
                // const p = n.cast(.procedure);
                // if (p.isMacro) {
                //     try writer.print("( <macro>", .{});
                // } else {
                //     try writer.print("( ...", .{});
                // }
                // p.args.print();
                // p.body.printList();
                try writer.print("proc", .{});
            },
            .string => {
                const b = n.cast(.string);
                try writer.print("{s} ", .{b.s});
            },
            .bool => {
                if (n.getBoolValue()) {
                    try writer.print("#t ", .{});
                } else {
                    try writer.print("#f ", .{});
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
    pub fn tryTail(self: *const NodePtr) anyerror!NodePtr {
        if (self.getId() != .pair) {
            return error.IllegalArgument;
        }
        return self.cast(.pair).snd;
    }
    pub fn second(self: *const NodePtr) anyerror!NodePtr {
        return (try self.tryTail()).tryHead();
    }
    pub fn third(self: *const NodePtr) anyerror!NodePtr {
        return (try (try self.tryTail()).tryTail()).tryHead();
    }
    pub fn fourth(self: *const NodePtr) anyerror!NodePtr {
        return (try (try (try self.tryTail()).tryTail()).tryTail()).tryHead();
    }
    pub fn ttail(self: *const NodePtr) anyerror!NodePtr {
        return (try self.tryTail()).tryTail();
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
        env: NodePtr,
        params: []Params,

        pub const Params = struct {
          code: u32,
          numArgs: u32,
          varargs: bool,
        };
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

    pub const Resource = struct {
        base: Node = .{},
        resourceType: ResourceType,
        metadata: NodePtr,
        closed: bool = false,
        ptr: *anyopaque,

        pub const ResourceType = enum {
            outputFile
        };

        pub const OutputFile = struct {
            io: std.Io,
            buffer: [2048]u8 = undefined,
            writer: std.Io.File.Writer = undefined,
            file: std.Io.File,
        };

        pub fn getWriter(self: *Resource) anyerror!*std.Io.Writer {
            if (self.resourceType != .outputFile) {
                return error.IllegalArgument;
            }
            const _stdout = @as(*Node.Resource.OutputFile, @alignCast(@ptrCast(self.ptr)));
            return &_stdout.writer.interface;
        }
        pub fn close(self: *Resource) anyerror!void {
            if (self.resourceType != .outputFile) {
                return error.IllegalArgument;
            }
            const file = @as(*Node.Resource.OutputFile, @alignCast(@ptrCast(self.ptr)));
            // try file.writer.end(); // should we?
            file.file.close(file.io);
            self.closed = true;

        }

        pub fn finalize(self: *Resource, allocator: std.mem.Allocator) void {
            // std.debug.print("running finalizer\n", .{});
            switch (self.resourceType) {
                .outputFile => {
                    allocator.destroy(@as(*OutputFile, @alignCast(@ptrCast(self.ptr))));
                },
            }
        }
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

    // pub fn newProcFromParams(self: *NodeBuilder, params: NodePtr) !void {
    //     const xs = params.cast(.vector).xs;
    //     var ps = try self.allocator.alloc(Node.Procedure.Params, xs.len);
    //     defer self.allocator.free(ps);
    //     for (xs, 0..) |x, i| {
    //         ps[i] = .{
    //             .code = @intCast(x.head().getIntValue()),
    //             .varargs = (try x.second()).getBoolValue(),
    //             .numArgs = @intCast((try x.third()).getIntValue()),
    //         };
    //     }
    //     return self.newProc(ps);
    // }

    pub fn newProc(self: *NodeBuilder, params: []const Node.Procedure.Params) !void {
        if (self.vm.numObjects >= self.vm.maxObjects) {
            self.vm.gc();
        }
        self.vm.stats.numAllocs +%= 1;
        const l = try self.allocator.create(Node.Procedure);
        l.* = .{ .params = try self.allocator.dupe(Node.Procedure.Params, params),  .env =  try self.vm.stack.pop()};
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
        if (self.vm.symbolMap.get(xs)) |ptr| {
            try self.vm.stack.push(ptr);
            return;
        }
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
        try self.vm.symbolMap.put(l.name, ptr);
        try self.vm.data.append(self.allocator, ptr);
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
    pub fn newChar(self: *NodeBuilder, n: u8) !void {
        try self.vm.stack.push(NodePtr.initU64(n, .{ .id = .char }));
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

    pub fn newOutputFile(self: *NodeBuilder, file: std.Io.File, metadata: ?NodePtr) !void {
        const outputFile = try self.allocator.create(Node.Resource.OutputFile);
        errdefer self.allocator.destroy(outputFile);

        outputFile.* = .{
            .io = self.vm.io,
            .file = file,
        };
        outputFile.writer = outputFile.file.writer(self.vm.io, &outputFile.buffer);

        try self.newResource(metadata, .outputFile , outputFile);
    }

    pub fn newResource(self: *NodeBuilder, metadata: ?NodePtr, resourceType: Node.Resource.ResourceType, pointer: *anyopaque) !void {
        if (self.vm.numObjects >= self.vm.maxObjects) {
            self.vm.gc();
        }
        self.vm.stats.numAllocs +%= 1;
        const r = try self.allocator.create(Node.Resource);
        r.* = .{
            .metadata = if (metadata) |m| m else _void,
            .resourceType = resourceType,
            .ptr = pointer,
        };
        r.base.next = self.vm.lastNode;
        const ptr = NodePtr.init(&r.base, .{ .id = .resource });
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
               l.xs[i] = _false;
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

        try self.vm.stack.push(self.vm.closure);
        try self.appendToListRev();
        // self.vm.env = try self.vm.stack.pop();
    }


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
