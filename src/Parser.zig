const std = @import("std");
const lex = @import("lexer.zig");
pub const ParserError = error{ WrongImproperList, IllegalUseOfDot, InvalidStringEscape, UnmatchedParen, UnmatchedRParen };


pub const AstNode = struct {
    id: Id,
    pub const Id = enum(u5) {
        atom,
        intNumber,
        floatNumber,
        string,
        bool,
        list,
        improperList,
        vector,

        pub fn Type(comptime id: Id) type {
            return switch (id) {
                .atom => AstNode.Atom,
                .intNumber => AstNode.IntNumber,
                .floatNumber => AstNode.FloatNumber,
                .string => AstNode.String,
                .bool => AstNode.Bool,
                .vector => AstNode.Vector,
                .list => AstNode.List,
                .improperList => AstNode.ImproperList,
            };
        }
    };

    pub fn cast(self: *AstNode, comptime id: Id) *id.Type() {
        if (self.id != id) {
            std.debug.panic("wrong cast expected: {}, got: {}", .{ id, self.id});
        }
        return @alignCast(@fieldParentPtr("base", self));
    }

    pub const Atom = struct { base: AstNode = .{.id = .atom }, name: []const u8 };
    pub const Vector = struct {
        base: AstNode = .{ .id = .vector },
        xs: []*AstNode,
    };
    pub const List = struct {
        base: AstNode = .{ .id = .list },
        xs: []*AstNode,
    };
    pub const ImproperList = struct {
        base: AstNode = .{ .id = .improperList },
        xs: []*AstNode,
        last: *AstNode,
    };
    pub const String = struct {
        base: AstNode = .{ .id = .string },
        s: []u8,
    };
    pub const IntNumber = struct {
        base: AstNode = .{ .id = .intNumber },
        value: i48,
    };
    pub const FloatNumber = struct {
        base: AstNode = .{ .id = .floatNumber },
        value: f32,
    };
    pub const Bool = struct { base: AstNode = .{.id = .bool }, value: bool };




    pub fn debugprint(self: *AstNode, msg: []const u8) void {
        std.debug.print("{s} ", .{msg});
        std.debug.print("'", .{});
        self.print();
        std.debug.print("\n", .{});
    }
    pub fn printList(n: *const AstNode) void {
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
    pub fn print(n: *AstNode) void {
        switch (n.id) {
            .atom => {
                const b = n.cast(.atom);
                std.debug.print("{s} ", .{b.name});
            },
            .improperList => {
                const l = n.cast(.improperList);
                std.debug.print("(", .{});

                for (l.xs) |x| {
                   x.print(); 
                }
                std.debug.print(". ", .{});
                l.last.print();
                std.debug.print(") ", .{});
            },
            .list => {
                const l = n.cast(.list);
                std.debug.print("(", .{});

                for (l.xs) |x| {
                   x.print(); 
                }
                std.debug.print(") ", .{});
            },
            .intNumber => {
                std.debug.print("{d} ", .{n.cast(.intNumber).value});
            },
            .floatNumber => {
                std.debug.print("{d} ", .{n.cast(.floatNumber).value});
            },
            .vector => {
                const v = n.cast(.vector);
                std.debug.print("#(", .{});
                for (v.xs) |x| {
                 x.print();
                }
                std.debug.print(")", .{});
            },
            .string => {
                const b = n.cast(.string);
                std.debug.print("\"{s}\" ", .{b.s});
            },
            .bool => {
                if (n.cast(.bool).value) {
                    std.debug.print("#t ", .{});
                } else {
                    std.debug.print("#f ", .{});
                }
            },
        }
    }


};

pub const Parser = struct {
    lexer: lex.Lexer = undefined,
    // allocator: std.mem.Allocator,
    // vm: *VM,
    token: lex.Token = undefined,
    // arena: std.heap.ArenaAllocator = undefined,
    arena: std.mem.Allocator = undefined,

    pub fn init() Parser {
        return .{
        };
    }

    pub fn parse(self: *Parser, buff: [:0]const u8, arena: std.mem.Allocator) ![]*AstNode {
        self.lexer = lex.Lexer.init(buff);
        self.arena = arena;
        // self.arena = std.heap.ArenaAllocator.init(self.allocator);
        // self.arenaAllocator = self.arena.allocator();
        // defer self.arena.deinit();
        return self.parseExprs();
    }

    pub fn parseExprs(self: *Parser) ![]*AstNode {
        var buffer = try std.ArrayList(*AstNode).initCapacity(self.arena, 16);
        while (true) {
            self.token = try self.lexer.nextToken();
            if (self.token.id == .eof) {
                break;
            }
            try buffer.append(self.arena, try self.parseExpr());
        }
        return buffer.items;
    }
    pub fn parseExpr(self: *Parser) anyerror!*AstNode {
        switch (self.token.id) {
            .boolean => {
                const b = std.mem.eql(u8, "#t", self.token.slice(self.lexer.yyinput));
                var node = try self.arena.create(AstNode.Bool);
                node.* = .{ .value = b };
                return &node.base;
            },
            .lparen => {
                return self.parseList();
            },
            .vector => {
                self.token = try self.lexer.nextToken();
                const xs = (try self.parseList()).cast(.list).xs;
                var node = try self.arena.create(AstNode.Vector);
                node.* = .{ .xs = xs };
                return &node.base;
            },
            .intNumber => {
                const n = try std.fmt.parseInt(i48, self.token.slice(self.lexer.yyinput), 0);
                var node = try self.arena.create(AstNode.IntNumber);
                node.* = .{ .value = n };
                return &node.base;
            },
            .floatNumber => {
                const n = try std.fmt.parseFloat(f32, self.token.slice(self.lexer.yyinput));
                var node = try self.arena.create(AstNode.FloatNumber);
                node.* = .{ .value = n };
                return &node.base;
            },
            .string => {
                const slice = self.token.slice(self.lexer.yyinput);
                var buff = try self.arena.alloc(u8, slice.len);
                var i:u32 = 1;
                var j:u32 = 0;
                while (i < (slice.len - 1)) : (i += 1) {
                    const char = slice[i];
                    switch (char) {
                        '\\' => {
                            i += 1;
                            buff[j] = switch (slice[i]) {
                                '\\' => '\\',
                                'n'  => '\n',
                                'r'  => '\r',
                                't'  => '\t',
                                '"'  => '"',
                                else => return error.InvalidStringEscape,
                            };
                        },
                        else => {
                            buff[j] = char;
                        }
                    }
                    j += 1;
                }
                var node = try self.arena.create(AstNode.String);
                node.* = .{ .s = buff[0..j] };
                return &node.base;
            },
            .identifier => {
                var node = try self.arena.create(AstNode.Atom);
                node.* = .{ .name = self.token.slice(self.lexer.yyinput) };
                return &node.base;
            },
            .quote => {
                self.token = try self.lexer.nextToken();
                const expr = try self.parseExpr();
                var node = try self.arena.create(AstNode.List);
                var buff = try self.arena.alloc(*AstNode, 2);
                var atom = try self.arena.create(AstNode.Atom);
                atom.* = .{ .name = "quote" };
                buff[0] = &atom.base;
                buff[1]  = expr;
                node.* = .{ .xs = buff };
                return &node.base;
            },
            .quasiquote => {
                self.token = try self.lexer.nextToken();
                const expr = try self.parseExpr();
                var node = try self.arena.create(AstNode.List);
                var buff = try self.arena.alloc(*AstNode, 2);
                var atom = try self.arena.create(AstNode.Atom);
                atom.* = .{ .name = "quasiquote" };
                buff[0] = &atom.base;
                buff[1]  = expr;
                node.* = .{ .xs = buff };
                return &node.base;
            },
            .unquote => {
                self.token = try self.lexer.nextToken();
                const expr = try self.parseExpr();
                var node = try self.arena.create(AstNode.List);
                var buff = try self.arena.alloc(*AstNode, 2);
                var atom = try self.arena.create(AstNode.Atom);
                atom.* = .{ .name = "unquote" };
                buff[0] = &atom.base;
                buff[1]  = expr;
                node.* = .{ .xs = buff };
                return &node.base;
            },
            .unquote_splicing => {
                self.token = try self.lexer.nextToken();
                const expr = try self.parseExpr();
                var node = try self.arena.create(AstNode.List);
                var buff = try self.arena.alloc(*AstNode, 2);
                var atom = try self.arena.create(AstNode.Atom);
                atom.* = .{ .name = "unquote-splicing" };
                buff[0] = &atom.base;
                buff[1]  = expr;
                node.* = .{ .xs = buff };
                return &node.base;
            },
            .eof => {
                return error.UnmatchedParen;
            },
            .rparen => {
                return error.UnmatchedRParen;
            },
            else => {
                std.debug.panic("not implemented {any}\n", .{self.token.id});
            },
        }
    }
    pub fn parseList(self: *Parser) !*AstNode {
        var buffer = try std.ArrayList(*AstNode).initCapacity(self.arena, 32);
        // return buffer.items;
        var n:usize = 0;
        while (true) {
            self.token = try self.lexer.nextToken();
            if (self.token.id == .rparen) {
                break;
            }
            if (self.token.id == .identifier and std.mem.eql(u8, ".", self.token.slice(self.lexer.yyinput))) {
                if (n == 0) {
                    return error.IllegalUseOfDot;
                }
                self.token = try self.lexer.nextToken();
                const last =  try self.parseExpr();
                self.token = try self.lexer.nextToken();
                if (self.token.id != .rparen) {
                    return error.WrongImproperList;
                }
                var node = try self.arena.create(AstNode.ImproperList);
                node.* = .{ .xs = buffer.items, .last = last };
                return &node.base;
            }
            try buffer.append(self.arena, try self.parseExpr());
            n += 1;
        }
        var node = try self.arena.create(AstNode.List);
        node.* = .{ .xs = buffer.items };
        return &node.base;
    }
};

pub const Builder = struct {
    arena: std.mem.Allocator,
    pub fn init(arena: std.mem.Allocator) Builder {
        return .{ .arena = arena };
    }
    pub fn emptyList(self: *const Builder, n: usize) *AstNode {
       const list = self.arena.create(AstNode.List) catch @panic("oom");
        
       list.* = .{
            .xs = self.arena.alloc(*AstNode, n) catch @panic("oom"),
       };
        return &list.base;
    }
    pub fn newImproperList(self: *const Builder, xs: []const *AstNode, last: *AstNode) *AstNode {
           const list = self.arena.create(AstNode.ImproperList) catch @panic("oom");
            
           list.* = .{
              .xs = self.arena.dupe(*AstNode, xs) catch @panic("oom"),
              .last = last,
           };
            return &list.base;
    }
    pub fn newList(self: *const Builder, xs: []const *AstNode) *AstNode {
           const list = self.arena.create(AstNode.List) catch @panic("oom");
            
           list.* = .{
                .xs = self.arena.dupe(*AstNode, xs) catch @panic("oom"),
           };
            return &list.base;
    }
    pub fn newAtom(self: *const Builder, name: []const u8) *AstNode {
            const atom = self.arena.create(AstNode.Atom) catch @panic("oom");
            atom.* = .{
                .name = self.arena.dupe(u8, name) catch @panic("oom"),
            };
        return &atom.base;
    }
    pub fn newString(self: *const Builder, name: []const u8) *AstNode {
            const atom = self.arena.create(AstNode.String) catch @panic("oom");
            atom.* = .{
                .s = self.arena.dupe(u8, name) catch @panic("oom"),
            };
        return &atom.base;
    }

    pub fn intNumber(self: *const Builder, n: i48) *AstNode {
        var node = self.arena.create(AstNode.IntNumber) catch @panic("oom");
        node.* = .{ .value = n };
        return &node.base;
    }
    pub fn floatNumber(self: *const Builder, f: f32) *AstNode {
        var node = self.arena.create(AstNode.FloatNumber) catch @panic("oom");
        node.* = .{ .value = f };
        return &node.base;
    }
    pub fn boolean(self: *const Builder, b: bool) *AstNode {
        var node = self.arena.create(AstNode.Bool) catch @panic("oom");
        node.* = .{ .value = b };
        return &node.base;
    }
};

