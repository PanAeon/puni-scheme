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
        quote,
        quasiquote,
        unquote,
        unquoteSplicing,
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
                .quote => AstNode.Quote,
                .quasiquote => AstNode.Quasiquote,
                .unquote => AstNode.Unquote,
                .unquoteSplicing => AstNode.UnquoteSplicing,
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
    pub const Quote = struct {
        base: AstNode = .{ .id = .quote },
        value: *AstNode,
    };
    pub const Quasiquote = struct {
        base: AstNode = .{ .id = .quasiquote },
        value: *AstNode,
    };
    pub const Unquote = struct {
        base: AstNode = .{ .id = .unquote },
        value: *AstNode,
    };
    pub const UnquoteSplicing = struct {
        base: AstNode = .{ .id = .unquoteSplicing },
        value: *AstNode,
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
                var node = try self.arena.create(AstNode.Quote);
                node.* = .{ .value = expr };
                return &node.base;
            },
            .quasiquote => {
                self.token = try self.lexer.nextToken();
                const expr = try self.parseExpr();
                var node = try self.arena.create(AstNode.Quasiquote);
                node.* = .{ .value = expr };
                return &node.base;
            },
            .unquote => {
                self.token = try self.lexer.nextToken();
                const expr = try self.parseExpr();
                var node = try self.arena.create(AstNode.Unquote);
                node.* = .{ .value = expr };
                return &node.base;
            },
            .unquote_splicing => {
                self.token = try self.lexer.nextToken();
                const expr = try self.parseExpr();
                var node = try self.arena.create(AstNode.UnquoteSplicing);
                node.* = .{ .value = expr };
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

