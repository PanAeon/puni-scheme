const std = @import("std");
const lex = @import("lexer.zig");
const VM = @import("main.zig").VM;
pub const ParserError = error{ WrongImproperList, IllegalUseOfDot, InvalidStringEscape, UnmatchedParen, UnmatchedRParen };
const _true = @import("Node.zig")._true;
const _false = @import("Node.zig")._false;


pub const Parser = struct {
    lexer: lex.Lexer = undefined,
    vm: *VM,
    token: lex.Token = undefined,
    arena: std.mem.Allocator = undefined,

    pub fn init(vm: *VM) Parser {
        return .{
            .vm = vm,
        };
    }

    pub fn parse(self: *Parser, buff: [:0]const u8, arena: std.mem.Allocator) !void {
        self.lexer = lex.Lexer.init(buff);
        self.arena = arena;
        try self.parseExprs();
    }

    pub fn parseExprs(self: *Parser) !void {
        var n: usize = 0;
        while (true) {
            self.token = try self.lexer.nextToken();
            if (self.token.id == .eof) {
                break;
            }
            try self.parseExpr();
            n += 1;
        }
    }
    pub fn parseExpr(self: *Parser) anyerror!void {
        switch (self.token.id) {
            .boolean => {
                const b = std.mem.eql(u8, "#t", self.token.slice(self.lexer.yyinput));
                try self.vm.bldr.newBool(b);
            },
            .lparen => {
                var n: usize = 0;
                const isDotted = try self.parseList(&n);
                if (!isDotted) {
                    try self.vm.bldr.newList();
                } else {
                    n -= 1;
                }
                for (0..n) |_| {
                    try self.vm.bldr.appendToListRev();
                }
            },
            .intNumber => {
                const n = try std.fmt.parseInt(i48, self.token.slice(self.lexer.yyinput), 0);
                try self.vm.bldr.newIntNumber(n);
            },
            .floatNumber => {
                const n = try std.fmt.parseFloat(f32, self.token.slice(self.lexer.yyinput));
                try self.vm.bldr.newFloatNumber(n);
            },
            .character => {
                const c =  self.token.slice(self.lexer.yyinput);
                // std.debug.print("{s}\n", .{c});
                if (std.mem.eql(u8, "#\\newline", c )) {
                   try self.vm.bldr.newChar(10);
                } else if (std.mem.eql(u8, "#\\space", c )) {
                   try self.vm.bldr.newChar(32);
                } else {
                   try self.vm.bldr.newChar(c[2]);
                }
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
                try self.vm.bldr.newString(buff[0..j]);
            },
            .identifier => {
                try self.vm.bldr.newAtom(self.token.slice(self.lexer.yyinput));
            },
            .quote => {
                self.token = try self.lexer.nextToken();
                try self.parseExpr();
                try self.vm.bldr.newList();
                try self.vm.bldr.appendToListRev();
                try self.vm.bldr.newAtom("quote");
                try self.vm.bldr.appendToList();
            },
            .quasiquote => {
                self.token = try self.lexer.nextToken();
                try self.parseExpr();
                try self.vm.bldr.newList();
                try self.vm.bldr.appendToListRev();
                try self.vm.bldr.newAtom("quasiquote");
                try self.vm.bldr.appendToList();
            },
            .unquote => {
                self.token = try self.lexer.nextToken();
                try self.parseExpr();
                try self.vm.bldr.newList();
                try self.vm.bldr.appendToListRev();
                try self.vm.bldr.newAtom("unquote");
                try self.vm.bldr.appendToList();
            },
            .unquote_splicing => {
                self.token = try self.lexer.nextToken();
                try self.parseExpr();
                try self.vm.bldr.newList();
                try self.vm.bldr.appendToListRev();
                try self.vm.bldr.newAtom("unquote-splicing");
                try self.vm.bldr.appendToList();
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
    pub fn parseList(self: *Parser, n: *usize) !bool {
        while (true) {
            self.token = try self.lexer.nextToken();
            if (self.token.id == .rparen) {
                break;
            }
            if (self.token.id == .identifier and std.mem.eql(u8, ".", self.token.slice(self.lexer.yyinput))) {
                if (n.* == 0) {
                    return error.IllegalUseOfDot;
                }
                self.token = try self.lexer.nextToken();
                try self.parseExpr();
                n.* += 1;
                self.token = try self.lexer.nextToken();
                if (self.token.id != .rparen) {
                    _ = try self.vm.stack.pop();
                    return error.WrongImproperList;
                }
                return true;
            }
            try self.parseExpr();
            n.* += 1;
        }
        return false;
    }
};

