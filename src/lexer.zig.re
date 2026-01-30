// re2zig $INPUT -o $OUTPUT

// https://conservatory.scheme.org/schemers/Documents/Standards/R5RS/HTML/r5rs-Z-H-10.html#%_sec_7.1.1
const std = @import("std");

pub const LexerError = error{ UnclosedString, UnknownToken };
pub const Token = struct {
    id: Id,
    start: usize,
    end: usize,


    pub const Id = enum {
        invalid,
        lparen,
        rparen,
        identifier,
        boolean,
        intNumber,
        floatNumber,
        string,
        character,
        vector,
        eof,
        quote,
        quasiquote,
        unquote,
        unquote_splicing,
        syntax,
        quasisyntax,
        unsyntax,
        unsyntax_splicing,
    };

    pub fn slice(self: Token, buff: [] const u8) []const u8 {
        return buff[self.start..self.end];
    }
};

pub const Lexer = struct {
    yyinput: [:0]const u8,
    yycursor: usize = 0,
    yymarker: usize = 0,
    yylimit: usize,

    pub fn init(text: [:0]const u8) Lexer {
        return .{ .yyinput = text, .yylimit = text.len };
    }

    pub fn nextToken(self:*Lexer) LexerError!Token {
       return _nextToken(self);
    }
};
pub fn _nextToken(yyrecord: *Lexer) LexerError!Token {
   loop: while(true) {
      const start = yyrecord.yycursor;
        %{
            re2c:api = record;
            re2c:yyfill:enable = 0;
            re2c:eof = 0;

            digit = [0-9];
            peculiar_id = "+" | "-" | "...";
            letter = [a-zA-Z];
            special_initial =  "!" | "$" | "%" | "&" | "*" | "/" | ":" | "<" | "=" | ">" | "?" | "^" | "_" | "~";
            initial = letter | special_initial ;
            special_subsequent = "+" | "." | "-" | "@" ;
            subsequent = initial | digit | special_subsequent ;

            identifier = (initial subsequent*) | peculiar_id;
            str = ["] ([^"\\] | [\\][^])* ["];
            comment = ";"[^\n]*;
            // floating-point numbers
            frac  = [0-9]* "." [0-9]+ | [0-9]+ ".";
            exp   = 'e' [+-]? [0-9]+;
            float = frac exp? | [0-9]+ exp;

            token = "." ; 

            "-"?[0-9]+       { return .{ .id = .intNumber, .start = start, .end = yyrecord.yycursor}; }
            float        { return .{ .id = .floatNumber, .start = start, .end = yyrecord.yycursor}; }
            "("          { return .{ .id = .lparen, .start = start, .end = yyrecord.yycursor}; }
            ")"          { return .{ .id = .rparen, .start = start, .end = yyrecord.yycursor}; }
            "["          { return .{ .id = .lparen, .start = start, .end = yyrecord.yycursor}; }
            "]"          { return .{ .id = .rparen, .start = start, .end = yyrecord.yycursor}; }
            "#t"         { return .{ .id = .boolean,  .start = start, .end = yyrecord.yycursor}; }
            "#f"         { return .{ .id = .boolean, .start = start, .end = yyrecord.yycursor}; }
            "'"          { return .{ .id = .quote,   .start = start, .end = yyrecord.yycursor}; }
            "`"          { return .{ .id = .quasiquote, .start = start, .end = yyrecord.yycursor}; }
            ","          { return .{ .id = .unquote,   .start = start, .end = yyrecord.yycursor}; }
            ",@"         { return .{ .id = .unquote_splicing,   .start = start, .end = yyrecord.yycursor}; }
            "#'"         { return .{ .id = .syntax,   .start = start, .end = yyrecord.yycursor}; }
            "#`"         { return .{ .id = .quasisyntax,   .start = start, .end = yyrecord.yycursor}; }
            "#,"         { return .{ .id = .unsyntax,   .start = start, .end = yyrecord.yycursor}; }
            "#,@"        { return .{ .id = .unsyntax_splicing,   .start = start, .end = yyrecord.yycursor}; }
            "#"/"("      { return .{ .id = .vector, .start = start, .end = yyrecord.yycursor}; }
            identifier   { return .{ .id = .identifier, .start = start, .end = yyrecord.yycursor}; }
            token        { return .{ .id = .identifier, .start = start, .end = yyrecord.yycursor}; }
            str          { return .{ .id = .string, .start = start, .end = yyrecord.yycursor}; }
            "#\\space"   { return .{ .id = .character, .start = start, .end = yyrecord.yycursor}; }
            "#\\newline" { return .{ .id = .character, .start = start, .end = yyrecord.yycursor}; }
            "#\\".       { return .{ .id = .character, .start = start, .end = yyrecord.yycursor}; }
            [ \r\n\t]+   { continue :loop; }
            comment      { continue :loop; }
            "\""         { return error.UnclosedString; }
            *            { std.debug.print("unknown token: '{s}'", .{yyrecord.yyinput[start..yyrecord.yycursor]}); return error.UnknownToken; }
            $            { return .{ .id = .eof, .start = start, .end = yyrecord.yycursor}; }
        %}
    }
}

pub fn main() !void {
    const text =
          \\123 foo "foo bar" "foo \t \" bar" ; foo bar
          \\ #( 1456 ()) #t #f 123.456 ... a+ b-
          \\ #\z #\space #\newline (+ . ) let* set! unquote-splicing
          ;
    //var st : State = .{ .yyinput = text, .yylimit = text.len };
    var lexer = Lexer.init(text);
    var t: Token = lexer.nextToken();
    while (t.id != .eof) {
       std.debug.print("token: {any} {s}\n", .{t.id, t.slice(text)});
       t = lexer.nextToken();
    }
}
