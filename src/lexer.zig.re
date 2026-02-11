// re2zig $INPUT -o $OUTPUT --utf8

// https://conservatory.scheme.org/schemers/Documents/Standards/R5RS/HTML/r5rs-Z-H-10.html#%_sec_7.1.1
const std = @import("std");

%{include "unicode_categories.re" %}

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
const bufsize = 4095;

pub const Lexer = struct {
    file: *std.Io.Reader,
    yyinput: [bufsize+1] u8,
    yycursor: usize = 0,
    yymarker: usize = 0,
    yylimit: usize,
    token: usize,
    eof: bool,

    pub fn init(file: *std.Io.Reader) Lexer {
        var lexer: Lexer =  .{ 
        .file = file,
        .yyinput = undefined,  
        .yycursor = bufsize,
        .yymarker = bufsize,
        .yylimit = bufsize,
        .token = bufsize,
        .eof = false
        };
        lexer.yyinput[lexer.yylimit] = 0;
        return lexer;
    }

    pub fn nextToken(self:*Lexer) LexerError!Token {
       return _nextToken(self);
    }
};

fn fill(st: *Lexer) i32 {
    if (st.eof) { return -1; } // unexpected EOF

    // Error: lexeme too long. In real life can reallocate a larger buffer.
    if (st.token < 1) { return -2; }

    // Shift buffer contents (discard everything up to the current token).
    std.mem.copyBackwards(
        u8, st.yyinput[0..st.yylimit - st.token], st.yyinput[st.token..st.yylimit]);
    st.yycursor -= st.token;
    st.yymarker = @subWithOverflow(st.yymarker, st.token)[0];
    st.yylimit -= st.token;
    st.token = 0;

    // Fill free space at the end of buffer with new data from file.
    st.yylimit += st.file.readSliceShort(st.yyinput[st.yylimit..bufsize]) catch 0;
    st.yyinput[st.yylimit] = 0; // append sentinel symbol

    // If read less than expected, this is the end of input.
    st.eof = st.yylimit < bufsize;

    return 0;
}

pub fn _nextToken(yyrecord: *Lexer) LexerError!Token {
   loop: while(true) {
      //const start = yyrecord.yycursor;
      yyrecord.token = yyrecord.yycursor;
        %{
            re2c:encoding:utf8 = 1;
            re2c:api = record;
            re2c:YYFILL = "fill(yyrecord) == 0";
            //re2c:yyfill:enable = 0;
            re2c:eof = 0;

            digit = [0-9];
            peculiar_id = "+" | "-" | "...";
            letter = [a-zA-Z] | L | Nl;
            special_initial =  "!" | "$" | "%" | "&" | "*" | "/" | ":" | "<" | "=" | ">" | "?" | "^" | "_" | "~";
            initial = letter | special_initial ;
            special_subsequent = "+" | "." | "-" | "@" | Mn | Mc | Nd | Pc | [\u200D\u05F3];
            subsequent = initial | digit | special_subsequent ;

            identifier = (initial subsequent*) | peculiar_id;
            str = ["] ([^"\\] | [\\][^])* ["];
            comment = ";"[^\n]*;
            blockComment = "#|"[^|]*"|#";
            // floating-point numbers
            frac  = [0-9]* "." [0-9]+ | [0-9]+ ".";
            exp   = 'e' [+-]? [0-9]+;
            float = frac exp? | [0-9]+ exp;

            token = "." ; 

            "-"?[0-9]+       { return .{ .id = .intNumber, .start = yyrecord.token, .end = yyrecord.yycursor}; }
            float        { return .{ .id = .floatNumber, .start = yyrecord.token, .end = yyrecord.yycursor}; }
            "("          { return .{ .id = .lparen, .start = yyrecord.token, .end = yyrecord.yycursor}; }
            ")"          { return .{ .id = .rparen, .start = yyrecord.token, .end = yyrecord.yycursor}; }
            "["          { return .{ .id = .lparen, .start = yyrecord.token, .end = yyrecord.yycursor}; }
            "]"          { return .{ .id = .rparen, .start = yyrecord.token, .end = yyrecord.yycursor}; }
            "#t"         { return .{ .id = .boolean,  .start = yyrecord.token, .end = yyrecord.yycursor}; }
            "#f"         { return .{ .id = .boolean, .start = yyrecord.token, .end = yyrecord.yycursor}; }
            "'"          { return .{ .id = .quote,   .start = yyrecord.token, .end = yyrecord.yycursor}; }
            "`"          { return .{ .id = .quasiquote, .start = yyrecord.token, .end = yyrecord.yycursor}; }
            ","          { return .{ .id = .unquote,   .start = yyrecord.token, .end = yyrecord.yycursor}; }
            ",@"         { return .{ .id = .unquote_splicing,   .start = yyrecord.token, .end = yyrecord.yycursor}; }
            "#'"         { return .{ .id = .syntax,   .start = yyrecord.token, .end = yyrecord.yycursor}; }
            "#`"         { return .{ .id = .quasisyntax,   .start = yyrecord.token, .end = yyrecord.yycursor}; }
            "#,"         { return .{ .id = .unsyntax,   .start = yyrecord.token, .end = yyrecord.yycursor}; }
            "#,@"        { return .{ .id = .unsyntax_splicing,   .start = yyrecord.token, .end = yyrecord.yycursor}; }
            "#"/"("      { return .{ .id = .vector, .start = yyrecord.token, .end = yyrecord.yycursor}; }
            identifier   { return .{ .id = .identifier, .start = yyrecord.token, .end = yyrecord.yycursor}; }
            token        { return .{ .id = .identifier, .start = yyrecord.token, .end = yyrecord.yycursor}; }
            str          { return .{ .id = .string, .start = yyrecord.token, .end = yyrecord.yycursor}; }
            "#\\space"   { return .{ .id = .character, .start = yyrecord.token, .end = yyrecord.yycursor}; }
            "#\\newline" { return .{ .id = .character, .start = yyrecord.token, .end = yyrecord.yycursor}; }
            "#\\".       { return .{ .id = .character, .start = yyrecord.token, .end = yyrecord.yycursor}; }
            [ \r\n\t]+   { continue :loop; }
            comment      { continue :loop; }
            blockComment { continue :loop; }
            "\""         { return error.UnclosedString; }
            *            { std.debug.print("unknown token: '{s}'", .{yyrecord.yyinput[yyrecord.token..yyrecord.yycursor]}); return error.UnknownToken; }
            $            { return .{ .id = .eof, .start = yyrecord.token, .end = yyrecord.yycursor}; }
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
