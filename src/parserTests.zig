const std = @import("std");
const lexer = @import("./lexer.zig");
const parser = @import("./parser.zig");
const ast = @import("./AST.zig");

test "testing lexer alloc" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const buffer = "int main(){ return 42; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(buffer)));
    _ = try std.testing.expectEqual(l.buffer, @as([]u8, @constCast(buffer)));
}

test "testing lexer basic" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const buffer = "int main(){ return 42; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(buffer)));
    const firstToken = try l.nextToken(allocator);
    const secondToken = try l.nextToken(allocator);
    const thirdToken = try l.nextToken(allocator);
    const fourthToken = try l.nextToken(allocator);
    const fifthToken = try l.nextToken(allocator);
    const sixthToken = try l.nextToken(allocator);
    const seventhToken = try l.nextToken(allocator);
    const eighthToken = try l.nextToken(allocator);
    const ninthToken = try l.nextToken(allocator);
    _ = try std.testing.expectEqual(firstToken.*, lexer.Token{ .type = lexer.TokenType.INT_TYPE, .start = 0, .end = 2 });
    _ = try std.testing.expectEqual(secondToken.*, lexer.Token{ .type = lexer.TokenType.IDENTIFIER, .start = 4, .end = 7 });
    _ = try std.testing.expectEqual(thirdToken.*, lexer.Token{ .type = lexer.TokenType.LPAREN, .start = 8, .end = 8 });
    _ = try std.testing.expectEqual(fourthToken.*, lexer.Token{ .type = lexer.TokenType.RPAREN, .start = 9, .end = 9 });
    _ = try std.testing.expectEqual(fifthToken.*, lexer.Token{ .type = lexer.TokenType.LBRACE, .start = 10, .end = 10 });
    _ = try std.testing.expectEqual(sixthToken.*, lexer.Token{ .type = lexer.TokenType.RETURN, .start = 12, .end = 17 });
    _ = try std.testing.expectEqual(seventhToken.*, lexer.Token{ .type = lexer.TokenType.INTEGER, .start = 19, .end = 20 });
    _ = try std.testing.expectEqual(try std.fmt.parseInt(u32, l.buffer[seventhToken.*.start .. seventhToken.*.end + 1], 10), 42);
    _ = try std.testing.expectEqual(eighthToken.*, lexer.Token{ .type = lexer.TokenType.SEMICOLON, .start = 21, .end = 21 });
    _ = try std.testing.expectEqual(ninthToken.*, lexer.Token{ .type = lexer.TokenType.RBRACE, .start = 23, .end = 23 });
    _ = try std.testing.expect(std.mem.eql(u8, l.buffer[ninthToken.*.start .. ninthToken.*.end + 1], "}"));
}

test "testing char lexing" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const buffer =
        \\ int main(){ char a = 'a'; char b[4] = "abc"; char c[5] = "abc\n";  return 0; }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(buffer)));
    // Advance to the 'char' keyword and assert its token type
    _ = try l.nextToken(allocator); // int
    _ = try l.nextToken(allocator); // main
    _ = try l.nextToken(allocator); // (
    _ = try l.nextToken(allocator); // )
    _ = try l.nextToken(allocator); // {
    const charToken = try l.nextToken(allocator); // char
    _ = try std.testing.expectEqual(charToken.type, lexer.TokenType.CHAR_TYPE);
    // Next tokens reflect updated program: identifier 'a' then '='
    const aTok = try l.nextToken(allocator);
    const assignTok = try l.nextToken(allocator);
    _ = try std.testing.expectEqual(aTok.type, lexer.TokenType.IDENTIFIER);
    _ = try std.testing.expectEqual(assignTok.type, lexer.TokenType.ASSIGN);
    // Char literal 'a' (peek, then consume)
    const saved_char_pos = l.current;
    const peekCharLit = (try l.peekToken(allocator)).?;
    _ = try std.testing.expectEqual(peekCharLit.type, lexer.TokenType.CHAR_LITERAL);
    _ = try std.testing.expectEqual(saved_char_pos, l.current);
    const charLitTok = try l.nextToken(allocator);
    _ = try std.testing.expectEqual(charLitTok.type, lexer.TokenType.CHAR_LITERAL);
    _ = try l.nextToken(allocator); // ;
    // Next declaration: char b[4] = "abc";
    const charToken2 = try l.nextToken(allocator); // char
    _ = try std.testing.expectEqual(charToken2.type, lexer.TokenType.CHAR_TYPE);
    const bTok = try l.nextToken(allocator); // b
    _ = try std.testing.expectEqual(bTok.type, lexer.TokenType.IDENTIFIER);
    const lSq = try l.nextToken(allocator); // [
    _ = try std.testing.expectEqual(lSq.type, lexer.TokenType.LSQUARE);
    const fourTok = try l.nextToken(allocator); // 4
    _ = try std.testing.expectEqual(fourTok.type, lexer.TokenType.INTEGER);
    const rSq = try l.nextToken(allocator); // ]
    _ = try std.testing.expectEqual(rSq.type, lexer.TokenType.RSQUARE);
    const assignTok2 = try l.nextToken(allocator); // =
    _ = try std.testing.expectEqual(assignTok2.type, lexer.TokenType.ASSIGN);
    // String literal "abc" (peek, then consume)
    const saved_str_pos = l.current;
    const peekStr = (try l.peekToken(allocator)).?;
    _ = try std.testing.expectEqual(peekStr.type, lexer.TokenType.STRING_LITERAL);
    _ = try std.testing.expectEqual(saved_str_pos, l.current);
    const strTok = try l.nextToken(allocator); // "abc"
    _ = try std.testing.expectEqual(strTok.type, lexer.TokenType.STRING_LITERAL);
    const semi1 = try l.nextToken(allocator); // ;
    _ = try std.testing.expectEqual(semi1.type, lexer.TokenType.SEMICOLON);
    // Added line: char c[5] = "abc\n";
    const charToken3 = try l.nextToken(allocator); // char
    _ = try std.testing.expectEqual(charToken3.type, lexer.TokenType.CHAR_TYPE);
    const cTok = try l.nextToken(allocator); // c
    _ = try std.testing.expectEqual(cTok.type, lexer.TokenType.IDENTIFIER);
    const lSq2 = try l.nextToken(allocator); // [
    _ = try std.testing.expectEqual(lSq2.type, lexer.TokenType.LSQUARE);
    const fiveTok = try l.nextToken(allocator); // 5
    _ = try std.testing.expectEqual(fiveTok.type, lexer.TokenType.INTEGER);
    const rSq2 = try l.nextToken(allocator); // ]
    _ = try std.testing.expectEqual(rSq2.type, lexer.TokenType.RSQUARE);
    const assignTok3 = try l.nextToken(allocator); // =
    _ = try std.testing.expectEqual(assignTok3.type, lexer.TokenType.ASSIGN);
    // String literal with escape "abc\n" (peek, then consume)
    const saved_str2_pos = l.current;
    const peekStr2 = (try l.peekToken(allocator)).?;
    _ = try std.testing.expectEqual(peekStr2.type, lexer.TokenType.STRING_LITERAL);
    _ = try std.testing.expectEqual(saved_str2_pos, l.current);
    const strTok2 = try l.nextToken(allocator); // "abc\n"
    _ = try std.testing.expectEqual(strTok2.type, lexer.TokenType.STRING_LITERAL);
    const semi2 = try l.nextToken(allocator); // ;
    _ = try std.testing.expectEqual(semi2.type, lexer.TokenType.SEMICOLON);
}

test "invalid string escape in declaration" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const buffer =
        \\ int main(){ char a[4] = "ab\y"; return 0; }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(buffer)));
    _ = try l.nextToken(allocator); // int
    _ = try l.nextToken(allocator); // main
    _ = try l.nextToken(allocator); // (
    _ = try l.nextToken(allocator); // )
    _ = try l.nextToken(allocator); // {
    _ = try l.nextToken(allocator); // char
    _ = try l.nextToken(allocator); // a
    _ = try l.nextToken(allocator); // [
    _ = try l.nextToken(allocator); // 4
    _ = try l.nextToken(allocator); // ]
    _ = try l.nextToken(allocator); // =
    // Next should be an invalid string literal due to \y
    // Expect the lexer to throw InvalidToken when lexing the string
    _ = try std.testing.expectError(lexer.LexerError.InvalidToken, l.nextToken(allocator));
}

test "invalid char escape in declaration" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const buffer =
        \\ int main(){ char c = '\\y'; return 0; }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(buffer)));
    _ = try l.nextToken(allocator); // int
    _ = try l.nextToken(allocator); // main
    _ = try l.nextToken(allocator); // (
    _ = try l.nextToken(allocator); // )
    _ = try l.nextToken(allocator); // {
    _ = try l.nextToken(allocator); // char
    _ = try l.nextToken(allocator); // c
    _ = try l.nextToken(allocator); // =
    // Peek should also report invalid escape
    _ = try std.testing.expectError(lexer.LexerError.InvalidToken, l.peekToken(allocator));
    // And advancing should also error
    _ = try std.testing.expectError(lexer.LexerError.InvalidToken, l.nextToken(allocator));
}

test "invalid char literal: multiple units" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const buffer =
        \\ int main(){ char c = 'ab'; return 0; }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(buffer)));
    _ = try l.nextToken(allocator); // int
    _ = try l.nextToken(allocator); // main
    _ = try l.nextToken(allocator); // (
    _ = try l.nextToken(allocator); // )
    _ = try l.nextToken(allocator); // {
    _ = try l.nextToken(allocator); // char
    _ = try l.nextToken(allocator); // c
    _ = try l.nextToken(allocator); // =
    _ = try std.testing.expectError(lexer.LexerError.InvalidToken, l.peekToken(allocator));
    _ = try std.testing.expectError(lexer.LexerError.InvalidToken, l.nextToken(allocator));
}

test "invalid char literal: empty" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const buffer =
        \\ int main(){ char c = ''; return 0; }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(buffer)));
    _ = try l.nextToken(allocator); // int
    _ = try l.nextToken(allocator); // main
    _ = try l.nextToken(allocator); // (
    _ = try l.nextToken(allocator); // )
    _ = try l.nextToken(allocator); // {
    _ = try l.nextToken(allocator); // char
    _ = try l.nextToken(allocator); // c
    _ = try l.nextToken(allocator); // =
    _ = try std.testing.expectError(lexer.LexerError.InvalidToken, l.peekToken(allocator));
    _ = try std.testing.expectError(lexer.LexerError.InvalidToken, l.nextToken(allocator));
}

test "unterminated char literal" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const buffer =
        \\ int main(){ char c = 'a; return 0; }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(buffer)));
    _ = try l.nextToken(allocator); // int
    _ = try l.nextToken(allocator); // main
    _ = try l.nextToken(allocator); // (
    _ = try l.nextToken(allocator); // )
    _ = try l.nextToken(allocator); // {
    _ = try l.nextToken(allocator); // char
    _ = try l.nextToken(allocator); // c
    _ = try l.nextToken(allocator); // =
    const pk = (try l.peekToken(allocator)).?;
    _ = try std.testing.expectEqual(lexer.TokenType.INVALID, pk.type);
    const nx = try l.nextToken(allocator);
    _ = try std.testing.expectEqual(lexer.TokenType.INVALID, nx.type);
}

test "unterminated string literal" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const buffer =
        \\ int main(){ char a[4] = "ab; return 0; }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(buffer)));
    _ = try l.nextToken(allocator); // int
    _ = try l.nextToken(allocator); // main
    _ = try l.nextToken(allocator); // (
    _ = try l.nextToken(allocator); // )
    _ = try l.nextToken(allocator); // {
    _ = try l.nextToken(allocator); // char
    _ = try l.nextToken(allocator); // a
    _ = try l.nextToken(allocator); // [
    _ = try l.nextToken(allocator); // 4
    _ = try l.nextToken(allocator); // ]
    _ = try l.nextToken(allocator); // =
    const pk = (try l.peekToken(allocator)).?;
    _ = try std.testing.expectEqual(lexer.TokenType.INVALID, pk.type);
    const nx = try l.nextToken(allocator);
    _ = try std.testing.expectEqual(lexer.TokenType.INVALID, nx.type);
}

test "--" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const buffer = "int main(){ return --2; }";
    //const buffer2 = "int main(){ return ~~2; }";
    //const buffer3 = "int main(){ return ~-2; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(buffer)));
    _ = try l.nextToken(allocator); //int
    _ = try l.nextToken(allocator); //main
    _ = try l.nextToken(allocator); //(
    _ = try l.nextToken(allocator); //)
    _ = try l.nextToken(allocator); //{
    _ = try l.nextToken(allocator); //return
    const decrementToken = try l.nextToken(allocator); //-
    const twoToken = try l.nextToken(allocator); //2
    //std
    _ = try std.testing.expectEqual(decrementToken.type, lexer.TokenType.DECREMENT);
    _ = try std.testing.expectEqual(twoToken.type, lexer.TokenType.INTEGER);
    //const lexer2 = try lexer.Lexer.init(allocator, @as([]u8, @constCast(buffer2)));
    //const lexer3 = try lexer.Lexer.init(allocator, @as([]u8, @constCast(buffer3)));
}

test "~-" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const buffer = "int main(){ return ~-2; }";
    //const buffer2 = "int main(){ return ~~2; }";
    //const buffer3 = "int main(){ return ~-2; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(buffer)));
    _ = try l.nextToken(allocator); //int
    _ = try l.nextToken(allocator); //main
    _ = try l.nextToken(allocator); //(
    _ = try l.nextToken(allocator); //)
    _ = try l.nextToken(allocator); //{
    _ = try l.nextToken(allocator); //return
    const tildeToken = try l.nextToken(allocator); //-
    const minusToken = try l.nextToken(allocator); //2
    //std
    _ = try std.testing.expectEqual(tildeToken.type, lexer.TokenType.TILDE);
    _ = try std.testing.expectEqual(minusToken.type, lexer.TokenType.MINUS);
    //const lexer2 = try lexer.Lexer.init(allocator, @as([]u8, @constCast(buffer2)));
    //const lexer3 = try lexer.Lexer.init(allocator, @as([]u8, @constCast(buffer3)));
}

test "+*%" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const buffer = "int main(){ return (2*3)%5+6; }";
    //const buffer2 = "int main(){ return ~~2; }";
    //const buffer3 = "int main(){ return ~-2; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(buffer)));
    _ = try l.nextToken(allocator); //int
    _ = try l.nextToken(allocator); //main
    _ = try l.nextToken(allocator); //(
    _ = try l.nextToken(allocator); //)
    _ = try l.nextToken(allocator); //{
    _ = try l.nextToken(allocator); //return
    _ = try l.nextToken(allocator); //LPAREN
    _ = try l.nextToken(allocator); //2
    const multiplyToken = try l.nextToken(allocator); //*
    _ = try l.nextToken(allocator); //3
    _ = try l.nextToken(allocator); //RPAREN
    const moduloToken = try l.nextToken(allocator);
    _ = try l.nextToken(allocator); //5
    const plusToken = try l.nextToken(allocator); //+
    //std
    _ = try std.testing.expectEqual(multiplyToken.type, lexer.TokenType.MULTIPLY);
    _ = try std.testing.expectEqual(moduloToken.type, lexer.TokenType.MODULO);
    _ = try std.testing.expectEqual(plusToken.type, lexer.TokenType.PLUS);
    //const lexer2 = try lexer.Lexer.init(allocator, @as([]u8, @constCast(buffer2)));
    //const lexer3 = try lexer.Lexer.init(allocator, @as([]u8, @constCast(buffer3)));
}

test "> and >=" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const buffer = "int main(){ return (2>3)*(2>=4); }";
    //const buffer2 = "int main(){ return ~~2; }";
    //const buffer3 = "int main(){ return ~-2; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(buffer)));
    _ = try l.nextToken(allocator); //int
    _ = try l.nextToken(allocator); //main
    _ = try l.nextToken(allocator); //(
    _ = try l.nextToken(allocator); //)
    _ = try l.nextToken(allocator); //{
    _ = try l.nextToken(allocator); //return
    _ = try l.nextToken(allocator); //LPAREN
    _ = try l.nextToken(allocator); //2
    const greaterThan = try l.nextToken(allocator); //*
    _ = try l.nextToken(allocator); //3
    _ = try l.nextToken(allocator); //RPAREN
    _ = try l.nextToken(allocator); //multiply
    _ = try l.nextToken(allocator); //LPARENj
    _ = try l.nextToken(allocator); //2
    const greaterThanEq = try l.nextToken(allocator); //>=
    _ = try l.nextToken(allocator); //4
    _ = try l.nextToken(allocator); //RPAREN
    _ = try l.nextToken(allocator); //SEMICOLON
    //std
    _ = try std.testing.expectEqual(greaterThan.type, lexer.TokenType.GREATER);
    _ = try std.testing.expectEqual(greaterThanEq.type, lexer.TokenType.GREATEREQ);
}

test "== <= and <" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const buffer = "== <= <";
    //const buffer2 = "int main(){ return ~~2; }";
    //const buffer3 = "int main(){ return ~-2; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(buffer)));
    const equals = try l.nextToken(allocator); //int
    const lessEq = try l.nextToken(allocator); //main
    const less = try l.nextToken(allocator); //(
    //std
    _ = try std.testing.expectEqual(equals.type, lexer.TokenType.EQUALS);
    _ = try std.testing.expectEqual(lessEq.type, lexer.TokenType.LESSEQ);
    _ = try std.testing.expectEqual(less.type, lexer.TokenType.LESS);
}
test "! and !=" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const buffer = "! !=";
    //const buffer2 = "int main(){ return ~~2; }";
    //const buffer3 = "int main(){ return ~-2; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(buffer)));
    const not = try l.nextToken(allocator); //not
    const notEq = try l.nextToken(allocator); //not_eq
    //std
    _ = try std.testing.expectEqual(not.type, lexer.TokenType.NOT);
    _ = try std.testing.expectEqual(notEq.type, lexer.TokenType.NOT);
}

test "Ternary and if else" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const buffer = "? : if else";
    //const buffer2 = "int main(){ return ~~2; }";
    //const buffer3 = "int main(){ return ~-2; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(buffer)));
    const ternary = try l.nextToken(allocator); //ternary
    const colon = try l.nextToken(allocator); //colon
    const ifTok = try l.nextToken(allocator); //if
    const elseTok = try l.nextToken(allocator); //if
    _ = try std.testing.expectEqual(ternary.type, lexer.TokenType.TERNARY);
    _ = try std.testing.expectEqual(colon.type, lexer.TokenType.COLON);
    _ = try std.testing.expectEqual(ifTok.type, lexer.TokenType.IF);
    _ = try std.testing.expectEqual(elseTok.type, lexer.TokenType.ELSE);
}

test "comma and void" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const buffer = ", void";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(buffer)));
    const comma = try l.nextToken(allocator);
    const voidTok = try l.nextToken(allocator);
    _ = try std.testing.expectEqual(comma.type, lexer.TokenType.COMMA);
    _ = try std.testing.expectEqual(voidTok.type, lexer.TokenType.VOID);
}

test "static and extern" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const buffer = "static extern";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(buffer)));
    const staticTok = try l.nextToken(allocator);
    const externTok = try l.nextToken(allocator);
    _ = try std.testing.expectEqual(staticTok.type, lexer.TokenType.STATIC);
    _ = try std.testing.expectEqual(externTok.type, lexer.TokenType.EXTERN);
}

test "long lexing" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const programStr = "long 102L";
    var l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    const longType = try l.nextToken(allocator);
    const longPeeked = try l.peekToken(allocator);
    const long = try l.nextToken(allocator);
    _ = try std.testing.expectEqual(lexer.TokenType.LONG_TYPE, longType.type);
    _ = try std.testing.expectEqual(lexer.TokenType.LONG, longPeeked.?.type);
    _ = try std.testing.expectEqual(lexer.TokenType.LONG, long.type);
}

test "unsigned long and unsigned integer" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const programStr = "unsigned 73UL 46U";
    var l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    _ = try std.testing.expectEqual(lexer.TokenType.UNSIGNED, (try l.nextToken(allocator)).type);
    _ = try std.testing.expectEqual(lexer.TokenType.UNSIGNED_LONG, (try l.nextToken(allocator)).type);
    _ = try std.testing.expectEqual(lexer.TokenType.UNSIGNED_INT, (try l.nextToken(allocator)).type);
}

test "lexing doubles" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const programStr = "double 32.75 &";
    var l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    _ = try std.testing.expectEqual(lexer.TokenType.FLOAT_TYPE, (try l.nextToken(allocator)).type);
    const doubleLiteral = try l.nextToken(allocator);
    const ampersand = try l.nextToken(allocator);
    _ = try std.testing.expectEqual(lexer.TokenType.FLOAT, doubleLiteral.type);
    _ = try std.testing.expect(std.mem.eql(u8, l.buffer[doubleLiteral.start .. doubleLiteral.end + 1], "32.75"));
    _ = try std.testing.expectEqual(lexer.TokenType.BITWISE_AND, ampersand.type);
}

test "array literal" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int a[3] = {1,2,3}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    _ = try std.testing.expectEqual((try l.nextToken(allocator)).type, lexer.TokenType.INT_TYPE);
    _ = try std.testing.expectEqual((try l.nextToken(allocator)).type, lexer.TokenType.IDENTIFIER);
    _ = try std.testing.expectEqual((try l.nextToken(allocator)).type, lexer.TokenType.LSQUARE);
    _ = try std.testing.expectEqual((try l.nextToken(allocator)).type, lexer.TokenType.INTEGER);
    _ = try std.testing.expectEqual((try l.nextToken(allocator)).type, lexer.TokenType.RSQUARE);
    _ = try std.testing.expectEqual((try l.nextToken(allocator)).type, lexer.TokenType.ASSIGN);
    _ = try std.testing.expectEqual((try l.nextToken(allocator)).type, lexer.TokenType.LBRACE);
    _ = try std.testing.expectEqual((try l.nextToken(allocator)).type, lexer.TokenType.INTEGER);
    _ = try std.testing.expectEqual((try l.nextToken(allocator)).type, lexer.TokenType.COMMA);
    _ = try std.testing.expectEqual((try l.nextToken(allocator)).type, lexer.TokenType.INTEGER);
    _ = try std.testing.expectEqual((try l.nextToken(allocator)).type, lexer.TokenType.COMMA);
    _ = try std.testing.expectEqual((try l.nextToken(allocator)).type, lexer.TokenType.INTEGER);
    _ = try std.testing.expectEqual((try l.nextToken(allocator)).type, lexer.TokenType.RBRACE);
}

test "testing basic parser" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ return 42; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    _ = try std.testing.expect(std.mem.eql(u8, program.externalDecls.items[0].FunctionDecl.declarator.FunDeclarator.declarator.Ident, "main"));
}

test "parse factor" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const unary = "-42";
    const l2 = try lexer.Lexer.init(allocator, @as([]u8, @constCast(unary)));
    var p2 = try parser.Parser.init(allocator, l2);
    const factor2 = try p2.parseFactor();
    _ = try std.testing.expectEqual(factor2.Unary.unaryOp, ast.UnaryOp.NEGATE);
    _ = try std.testing.expectEqual(factor2.Unary.exp.Constant.value.Integer, 42);
}

test "parse declarator" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "(*hello)(int a,int *b) = add";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const declarator = try p.parseDeclarator();
    std.log.warn("\n{}", .{declarator});
}

test "parse type" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "unsigned int";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    const p = try parser.Parser.init(allocator, l);
    _ = try std.testing.expectEqual(try p.parseType(), ast.Type.UInteger);
}

test "parsing expression with precedence" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ return 2-3*5; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[0].Statement.Return.expression});
    //_ = try std.testing.expectEqual(program.function.statement.Return.expression, 2);
}

test "more complicated precedence" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ return (2*3)%5+6; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[0].Statement.Return.expression});
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[0].Statement.Return.expression.Binary.lhs});
}

test "precedence with >= and <=" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ return 2 >= 3 + 1 <= 5; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    std.log.warn("Return exp: \x1b[34m{any}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[0].Statement.Return.expression});
    std.log.warn("Binary lhs: \x1b[34m{any}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[0].Statement.Return.expression.Binary.lhs});
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[0].Statement.Return.expression.Binary.lhs.Binary.lhs});
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[0].Statement.Return.expression.Binary.lhs.Binary.rhs});
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[0].Statement.Return.expression.Binary.rhs.Constant.value.Integer});
}

test "parsing declarations and statements" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){int x = 3; return x;}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[0]});
    std.log.warn("\x1b[34m{s}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[0].Declaration.name});
}

test "parsing declarations right associativity" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){int y; int x = y = 3; return x;}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[0]});
    std.log.warn("\x1b[34m{s}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[0].Declaration.name});
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[1]});
    std.log.warn("\x1b[34m{s}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[1].Declaration.name});
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[1].Declaration.varInitValue});
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[1].Declaration.varInitValue.?.Expression.Assignment.lhs.Identifier});
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[1].Declaration.varInitValue.?.Expression.Assignment.rhs});
}

test "test if statements" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){int y; int x = y = 3;if(x == y) return x; else return y;}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    std.log.warn("Cond: \x1b[34m{any}\x1b[0m\n", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[2].Statement.If.condition});
    std.log.warn("Then: \x1b[34m{any}\x1b[0m\n", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[2].Statement.If.thenStmt});
    std.log.warn("Else: \x1b[34m{any}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[2].Statement.If.elseStmt.?});
}

test "test ternary statements" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){int y = 4; int x = 3;return x == 3?x:y}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    std.log.warn("lhs: {any}\n", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[2].Statement.Return.expression.Ternary.lhs});
    std.log.warn("rhs: {any}\n", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[2].Statement.Return.expression.Ternary.rhs});
    std.log.warn("condition: {any}\n", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[2].Statement.Return.expression.Ternary.condition});
}

test "test label" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){int y = 4; int x = 3;goto sup;sup:return x == 3?x:y}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    std.log.warn("Goto: {s}\n", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[2].Statement.Goto});
    std.log.warn("Label: {s}\n", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[3].Statement.Label});
}

test "test compound statement parsing" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){int y = 4; int x = 2; {int x = 3;} return x+y;}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    std.log.warn("Compound statement: first statement: {any}\n", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[2].Statement.Compound.items[0]});
    std.log.warn("Declaration: of x: {s}\n", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[1].Declaration.name});
    std.log.warn("Compound statement: first statement decl varName: {s}\n", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[2].Statement.Compound.items[0].Declaration.name});
}

test "test while and dowhile" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int main(){
        \\     int x = 0;
        \\     while(x < 5) x = x + 1;
        \\     do x = x + 1; while(x < 10);
        \\     return x;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    std.log.warn("While statement: condition: {any}\n", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[1].Statement.While.condition});
    std.log.warn("While statement: body: {any}\n", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[1].Statement.While.body});
    std.log.warn("DoWhile statement: body: {any}\n", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[2].Statement.DoWhile.body});
}

test "test for statement" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int main(){
        \\     int y = 0;
        \\     for(int x = 0; x < 5; x = x + 1){ y = y + 1;}
        \\     return y;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    std.log.warn("for init:{any} ", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[1].Statement.For.init});
    std.log.warn("for condition:{any} ", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[1].Statement.For.condition});
    std.log.warn("for post:{any} ", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[1].Statement.For.post});
    std.log.warn("for statement:{any} ", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[1].Statement.For.body});
}

test "parse multiple functions" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int add(int x, int y) { return x+y;}
        \\ int main(){
        \\     return add(2,3);
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    std.log.warn("Add function : {s}, body: {any}\n", .{
        program.externalDecls.items[0].FunctionDecl.declarator.FunDeclarator.declarator.Ident,
        program.externalDecls.items[0].FunctionDecl.blockItems.items[0].Statement.Return,
    });
    std.log.warn("Main function : {s}, body: {any}\n", .{
        program.externalDecls.items[1].FunctionDecl.declarator.FunDeclarator.declarator.Ident,
        program.externalDecls.items[1].FunctionDecl.blockItems.items[0].Statement.Return.expression.FunctionCall,
    });
    for (program.externalDecls.items[1].FunctionDecl.blockItems.items[0].Statement.Return.expression.FunctionCall.args.items) |arg| {
        std.log.warn("Arg is {any}\n", .{arg});
    }
}

test "parser param types: array decays to pointer" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int f(int a[5]) { return 0; }
        \\ int main(){ return 0; }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const prog = try p.parseProgram();
    const fdecl = prog.externalDecls.items[0].FunctionDecl;
    const param = fdecl.declarator.FunDeclarator.params.items[0].NonVoidArg;
    // Expect param type is pointer to int (array parameter adjustment)
    try std.testing.expect(std.meta.activeTag(param.type) == .Pointer);
    try std.testing.expect(std.meta.activeTag(param.type.Pointer.*) == .Integer);
}

test "parser param types: multi-dim array decays one level" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int g(int a[2][3]) { return 0; }
        \\ int main(){ return 0; }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const prog = try p.parseProgram();
    const gdecl = prog.externalDecls.items[0].FunctionDecl;
    const param = gdecl.declarator.FunDeclarator.params.items[0].NonVoidArg;
    // Expect pointer to int[3]
    try std.testing.expect(std.meta.activeTag(param.type) == .Pointer);
    try std.testing.expect(std.meta.activeTag(param.type.Pointer.*) == .Array);
    try std.testing.expect(param.type.Pointer.*.Array.size == 3);
    try std.testing.expect(std.meta.activeTag(param.type.Pointer.*.Array.ty.*) == .Integer);
}

test "parser param types: pointer then array => extra pointer" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int h(int *a[5]) { return 0; }
        \\ int main(){ return 0; }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const prog = try p.parseProgram();
    const hdecl = prog.externalDecls.items[0].FunctionDecl;
    const param = hdecl.declarator.FunDeclarator.params.items[0].NonVoidArg;
    // int *a[5] parameter => decay one array layer, plus nested pointer => int**
    try std.testing.expect(std.meta.activeTag(param.type) == .Pointer);
    try std.testing.expect(std.meta.activeTag(param.type.Pointer.*) == .Pointer);
    try std.testing.expect(std.meta.activeTag(param.type.Pointer.*.Pointer.*) == .Integer);
}

test "Negation and bitwise complement codegeneration" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ return ~(-2); }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    std.log.warn("{}", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[0].Statement.Return.expression.Unary.exp});
}

test "parsing long declarations" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const statement = "long k = 32L;";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(statement)));
    const p = try parser.Parser.init(allocator, l);
    const declaration = try p.parseDeclaration();
    _ = try std.testing.expectEqual(declaration.type, ast.Type.Long);
    _ = try std.testing.expectEqualStrings(declaration.name, "k");
    _ = try std.testing.expectEqual(declaration.varInitValue.?.Expression.Constant.value.Long, 32);
}

test "parse function args as long" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const functionStr = "long add(long a, long b){ return a+b; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(functionStr)));
    const p = try parser.Parser.init(allocator, l);
    const decl = try p.parseExternalDecl();
    _ = try std.testing.expectEqual(decl.FunctionDecl.declarator.FunDeclarator.params.items[0].NonVoidArg.type, ast.Type.Long);
    _ = try std.testing.expectEqualStrings(decl.FunctionDecl.declarator.FunDeclarator.params.items[0].NonVoidArg.declarator.Ident, "a");
    _ = try std.testing.expectEqual(decl.FunctionDecl.declarator.FunDeclarator.params.items[1].NonVoidArg.type, ast.Type.Long);
    _ = try std.testing.expectEqualStrings(decl.FunctionDecl.declarator.FunDeclarator.params.items[1].NonVoidArg.declarator.Ident, "b");
}

test "divide" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int p = k / i ;";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    _ = try p.parseDeclaration();
}

test "long declarations and divide" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ long k = 3; int i = 7; int result = k / i; return result; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    _ = try p.parseProgram();
}

test "double declaration" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\double add(double a, double b) {
        \\      double k = 0.245;
        \\      return a + b;
        \\}
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const externalDecl = try p.parseExternalDecl();
    std.log.warn("externalDecl: {any}\n", .{externalDecl});
}

test "floating point representations" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const buffer = ".0004 42.3e4 3e-1";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(buffer)));
    const tokens = [_]*lexer.Token{
        try l.nextToken(allocator),
        try l.nextToken(allocator),
        try l.nextToken(allocator),
    };
    const values = [_]f64{ 0.0004, 42.3e4, 3e-1 };
    for (tokens, values) |tok, val| {
        const floatFromTok = try std.fmt.parseFloat(
            f64,
            l.buffer[tok.start .. tok.end + 1],
        );
        _ = try std.testing.expectEqual(floatFromTok, val);
    }
}

test "floating point representations (peek)" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const buffer = ".0004 42.3e4 3e-1";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(buffer)));
    const values = [_]f64{ 0.0004, 42.3e4, 3e-1 };
    for (values) |val| {
        const tok = (try l.peekToken(allocator)).?;
        const floatFromTok = try std.fmt.parseFloat(
            f64,
            l.buffer[tok.start .. tok.end + 1],
        );
        _ = try std.testing.expectEqual(floatFromTok, val);
        _ = try l.nextToken(allocator);
    }
}

test "deref and addrof parsing" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int k = 3;
        \\ int main(){
        \\     int* c = &k;
        \\     return *c;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
}

test "parse initializer" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStrings = [_][]u8{
        @constCast("{1}"),
        @constCast("{1,2}"),
        @constCast("{1,2,3,}"),
    };
    for (programStrings) |programStr| {
        const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
        var p = try parser.Parser.init(allocator, l);
        const initializer = try p.parseInitializer();
        std.log.warn("\n{}", .{initializer});
    }
}

test "parse array literal" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStrings = [_][]u8{
        @constCast("double array[4] = {1.0,2.0,3.0,4.};"),
        @constCast("long array[4] = {1};"),
    };
    const lengths = [_]usize{ 4, 1 };
    for (programStrings, lengths) |programStr, length| {
        const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
        var p = try parser.Parser.init(allocator, l);
        const declaration = try p.parseDeclaration();
        _ = try std.testing.expectEqual(declaration.varInitValue.?.ArrayExpr.initializers.items.len, length);
    }
}

test "indexing an array" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStrings = [_][]u8{
        @constCast("array[4]"),
        @constCast("array[4][8]"),
    };

    for (programStrings) |programStr| {
        const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
        var p = try parser.Parser.init(allocator, l);
        const expression = try p.parseExpression(0);
        std.log.warn("Expression: {any}\n", .{expression.ArrSubscript.index});
        if (std.meta.activeTag(expression.ArrSubscript.arr.*) == .ArrSubscript) {
            std.log.warn("Inner dimension: {any}\n", .{expression.ArrSubscript.arr.ArrSubscript.index});
        }
    }
}

test "multidim array declaration" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int k[2][2] = {{1,2},{2,3}};";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const externalDecl = try p.parseDeclaration();
    std.log.warn("externalDecl: {any}\n", .{externalDecl});
}

test "multidim arrays with fancy expressions" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "long nestedArray[3][2] = {{a, a + 1}, {3L, -4}, {foo(), 6}};";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const externalDecl = try p.parseDeclaration();
    std.log.warn("externalDecl: {any}\n", .{externalDecl});
    for (externalDecl.varInitValue.?.ArrayExpr.initializers.items) |arrayItem| {
        for (arrayItem.ArrayExpr.initializers.items) |innerItem| {
            std.log.warn("innerItem: {any}\n", .{innerItem});
        }
    }
}
