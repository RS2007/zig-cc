const std = @import("std");

pub const TokenType = enum {
    INT_TYPE,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    IDENTIFIER,
    INTEGER,
    RETURN,
    MINUS,
    TILDE,
    DECREMENT,
    PLUS,
    MULTIPLY,
    DIVIDE,
    MODULO,
    LESS,
    LESSEQ,
    GREATER,
    GREATEREQ,
    ASSIGN,
    EQUALS,
    NOT_EQUALS,
    BITWISE_AND,
    BITWISE_OR,
    LOGIC_AND,
    LOGIC_OR,
    NOT,
    INVALID,
    IF,
    ELSE,
    TERNARY,
    COLON,
    GOTO,
    DO,
    WHILE,
    FOR,
    BREAK,
    CONTINUE,
    COMMA,
    VOID,
};

pub const Token = struct {
    type: TokenType,
    start: u32,
    end: u32,
};

pub const LexerError = error{ BufferEmpty, OutOfMemory, InvalidToken };

pub const MemoryError = error{
    OutOfMemory,
};

pub const Lexer = struct {
    buffer: []u8,
    current: u32 = 0,
    currentToken: ?*Token = null,
    pub fn init(allocator: std.mem.Allocator, buffer: []u8) MemoryError!*Lexer {
        const lexer = try allocator.create(Lexer);
        lexer.* = Lexer{
            .buffer = buffer,
        };
        return lexer;
    }
    fn skipWhitespace(lexer: *Lexer) void {
        while (lexer.current < lexer.buffer.len and std.ascii.isWhitespace(lexer.buffer[lexer.current])) {
            lexer.current += 1;
        }
    }

    inline fn createSingleWidthToken(comptime tokenType: TokenType, allocator: std.mem.Allocator, lexer: *Lexer) LexerError!*Token {
        var token = try allocator.create(Token);
        token.type = tokenType;
        token.start = lexer.current;
        token.end = lexer.current;
        return token;
    }

    inline fn createDoubleWidthToken(comptime lookAheadChars: []const u8, comptime returnToks: []const TokenType, fallBackTok: TokenType, allocator: std.mem.Allocator, lexer: *Lexer) LexerError!*Token {
        var token = try allocator.create(Token);
        inline for (lookAheadChars, returnToks) |lookAheadChar, returnTok| {
            if (lexer.buffer[lexer.current + 1] == lookAheadChar) {
                token.type = returnTok;
                token.start = lexer.current;
                token.end = lexer.current + 1;
                return token;
            }
            token.type = fallBackTok;
            token.start = lexer.current;
            token.end = lexer.current;
            return token;
        }
    }

    pub fn peekTwoTokens(lexer: *Lexer, allocator: std.mem.Allocator) LexerError![]?*Token {
        lexer.skipWhitespace();
        const resetIndx = lexer.current;
        const resetToken = lexer.currentToken;
        std.log.warn("Rest index is {} and reset token is {any}\n", .{ resetIndx, resetToken });
        const first = try lexer.nextToken(allocator);
        const second = try lexer.peekToken(allocator);
        lexer.current = resetIndx;
        lexer.currentToken = resetToken;
        std.log.warn("lexer index is {} and lexer current token is {any}\n", .{ lexer.current, lexer.currentToken });
        var tokenList = try std.ArrayList(?*Token).initCapacity(allocator, 2);
        try tokenList.append(first);
        try tokenList.append(second);
        return (try tokenList.toOwnedSlice());
    }

    inline fn peekKeyword(comptime keywordStrings: []const []const u8, comptime returnTokens: []const TokenType, lexer: *Lexer, token: *Token) ?*Token {
        const initialPtr = lexer.current;
        var offset: u32 = 0;
        while (initialPtr + offset < lexer.buffer.len and !std.ascii.isWhitespace(lexer.buffer[initialPtr + offset]) and (std.ascii.isLower(lexer.buffer[initialPtr + offset]) or std.ascii.isUpper(lexer.buffer[initialPtr + offset]))) {
            offset += 1;
        }
        inline for (keywordStrings, returnTokens) |keywordString, returnTok| {
            if (std.mem.eql(u8, lexer.buffer[initialPtr .. initialPtr + offset], keywordString)) {
                token.type = returnTok;
                token.start = initialPtr;
                token.end = initialPtr + offset - 1;
                return token;
            }
        }
        token.type = TokenType.IDENTIFIER;
        token.start = initialPtr;
        token.end = initialPtr + offset - 1;
        lexer.currentToken = token;
        return token;
    }

    pub fn peekToken(lexer: *Lexer, allocator: std.mem.Allocator) LexerError!?*Token {
        lexer.skipWhitespace();
        if (lexer.current + 1 >= lexer.buffer.len) {
            return null;
        }
        switch (lexer.buffer[lexer.current]) {
            ';' => {
                return (try createSingleWidthToken(TokenType.SEMICOLON, allocator, lexer));
            },
            '(' => {
                return (try createSingleWidthToken(TokenType.LPAREN, allocator, lexer));
            },
            ')' => {
                return (try createSingleWidthToken(TokenType.RPAREN, allocator, lexer));
            },
            '?' => {
                return (try createSingleWidthToken(TokenType.TERNARY, allocator, lexer));
            },
            ':' => {
                return (try createSingleWidthToken(TokenType.COLON, allocator, lexer));
            },
            '+' => {
                return (try createSingleWidthToken(TokenType.PLUS, allocator, lexer));
            },
            '*' => {
                return (try createSingleWidthToken(TokenType.MULTIPLY, allocator, lexer));
            },
            '%' => {
                return (try createSingleWidthToken(TokenType.MODULO, allocator, lexer));
            },
            '~' => {
                return (try createSingleWidthToken(TokenType.TILDE, allocator, lexer));
            },
            '{' => {
                return (try createSingleWidthToken(TokenType.LBRACE, allocator, lexer));
            },
            '}' => {
                return (try createSingleWidthToken(TokenType.RBRACE, allocator, lexer));
            },
            ',' => {
                return (try createSingleWidthToken(TokenType.COMMA, allocator, lexer));
            },
            '<' => {
                return (try createDoubleWidthToken(&[_]u8{'='}, &[_]TokenType{TokenType.LESSEQ}, TokenType.LESS, allocator, lexer));
            },
            '>' => {
                return (try createDoubleWidthToken(&[_]u8{'='}, &[_]TokenType{TokenType.GREATEREQ}, TokenType.GREATER, allocator, lexer));
            },
            '-' => {
                return (try createDoubleWidthToken(&[_]u8{'-'}, &[_]TokenType{TokenType.DECREMENT}, TokenType.MINUS, allocator, lexer));
            },
            '=' => {
                return (try createDoubleWidthToken(&[_]u8{'='}, &[_]TokenType{TokenType.EQUALS}, TokenType.ASSIGN, allocator, lexer));
            },
            '&' => {
                return (try createDoubleWidthToken(&[_]u8{'&'}, &[_]TokenType{TokenType.LOGIC_AND}, TokenType.BITWISE_AND, allocator, lexer));
            },
            '|' => {
                return (try createDoubleWidthToken(&[_]u8{'|'}, &[_]TokenType{TokenType.LOGIC_OR}, TokenType.BITWISE_OR, allocator, lexer));
            },
            '!' => {
                return (try createDoubleWidthToken(&[_]u8{'='}, &[_]TokenType{TokenType.NOT_EQUALS}, TokenType.NOT, allocator, lexer));
            },
            else => {
                if (std.ascii.isDigit(lexer.buffer[lexer.current])) {
                    var token = try allocator.create(Token);
                    const initialPtr = lexer.current;
                    var finalPtr = lexer.current;
                    while (finalPtr < lexer.buffer.len and std.ascii.isDigit(lexer.buffer[lexer.current])) {
                        finalPtr += 1;
                    }
                    token.type = TokenType.INTEGER;
                    token.start = initialPtr;
                    token.end = finalPtr;
                    return token;
                }
                const token = try allocator.create(Token);
                const initialPtr = lexer.current;
                var offset: u32 = 0;
                while (initialPtr + offset < lexer.buffer.len and !std.ascii.isWhitespace(lexer.buffer[initialPtr + offset]) and (std.ascii.isLower(lexer.buffer[initialPtr + offset]) or std.ascii.isUpper(lexer.buffer[initialPtr + offset]))) {
                    offset += 1;
                }

                return peekKeyword(&[_][]const u8{
                    "int",
                    "return",
                    "if",
                    "else",
                    "goto",
                    "do",
                    "while",
                    "for",
                    "continue",
                    "break",
                    "void",
                }, &[_]TokenType{
                    TokenType.INT_TYPE,
                    TokenType.RETURN,
                    TokenType.IF,
                    TokenType.ELSE,
                    TokenType.GOTO,
                    TokenType.DO,
                    TokenType.WHILE,
                    TokenType.FOR,
                    TokenType.CONTINUE,
                    TokenType.BREAK,
                    TokenType.VOID,
                }, lexer, token);
            },
        }
        return null;
    }

    inline fn nextSingleWidthTokMacro(comptime tokenType: TokenType, token: *Token, lexer: *Lexer) void {
        token.type = tokenType;
        token.start = lexer.current;
        token.end = lexer.current;
        lexer.current += 1;
    }

    inline fn nextDoubleWidthTokMacro(comptime lookAheadToks: []const TokenType, comptime returnToks: []const TokenType, fallBackTok: TokenType, lexer: *Lexer, token: *Token, allocator: std.mem.Allocator) LexerError!void {
        lexer.current += 1;
        const peekedToken = try lexer.peekToken(allocator);
        if (peekedToken) |nonNullPeeked| {
            inline for (lookAheadToks, returnToks) |lookAheadTok, returnTok| {
                if (nonNullPeeked.*.type == lookAheadTok) {
                    token.type = returnTok;
                    token.start = lexer.current;
                    token.end = lexer.current + 1;
                    lexer.current += 1;
                    return;
                }
            }
            token.type = fallBackTok;
            token.start = lexer.current;
            token.end = lexer.current;
        } else {
            token.type = fallBackTok;
            token.start = lexer.current;
            token.end = lexer.current;
        }
    }

    inline fn lexKeyword(comptime keywordStrings: []const []const u8, comptime returnTokens: []const TokenType, lexer: *Lexer, token: *Token) *Token {
        const initialPtr = lexer.current;
        while (lexer.current < lexer.buffer.len and !std.ascii.isWhitespace(lexer.buffer[lexer.current]) and (std.ascii.isLower(lexer.buffer[lexer.current]) or std.ascii.isUpper(lexer.buffer[lexer.current]))) {
            lexer.current += 1;
        }
        inline for (keywordStrings, returnTokens) |keywordString, returnTok| {
            if (std.mem.eql(u8, lexer.buffer[initialPtr..lexer.current], keywordString)) {
                token.type = returnTok;
                token.start = initialPtr;
                token.end = lexer.current - 1;
                lexer.currentToken = token;
                return token;
            }
        }
        token.type = TokenType.IDENTIFIER;
        token.start = initialPtr;
        token.end = lexer.current - 1;
        lexer.currentToken = token;
        return token;
    }

    pub fn nextToken(lexer: *Lexer, allocator: std.mem.Allocator) LexerError!*Token {
        lexer.skipWhitespace();
        if (lexer.current >= lexer.buffer.len) {
            return LexerError.BufferEmpty;
        }
        var token = try allocator.create(Token);
        switch (lexer.buffer[lexer.current]) {
            '(' => {
                nextSingleWidthTokMacro(TokenType.LPAREN, token, lexer);
            },
            ')' => {
                nextSingleWidthTokMacro(TokenType.RPAREN, token, lexer);
            },
            '?' => {
                nextSingleWidthTokMacro(TokenType.TERNARY, token, lexer);
            },
            ':' => {
                nextSingleWidthTokMacro(TokenType.COLON, token, lexer);
            },
            '{' => {
                nextSingleWidthTokMacro(TokenType.LBRACE, token, lexer);
            },
            '}' => {
                nextSingleWidthTokMacro(TokenType.RBRACE, token, lexer);
            },
            ';' => {
                nextSingleWidthTokMacro(TokenType.SEMICOLON, token, lexer);
            },
            ',' => {
                nextSingleWidthTokMacro(TokenType.COMMA, token, lexer);
            },
            '!' => {
                try nextDoubleWidthTokMacro(&[_]TokenType{TokenType.ASSIGN}, &[_]TokenType{TokenType.NOT_EQUALS}, TokenType.NOT, lexer, token, allocator);
            },
            '-' => {
                try nextDoubleWidthTokMacro(&[_]TokenType{TokenType.MINUS}, &[_]TokenType{TokenType.DECREMENT}, TokenType.MINUS, lexer, token, allocator);
            },
            '=' => {
                try nextDoubleWidthTokMacro(&[_]TokenType{TokenType.ASSIGN}, &[_]TokenType{TokenType.EQUALS}, TokenType.ASSIGN, lexer, token, allocator);
            },
            '>' => {
                try nextDoubleWidthTokMacro(&[_]TokenType{TokenType.ASSIGN}, &[_]TokenType{TokenType.GREATEREQ}, TokenType.GREATER, lexer, token, allocator);
            },
            '<' => {
                try nextDoubleWidthTokMacro(&[_]TokenType{TokenType.ASSIGN}, &[_]TokenType{TokenType.LESSEQ}, TokenType.LESS, lexer, token, allocator);
            },
            '&' => {
                try nextDoubleWidthTokMacro(&[_]TokenType{TokenType.BITWISE_AND}, &[_]TokenType{TokenType.LOGIC_AND}, TokenType.BITWISE_AND, lexer, token, allocator);
            },
            '|' => {
                try nextDoubleWidthTokMacro(&[_]TokenType{TokenType.BITWISE_OR}, &[_]TokenType{TokenType.LOGIC_OR}, TokenType.BITWISE_OR, lexer, token, allocator);
            },
            '+' => {
                nextSingleWidthTokMacro(TokenType.PLUS, token, lexer);
            },
            '*' => {
                nextSingleWidthTokMacro(TokenType.MULTIPLY, token, lexer);
            },
            '%' => {
                nextSingleWidthTokMacro(TokenType.MODULO, token, lexer);
            },
            '~' => {
                nextSingleWidthTokMacro(TokenType.TILDE, token, lexer);
            },
            else => {
                if (std.ascii.isDigit(lexer.buffer[lexer.current])) {
                    const initialPtr = lexer.current;
                    while (lexer.current < lexer.buffer.len and std.ascii.isDigit(lexer.buffer[lexer.current])) {
                        lexer.current += 1;
                    }
                    token.type = TokenType.INTEGER;
                    token.start = initialPtr;
                    token.end = lexer.current;
                    lexer.currentToken = token;
                    return token;
                }
                return lexKeyword(&[_][]const u8{
                    "int",
                    "return",
                    "if",
                    "else",
                    "goto",
                    "do",
                    "while",
                    "for",
                    "continue",
                    "break",
                    "void",
                }, &[_]TokenType{
                    TokenType.INT_TYPE,
                    TokenType.RETURN,
                    TokenType.IF,
                    TokenType.ELSE,
                    TokenType.GOTO,
                    TokenType.DO,
                    TokenType.WHILE,
                    TokenType.FOR,
                    TokenType.CONTINUE,
                    TokenType.BREAK,
                    TokenType.VOID,
                }, lexer, token);
            },
        }
        lexer.currentToken = token;
        return token;
    }
};

test "testing lexer alloc" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const buffer = "int main(){ return 42; }";
    const lexer = try Lexer.init(allocator, @as([]u8, @constCast(buffer)));
    _ = try std.testing.expectEqual(lexer.buffer, @as([]u8, @constCast(buffer)));
}

test "testing lexer basic" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const buffer = "int main(){ return 42; }";
    const lexer = try Lexer.init(allocator, @as([]u8, @constCast(buffer)));
    const firstToken = try lexer.nextToken(allocator);
    const secondToken = try lexer.nextToken(allocator);
    const thirdToken = try lexer.nextToken(allocator);
    const fourthToken = try lexer.nextToken(allocator);
    const fifthToken = try lexer.nextToken(allocator);
    const sixthToken = try lexer.nextToken(allocator);
    const seventhToken = try lexer.nextToken(allocator);
    const eighthToken = try lexer.nextToken(allocator);
    const ninthToken = try lexer.nextToken(allocator);
    _ = try std.testing.expectEqual(firstToken.*, Token{ .type = TokenType.INT_TYPE, .start = 0, .end = 2 });
    _ = try std.testing.expectEqual(secondToken.*, Token{ .type = TokenType.IDENTIFIER, .start = 4, .end = 7 });
    _ = try std.testing.expectEqual(thirdToken.*, Token{ .type = TokenType.LPAREN, .start = 8, .end = 8 });
    _ = try std.testing.expectEqual(fourthToken.*, Token{ .type = TokenType.RPAREN, .start = 9, .end = 9 });
    _ = try std.testing.expectEqual(fifthToken.*, Token{ .type = TokenType.LBRACE, .start = 10, .end = 10 });
    _ = try std.testing.expectEqual(sixthToken.*, Token{ .type = TokenType.RETURN, .start = 12, .end = 17 });
    _ = try std.testing.expectEqual(seventhToken.*, Token{ .type = TokenType.INTEGER, .start = 19, .end = 21 });
    _ = try std.testing.expectEqual(try std.fmt.parseInt(u32, lexer.buffer[seventhToken.*.start..seventhToken.*.end], 10), 42);
    _ = try std.testing.expectEqual(eighthToken.*, Token{ .type = TokenType.SEMICOLON, .start = 21, .end = 21 });
    _ = try std.testing.expectEqual(ninthToken.*, Token{ .type = TokenType.RBRACE, .start = 23, .end = 23 });
    _ = try std.testing.expect(std.mem.eql(u8, lexer.buffer[ninthToken.*.start .. ninthToken.*.end + 1], "}"));
}

test "--" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const buffer = "int main(){ return --2; }";
    //const buffer2 = "int main(){ return ~~2; }";
    //const buffer3 = "int main(){ return ~-2; }";
    const lexer = try Lexer.init(allocator, @as([]u8, @constCast(buffer)));
    _ = try lexer.nextToken(allocator); //int
    _ = try lexer.nextToken(allocator); //main
    _ = try lexer.nextToken(allocator); //(
    _ = try lexer.nextToken(allocator); //)
    _ = try lexer.nextToken(allocator); //{
    _ = try lexer.nextToken(allocator); //return
    const decrementToken = try lexer.nextToken(allocator); //-
    const twoToken = try lexer.nextToken(allocator); //2
    //std
    _ = try std.testing.expectEqual(decrementToken.type, TokenType.DECREMENT);
    _ = try std.testing.expectEqual(twoToken.type, TokenType.INTEGER);
    //const lexer2 = try Lexer.init(allocator, @as([]u8, @constCast(buffer2)));
    //const lexer3 = try Lexer.init(allocator, @as([]u8, @constCast(buffer3)));
}

test "~-" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const buffer = "int main(){ return ~-2; }";
    //const buffer2 = "int main(){ return ~~2; }";
    //const buffer3 = "int main(){ return ~-2; }";
    const lexer = try Lexer.init(allocator, @as([]u8, @constCast(buffer)));
    _ = try lexer.nextToken(allocator); //int
    _ = try lexer.nextToken(allocator); //main
    _ = try lexer.nextToken(allocator); //(
    _ = try lexer.nextToken(allocator); //)
    _ = try lexer.nextToken(allocator); //{
    _ = try lexer.nextToken(allocator); //return
    const tildeToken = try lexer.nextToken(allocator); //-
    const minusToken = try lexer.nextToken(allocator); //2
    //std
    _ = try std.testing.expectEqual(tildeToken.type, TokenType.TILDE);
    _ = try std.testing.expectEqual(minusToken.type, TokenType.MINUS);
    //const lexer2 = try Lexer.init(allocator, @as([]u8, @constCast(buffer2)));
    //const lexer3 = try Lexer.init(allocator, @as([]u8, @constCast(buffer3)));
}

test "+*%" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const buffer = "int main(){ return (2*3)%5+6; }";
    //const buffer2 = "int main(){ return ~~2; }";
    //const buffer3 = "int main(){ return ~-2; }";
    const lexer = try Lexer.init(allocator, @as([]u8, @constCast(buffer)));
    _ = try lexer.nextToken(allocator); //int
    _ = try lexer.nextToken(allocator); //main
    _ = try lexer.nextToken(allocator); //(
    _ = try lexer.nextToken(allocator); //)
    _ = try lexer.nextToken(allocator); //{
    _ = try lexer.nextToken(allocator); //return
    _ = try lexer.nextToken(allocator); //LPAREN
    _ = try lexer.nextToken(allocator); //2
    const multiplyToken = try lexer.nextToken(allocator); //*
    _ = try lexer.nextToken(allocator); //3
    _ = try lexer.nextToken(allocator); //RPAREN
    const moduloToken = try lexer.nextToken(allocator);
    _ = try lexer.nextToken(allocator); //5
    const plusToken = try lexer.nextToken(allocator); //+
    //std
    _ = try std.testing.expectEqual(multiplyToken.type, TokenType.MULTIPLY);
    _ = try std.testing.expectEqual(moduloToken.type, TokenType.MODULO);
    _ = try std.testing.expectEqual(plusToken.type, TokenType.PLUS);
    //const lexer2 = try Lexer.init(allocator, @as([]u8, @constCast(buffer2)));
    //const lexer3 = try Lexer.init(allocator, @as([]u8, @constCast(buffer3)));
}

test "> and >=" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const buffer = "int main(){ return (2>3)*(2>=4); }";
    //const buffer2 = "int main(){ return ~~2; }";
    //const buffer3 = "int main(){ return ~-2; }";
    const lexer = try Lexer.init(allocator, @as([]u8, @constCast(buffer)));
    _ = try lexer.nextToken(allocator); //int
    _ = try lexer.nextToken(allocator); //main
    _ = try lexer.nextToken(allocator); //(
    _ = try lexer.nextToken(allocator); //)
    _ = try lexer.nextToken(allocator); //{
    _ = try lexer.nextToken(allocator); //return
    _ = try lexer.nextToken(allocator); //LPAREN
    _ = try lexer.nextToken(allocator); //2
    const greaterThan = try lexer.nextToken(allocator); //*
    _ = try lexer.nextToken(allocator); //3
    _ = try lexer.nextToken(allocator); //RPAREN
    _ = try lexer.nextToken(allocator); //multiply
    _ = try lexer.nextToken(allocator); //LPARENj
    _ = try lexer.nextToken(allocator); //2
    const greaterThanEq = try lexer.nextToken(allocator); //>=
    _ = try lexer.nextToken(allocator); //4
    _ = try lexer.nextToken(allocator); //RPAREN
    _ = try lexer.nextToken(allocator); //SEMICOLON
    //std
    _ = try std.testing.expectEqual(greaterThan.type, TokenType.GREATER);
    _ = try std.testing.expectEqual(greaterThanEq.type, TokenType.GREATEREQ);
}

test "== <= and <" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const buffer = "== <= <";
    //const buffer2 = "int main(){ return ~~2; }";
    //const buffer3 = "int main(){ return ~-2; }";
    const lexer = try Lexer.init(allocator, @as([]u8, @constCast(buffer)));
    const equals = try lexer.nextToken(allocator); //int
    const lessEq = try lexer.nextToken(allocator); //main
    const less = try lexer.nextToken(allocator); //(
    //std
    _ = try std.testing.expectEqual(equals.type, TokenType.EQUALS);
    _ = try std.testing.expectEqual(lessEq.type, TokenType.LESSEQ);
    _ = try std.testing.expectEqual(less.type, TokenType.LESS);
}
test "! and !=" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const buffer = "! !=";
    //const buffer2 = "int main(){ return ~~2; }";
    //const buffer3 = "int main(){ return ~-2; }";
    const lexer = try Lexer.init(allocator, @as([]u8, @constCast(buffer)));
    const not = try lexer.nextToken(allocator); //not
    const notEq = try lexer.nextToken(allocator); //not_eq
    //std
    _ = try std.testing.expectEqual(not.type, TokenType.NOT);
    _ = try std.testing.expectEqual(notEq.type, TokenType.NOT);
}

test "Ternary and if else" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const buffer = "? : if else";
    //const buffer2 = "int main(){ return ~~2; }";
    //const buffer3 = "int main(){ return ~-2; }";
    const lexer = try Lexer.init(allocator, @as([]u8, @constCast(buffer)));
    const ternary = try lexer.nextToken(allocator); //ternary
    const colon = try lexer.nextToken(allocator); //colon
    const ifTok = try lexer.nextToken(allocator); //if
    const elseTok = try lexer.nextToken(allocator); //if
    _ = try std.testing.expectEqual(ternary.type, TokenType.TERNARY);
    _ = try std.testing.expectEqual(colon.type, TokenType.COLON);
    _ = try std.testing.expectEqual(ifTok.type, TokenType.IF);
    _ = try std.testing.expectEqual(elseTok.type, TokenType.ELSE);
}

test "comma and void" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const buffer = ", void";
    const lexer = try Lexer.init(allocator, @as([]u8, @constCast(buffer)));
    const comma = try lexer.nextToken(allocator);
    const voidTok = try lexer.nextToken(allocator);
    _ = try std.testing.expectEqual(comma.type, TokenType.COMMA);
    _ = try std.testing.expectEqual(voidTok.type, TokenType.VOID);
}
