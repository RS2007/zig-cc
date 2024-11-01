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
    STATIC,
    EXTERN,
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
        const first = try lexer.nextToken(allocator);
        const second = try lexer.peekToken(allocator);
        lexer.current = resetIndx;
        lexer.currentToken = resetToken;
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
                    "static",
                    "extern",
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
                    TokenType.STATIC,
                    TokenType.EXTERN,
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
                    "static",
                    "extern",
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
                    TokenType.STATIC,
                    TokenType.EXTERN,
                }, lexer, token);
            },
        }
        lexer.currentToken = token;
        return token;
    }
};
