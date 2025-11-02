const std = @import("std");

pub const TokenType = enum {
    INT_TYPE,
    FLOAT_TYPE,
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
    LONG_TYPE,
    LONG,
    UNSIGNED,
    SIGNED,
    UNSIGNED_LONG,
    UNSIGNED_INT,
    FLOAT,
    LSQUARE,
    RSQUARE,
};

pub const Token = struct {
    type: TokenType,
    start: u32,
    end: u32,
};

//pub const suffixMap = std.StaticStringMap(Suffix).initComptime(.{
//    .{ "u", .U },
//    .{ "L", .L },
//    .{ "UL", .UL },
//});

pub const LexerError = error{ BufferEmpty, OutOfMemory, InvalidToken };

pub const MemoryError = error{
    OutOfMemory,
};

pub const Suffix = enum { U, L, UL };

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

    inline fn createDoubleWidthToken(comptime lookAheadChars: []const u8, comptime returnToks: []const TokenType, fallBackTok: TokenType, allocator: std.mem.Allocator, lexer: *Lexer) LexerError!?*Token {
        if (lexer.current + 1 >= lexer.buffer.len) {
            return null;
        }
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

    inline fn lexFloatLike(lexer: *Lexer, allocator: std.mem.Allocator) LexerError!?*Token {
        if (!(lexer.current < lexer.buffer.len and (std.ascii.isDigit(lexer.buffer[lexer.current]) or lexer.buffer[lexer.current] == '.'))) {
            // Early return, no digit found
            return null;
        }
        var token = try allocator.create(Token);
        const initialPtr = lexer.current;
        // lexes the numeric part
        while (lexer.current < lexer.buffer.len and std.ascii.isDigit(lexer.buffer[lexer.current])) {
            lexer.current += 1;
        }
        if (lexer.current < lexer.buffer.len and lexer.buffer[lexer.current] == '.') {
            lexer.current += 1;
        }
        while (lexer.current < lexer.buffer.len and std.ascii.isDigit(lexer.buffer[lexer.current])) {
            lexer.current += 1;
        }
        if (lexer.current < lexer.buffer.len and (lexer.buffer[lexer.current] == 'e' or lexer.buffer[lexer.current] == 'E')) {
            lexer.current += 1;
            if (lexer.buffer[lexer.current] == '-') lexer.current += 1;
        }
        while (lexer.current < lexer.buffer.len and std.ascii.isDigit(lexer.buffer[lexer.current])) {
            lexer.current += 1;
        }
        token.type = TokenType.FLOAT;
        token.start = initialPtr;
        token.end = lexer.current - 1;
        return token;
    }

    inline fn lexIntegerLike(lexer: *Lexer, allocator: std.mem.Allocator) LexerError!?*Token {
        if (!(lexer.current < lexer.buffer.len and std.ascii.isDigit(lexer.buffer[lexer.current]))) {
            // Early return, no digit found
            return null;
        }
        var token = try allocator.create(Token);
        const initialPtr = lexer.current;
        // lexes the numeric part
        while (lexer.current < lexer.buffer.len and std.ascii.isDigit(lexer.buffer[lexer.current])) {
            lexer.current += 1;
        }
        // After integer parsing is done, check for a '.' or e. If a dot is
        // encountered, integer parsing fails and returns null
        if (lexer.current < lexer.buffer.len and (lexer.buffer[lexer.current] == '.' or lexer.buffer[lexer.current] == 'e' or lexer.buffer[lexer.current] == 'E')) {
            lexer.current = initialPtr;
            return null;
        }
        token.type = TokenType.INTEGER;
        token.start = initialPtr;
        token.end = lexer.current - 1;
        return token;
    }

    inline fn peekLexFloatLike(lexer: *Lexer, allocator: std.mem.Allocator) LexerError!?*Token {
        if (!(lexer.current < lexer.buffer.len and (std.ascii.isDigit(lexer.buffer[lexer.current]) or lexer.buffer[lexer.current] == '.'))) {
            // Early return, no digit found
            return null;
        }
        var token = try allocator.create(Token);
        const initialPtr = lexer.current;
        var finalPtr = lexer.current;
        // lexes the numeric part
        while (finalPtr < lexer.buffer.len and std.ascii.isDigit(lexer.buffer[finalPtr])) {
            finalPtr += 1;
        }
        if (finalPtr < lexer.buffer.len and lexer.buffer[finalPtr] == '.') {
            finalPtr += 1;
        }
        while (finalPtr < lexer.buffer.len and std.ascii.isDigit(lexer.buffer[finalPtr])) {
            finalPtr += 1;
        }

        if (finalPtr < lexer.buffer.len and (lexer.buffer[finalPtr] == 'e' or lexer.buffer[finalPtr] == 'E')) {
            finalPtr += 1;
            if (lexer.buffer[finalPtr] == '-') finalPtr += 1;
        }
        while (finalPtr < lexer.buffer.len and std.ascii.isDigit(lexer.buffer[finalPtr])) {
            finalPtr += 1;
        }

        token.type = TokenType.FLOAT;
        token.start = initialPtr;
        token.end = finalPtr - 1;
        return token;
    }

    inline fn peekLexIntegerLike(lexer: *Lexer, allocator: std.mem.Allocator) LexerError!?*Token {
        if (!(lexer.current < lexer.buffer.len and std.ascii.isDigit(lexer.buffer[lexer.current]))) {
            // Early return, no digit or . found
            return null;
        }
        var token = try allocator.create(Token);
        const initialPtr = lexer.current;
        var finalPtr = lexer.current;
        // lexes the numeric part
        while (finalPtr < lexer.buffer.len and std.ascii.isDigit(lexer.buffer[finalPtr])) {
            finalPtr += 1;
        }
        // After integer parsing is done, check for a '.'. If a dot is
        // encountered, integer parsing fails and returns null
        if (finalPtr < lexer.buffer.len and (lexer.buffer[finalPtr] == '.' or lexer.buffer[finalPtr] == 'e' or lexer.buffer[finalPtr] == 'E')) return null;
        token.type = TokenType.INTEGER;
        token.start = initialPtr;
        token.end = finalPtr - 1;
        return token;
    }
    inline fn peekNumericSuffix(lexer: *Lexer, token: *Token) ?Suffix {
        if (token.end + 2 < lexer.buffer.len and std.mem.eql(u8, lexer.buffer[token.end + 1 .. token.end + 3], "UL")) return .UL;
        if (token.end + 1 < lexer.buffer.len and lexer.buffer[token.end + 1] == 'U') return .U;
        if (token.end + 1 < lexer.buffer.len and lexer.buffer[token.end + 1] == 'L') return .L;
        return null;
    }

    inline fn convert(token: *Token, suffix: ?Suffix) void {
        if (suffix == null) {
            return;
        }
        switch (suffix.?) {
            .L => {
                token.type = .LONG;
                token.end += 1;
            },
            .U => {
                token.type = .UNSIGNED_INT;
                token.end += 1;
            },
            .UL => {
                token.type = .UNSIGNED_LONG;
                token.end += 2;
            },
        }
    }

    pub fn peekToken(lexer: *Lexer, allocator: std.mem.Allocator) LexerError!?*Token {
        lexer.skipWhitespace();
        if (lexer.current >= lexer.buffer.len) {
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
            '[' => {
                return (try createSingleWidthToken(TokenType.LSQUARE, allocator, lexer));
            },
            ']' => {
                return (try createSingleWidthToken(TokenType.RSQUARE, allocator, lexer));
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
            '/' => {
                return (try createSingleWidthToken(TokenType.DIVIDE, allocator, lexer));
            },
            else => {
                if (try lexer.peekLexIntegerLike(allocator)) |integerNode| {
                    const suffix = lexer.peekNumericSuffix(integerNode);
                    convert(integerNode, suffix); // Mutates in place
                    return integerNode;
                }

                if (try lexer.peekLexFloatLike(allocator)) |floatNode| {
                    // TODO: handle float suffixes
                    return floatNode;
                }
                const token = try allocator.create(Token);

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
                    "long",
                    "unsigned",
                    "signed",
                    "double",
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
                    TokenType.LONG_TYPE,
                    TokenType.UNSIGNED,
                    TokenType.SIGNED,
                    TokenType.FLOAT_TYPE,
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
        const token = try allocator.create(Token);
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
            '/' => {
                nextSingleWidthTokMacro(TokenType.DIVIDE, token, lexer);
            },
            ';' => {
                nextSingleWidthTokMacro(TokenType.SEMICOLON, token, lexer);
            },
            ',' => {
                nextSingleWidthTokMacro(TokenType.COMMA, token, lexer);
            },
            '[' => {
                nextSingleWidthTokMacro(TokenType.LSQUARE, token, lexer);
            },
            ']' => {
                nextSingleWidthTokMacro(TokenType.RSQUARE, token, lexer);
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
                if (try lexer.lexIntegerLike(allocator)) |integerNode| {
                    const suffix = lexer.peekNumericSuffix(integerNode);
                    convert(integerNode, suffix); // Mutates in place
                    lexer.current = integerNode.end + 1;
                    return integerNode;
                }

                if (try lexer.lexFloatLike(allocator)) |floatNode| {
                    // TODO: handle float suffixes
                    return floatNode;
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
                    "long",
                    "unsigned",
                    "signed",
                    "double",
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
                    TokenType.LONG_TYPE,
                    TokenType.UNSIGNED,
                    TokenType.SIGNED,
                    TokenType.FLOAT_TYPE,
                }, lexer, token);
            },
        }
        lexer.currentToken = token;
        return token;
    }
};
