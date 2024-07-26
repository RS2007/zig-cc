const std = @import("std");

pub const TokenType = enum { INT_TYPE, SEMICOLON, LPAREN, RPAREN, LBRACE, RBRACE, IDENTIFIER, INTEGER, RETURN, INVALID };

pub const Token = struct {
    type: TokenType,
    start: u32,
    end: u32,
};

pub const LexerError = error{ BufferEmpty, OutOfMemory };

pub const MemoryError = error{
    OutOfMemory,
};

pub const Lexer = struct {
    buffer: []u8,
    current: u32 = 0,
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

    pub fn nextToken(lexer: *Lexer, allocator: std.mem.Allocator) LexerError!*Token {
        lexer.skipWhitespace();
        if (lexer.current >= lexer.buffer.len) {
            return LexerError.BufferEmpty;
        }
        var token = try allocator.create(Token);
        switch (lexer.buffer[lexer.current]) {
            '(' => {
                token.type = TokenType.LPAREN;
                token.start = lexer.current;
                token.end = lexer.current;
                lexer.current += 1;
            },
            ')' => {
                token.type = TokenType.RPAREN;
                token.start = lexer.current;
                token.end = lexer.current;
                lexer.current += 1;
            },
            '{' => {
                token.type = TokenType.LBRACE;
                token.start = lexer.current;
                token.end = lexer.current;
                lexer.current += 1;
            },
            '}' => {
                token.type = TokenType.RBRACE;
                token.start = lexer.current;
                token.end = lexer.current;
                lexer.current += 1;
            },
            ';' => {
                token.type = TokenType.SEMICOLON;
                token.start = lexer.current;
                token.end = lexer.current;
                lexer.current += 1;
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
                    return token;
                }
                const initialPtr = lexer.current;
                while (lexer.current < lexer.buffer.len and !std.ascii.isWhitespace(lexer.buffer[lexer.current]) and (std.ascii.isLower(lexer.buffer[lexer.current]) or std.ascii.isUpper(lexer.buffer[lexer.current]))) {
                    lexer.current += 1;
                }
                if (std.mem.eql(u8, lexer.buffer[initialPtr..lexer.current], "int")) {
                    token.type = TokenType.INT_TYPE;
                    token.start = initialPtr;
                    token.end = lexer.current - 1;
                    return token;
                } else if (std.mem.eql(u8, lexer.buffer[initialPtr..lexer.current], "return")) {
                    token.type = TokenType.RETURN;
                    token.start = initialPtr;
                    token.end = lexer.current - 1;
                    return token;
                }
                token.type = TokenType.IDENTIFIER;
                token.start = initialPtr;
                token.end = lexer.current - 1;
                return token;
            },
        }
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
