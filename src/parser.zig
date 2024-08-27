const std = @import("std");
const lexer = @import("./lexer.zig");
const AST = @import("./AST.zig");

pub const MemoryError = error{
    OutOfMemory,
};

pub const ParserError = error{
    OutOfMemory,
    BufferEmpty,
    UnknownStatement,
    Overflow,
    InvalidToken,
    InvalidCharacter,
};

pub const Parser = struct {
    l: *lexer.Lexer,
    allocator: std.mem.Allocator,
    pub fn init(allocator: std.mem.Allocator, l: *lexer.Lexer) MemoryError!*Parser {
        const parser = try allocator.create(Parser);
        parser.* = Parser{ .l = l, .allocator = allocator };
        return parser;
    }

    pub fn parseProgram(self: *Parser) ParserError!*AST.Program {
        const intToken = try self.l.nextToken(self.allocator);
        std.debug.assert(intToken.type == lexer.TokenType.INT_TYPE);
        const fnNameToken = try self.l.nextToken(self.allocator);
        std.debug.assert(fnNameToken.type == lexer.TokenType.IDENTIFIER);
        std.debug.assert((try self.l.nextToken(self.allocator)).type == lexer.TokenType.LPAREN);
        std.debug.assert((try self.l.nextToken(self.allocator)).type == lexer.TokenType.RPAREN);
        std.debug.assert((try self.l.nextToken(self.allocator)).type == lexer.TokenType.LBRACE);
        const stmt = try self.parseStatement();
        const program = try self.allocator.create(AST.Program);
        program.* = AST.Program{
            .function = AST.FunctionDef{
                .name = self.l.buffer[fnNameToken.start .. fnNameToken.end + 1],
                .statement = stmt,
            },
        };
        return program;
    }

    pub fn parseStatement(self: *Parser) ParserError!*AST.Statement {
        switch ((try self.l.nextToken(self.allocator)).type) {
            .RETURN => {
                const expr = try self.parseExpression(0);
                var retStmt = try self.allocator.create(AST.Statement);
                retStmt.* = AST.Statement{
                    .Return = AST.Return{
                        .expression = expr,
                    },
                };
                //const semicolon = try self.l.nextToken(self.allocator);
                //std.debug.assert(semicolon.type == lexer.TokenType.SEMICOLON);
                return retStmt;
            },
            else => {
                std.debug.assert(false);
                return ParserError.UnknownStatement;
            },
        }
    }

    fn tokToUnaryOp(tokenType: lexer.TokenType) ParserError!AST.UnaryOp {
        switch (tokenType) {
            .MINUS => {
                return AST.UnaryOp.NEGATE;
            },
            .TILDE => {
                return AST.UnaryOp.COMPLEMENT;
            },
            else => {
                return lexer.LexerError.InvalidToken;
            },
        }
    }

    pub fn parseFactor(self: *Parser) ParserError!*AST.Expression {
        const currToken = try self.l.nextToken(self.allocator);
        switch (currToken.type) {
            .INTEGER => {
                var integerNode = try self.allocator.create(AST.Expression);
                integerNode.* = AST.Expression{
                    .Integer = try std.fmt.parseInt(u32, self.l.buffer[currToken.start..currToken.end], 10),
                };
                return integerNode;
            },
            .MINUS, .TILDE => {
                const op = try tokToUnaryOp(currToken.type);
                const factor = try self.parseFactor();
                var unaryNode = try self.allocator.create(AST.Expression);
                unaryNode.* = AST.Expression{ .Unary = AST.Unary{
                    .unaryOp = op,
                    .exp = factor,
                } };
                return unaryNode;
            },
            .LPAREN => {
                const exp = try self.parseExpression(0);
                return exp;
            },
            else => {
                unreachable;
            },
        }
    }

    fn getPrecedence(tok: lexer.TokenType) u32 {
        switch (tok) {
            .MULTIPLY, .DIVIDE, .MODULO => {
                return 50;
            },
            .PLUS, .MINUS => {
                return 45;
            },
            .LESS, .LESSEQ, .GREATER, .GREATEREQ => {
                return 35;
            },
            .EQUALS, .NOT_EQUALS => {
                return 30;
            },
            .LOGIC_AND => {
                return 10;
            },
            .LOGIC_OR => {
                return 5;
            },
            else => {
                return 0;
            },
        }
    }
    fn binaryOpFromTokType(tok: lexer.TokenType) AST.BinOp {
        return switch (tok) {
            .MINUS => AST.BinOp.SUBTRACT,
            .PLUS => AST.BinOp.ADD,
            .MULTIPLY => AST.BinOp.MULTIPLY,
            .DIVIDE => AST.BinOp.DIVIDE,
            .MODULO => AST.BinOp.REMAINDER,
            else => {
                unreachable;
            },
        };
    }

    pub fn parseInfix(self: *Parser, lhs: *AST.Expression) ParserError!*AST.Expression {
        if (self.l.currentToken) |currToken| {
            if (currToken.type == lexer.TokenType.RPAREN) {
                return lhs;
            }
            if (currToken.type == lexer.TokenType.SEMICOLON) {}
            const op = self.l.currentToken.?;
            const hasRhs = switch (op.type) {
                .MINUS, .PLUS, .MULTIPLY, .DIVIDE, .MODULO => try self.parseExpression(getPrecedence(currToken.type)),
                else => null,
            };
            if (hasRhs) |rhs| {
                var expr = try self.allocator.create(AST.Expression);
                expr.* = AST.Expression{ .Binary = AST.Binary{
                    .op = binaryOpFromTokType(op.type),
                    .lhs = lhs,
                    .rhs = rhs,
                } };
                return expr;
            }
        } else {
            unreachable;
        }
        return lhs;
    }

    pub fn parseExpression(self: *Parser, precedence: u32) ParserError!*AST.Expression {
        var lhs = try self.parseFactor();
        const hasPeeked = try self.l.peekToken(self.allocator);
        if (hasPeeked) |peeked| {
            if (peeked.type == lexer.TokenType.SEMICOLON) {
                return lhs;
            }
            const peekedPrecedence = getPrecedence(peeked.type);
            while ((precedence < peekedPrecedence)) {
                if (self.l.currentToken) |currTok| {
                    if (currTok.type == lexer.TokenType.SEMICOLON) {
                        return lhs;
                    }
                }
                const hasNextToken = self.l.nextToken(self.allocator) catch null;
                if (hasNextToken) |_| {
                    lhs = try self.parseInfix(lhs);
                } else {
                    // factor case in the grammar
                    return lhs;
                }
            }
        }
        return lhs;
    }
};

test "testing basic parser" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ return 42; }";
    var l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try Parser.init(allocator, l);
    var program = try p.parseProgram();
    _ = try std.testing.expect(std.mem.eql(u8, program.function.name, "main"));
}

test "parse factor" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const unary = "-42";
    var l2 = try lexer.Lexer.init(allocator, @as([]u8, @constCast(unary)));
    var p2 = try Parser.init(allocator, l2);
    var factor2 = try p2.parseFactor();
    _ = try std.testing.expectEqual(factor2.Unary.unaryOp, AST.UnaryOp.NEGATE);
    _ = try std.testing.expectEqual(factor2.Unary.exp.Integer, 42);
}

test "parsing expression with precedence" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ return 2-3*5; }";
    var l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try Parser.init(allocator, l);
    var program = try p.parseProgram();
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.function.statement.Return.expression});
    //_ = try std.testing.expectEqual(program.function.statement.Return.expression, 2);
}

test "more complicated precedence" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ return (2*3)%5+6; }";
    var l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try Parser.init(allocator, l);
    var program = try p.parseProgram();
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.function.statement.Return.expression});
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.function.statement.Return.expression.Binary.lhs});
}
