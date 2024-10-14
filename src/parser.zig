const std = @import("std");
const lexer = @import("./lexer.zig");
const AST = @import("./AST.zig");
const Logger = @import("./Logger.zig");
const MyLogger = Logger.Logger;

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
    AccessDenied,
    Unexpected,
    DiskQuota,
    FileTooBig,
    InputOutput,
    NoSpaceLeft,
    DeviceBusy,
    InvalidArgument,
    BrokenPipe,
    SystemResources,
    OperationAborted,
    NotOpenForWriting,
    LockViolation,
    WouldBlock,
    ConnectionResetByPeer,
};

pub const Parser = struct {
    l: *lexer.Lexer,
    allocator: std.mem.Allocator,
    logger: *MyLogger,
    pub fn init(allocator: std.mem.Allocator, l: *lexer.Lexer) MemoryError!*Parser {
        const parser = try allocator.create(Parser);
        parser.* = Parser{ .l = l, .allocator = allocator, .logger = MyLogger.init(allocator) };
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
        const blockItems = std.ArrayList(*AST.BlockItem).init(self.allocator);
        var peekToken = try self.l.peekToken(self.allocator);
        const program = try self.allocator.create(AST.Program);
        program.* = AST.Program{
            .function = AST.FunctionDef{
                .name = self.l.buffer[fnNameToken.start .. fnNameToken.end + 1],
                .blockItems = blockItems,
            },
        };
        while (peekToken != null and peekToken.?.type != lexer.TokenType.RBRACE) {
            try self.logger.info("Next peek token is {any} and {any}\n", .{ peekToken, self.l.currentToken });
            const blockItem = try self.parseBlockItem();
            try program.function.blockItems.append(blockItem);
            peekToken = try self.l.peekToken(self.allocator);
        }
        return program;
    }

    pub fn parseBlockItem(self: *Parser) ParserError!*AST.BlockItem {
        const nextToken = try self.l.peekToken(self.allocator);
        const blockItem = try self.allocator.create(AST.BlockItem);
        if (nextToken) |nextTok| {
            switch (nextTok.type) {
                .INT_TYPE => {
                    blockItem.* = AST.BlockItem{
                        .Declaration = (try self.parseDeclaration()),
                    };
                },
                else => {
                    blockItem.* = AST.BlockItem{
                        .Statement = (try self.parseStatement()),
                    };
                },
            }
        } else {
            unreachable;
        }
        return blockItem;
    }

    pub fn parseDeclaration(self: *Parser) ParserError!*AST.Declaration {
        const nextToken = try self.l.nextToken(self.allocator);
        switch (nextToken.type) {
            .INT_TYPE => {
                const identifier = try self.l.nextToken(self.allocator);
                std.debug.assert(identifier.type == lexer.TokenType.IDENTIFIER);
                const declaration = try self.allocator.create(AST.Declaration);

                // declarations can be of the form:
                // int x;
                // int x = 3;
                // int x = y = 3;
                declaration.* = AST.Declaration{
                    .name = self.l.buffer[identifier.start .. identifier.end + 1],
                    .expression = null,
                };
                switch ((try self.l.nextToken(self.allocator)).type) {
                    .SEMICOLON => {
                        return declaration;
                    },
                    .ASSIGN => {
                        const expression = try self.parseExpression(0);
                        declaration.expression = expression;
                        std.debug.assert(if (self.l.currentToken) |currTok| currTok.type == lexer.TokenType.SEMICOLON or ((try self.l.nextToken(self.allocator)).type == lexer.TokenType.SEMICOLON) else ((try self.l.nextToken(self.allocator)).type == lexer.TokenType.SEMICOLON));
                        return declaration;
                    },
                    else => {
                        unreachable;
                    },
                }
                return declaration;
            },
            else => {
                unreachable;
            },
        }
    }

    pub fn parseStatement(self: *Parser) ParserError!*AST.Statement {
        switch ((try self.l.peekToken(self.allocator)).?.type) {
            .RETURN => {
                _ = try self.l.nextToken(self.allocator);
                const expr = try self.parseExpression(0);
                try self.logger.info("Expr obtained from return: {any} and {any}\n", .{ expr, if (std.meta.activeTag(expr.*) == AST.ExpressionType.Unary) expr.Unary.exp else null });
                const retStmt = try self.allocator.create(AST.Statement);
                retStmt.* = AST.Statement{
                    .Return = AST.Return{
                        .expression = expr,
                    },
                };
                const peeked = try self.l.peekToken(self.allocator);
                try self.logger.info("Peeked after return expression parse: {any}\n", .{peeked});
                if (peeked != null and peeked.?.type == lexer.TokenType.SEMICOLON) {
                    _ = try self.l.nextToken(self.allocator);
                }
                return retStmt;
            },
            .SEMICOLON => {
                _ = try self.l.nextToken(self.allocator);
                try self.logger.info("Found a null stmt\n", .{});
                const nullStmt = try self.allocator.create(AST.Statement);
                nullStmt.* = AST.Statement{
                    .Null = {},
                };
                return nullStmt;
            },
            .IF => {
                _ = try self.l.nextToken(self.allocator);
                const ifStmt = try self.allocator.create(AST.Statement);
                const expr = try self.parseFactor();
                const thenStmt = try self.parseStatement();
                ifStmt.* = AST.Statement{ .If = AST.If{
                    .condition = expr,
                    .thenStmt = thenStmt,
                } };
                if ((try self.l.nextToken(self.allocator)).type == lexer.TokenType.ELSE) {
                    const elseStmt = try self.parseStatement();
                    ifStmt.If.elseStmt = elseStmt;
                }
                return ifStmt;
            },
            .LBRACE => {
                std.log.warn("LBrace Found", .{});
                _ = try self.l.nextToken(self.allocator);
                const blockItemsList = std.ArrayList(*AST.BlockItem).init(self.allocator);
                const compoundStatement = try self.allocator.create(AST.Statement);
                compoundStatement.* = AST.Statement{
                    .Compound = blockItemsList,
                };
                while (true) {
                    const hasPeeked = try self.l.peekToken(self.allocator);
                    if (hasPeeked == null) break;
                    if (hasPeeked.?.type == lexer.TokenType.RBRACE) break;
                    const blockItem = try self.parseBlockItem();
                    try compoundStatement.Compound.append(blockItem);
                }
                const rbrace = try self.l.nextToken(self.allocator);
                std.debug.assert(rbrace.type == lexer.TokenType.RBRACE);
                return compoundStatement;
            },
            .GOTO => {
                _ = try self.l.nextToken(self.allocator);
                const jumpLabel = try self.l.nextToken(self.allocator);
                const gotoStatement = try self.allocator.create(AST.Statement);
                gotoStatement.* = AST.Statement{
                    .Goto = self.l.buffer[jumpLabel.start .. jumpLabel.end + 1],
                };
                const semicolon = try self.l.nextToken(self.allocator);
                std.debug.assert(semicolon.type == lexer.TokenType.SEMICOLON);
                return gotoStatement;
            },
            .DO => {
                _ = try self.l.nextToken(self.allocator);
                const body = try self.parseStatement();
                const whileTok = try self.l.nextToken(self.allocator);
                std.debug.assert(whileTok.type == lexer.TokenType.WHILE);
                const lparen = try self.l.nextToken(self.allocator);
                std.debug.assert(lparen.type == lexer.TokenType.LPAREN);
                const condition = try self.parseExpression(0);
                const rparen = try self.l.nextToken(self.allocator);
                std.debug.assert(rparen.type == lexer.TokenType.RPAREN);
                const doWhileStatement = try self.allocator.create(AST.Statement);
                doWhileStatement.* = AST.Statement{ .DoWhile = .{
                    .body = body,
                    .condition = condition,
                } };
                return doWhileStatement;
            },
            .WHILE => {
                _ = try self.l.nextToken(self.allocator);
                const lparen = try self.l.nextToken(self.allocator);
                std.debug.assert(lparen.type == lexer.TokenType.LPAREN);
                const condition = try self.parseExpression(0);
                const rparen = try self.l.nextToken(self.allocator);
                std.debug.assert(rparen.type == lexer.TokenType.RPAREN);
                const body = try self.parseStatement();
                const whileStatement = try self.allocator.create(AST.Statement);
                whileStatement.* = AST.Statement{ .While = .{
                    .body = body,
                    .condition = condition,
                } };
                return whileStatement;
            },
            else => {
                const twoToks = try self.l.peekTwoTokens(self.allocator);
                if (twoToks[1]) |secondTok| {
                    std.log.warn("Second tok: {any}\n", .{secondTok});
                    if (secondTok.type == lexer.TokenType.COLON) {
                        const identifier = try self.l.nextToken(self.allocator);
                        const colon = try self.l.nextToken(self.allocator);
                        std.debug.assert(colon.type == lexer.TokenType.COLON);
                        const labelStmt = try self.allocator.create(AST.Statement);
                        std.log.warn("Found label: {s}\n", .{self.l.buffer[identifier.start .. identifier.end + 1]});
                        labelStmt.* = AST.Statement{
                            .Label = self.l.buffer[identifier.start .. identifier.end + 1],
                        };
                        return labelStmt;
                    }
                }
                const expression = try self.parseExpression(0);
                const expressionStmt = try self.allocator.create(AST.Statement);
                expressionStmt.* = AST.Statement{
                    .Expression = expression,
                };
                return expressionStmt;
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
        const peekToken = (try self.l.peekToken(self.allocator)).?;
        switch (peekToken.type) {
            .INTEGER => {
                const currToken = try self.l.nextToken(self.allocator);
                const integerNode = try self.allocator.create(AST.Expression);
                integerNode.* = AST.Expression{
                    .Integer = try std.fmt.parseInt(u32, self.l.buffer[currToken.start..currToken.end], 10),
                };
                return integerNode;
            },
            .MINUS, .TILDE => {
                const op = try tokToUnaryOp((try self.l.nextToken(self.allocator)).type);
                const factor = try self.parseFactor();
                const unaryNode = try self.allocator.create(AST.Expression);
                unaryNode.* = AST.Expression{ .Unary = AST.Unary{
                    .unaryOp = op,
                    .exp = factor,
                } };
                return unaryNode;
            },
            .LPAREN => {
                _ = try self.l.nextToken(self.allocator);
                const exp = try self.parseExpression(0);
                const rparen = try self.l.nextToken(self.allocator);
                std.debug.assert(rparen.type == lexer.TokenType.RPAREN);
                return exp;
            },
            .IDENTIFIER => {
                const currToken = try self.l.nextToken(self.allocator);
                const identifier = try self.allocator.create(AST.Expression);
                identifier.* = AST.Expression{ .Identifier = self.l.buffer[currToken.start .. currToken.end + 1] };
                return identifier;
            },
            else => |tokType| {
                std.log.warn("Parse factor unknown type: {any}\n", .{tokType});
                unreachable;
            },
        }
    }

    fn getPrecedence(tok: lexer.TokenType) ?u32 {
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
            .TERNARY => {
                return 3;
            },
            .ASSIGN => {
                return 1;
            },
            else => {
                return null;
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
            .LESS => AST.BinOp.LESS_THAN,
            .LESSEQ => AST.BinOp.LESS_THAN_EQ,
            .GREATER => AST.BinOp.GREATER_THAN,
            .GREATEREQ => AST.BinOp.GREATER_THAN_EQ,
            .EQUALS => AST.BinOp.EQUALS,
            .NOT_EQUALS => AST.BinOp.NOT_EQUALS,
            .LOGIC_AND => AST.BinOp.LOGIC_AND,
            .LOGIC_OR => AST.BinOp.LOGIC_OR,
            else => {
                std.log.warn("Found in binaryOpFromTokType unhandled {}\n", .{tok});
                unreachable;
            },
        };
    }

    pub fn parseTernaryMiddle(self: *Parser) ParserError!*AST.Expression {
        const expr = self.parseExpression(0);
        const colon = try self.l.nextToken(self.allocator);
        std.debug.assert(colon.type == lexer.TokenType.COLON);
        return expr;
    }

    pub fn parseInfix(self: *Parser, lhs: *AST.Expression) ParserError!*AST.Expression {
        if (self.l.currentToken) |currToken| {
            const op = self.l.currentToken.?;
            switch (op.type) {
                .MINUS, .PLUS, .MULTIPLY, .DIVIDE, .MODULO, .LESS, .LESSEQ, .GREATER, .GREATEREQ, .EQUALS, .NOT_EQUALS, .LOGIC_AND, .LOGIC_OR => {
                    const expr = try self.allocator.create(AST.Expression);
                    const rhs = try self.parseExpression(getPrecedence(currToken.type).? + 1);
                    expr.* = AST.Expression{ .Binary = AST.Binary{
                        .op = binaryOpFromTokType(op.type),
                        .lhs = lhs,
                        .rhs = rhs,
                    } };
                    return expr;
                },
                .ASSIGN => {
                    const expr = try self.allocator.create(AST.Expression);
                    const rhs = try self.parseExpression(getPrecedence(currToken.type).?);
                    expr.* = AST.Expression{ .Assignment = AST.Assignment{
                        .lhs = lhs,
                        .rhs = rhs,
                    } };
                    return expr;
                },
                .TERNARY => {
                    const expr = try self.allocator.create(AST.Expression);
                    const middle = try self.parseTernaryMiddle();
                    const end = try self.parseExpression(0);
                    expr.* = AST.Expression{ .Ternary = .{
                        .lhs = middle,
                        .rhs = end,
                        .condition = lhs,
                    } };
                    return expr;
                },
                else => {
                    return lhs;
                },
            }
        } else {
            unreachable;
        }
        return lhs;
    }

    pub fn parseExpression(self: *Parser, precedence: u32) ParserError!*AST.Expression {
        var lhs = try self.parseFactor();
        std.log.warn("LHS Obtained: {}\n", .{lhs});
        while (true) {
            const hasPeeked = try self.l.peekToken(self.allocator);
            if (hasPeeked) |peeked| {
                if (peeked.type == lexer.TokenType.SEMICOLON) {
                    return lhs;
                }
                const hasPeekedPrecedence = getPrecedence(peeked.type);
                if (hasPeekedPrecedence == null) {
                    std.log.warn("parseExpression: exited cause of no precdence for token and token={any}\n", .{peeked});
                    return lhs;
                }
                const peekedPrecedence = hasPeekedPrecedence.?;
                if (precedence > peekedPrecedence) {
                    std.log.warn("parseExpression: exited cause of precedence of token being greater", .{});
                    return lhs;
                }
                const hasNextToken = self.l.nextToken(self.allocator) catch null;
                if (hasNextToken) |_| {
                    lhs = try self.parseInfix(lhs);
                } else {
                    // factor case in the grammar
                    return lhs;
                }
            } else {
                return lhs;
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
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try Parser.init(allocator, l);
    const program = try p.parseProgram();
    _ = try std.testing.expect(std.mem.eql(u8, program.function.name, "main"));
}

test "parse factor" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const unary = "-42";
    const l2 = try lexer.Lexer.init(allocator, @as([]u8, @constCast(unary)));
    var p2 = try Parser.init(allocator, l2);
    const factor2 = try p2.parseFactor();
    _ = try std.testing.expectEqual(factor2.Unary.unaryOp, AST.UnaryOp.NEGATE);
    _ = try std.testing.expectEqual(factor2.Unary.exp.Integer, 42);
}

test "parsing expression with precedence" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ return 2-3*5; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try Parser.init(allocator, l);
    const program = try p.parseProgram();
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.function.blockItems.items[0].Statement.Return.expression});
    //_ = try std.testing.expectEqual(program.function.statement.Return.expression, 2);
}

test "more complicated precedence" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ return (2*3)%5+6; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try Parser.init(allocator, l);
    const program = try p.parseProgram();
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.function.blockItems.items[0].Statement.Return.expression});
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.function.blockItems.items[0].Statement.Return.expression.Binary.lhs});
}

test "precedence with >= and <=" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ return 2 >= 3 + 1 <= 5; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try Parser.init(allocator, l);
    const program = try p.parseProgram();
    std.log.warn("Return exp: \x1b[34m{any}\x1b[0m", .{program.function.blockItems.items[0].Statement.Return.expression});
    std.log.warn("Binary lhs: \x1b[34m{any}\x1b[0m", .{program.function.blockItems.items[0].Statement.Return.expression.Binary.lhs});
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.function.blockItems.items[0].Statement.Return.expression.Binary.lhs.Binary.lhs});
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.function.blockItems.items[0].Statement.Return.expression.Binary.lhs.Binary.rhs});
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.function.blockItems.items[0].Statement.Return.expression.Binary.rhs.Integer});
}

test "parsing declarations and statements" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){int x = 3; return x;}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try Parser.init(allocator, l);
    const program = try p.parseProgram();
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.function.blockItems.items[0]});
    std.log.warn("\x1b[34m{s}\x1b[0m", .{program.function.blockItems.items[0].Declaration.name});
}

test "parsing declarations right associativity" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){int y; int x = y = 3; return x;}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try Parser.init(allocator, l);
    const program = try p.parseProgram();
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.function.blockItems.items[0]});
    std.log.warn("\x1b[34m{s}\x1b[0m", .{program.function.blockItems.items[0].Declaration.name});
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.function.blockItems.items[1]});
    std.log.warn("\x1b[34m{s}\x1b[0m", .{program.function.blockItems.items[1].Declaration.name});
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.function.blockItems.items[1].Declaration.expression});
    std.log.warn("\x1b[34m{s}\x1b[0m", .{program.function.blockItems.items[1].Declaration.expression.?.Assignment.lhs.Identifier});
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.function.blockItems.items[1].Declaration.expression.?.Assignment.rhs});
}

test "test if statements" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){int y; int x = y = 3;if(x == y) return x; else return y;}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try Parser.init(allocator, l);
    const program = try p.parseProgram();
    std.log.warn("Cond: \x1b[34m{any}\x1b[0m\n", .{program.function.blockItems.items[2].Statement.If.condition});
    std.log.warn("Then: \x1b[34m{any}\x1b[0m\n", .{program.function.blockItems.items[2].Statement.If.thenStmt});
    std.log.warn("Else: \x1b[34m{any}\x1b[0m", .{program.function.blockItems.items[2].Statement.If.elseStmt.?});
}

test "test ternary statements" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){int y = 4; int x = 3;return x == 3?x:y}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try Parser.init(allocator, l);
    const program = try p.parseProgram();
    std.log.warn("lhs: {any}\n", .{program.function.blockItems.items[2].Statement.Return.expression.Ternary.lhs});
    std.log.warn("rhs: {any}\n", .{program.function.blockItems.items[2].Statement.Return.expression.Ternary.rhs});
    std.log.warn("condition: {any}\n", .{program.function.blockItems.items[2].Statement.Return.expression.Ternary.condition});
}

test "test label" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){int y = 4; int x = 3;goto sup;sup:return x == 3?x:y}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try Parser.init(allocator, l);
    const program = try p.parseProgram();
    std.log.warn("Goto: {s}\n", .{program.function.blockItems.items[2].Statement.Goto});
    std.log.warn("Label: {s}\n", .{program.function.blockItems.items[3].Statement.Label});
}

test "test compound statement parsing" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){int y = 4; int x = 2; {int x = 3;} return x+y;";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try Parser.init(allocator, l);
    const program = try p.parseProgram();
    try AST.scopeVariableResolutionPass(program, allocator);
    std.log.warn("Compound statement: first statement: {any}\n", .{program.function.blockItems.items[2].Statement.Compound.items[0]});
    std.log.warn("Declaration: of x: {s}\n", .{program.function.blockItems.items[1].Declaration.name});
    std.log.warn("Compound statement: first statement decl varName: {s}\n", .{program.function.blockItems.items[2].Statement.Compound.items[0].Declaration.name});
}
