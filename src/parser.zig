const std = @import("std");
const lexer = @import("./lexer.zig");
const AST = @import("./AST.zig");
//const logz = @import("logz");

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
    Unimplemented,
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
        const program = try self.allocator.create(AST.Program);
        const externalDeclList = std.ArrayList(*AST.ExternalDecl).init(self.allocator);
        program.externalDecls = externalDeclList;
        while ((try self.l.peekToken(self.allocator)) != null) {
            const externalDecl = try self.parseExternalDecl();
            try program.externalDecls.append(externalDecl);
        }
        return program;
    }

    pub fn parseExternalDecl(self: *Parser) ParserError!*AST.ExternalDecl {
        // TODO: parse_if_qualifier
        const qualifier = AST.Qualifier.from((try self.l.peekToken(self.allocator)).?.type);
        if (qualifier != null) {
            _ = try self.l.nextToken(self.allocator);
        }
        const returnType = try self.l.nextToken(self.allocator);
        std.debug.assert(returnType.type == lexer.TokenType.INT_TYPE or returnType.type == lexer.TokenType.VOID);
        const peekTwo = try self.l.peekTwoTokens(self.allocator);
        if (peekTwo[1].?.type != lexer.TokenType.LPAREN) {
            const varName = try self.l.nextToken(self.allocator);
            const externalDecl = try self.allocator.create(AST.ExternalDecl);
            const declaration = try self.allocator.create(AST.Declaration);
            declaration.* = .{
                .name = self.l.buffer[varName.start .. varName.end + 1],
                .expression = null,
                .type = AST.Type.Integer,
                .storageClass = qualifier,
                // TODO: should support other types
            };
            externalDecl.* = .{
                .VarDeclaration = declaration,
            };
            switch (peekTwo[1].?.type) {
                .SEMICOLON => {
                    _ = try self.l.nextToken(self.allocator);
                    return externalDecl;
                },
                .ASSIGN => {
                    _ = try self.l.nextToken(self.allocator);
                    const expression = try self.parseExpression(0);
                    declaration.expression = expression;
                    std.debug.assert(if ((try self.l.peekToken(self.allocator) != null)) (try self.l.peekToken(self.allocator)).?.type == lexer.TokenType.SEMICOLON else false);
                    _ = try self.l.nextToken(self.allocator);
                    return externalDecl;
                },
                else => {
                    unreachable;
                },
            }
        }
        const fnNameToken = try self.l.nextToken(self.allocator);
        var argList = std.ArrayList(*AST.Arg).init(self.allocator);
        std.debug.assert(fnNameToken.type == lexer.TokenType.IDENTIFIER);
        std.debug.assert((try self.l.nextToken(self.allocator)).type == lexer.TokenType.LPAREN);
        // TODO: parse function arguments
        while ((try self.l.peekToken(self.allocator)).?.type != lexer.TokenType.RPAREN) {
            const arg = try self.parseArg();
            try argList.append(arg);
            if ((try self.l.peekToken(self.allocator)).?.type == lexer.TokenType.COMMA) {
                _ = try self.l.nextToken(self.allocator);
            }
        }
        std.debug.assert((try self.l.nextToken(self.allocator)).type == lexer.TokenType.RPAREN);
        std.debug.assert((try self.l.nextToken(self.allocator)).type == lexer.TokenType.LBRACE);
        const blockItems = std.ArrayList(*AST.BlockItem).init(self.allocator);
        var peekToken = try self.l.peekToken(self.allocator);
        const functionDecl = try self.allocator.create(AST.ExternalDecl);
        const functionDef = try self.allocator.create(AST.FunctionDef);
        functionDef.* = .{
            .name = self.l.buffer[fnNameToken.start .. fnNameToken.end + 1],
            .blockItems = blockItems,
            .args = argList,
            .returnType = if (returnType.type == lexer.TokenType.INT_TYPE) AST.Type.Integer else AST.Type.Void,
            .storageClass = qualifier,
        };
        functionDecl.* = .{
            .FunctionDecl = functionDef,
        };
        while (peekToken != null and peekToken.?.type != lexer.TokenType.RBRACE) {
            const blockItem = try self.parseBlockItem();
            try functionDef.blockItems.append(blockItem);
            peekToken = try self.l.peekToken(self.allocator);
        }
        const rbrace = try self.l.nextToken(self.allocator);
        std.debug.assert(rbrace.type == lexer.TokenType.RBRACE);
        return functionDecl;
    }

    pub fn parseArg(self: *Parser) ParserError!*AST.Arg {
        const hasPeeked = try self.l.peekToken(self.allocator);
        std.debug.assert(hasPeeked != null);
        switch (hasPeeked.?.type) {
            .INT_TYPE => {
                const arg = try self.allocator.create(AST.Arg);
                _ = try self.l.nextToken(self.allocator);
                const argName = try self.l.nextToken(self.allocator);
                arg.* = .{ .NonVoidArg = .{ .type = AST.Type.Integer, .identifier = self.l.buffer[argName.start .. argName.end + 1] } };
                return arg;
            },
            //.VOID => {
            //    const arg = self.allocator.create(AST.Arg);
            //    arg.* = .{
            //        .Void = {},
            //    };
            //    return arg;
            //},
            else => {
                std.log.warn("Unexpected peeked token in parseArg: {any}\n", .{hasPeeked});
            },
        }
        return ParserError.Unexpected;
    }

    pub fn parseBlockItem(self: *Parser) ParserError!*AST.BlockItem {
        const nextToken = try self.l.peekToken(self.allocator);
        const blockItem = try self.allocator.create(AST.BlockItem);
        if (nextToken) |nextTok| {
            switch (nextTok.type) {
                .EXTERN, .STATIC, .INT_TYPE => {
                    blockItem.* = AST.BlockItem{
                        .Declaration = (try self.parseDeclaration()),
                    };
                    const semicolon = try self.l.nextToken(self.allocator);
                    std.debug.assert(semicolon.type == lexer.TokenType.SEMICOLON);
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
            .STATIC, .EXTERN => {
                const qualifier = AST.Qualifier.from(nextToken.type);
                const returnType = try self.l.nextToken(self.allocator);
                std.debug.assert(returnType.type == lexer.TokenType.INT_TYPE or returnType.type == lexer.TokenType.VOID);
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
                    .type = AST.Type.Integer,
                    .storageClass = qualifier,
                };
                const peeked = try self.l.peekToken(self.allocator);
                std.debug.assert(peeked != null);
                switch ((peeked.?).type) {
                    .SEMICOLON => {
                        return declaration;
                    },
                    .ASSIGN => {
                        _ = try self.l.nextToken(self.allocator);
                        const expression = try self.parseExpression(0);
                        declaration.expression = expression;
                        std.debug.assert(if ((try self.l.peekToken(self.allocator) != null)) (try self.l.peekToken(self.allocator)).?.type == lexer.TokenType.SEMICOLON else false);
                        return declaration;
                    },
                    else => {
                        unreachable;
                    },
                }
                return declaration;
            },
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
                    .type = AST.Type.Integer,
                };
                const peeked = try self.l.peekToken(self.allocator);
                std.debug.assert(peeked != null);
                switch ((peeked.?).type) {
                    .SEMICOLON => {
                        return declaration;
                    },
                    .ASSIGN => {
                        _ = try self.l.nextToken(self.allocator);
                        const expression = try self.parseExpression(0);
                        declaration.expression = expression;
                        std.debug.assert(if ((try self.l.peekToken(self.allocator) != null)) (try self.l.peekToken(self.allocator)).?.type == lexer.TokenType.SEMICOLON else false);
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

    pub fn parseForInit(self: *Parser) ParserError!*AST.ForInit {
        const peekedToken = try self.l.peekToken(self.allocator);
        const forInit = try self.allocator.create(AST.ForInit);
        switch (peekedToken.?.type) {
            .INT_TYPE => {
                const decl = try self.parseDeclaration();
                forInit.* = AST.ForInit{ .Declaration = decl };
            },
            else => {
                const expr = try self.parseExpression(0);
                forInit.* = AST.ForInit{ .Expression = expr };
            },
        }
        const semicolon1 = try self.l.nextToken(self.allocator);
        if (semicolon1.type != lexer.TokenType.SEMICOLON) {
            std.log.warn("Semicolon expected, found {any}\n", .{semicolon1.type});
            unreachable;
        }
        return forInit;
    }

    pub fn parseStatement(self: *Parser) ParserError!*AST.Statement {
        switch ((try self.l.peekToken(self.allocator)).?.type) {
            .RETURN => {
                _ = try self.l.nextToken(self.allocator);
                const expr = try self.parseExpression(0);
                const retStmt = try self.allocator.create(AST.Statement);
                retStmt.* = AST.Statement{
                    .Return = AST.Return{
                        .expression = expr,
                    },
                };
                const peeked = try self.l.peekToken(self.allocator);
                if (peeked != null and peeked.?.type == lexer.TokenType.SEMICOLON) {
                    _ = try self.l.nextToken(self.allocator);
                }
                return retStmt;
            },
            .SEMICOLON => {
                _ = try self.l.nextToken(self.allocator);
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

                const peekToken = try self.l.peekToken(self.allocator);
                if (peekToken != null and peekToken.?.type == lexer.TokenType.ELSE) {
                    _ = try self.l.nextToken(self.allocator);
                    const elseStmt = try self.parseStatement();
                    ifStmt.If.elseStmt = elseStmt;
                }
                return ifStmt;
            },
            .LBRACE => {
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
                const semicolon = try self.l.nextToken(self.allocator);
                std.debug.assert(semicolon.type == lexer.TokenType.SEMICOLON);
                const doWhileStatement = try self.allocator.create(AST.Statement);
                doWhileStatement.* = AST.Statement{ .DoWhile = .{
                    .body = body,
                    .condition = condition,
                    .loopId = 0,
                } };
                return doWhileStatement;
            },
            .FOR => {
                _ = try self.l.nextToken(self.allocator);
                const lparen = try self.l.nextToken(self.allocator);
                std.debug.assert(lparen.type == lexer.TokenType.LPAREN);
                const forInit = try self.parseForInit();
                var peeked = try self.l.peekToken(self.allocator);
                var condition: ?*AST.Expression = null;
                var post: ?*AST.Expression = null;
                if (peeked != null and peeked.?.type != lexer.TokenType.SEMICOLON) {
                    std.log.warn("Parsing condition", .{});
                    condition = try self.parseExpression(0);
                }
                const semicolon = try self.l.nextToken(self.allocator);
                std.debug.assert(semicolon.type == lexer.TokenType.SEMICOLON);
                peeked = try self.l.peekToken(self.allocator);
                if (peeked != null and peeked.?.type != lexer.TokenType.RPAREN) {
                    std.log.warn("Parsing post", .{});
                    post = try self.parseExpression(0);
                }
                const rparen = try self.l.nextToken(self.allocator);
                if (rparen.type != lexer.TokenType.RPAREN) {
                    std.log.warn("RParen expected, found {any}\n", .{rparen.type});
                    unreachable;
                }
                const body = try self.parseStatement();
                const forStmt = try self.allocator.create(AST.Statement);
                forStmt.* = .{
                    .For = .{
                        .init = forInit,
                        .condition = condition,
                        .post = post,
                        .body = body,
                        .loopId = 0,
                    },
                };
                return forStmt;
            },
            .BREAK => {
                std.log.warn("Break statement\n", .{});
                _ = try self.l.nextToken(self.allocator);
                const semicolon = try self.l.nextToken(self.allocator);
                std.debug.assert(semicolon.type == lexer.TokenType.SEMICOLON);
                const breakStatement = try self.allocator.create(AST.Statement);
                breakStatement.* = AST.Statement{ .Break = 0 };
                return breakStatement;
            },
            .CONTINUE => {
                _ = try self.l.nextToken(self.allocator);
                const semicolon = try self.l.nextToken(self.allocator);
                std.debug.assert(semicolon.type == lexer.TokenType.SEMICOLON);
                const continueStatement = try self.allocator.create(AST.Statement);
                continueStatement.* = AST.Statement{ .Continue = 0 };
                return continueStatement;
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
                    .loopId = 0,
                } };
                return whileStatement;
            },
            else => {
                const twoToks = try self.l.peekTwoTokens(self.allocator);
                if (twoToks[1]) |secondTok| {
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
                const semicolon = try self.l.nextToken(self.allocator);
                std.debug.assert(semicolon.type == lexer.TokenType.SEMICOLON);
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
                const peekedTwo = try self.l.peekTwoTokens(self.allocator);
                if (peekedTwo[1] != null and peekedTwo[1].?.type == lexer.TokenType.LPAREN) {
                    //Function Call
                    const fnName = self.l.buffer[peekedTwo[0].?.start .. peekedTwo[0].?.end + 1];
                    var args = std.ArrayList(*AST.Expression).init(self.allocator);
                    _ = try self.l.nextToken(self.allocator);
                    _ = try self.l.nextToken(self.allocator);
                    while ((try self.l.peekToken(self.allocator)).?.type != lexer.TokenType.RPAREN) {
                        const expr = try self.parseExpression(0);
                        try args.append(expr);
                        if ((try self.l.peekToken(self.allocator)).?.type == lexer.TokenType.COMMA) {
                            _ = try self.l.nextToken(self.allocator);
                        }
                    }
                    _ = try self.l.nextToken(self.allocator);
                    const functionCall = try self.allocator.create(AST.Expression);
                    functionCall.* = .{ .FunctionCall = .{
                        .name = fnName,
                        .args = args,
                    } };
                    return functionCall;
                }
                // Identifier
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
        while (true) {
            const hasPeeked = try self.l.peekToken(self.allocator);
            if (hasPeeked) |peeked| {
                if (peeked.type == lexer.TokenType.SEMICOLON) {
                    return lhs;
                }
                const hasPeekedPrecedence = getPrecedence(peeked.type);
                if (hasPeekedPrecedence == null) {
                    //logz.info().fmt("parseExpression", "exited cause of no precdence for token and token={any}\n", .{peeked}).log();
                    return lhs;
                }
                const peekedPrecedence = hasPeekedPrecedence.?;
                if (precedence > peekedPrecedence) {
                    //logz.info().fmt("parseExpression", "exited cause of precedence of token being greater", .{}).log();
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
    _ = try std.testing.expect(std.mem.eql(u8, program.externalDecls.items[0].FunctionDecl.name, "main"));
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
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[0].Statement.Return.expression});
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
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[0].Statement.Return.expression});
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[0].Statement.Return.expression.Binary.lhs});
}

test "precedence with >= and <=" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){ return 2 >= 3 + 1 <= 5; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try Parser.init(allocator, l);
    const program = try p.parseProgram();
    std.log.warn("Return exp: \x1b[34m{any}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[0].Statement.Return.expression});
    std.log.warn("Binary lhs: \x1b[34m{any}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[0].Statement.Return.expression.Binary.lhs});
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[0].Statement.Return.expression.Binary.lhs.Binary.lhs});
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[0].Statement.Return.expression.Binary.lhs.Binary.rhs});
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[0].Statement.Return.expression.Binary.rhs.Integer});
}

test "parsing declarations and statements" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){int x = 3; return x;}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try Parser.init(allocator, l);
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
    var p = try Parser.init(allocator, l);
    const program = try p.parseProgram();
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[0]});
    std.log.warn("\x1b[34m{s}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[0].Declaration.name});
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[1]});
    std.log.warn("\x1b[34m{s}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[1].Declaration.name});
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[1].Declaration.expression});
    std.log.warn("\x1b[34m{s}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[1].Declaration.expression.?.Assignment.lhs.Identifier});
    std.log.warn("\x1b[34m{any}\x1b[0m", .{program.externalDecls.items[0].FunctionDecl.blockItems.items[1].Declaration.expression.?.Assignment.rhs});
}

test "test if statements" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr = "int main(){int y; int x = y = 3;if(x == y) return x; else return y;}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try Parser.init(allocator, l);
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
    var p = try Parser.init(allocator, l);
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
    var p = try Parser.init(allocator, l);
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
    var p = try Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try AST.VarResolver.init(allocator);
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
    var p = try Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try AST.VarResolver.init(allocator);
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
    var p = try Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try AST.VarResolver.init(allocator);
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
    var p = try Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try AST.VarResolver.init(allocator);
    try varResolver.resolve(program);
    std.log.warn("Add function : {s}, body: {any}\n", .{
        program.externalDecls.items[0].FunctionDecl.name,
        program.externalDecls.items[0].FunctionDecl.blockItems.items[0].Statement.Return,
    });
    std.log.warn("Main function : {s}, body: {any}\n", .{
        program.externalDecls.items[1].FunctionDecl.name,
        program.externalDecls.items[1].FunctionDecl.blockItems.items[0].Statement.Return.expression.FunctionCall,
    });
    for (program.externalDecls.items[1].FunctionDecl.blockItems.items[0].Statement.Return.expression.FunctionCall.args.items) |arg| {
        std.log.warn("Arg is {any}\n", .{arg});
    }
}

test "parse globals" {
    const semantic = @import("semantic.zig");
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ int k = 3;
        \\ int main(){
        \\     int b = 0; 
        \\     return b+k;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try AST.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typeChecker = try semantic.Typechecker.init(allocator);
    if ((try typeChecker.check(program))) |typeError| {
        std.log.warn("Type error: {any}\n", .{typeError});
    }
    std.log.warn("Globals not renamed: {s}\n", .{program.externalDecls.items[0].VarDeclaration.name});
    std.log.warn("Globals1: {s}\n", .{program.externalDecls.items[1].FunctionDecl.name});
    std.log.warn("Locals renamed: {s}\n", .{
        program.externalDecls.items[1].FunctionDecl.blockItems.items[0].Declaration.name,
    });
}

test "parse with storage classes" {
    const semantic = @import("semantic.zig");
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const programStr =
        \\ extern int k;
        \\ static int main(){
        \\     int b = 0; 
        \\     return b+k;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try AST.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typeChecker = try semantic.Typechecker.init(allocator);
    if ((try typeChecker.check(program))) |typeError| {
        std.log.warn("Type error: {any}\n", .{typeError});
    }
    std.log.warn("Globals not renamed: {s}\n", .{program.externalDecls.items[0].VarDeclaration.name});
    std.log.warn("Globals storageClass: {any}\n", .{program.externalDecls.items[0].VarDeclaration.storageClass});
    std.log.warn("Globals1: {s}\n", .{program.externalDecls.items[1].FunctionDecl.name});
    std.log.warn("Globals1 storageClass: {any}\n", .{program.externalDecls.items[1].FunctionDecl.storageClass});
    std.log.warn("Locals renamed: {s}\n", .{
        program.externalDecls.items[1].FunctionDecl.blockItems.items[0].Declaration.name,
    });
}
