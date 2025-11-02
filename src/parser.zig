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
    NoInnerFuncDeclarator,
    FunctionDeclCantUnwrapIdent,
};

fn getDeclaratorType(allocator: std.mem.Allocator, declarator: *AST.Declarator, lhsType: AST.Type) !*AST.Type {
    const declaratorType = try allocator.create(AST.Type);
    declaratorType.* = switch (declarator.*) {
        .Ident => lhsType,
        .PointerDeclarator => |ptr| blk: {
            const innerType = try getDeclaratorType(allocator, ptr, lhsType);
            break :blk .{ .Pointer = innerType };
        },
        .ArrayDeclarator => |arr| blk: {
            // First, compute the base type from the inner declarator
            var inner_type = try getDeclaratorType(allocator, arr.declarator, lhsType);
            const sizes = arr.size.items;
            std.debug.assert(sizes.len >= 1);

            // Build nested array types from inner-most to outer-most.
            // For sizes like [s0, s1, s2], this builds:
            //   Array(size=s0, ty=Array(size=s1, ty=Array(size=s2, ty=inner_type)))
            if (sizes.len >= 2) {
                var i: usize = sizes.len - 1;
                while (i > 0) : (i -= 1) {
                    const t = try allocator.create(AST.Type);
                    t.* = .{ .Array = .{ .size = sizes[i], .ty = inner_type } };
                    inner_type = t;
                }
            }
            std.log.warn("lhsType: {any}, returning: {any}\n", .{ lhsType, AST.Type{ .Array = .{ .size = sizes[0], .ty = inner_type } } });

            break :blk .{ .Array = .{ .size = sizes[0], .ty = inner_type } };
        },
        .FunDeclarator => unreachable,
    };
    return declaratorType;
}

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
            std.log.warn("external decl:{any} and current token:{any} \n", .{ externalDecl, self.l.currentToken });
            try program.externalDecls.append(externalDecl);
        }
        return program;
    }

    pub fn parseType(self: *Parser) ParserError!AST.Type {
        const next = try self.l.nextToken(self.allocator);
        return switch (next.type) {
            .INT_TYPE => .Integer,
            .LONG_TYPE => .Long,
            .FLOAT_TYPE => .Float,
            .UNSIGNED => blk: {
                break :blk switch ((try self.l.nextToken(self.allocator)).type) {
                    .INT_TYPE => .UInteger,
                    .LONG_TYPE => .ULong,
                    .FLOAT_TYPE => .Float,
                    else => unreachable,
                };
            },
            .VOID => .Void,
            .SIGNED => blk: {
                break :blk switch ((try self.l.nextToken(self.allocator)).type) {
                    .INT_TYPE => .Integer,
                    .LONG_TYPE => .Long,
                    .FLOAT_TYPE => .Float,
                    else => unreachable,
                };
            },
            else => {
                std.log.warn("next type: {any}\n", .{next.type});
                unreachable;
            },
        };
    }

    inline fn parseFunctionDecl(self: *Parser, qualifier: ?AST.Qualifier, returnType: AST.Type) ParserError!*AST.ExternalDecl {
        const fnNameToken = try self.l.nextToken(self.allocator);
        var argList = std.ArrayList(*AST.Arg).init(self.allocator);
        std.debug.assert(fnNameToken.type == lexer.TokenType.IDENTIFIER);
        std.debug.assert((try self.l.nextToken(self.allocator)).type == lexer.TokenType.LPAREN);

        // INFO: Parse function arguments
        while ((try self.l.peekToken(self.allocator)).?.type != lexer.TokenType.RPAREN) {
            const arg = try self.parseArg();
            try argList.append(arg);
            if ((try self.l.peekToken(self.allocator)).?.type == lexer.TokenType.COMMA) {
                _ = try self.l.nextToken(self.allocator);
            }
        }
        std.debug.assert((try self.l.nextToken(self.allocator)).type == lexer.TokenType.RPAREN);

        //INFO: Parse function body
        const blockItems = std.ArrayList(*AST.BlockItem).init(self.allocator);
        var peekToken = try self.l.peekToken(self.allocator);
        const functionDecl = try self.allocator.create(AST.ExternalDecl);
        const functionDef = try self.allocator.create(AST.FunctionDef);
        functionDef.* = .{
            .name = self.l.buffer[fnNameToken.start .. fnNameToken.end + 1],
            .blockItems = blockItems,
            .args = argList,
            .returnType = returnType,
            .storageClass = qualifier,
        };
        functionDecl.* = .{
            .FunctionDecl = functionDef,
        };

        if ((try self.l.peekToken(self.allocator)).?.type == lexer.TokenType.SEMICOLON) {
            //INFO: Handle declared but not defined functions
            _ = try self.l.nextToken(self.allocator);
            return functionDecl;
        }

        //INFO: Parse function body
        std.debug.assert((try self.l.nextToken(self.allocator)).type == lexer.TokenType.LBRACE);
        while (peekToken != null and peekToken.?.type != lexer.TokenType.RBRACE) {
            const blockItem = try self.parseBlockItem();
            try functionDef.blockItems.append(blockItem);
            peekToken = try self.l.peekToken(self.allocator);
        }
        const rbrace = try self.l.nextToken(self.allocator);
        std.debug.assert(rbrace.type == lexer.TokenType.RBRACE);

        return functionDecl;
    }

    inline fn parseVarDecl(self: *Parser, qualifier: ?AST.Qualifier, returnType: AST.Type) ParserError!*AST.Declaration {
        const varName = try self.l.nextToken(self.allocator);
        const declaration = try self.allocator.create(AST.Declaration);
        declaration.* = .{
            .name = self.l.buffer[varName.start .. varName.end + 1],
            .expression = null,
            .type = returnType,
            .storageClass = qualifier,
        };

        const semicolonOrAssign = try self.l.nextToken(self.allocator);
        switch (semicolonOrAssign.type) {
            .SEMICOLON => {
                return declaration;
            },
            .ASSIGN => {
                const expression = try self.parseExpression(0);
                declaration.expression = expression;
                const semicolon = try self.l.nextToken(self.allocator);
                std.debug.assert(semicolon.type == lexer.TokenType.SEMICOLON);
                return declaration;
            },
            else => {
                std.log.warn("semicolon or assign expected,got: {any}\n", .{semicolonOrAssign});
                unreachable;
            },
        }
    }

    pub fn parseDeclaratorSuffix(self: *Parser) ParserError!?*AST.DeclaratorSuffix {
        const declaratorSuffix = try self.allocator.create(AST.DeclaratorSuffix);
        switch ((try self.l.peekToken(self.allocator)).?.type) {
            .LPAREN => {
                _ = try self.l.nextToken(self.allocator);
                var argList = std.ArrayList(*AST.Arg).init(self.allocator);
                while ((try self.l.peekToken(self.allocator)).?.type != lexer.TokenType.RPAREN) {
                    const arg = try self.parseArg();
                    try argList.append(arg);
                    if ((try self.l.peekToken(self.allocator)).?.type == lexer.TokenType.COMMA) {
                        _ = try self.l.nextToken(self.allocator);
                    }
                }
                const rparen = try self.l.nextToken(self.allocator);
                std.debug.assert(rparen.type == .RPAREN);
                declaratorSuffix.* = .{ .ArgList = argList };
                return declaratorSuffix;
            },
            .LSQUARE => {
                // parse multiple
                var exprList = std.ArrayList(usize).init(self.allocator);
                while ((try self.l.peekToken(self.allocator)).?.type == .LSQUARE) {
                    _ = try self.l.nextToken(self.allocator);
                    const expression = try self.parseExpression(0);
                    std.debug.assert(expression.getType() == .Integer);
                    const rsquare = try self.l.nextToken(self.allocator);
                    std.debug.assert(rsquare.type == .RSQUARE);
                    if (expression.Constant.value.Integer < 0) {
                        std.log.err("Yo your array size is negative\n", .{});
                        return ParserError.InvalidArgument;
                    }
                    try exprList.append(@intCast(expression.Constant.value.Integer));
                    std.log.warn("Done?: {}, next: {}\n", .{ self.l.currentToken.?, (try self.l.peekToken(self.allocator)).? });
                }
                declaratorSuffix.* = .{ .ArraySuffix = exprList };
                return declaratorSuffix;
            },
            else => {
                return null;
            },
        }
    }

    pub fn parseDirectDeclarator(self: *Parser) ParserError!*AST.Declarator {
        const simple = try self.parseSimpleDeclarator();
        // Can be one of these:
        // 1. Just a simple identifier
        // 2. Be a (*iden) => the first part of a function pointer (Not supported will assert false in the latter states of the compiler)
        // * Can be followed by a (type arg,type arg...), (simple identifier + (type arg, type arg) ) => Func Declarator
        // * Can be followed by nothing: just an identifier
        // When array literals are introduced, the second case can have the [ integer ] suffix
        // parseDeclaratorSuffix
        const declaratorSuffix = try self.parseDeclaratorSuffix();
        if (declaratorSuffix == null) {
            return simple;
        }

        const directDecl = try self.allocator.create(AST.Declarator);
        switch (declaratorSuffix.?.*) {
            .ArgList => |argList| {
                const funcDeclarator = try self.allocator.create(AST.FunDeclarator);
                funcDeclarator.* = .{
                    .declarator = simple,
                    .params = argList,
                };
                directDecl.* = .{ .FunDeclarator = funcDeclarator };
                return directDecl;
            },
            .ArraySuffix => |arraySuffix| {
                const arrDeclarator = try self.allocator.create(AST.ArrDeclarator);
                arrDeclarator.* = .{
                    .declarator = simple,
                    .size = arraySuffix,
                };
                directDecl.* = .{ .ArrayDeclarator = arrDeclarator };
                return directDecl;
            },
        }
        unreachable;
    }

    pub inline fn parseSimpleDeclarator(self: *Parser) ParserError!*AST.Declarator {
        switch ((try self.l.peekToken(self.allocator)).?.type) {
            .IDENTIFIER => {
                const iden = try self.l.nextToken(self.allocator);
                const decl = try self.allocator.create(AST.Declarator);
                decl.* = .{
                    .Ident = self.l.buffer[iden.start .. iden.end + 1],
                };
                return decl;
            },
            .LPAREN => {
                _ = try self.l.nextToken(self.allocator);
                const decl = try self.parseDeclarator();
                const rparen = try self.l.nextToken(self.allocator);
                std.debug.assert(rparen.type == .RPAREN);
                return decl;
            },
            else => {
                unreachable;
            },
        }
    }

    pub fn parseDeclarator(self: *Parser) ParserError!*AST.Declarator {
        switch ((try self.l.peekToken(self.allocator)).?.type) {
            .MULTIPLY => {
                _ = try self.l.nextToken(self.allocator);
                const decl = try self.parseDeclarator();
                const outerDecl = try self.allocator.create(AST.Declarator);
                outerDecl.* = .{
                    .PointerDeclarator = decl,
                };
                return outerDecl;
            },
            else => {
                return self.parseDirectDeclarator();
            },
        }
    }

    pub fn parseExternalDecl(self: *Parser) ParserError!*AST.ExternalDecl {
        const qualifier = AST.Qualifier.from((try self.l.peekToken(self.allocator)).?.type);
        if (qualifier != null) {
            _ = try self.l.nextToken(self.allocator);
        }
        const returnType = try self.parseType();
        const declarator = try self.parseDeclarator();
        const externalDecl = try self.allocator.create(AST.ExternalDecl);
        std.log.warn("Declarator: {any} and peek token:{any}\n", .{ declarator, (try self.l.peekToken(self.allocator)) });
        // INFO: We are not implementing function pointers for now, so if there
        // is a function declarator, that implies a function declaration.
        if (declarator.containsFuncDeclarator()) {
            const functionDecl = try self.allocator.create(AST.FunctionDef);
            functionDecl.* = .{
                .declarator = declarator,
                .blockItems = switch ((try self.l.peekToken(self.allocator)).?.type) {
                    .SEMICOLON => blk: {
                        _ = try self.l.nextToken(self.allocator);
                        break :blk std.ArrayList(*AST.BlockItem).init(self.allocator);
                    },
                    .LBRACE => blk: {
                        _ = try self.l.nextToken(self.allocator);
                        var blockList = std.ArrayList(*AST.BlockItem).init(self.allocator);
                        var peeked = try self.l.peekToken(self.allocator);
                        while (peeked != null and peeked.?.type != .RBRACE) {
                            const blockItem = try self.parseBlockItem();
                            try blockList.append(blockItem);
                            peeked = try self.l.peekToken(self.allocator);
                        }
                        _ = self.l.nextToken(self.allocator) catch |err| {
                            if (err == error.BufferEmpty) {
                                break :blk blockList;
                            }
                        };
                        break :blk blockList;
                    },
                    else => |tok| {
                        std.log.warn("semicolon or lbrace expected, got {any}\n", .{tok});
                        unreachable;
                    },
                },
                .returnType = returnType,
                .storageClass = qualifier,
            };
            externalDecl.* = .{
                .FunctionDecl = functionDecl,
            };
            return externalDecl;
        }
        const varDeclaration = try self.allocator.create(AST.Declaration);
        varDeclaration.* = .{
            .name = (try declarator.unwrapIdentDecl()).Ident,
            .type = (try getDeclaratorType(self.allocator, declarator, returnType)).*,
            .varInitValue = switch ((try self.l.nextToken(self.allocator)).type) {
                .SEMICOLON => null,
                .ASSIGN => blk: {
                    const varInitVal = try self.parseInitializer();
                    const semicolon = try self.l.nextToken(self.allocator);
                    std.debug.assert(semicolon.type == lexer.TokenType.SEMICOLON);
                    break :blk varInitVal;
                },
                else => |tok| {
                    std.log.warn("semicolon or assign expected,got: {any}\n", .{tok});
                    unreachable;
                },
            },
            .storageClass = qualifier,
        };
        externalDecl.* = .{
            .VarDeclaration = varDeclaration,
        };
        return externalDecl;
    }

    pub fn parseArg(self: *Parser) ParserError!*AST.Arg {
        const argType = try self.parseType();
        //INFO: The lines following this has to change and should parse a declarator instead
        const arg = try self.allocator.create(AST.Arg);

        arg.* = if (argType == .Void) .{ .Void = {} } else blk: {
            const argDeclarator = try self.parseDeclarator();
            break :blk .{
                .NonVoidArg = .{
                    .type = argType,
                    .declarator = argDeclarator,
                },
            };
        };
        return arg;
    }

    pub fn parseBlockItem(self: *Parser) ParserError!*AST.BlockItem {
        const nextToken = try self.l.peekToken(self.allocator);
        const blockItem = try self.allocator.create(AST.BlockItem);
        if (nextToken) |nextTok| {
            switch (nextTok.type) {
                .EXTERN, .STATIC, .INT_TYPE, .LONG_TYPE, .UNSIGNED, .FLOAT_TYPE => {
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
        const nextToken = (try self.l.peekToken(self.allocator)).?;
        const qualifier = AST.Qualifier.from(nextToken.type);
        if (qualifier != null) {
            _ = try self.l.nextToken(self.allocator);
        }
        const returnType = try self.parseType();
        const declarator = try self.parseDeclarator();
        const declaration = try self.allocator.create(AST.Declaration);
        std.log.warn("Creating declaration for {s}\n", .{(try declarator.unwrapIdentDecl()).Ident});
        declaration.* = .{
            .name = (try declarator.unwrapIdentDecl()).Ident,
            .type = (try getDeclaratorType(self.allocator, declarator, returnType)).*,
            .varInitValue = switch ((try self.l.peekToken(self.allocator)).?.type) {
                .SEMICOLON => blk: {
                    _ = try self.l.nextToken(self.allocator);
                    break :blk null;
                },
                .ASSIGN => blk: {
                    _ = try self.l.nextToken(self.allocator);
                    const initializer = try self.parseInitializer();

                    const semicolon = try self.l.nextToken(self.allocator);
                    std.debug.assert(semicolon.type == lexer.TokenType.SEMICOLON);
                    break :blk initializer;
                },
                else => |tok| {
                    std.log.warn("Expected semicolon or assign, got {any}\n", .{tok});
                    unreachable;
                },
            },
            .storageClass = qualifier,
        };
        return declaration;
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
                const semicolon1 = try self.l.nextToken(self.allocator);
                if (semicolon1.type != lexer.TokenType.SEMICOLON) {
                    //std.log.warn("Semicolon expected, found {any}\n", .{semicolon1.type});
                    unreachable;
                }
            },
        }
        return forInit;
    }

    pub inline fn parseReturn(self: *Parser) ParserError!*AST.Statement {
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
    }

    pub inline fn parseEmpty(self: *Parser) ParserError!*AST.Statement {
        _ = try self.l.nextToken(self.allocator);
        const nullStmt = try self.allocator.create(AST.Statement);
        nullStmt.* = AST.Statement{
            .Null = {},
        };
        return nullStmt;
    }

    pub inline fn parseIf(self: *Parser) ParserError!*AST.Statement {
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
    }

    pub inline fn parseCompound(self: *Parser) ParserError!*AST.Statement {
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
    }

    pub inline fn parseGoto(self: *Parser) ParserError!*AST.Statement {
        _ = try self.l.nextToken(self.allocator);
        const jumpLabel = try self.l.nextToken(self.allocator);
        const gotoStatement = try self.allocator.create(AST.Statement);
        gotoStatement.* = AST.Statement{
            .Goto = self.l.buffer[jumpLabel.start .. jumpLabel.end + 1],
        };
        const semicolon = try self.l.nextToken(self.allocator);
        std.debug.assert(semicolon.type == lexer.TokenType.SEMICOLON);
        return gotoStatement;
    }

    pub inline fn parseDoWhile(self: *Parser) ParserError!*AST.Statement {
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
    }

    inline fn parseFor(self: *Parser) ParserError!*AST.Statement {
        _ = try self.l.nextToken(self.allocator);
        const lparen = try self.l.nextToken(self.allocator);
        std.debug.assert(lparen.type == lexer.TokenType.LPAREN);
        const forInit = try self.parseForInit();
        var peeked = try self.l.peekToken(self.allocator);
        var condition: ?*AST.Expression = null;
        var post: ?*AST.Expression = null;
        if (peeked != null and peeked.?.type != lexer.TokenType.SEMICOLON) {
            // std.log.warn("Parsing condition", .{});
            condition = try self.parseExpression(0);
        }
        const semicolon = try self.l.nextToken(self.allocator);
        std.debug.assert(semicolon.type == lexer.TokenType.SEMICOLON);
        peeked = try self.l.peekToken(self.allocator);
        if (peeked != null and peeked.?.type != lexer.TokenType.RPAREN) {
            // std.log.warn("Parsing post", .{});
            post = try self.parseExpression(0);
        }
        const rparen = try self.l.nextToken(self.allocator);
        if (rparen.type != lexer.TokenType.RPAREN) {
            // std.log.warn("RParen expected, found {any}\n", .{rparen.type});
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
    }

    inline fn parseBreak(self: *Parser) ParserError!*AST.Statement {
        _ = try self.l.nextToken(self.allocator);
        const semicolon = try self.l.nextToken(self.allocator);
        std.debug.assert(semicolon.type == lexer.TokenType.SEMICOLON);
        const breakStatement = try self.allocator.create(AST.Statement);
        breakStatement.* = AST.Statement{ .Break = 0 };
        return breakStatement;
    }

    inline fn parseContinue(self: *Parser) ParserError!*AST.Statement {
        _ = try self.l.nextToken(self.allocator);
        const semicolon = try self.l.nextToken(self.allocator);
        std.debug.assert(semicolon.type == lexer.TokenType.SEMICOLON);
        const continueStatement = try self.allocator.create(AST.Statement);
        continueStatement.* = AST.Statement{ .Continue = 0 };
        return continueStatement;
    }

    inline fn parseWhile(self: *Parser) ParserError!*AST.Statement {
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
    }

    inline fn parseExpressionOrLabel(self: *Parser) ParserError!*AST.Statement {
        const twoToks = try self.l.peekTwoTokens(self.allocator);
        if (twoToks[1]) |secondTok| {
            if (secondTok.type == lexer.TokenType.COLON) {
                const identifier = try self.l.nextToken(self.allocator);
                const colon = try self.l.nextToken(self.allocator);
                std.debug.assert(colon.type == lexer.TokenType.COLON);
                const labelStmt = try self.allocator.create(AST.Statement);
                // std.log.warn("Found label: {s}\n", .{self.l.buffer[identifier.start .. identifier.end + 1]});
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
    }

    pub fn parseStatement(self: *Parser) ParserError!*AST.Statement {
        return switch ((try self.l.peekToken(self.allocator)).?.type) {
            .RETURN => try self.parseReturn(),
            .SEMICOLON => try self.parseEmpty(),
            .IF => try self.parseIf(),
            .LBRACE => try self.parseCompound(),
            .GOTO => try self.parseGoto(),
            .DO => try self.parseDoWhile(),
            .FOR => try self.parseFor(),
            .BREAK => try self.parseBreak(),
            .CONTINUE => try self.parseContinue(),
            .WHILE => try self.parseWhile(),
            else => try self.parseExpressionOrLabel(),
        };
    }

    inline fn tokToUnaryOp(tokenType: lexer.TokenType) ParserError!AST.UnaryOp {
        return switch (tokenType) {
            .MINUS => AST.UnaryOp.NEGATE,
            .TILDE => AST.UnaryOp.COMPLEMENT,
            else => lexer.LexerError.InvalidToken,
        };
    }

    pub inline fn parseVarOrFn(self: *Parser) ParserError!*AST.Expression {
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
        identifier.* = AST.Expression{ .Identifier = .{
            .name = self.l.buffer[currToken.start .. currToken.end + 1],
        } };
        return identifier;
    }

    pub inline fn parseBracketed(self: *Parser) ParserError!*AST.Expression {
        _ = try self.l.nextToken(self.allocator);
        const exp = try self.parseExpression(0);
        const rparen = try self.l.nextToken(self.allocator);
        std.debug.assert(rparen.type == lexer.TokenType.RPAREN);
        return exp;
    }

    pub inline fn parseUnaryExp(self: *Parser) ParserError!*AST.Expression {
        const op = try tokToUnaryOp((try self.l.nextToken(self.allocator)).type);
        const factor = try self.parseFactor();
        const unaryNode = try self.allocator.create(AST.Expression);
        unaryNode.* = AST.Expression{ .Unary = AST.Unary{
            .unaryOp = op,
            .exp = factor,
        } };
        return unaryNode;
    }
    inline fn handleSignedInt(buffer: []u8) ParserError!AST.Constant {
        const val = std.fmt.parseInt(i32, buffer, 10) catch |err| {
            if (err == error.Overflow)
                return .{
                    .type = .UInteger,
                    .value = .{ .UInteger = try std.fmt.parseInt(u32, buffer, 10) },
                };
            return err;
        };
        return .{
            .type = .Integer,
            .value = .{ .Integer = val },
        };
    }
    pub inline fn parseInteger(self: *Parser) ParserError!*AST.Expression {
        const currToken = try self.l.nextToken(self.allocator);
        const integerNode = try self.allocator.create(AST.Expression);
        // std.fmt.parseInt(i32, self.l.buffer[currToken.start .. currToken.end + 1], 10) catch ||
        //             std.fmt.parseInt(u32, self.l.buffer[currToken.start .. currToken.end + 1], 10);
        integerNode.* = AST.Expression{
            .Constant = try handleSignedInt(self.l.buffer[currToken.start .. currToken.end + 1]),
        };
        return integerNode;
    }

    pub inline fn parseFloat(self: *Parser) ParserError!*AST.Expression {
        const currToken = try self.l.nextToken(self.allocator);
        const floatNode = try self.allocator.create(AST.Expression);
        floatNode.* = AST.Expression{
            .Constant = AST.Constant{
                .type = .Float,
                .value = .{ .Float = try std.fmt.parseFloat(
                    f64,
                    self.l.buffer[currToken.start .. currToken.end + 1],
                ) },
            },
        };
        return floatNode;
    }

    pub inline fn parseLong(self: *Parser) ParserError!*AST.Expression {
        const currToken = try self.l.nextToken(self.allocator);

        const longNode = try self.allocator.create(AST.Expression);
        const suffixRemovedSlice = if (self.l.buffer[currToken.end - 1] == 'L') self.l.buffer[currToken.start .. currToken.end - 1] else self.l.buffer[currToken.start..currToken.end];
        longNode.* = AST.Expression{ .Constant = AST.Constant{
            .type = .Long,
            .value = .{ .Long = try std.fmt.parseInt(i64, suffixRemovedSlice, 10) },
        } };
        return longNode;
    }

    pub inline fn parseUnsignedInt(self: *Parser) ParserError!*AST.Expression {
        const currToken = try self.l.nextToken(self.allocator);

        const longNode = try self.allocator.create(AST.Expression);
        const suffixRemovedSlice = if (self.l.buffer[currToken.end - 1] == 'U') self.l.buffer[currToken.start .. currToken.end - 1] else self.l.buffer[currToken.start..currToken.end];
        longNode.* = AST.Expression{ .Constant = AST.Constant{
            .type = .UInteger,
            .value = .{ .UInteger = try std.fmt.parseInt(u32, suffixRemovedSlice, 10) },
        } };
        return longNode;
    }

    pub inline fn parseUnsignedLong(self: *Parser) ParserError!*AST.Expression {
        const currToken = try self.l.nextToken(self.allocator);

        const longNode = try self.allocator.create(AST.Expression);
        const suffixRemovedSlice = if (std.mem.eql(u8, self.l.buffer[currToken.end - 2 ..], "UL")) self.l.buffer[currToken.start .. currToken.end - 1] else self.l.buffer[currToken.start .. currToken.end - 1];
        longNode.* = AST.Expression{ .Constant = AST.Constant{
            .type = .ULong,
            .value = .{ .ULong = try std.fmt.parseInt(u64, suffixRemovedSlice, 10) },
        } };
        return longNode;
    }

    pub fn parseDeref(self: *Parser) ParserError!*AST.Expression {
        _ = try self.l.nextToken(self.allocator);
        const exp = try self.parseFactor();
        const derefNode = try self.allocator.create(AST.Expression);
        derefNode.* = AST.Expression{ .Deref = .{
            .type = null,
            .exp = exp,
        } };
        return derefNode;
    }

    pub fn parseAddrOf(self: *Parser) ParserError!*AST.Expression {
        _ = try self.l.nextToken(self.allocator);
        const exp = try self.parseFactor();
        const addrOfNode = try self.allocator.create(AST.Expression);
        addrOfNode.* = AST.Expression{ .AddrOf = .{
            .type = null,
            .exp = exp,
        } };
        return addrOfNode;
    }

    pub fn parseFactor(self: *Parser) ParserError!*AST.Expression {
        const peekToken = (try self.l.peekToken(self.allocator)).?;
        return switch (peekToken.type) {
            .LONG => try self.parseLong(),
            .INTEGER => try self.parseInteger(),
            .MINUS, .TILDE => try self.parseUnaryExp(),
            .LPAREN => try self.parseBracketed(),
            .IDENTIFIER => try self.parseVarOrFn(),
            .UNSIGNED_INT => try self.parseUnsignedInt(),
            .UNSIGNED_LONG => try self.parseUnsignedLong(),
            .FLOAT => try self.parseFloat(),
            .MULTIPLY => try self.parseDeref(),
            .BITWISE_AND => try self.parseAddrOf(),
            else => |tokType| {
                std.log.warn("Parse factor unknown type: {any}\n", .{tokType});
                unreachable;
            },
        };
    }

    inline fn getPrecedence(tok: lexer.TokenType) ?u32 {
        return switch (tok) {
            .LSQUARE => 55,
            .MULTIPLY, .DIVIDE, .MODULO => 50,
            .PLUS, .MINUS => 45,
            .LESS, .LESSEQ, .GREATER, .GREATEREQ => 35,
            .EQUALS, .NOT_EQUALS => 30,
            .LOGIC_AND => 10,
            .LOGIC_OR => 5,
            .TERNARY => 3,
            .ASSIGN => 1,
            else => null,
        };
    }
    inline fn binaryOpFromTokType(tok: lexer.TokenType) AST.BinOp {
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
                //std.log.warn("Found in binaryOpFromTokType unhandled {}\n", .{tok});
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
        std.debug.assert(self.l.currentToken != null);
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
                    std.log.warn("This: {any}\n", .{expr});
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
                .LSQUARE => {
                    const index = try self.parseExpression(0);
                    const rsquare = try self.l.nextToken(self.allocator);
                    std.debug.assert(rsquare.type == .RSQUARE);
                    const expr = try self.allocator.create(AST.Expression);
                    expr.* = AST.Expression{ .ArrSubscript = .{
                        .type = null,
                        .arr = lhs,
                        .index = index,
                    } };
                    return expr;
                },
                else => {
                    return lhs;
                },
            }
        }
        return lhs;
    }

    pub fn parseInitializer(self: *Parser) ParserError!*AST.Initializer {
        switch ((try self.l.peekToken(self.allocator)).?.type) {
            .LBRACE => {
                _ = try self.l.nextToken(self.allocator);
                const firstElem = try self.parseInitializer();
                var arrayInitializer = try self.allocator.create(AST.ArrayInitializer);
                arrayInitializer.* = .{
                    .type = null,
                    .initializers = std.ArrayList(*AST.Initializer).init(self.allocator),
                };
                try arrayInitializer.initializers.append(firstElem);
                while (true) {
                    const commaOrRBrace = try self.l.peekToken(self.allocator);
                    std.debug.assert(commaOrRBrace.?.type == .COMMA or commaOrRBrace.?.type == .RBRACE);
                    _ = try self.l.nextToken(self.allocator);
                    switch (commaOrRBrace.?.type) {
                        .COMMA => {
                            if ((try self.l.peekToken(self.allocator)).?.type == .RBRACE) {
                                _ = try self.l.nextToken(self.allocator);
                                const initializer = try self.allocator.create(AST.Initializer);
                                initializer.* = .{
                                    .ArrayExpr = arrayInitializer,
                                };
                                return initializer;
                            } else {
                                const initializer = try self.parseInitializer();
                                try arrayInitializer.initializers.append(initializer);
                            }
                        },
                        .RBRACE => {
                            const initializer = try self.allocator.create(AST.Initializer);
                            initializer.* = .{
                                .ArrayExpr = arrayInitializer,
                            };
                            return initializer;
                        },
                        else => unreachable,
                    }
                }
            },
            else => {
                const expr = try self.parseExpression(0);
                const initializer = try self.allocator.create(AST.Initializer);
                initializer.* = .{
                    .Expression = expr,
                };
                return initializer;
            },
        }
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
                std.log.warn("Precedence: {any} and peeked type: {s}\n", .{ peeked, self.l.buffer[peeked.start .. peeked.end + 1] });
                if (hasPeekedPrecedence == null) {
                    return lhs;
                }
                const peekedPrecedence = hasPeekedPrecedence.?;
                if (precedence > peekedPrecedence) {
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

test "getDeclaratorType: *unsigned long[5] -> **unsigned long" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const al = arena.allocator();

    // Build declarator: * ( [5] ident )
    const ident = try al.create(AST.Declarator);
    ident.* = .{ .Ident = @constCast("x"[0..1]) };

    var sizes = std.ArrayList(usize).init(al);
    try sizes.append(5);
    const arrNode = try al.create(AST.ArrDeclarator);
    arrNode.* = .{ .declarator = ident, .size = sizes };

    const arrDecl = try al.create(AST.Declarator);
    arrDecl.* = .{ .ArrayDeclarator = arrNode };

    const ptrDecl = try al.create(AST.Declarator);
    ptrDecl.* = .{ .PointerDeclarator = arrDecl };

    const ty_ptr = try getDeclaratorType(al, ptrDecl, .ULong);
    const resolved = ty_ptr.*.changeArrtoPointer();

    try std.testing.expect(std.meta.activeTag(resolved) == .Pointer);
    try std.testing.expect(std.meta.activeTag(resolved.Pointer.*) == .Pointer);
    try std.testing.expect(std.meta.activeTag(resolved.Pointer.*.Pointer.*) == .ULong);
}

test "getDeclaratorType: **int [5] -> ***int" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const al = arena.allocator();

    // Build declarator: * ( * ( [5] ident ) )
    const ident = try al.create(AST.Declarator);
    ident.* = .{ .Ident = @constCast("x"[0..1]) };

    var sizes = std.ArrayList(usize).init(al);
    try sizes.append(5);
    const arrNode = try al.create(AST.ArrDeclarator);
    arrNode.* = .{ .declarator = ident, .size = sizes };
    const arrDecl = try al.create(AST.Declarator);
    arrDecl.* = .{ .ArrayDeclarator = arrNode };

    const ptrInner = try al.create(AST.Declarator);
    ptrInner.* = .{ .PointerDeclarator = arrDecl };

    const ptrOuter = try al.create(AST.Declarator);
    ptrOuter.* = .{ .PointerDeclarator = ptrInner };

    const ty_ptr = try getDeclaratorType(al, ptrOuter, .Integer);
    const resolved = ty_ptr.*.changeArrtoPointer();

    try std.testing.expect(std.meta.activeTag(resolved) == .Pointer);
    const p1 = resolved.Pointer.*;
    try std.testing.expect(std.meta.activeTag(p1) == .Pointer);
    const p2 = p1.Pointer.*;
    try std.testing.expect(std.meta.activeTag(p2) == .Pointer);
    try std.testing.expect(std.meta.activeTag(p2.Pointer.*) == .Integer);
}

test "getDeclaratorType: *int[5][5] -> ***int" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const al = arena.allocator();

    // Build declarator: * ( [5][5] ident )
    const ident = try al.create(AST.Declarator);
    ident.* = .{ .Ident = @constCast("x"[0..1]) };

    var sizes = std.ArrayList(usize).init(al);
    try sizes.append(5);
    try sizes.append(5);
    const arrNode = try al.create(AST.ArrDeclarator);
    arrNode.* = .{ .declarator = ident, .size = sizes };
    const arrDecl = try al.create(AST.Declarator);
    arrDecl.* = .{ .ArrayDeclarator = arrNode };

    const ptrDecl = try al.create(AST.Declarator);
    ptrDecl.* = .{ .PointerDeclarator = arrDecl };

    const ty_ptr = try getDeclaratorType(al, ptrDecl, .Integer);
    const resolved = ty_ptr.*.changeArrtoPointer();

    // Expect ***int
    try std.testing.expect(std.meta.activeTag(resolved) == .Pointer);
    const p1 = resolved.Pointer.*;
    try std.testing.expect(std.meta.activeTag(p1) == .Pointer);
    const p2 = p1.Pointer.*;
    try std.testing.expect(std.meta.activeTag(p2) == .Pointer);
    try std.testing.expect(std.meta.activeTag(p2.Pointer.*) == .Integer);
}

test "getDeclaratorType: int[5][5] -> int**" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const al = arena.allocator();

    // Build declarator: [5][5] ident
    const ident = try al.create(AST.Declarator);
    ident.* = .{ .Ident = @constCast("x"[0..1]) };

    var sizes = std.ArrayList(usize).init(al);
    try sizes.append(5);
    try sizes.append(5);
    const arrNode = try al.create(AST.ArrDeclarator);
    arrNode.* = .{ .declarator = ident, .size = sizes };
    const arrDecl = try al.create(AST.Declarator);
    arrDecl.* = .{ .ArrayDeclarator = arrNode };

    const ty_ptr = try getDeclaratorType(al, arrDecl, .Integer);
    const resolved = ty_ptr.*.changeArrtoPointer();

    try std.testing.expect(std.meta.activeTag(resolved) == .Pointer);
    const p1 = resolved.Pointer.*;
    try std.testing.expect(std.meta.activeTag(p1) == .Pointer);
    try std.testing.expect(std.meta.activeTag(p1.Pointer.*) == .Integer);
}
