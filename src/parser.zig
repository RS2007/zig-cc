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
        const fnName = try self.l.nextToken(self.allocator);
        std.debug.assert(fnName.type == lexer.TokenType.IDENTIFIER);
        std.debug.assert((try self.l.nextToken(self.allocator)).type == lexer.TokenType.LPAREN);
        std.debug.assert((try self.l.nextToken(self.allocator)).type == lexer.TokenType.RPAREN);
        std.debug.assert((try self.l.nextToken(self.allocator)).type == lexer.TokenType.LBRACE);
        const stmt = try self.parseStatement();
        const program = try self.allocator.create(AST.Program);
        program.* = AST.Program{
            .function = AST.FunctionDef{
                .name = fnName,
                .statement = stmt,
            },
        };
        return program;
    }

    pub fn parseStatement(self: *Parser) ParserError!*AST.Statement {
        switch ((try self.l.nextToken(self.allocator)).type) {
            .RETURN => {
                const expr = try self.parseExpression();
                var retStmt = try self.allocator.create(AST.Statement);
                retStmt.* = AST.Statement{
                    .Return = AST.Return{
                        .expression = expr,
                    },
                };
                return retStmt;
            },
            else => {
                std.debug.assert(false);
                return ParserError.UnknownStatement;
            },
        }
    }

    pub fn parseExpression(self: *Parser) ParserError!*AST.Expression {
        const currToken = try self.l.nextToken(self.allocator);
        std.debug.assert(currToken.type == lexer.TokenType.INTEGER);
        var integerExpr = try self.allocator.create(AST.Expression);
        integerExpr.* = AST.Expression{
            .Integer = try std.fmt.parseInt(u32, self.l.buffer[currToken.start..currToken.end], 10),
        };
        return integerExpr;
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
    _ = try std.testing.expect(std.mem.eql(u8, programStr[program.function.name.start .. program.function.name.end + 1], "main"));
}
