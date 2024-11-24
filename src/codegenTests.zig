const std = @import("std");
const ast = @import("./AST.zig");
const lexer = @import("./lexer.zig");
const parser = @import("./parser.zig");
const semantic = @import("./semantic.zig");
const tac = @import("./TAC.zig");

test "testing assembly generation - unary" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/unary.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/unary.s", .{})).writer();
    defer arena.deinit();
    const programStr = "int main(){ return ~(-2); }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "testing assembly generation - binary" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/binary.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/binary.s", .{})).writer();
    const programStr = "int main(){ return (2*3)%5+6; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "testing assembly generation - >= and <=" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/gele.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/gele.s", .{})).writer();
    const programStr = "int main(){ return 2 >= 3 + 1 <= 5; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();

    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "testing assembly generation - short circuiting with logical AND and OR" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/logAndOr.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/logAndOr.s", .{})).writer();
    const programStr = "int main(){ return 2 && ( 3 || 4 ) ; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();

    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "testing assembly generation - declarations" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/declaration.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/declaration.s", .{})).writer();
    const programStr = "int main(){ int x = 2; int y = 3 || 4; return x && y; }";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "tac generation - if" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/if.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/if.s", .{})).writer();
    const programStr =
        \\ int main(){
        \\     int y;
        \\     int x = y = 3;
        \\     if(x == y) return x;
        \\     else return y;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    std.log.warn("first blk statement: {any}\n", .{
        program.externalDecls.items[0].FunctionDecl.blockItems.items[0].Declaration,
    });
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[31mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "tac generation - if nested" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/ifNested.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/ifNested.s", .{})).writer();
    const programStr = "int main(){int y; int x = y = 3;if(x == y) if(x > 3) return x;else; else return y; return 1;}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "assembly generation with ternary" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/ternary.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/ternary.s", .{})).writer();
    const programStr = "int main(){int y = 4; int x = 3; return x == 3?x:y;}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "assembly generation with nested ternary" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/nestedTernary.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/nestedTernary.s", .{})).writer();
    const programStr = "int main(){int y = 4; int x = 3; return x == 3?y == 4?x+y:x-y:0;}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "assembly generation with labelled statements and goto" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/labelledGoto.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/labelledGoto.s", .{})).writer();
    const programStr = "int main(){int y = 4; int x = 3; y = 69;goto sup; supTwo:return x+y;sup:return x-y;}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "testing assembly generation with compound statement parsing" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/compoundStatement.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/compoundStatement.s", .{})).writer();
    const programStr = "int main(){int y = 4; int x = 2; {int x = 3;} return x+y;}";
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "testing assembly generation with do and while loop" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/doWhile.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/doWhile.s", .{})).writer();
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
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "testing assembly generation loop with breaks and continue" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/breakContinue.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/breakContinue.s", .{})).writer();
    const programStr =
        \\ int main(){
        \\     int x = 0;
        \\     while(x < 5){
        \\      if(x == 3) break;
        \\      x = x + 1;
        \\     }
        \\     return x;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    // get the resolved identifier in the while condition
    // if condition and the add in the while loop
    //std.log.warn("while stmt condition: lhs = {any} and rhs = {any}\n", .{
    //    program.externalDecls.items[0].FunctionDecl.blockItems.items[1].Statement.While.condition.Binary.lhs,
    //    program.externalDecls.items[0].FunctionDecl.blockItems.items[1].Statement.While.condition.Binary.rhs,
    //});

    //std.log.warn("if: lhs = {any}, update statement: assigning {any} with lhs={any} and rhs = {any}\n", .{
    //    program.externalDecls.items[0].FunctionDecl.blockItems.items[1].Statement.While.body.Compound.items[0].Statement.If.condition.Binary.lhs,
    //    program.externalDecls.items[0].FunctionDecl.blockItems.items[1].Statement.While.body.Compound.items[1].Statement.Expression.Assignment.lhs,
    //    program.externalDecls.items[0].FunctionDecl.blockItems.items[1].Statement.While.body.Compound.items[1].Statement.Expression.Assignment.rhs.Binary.lhs,
    //    program.externalDecls.items[0].FunctionDecl.blockItems.items[1].Statement.While.body.Compound.items[1].Statement.Expression.Assignment.rhs.Binary.rhs,
    //});
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "nested while and do while loops with continue" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/nestedWhileDoWhile.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/nestedWhileDoWhile.s", .{})).writer();
    const programStr =
        \\ int main() {
        \\   int num = 1;
        \\   int count = 0;
        \\   while (num <= 10) {
        \\     if (num % 2 == 0) {
        \\       num = num + 1;
        \\       count = count + 1;
        \\       continue;
        \\     }
        \\     num = num + 1;
        \\   }
        \\   return count;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "test assembly generation for for loops" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/forLoop.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/forLoop.s", .{})).writer();
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
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}
//
test "multiple functions and call" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/multipleFuncs.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/multipleFuncs.s", .{})).writer();
    const programStr =
        \\ int add(int x, int y, int z) { return x+y+z;}
        \\ int main(){
        \\     return add(2,3,5);
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "global variable codegeneration" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/globalVar.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/globalVar.s", .{})).writer();
    const programStr =
        \\ int four = 4; 
        \\ int main(){
        \\     return four; 
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "global variable codegenaration with multiple funcs" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/globalVarMulFunc.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/globalVarMulFunc.s", .{})).writer();
    const programStr =
        \\ int four = 4; 
        \\ int add(int x, int y){ return x+y; }
        \\ int main(){
        \\     return add(four, 5); 
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "casting program" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/casting.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/casting.s", .{})).writer();
    const programStr =
        \\ int main() {
        \\  long l = 2147483653L;
        \\  int i = 10;
        \\  long result = i + l;
        \\  return (result == 2147483663L);
        \\}
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "casting with div" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/castingDiv.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/castingDiv.s", .{})).writer();
    const programStr =
        \\int main() {
        \\    long l = 2147483653L;
        \\    int i = 10;
        \\    int intResult = l / i;
        \\    return 0;
        \\}
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "truncation" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/truncation.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/truncation.s", .{})).writer();
    const programStr =
        \\int main() {
        \\    long l = 2147483653L;
        \\    int i = l;
        \\    return i;
        \\}
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "big test for casting" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/allCasting.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/allCasting.s", .{})).writer();
    const programStr =
        \\
        \\long a;
        \\long b;
        \\
        \\int addition() {
        \\    return (a + b == 4294967295L);
        \\}
        \\
        \\int subtraction() {
        \\    return (a - b == -4294967380L);
        \\}
        \\
        \\int multiplication() {
        \\    return (a * 4L == 17179869160L);
        \\}
        \\
        \\int division() {
        \\    b = a / 128L;
        \\    return (b == 33554431L);
        \\}
        \\
        \\int remaind() {
        \\    b = -a % 4294967290L;
        \\    return (b == -5L);
        \\}
        \\
        \\int complement() {
        \\    return (~a == -9223372036854775807L);
        \\}
        \\
        \\int main() {
        \\    a = 4294967290L;
        \\    b = 5L;
        \\    if (addition() != 0) {
        \\        return 1;
        \\    }
        \\
        \\    a = -4294967290L;
        \\    b = 90L;
        \\    if (subtraction() != 0) {
        \\        return 2;
        \\    }
        \\
        \\    a = 4294967290L;
        \\    if (multiplication() == 0) {
        \\        return 3;
        \\    }
        \\
        \\    a = 4294967290L;
        \\    if (division() == 0) {
        \\        return 4;
        \\    }
        \\
        \\    a = 8589934585L;
        \\    if (remaind() == 0) {
        \\        return 5;
        \\    }
        \\
        \\    a = 9223372036854775806L;
        \\    if (complement() == 0) {
        \\        return 6;
        \\    }
        \\
        \\    return 0;
        \\}
    ;

    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "basic unsigned numbers" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/unsignedBasic.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/unsignedBasic.s", .{})).writer();
    const programStr =
        \\unsigned int getLargeUnsigned(){
        \\    return 4294967290U;
        \\}
        \\int main() {
        \\    unsigned int notCasted = getLargeUnsigned();
        \\    int casted = getLargeUnsigned();
        \\    if(notCasted == casted){
        \\      return -1;
        \\    }
        \\    return 0;
        \\}
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "test unsigned divide" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/unsignedDiv.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/unsignedDiv.s", .{})).writer();
    const programStr =
        \\unsigned int getLargeUnsigned(){
        \\    return 4294967290U;
        \\}
        \\int main() {
        \\    unsigned int notCasted = getLargeUnsigned();
        \\    int casted = -1;
        \\    if(0 == (notCasted/casted)){
        \\      return -1;
        \\    }
        \\    return 0;
        \\}
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "unsigned compare" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/unsignedCmp.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/unsignedCmp.s", .{})).writer();
    const programStr =
        \\unsigned int getLargeUnsigned(){
        \\    return 4294967290U;
        \\}
        \\int main() {
        \\    unsigned int notCasted = getLargeUnsigned();
        \\    unsigned int notCastedMinusOne = getLargeUnsigned()-1;
        \\    if(notCasted > notCastedMinusOne){
        \\      return 0;
        \\    }
        \\    return -1;
        \\}
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "big test for unsigned" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/bigUnsigned.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/bigUnsigned.s", .{})).writer();
    const programStr =
        \\unsigned int uia;
        \\unsigned int uib;
        \\
        \\unsigned long ula;
        \\unsigned long ulb;
        \\
        \\int addition() {
        \\    return uia + uib == 0U;
        \\}
        \\
        \\int subtraction() {
        \\    return (ula - ulb == 18446744073709551606UL);
        \\}
        \\
        \\int neg() {
        \\    return -ula == 18446744073709551615UL; }
        \\
        \\int main() {
        \\    uia = 4294967293U;
        \\    uib = 3U;
        \\    if (addition() == 0) {
        \\        return 1;
        \\    }
        \\
        \\    ula = 10UL;
        \\    ulb = 20UL;
        \\    if (subtraction() == 0) {
        \\        return 2;
        \\    }
        \\
        \\    ula = 1UL;
        \\    if (neg() == 0) {
        \\        return 3;
        \\    }
        \\
        \\    return 0;
        \\
        \\}
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "float to int" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/floatToInt.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/floatToInt.s", .{})).writer();
    const programStr =
        \\ int main(){
        \\      return 3.2465;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "float arithmetic" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/floatArith.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/floatArith.s", .{})).writer();
    const programStr =
        \\ int main(){
        \\      int k = 3.25+(4.23*7.4);
        \\      return k;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "int to float" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/intToFloat.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/intToFloat.s", .{})).writer();
    const programStr =
        \\ int a(){
        \\  return 3;
        \\ }
        \\ int main(){
        \\   double b = a();
        \\   return (b == 3.00);
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "basic float" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/basicFloat.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/basicFloat.s", .{})).writer();
    const programStr =
        \\ double add(double a, double b){ return a + b;}
        \\ int addInt(int a, int b){ return a+b;}
        \\ int main(){
        \\    int k = add(2.0,3.2);
        \\    int c = add(2,3);
        \\    if(k == c){ return 0; }
        \\    return 0;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "float comparision" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/floatCompare.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/floatCompare.s", .{})).writer();
    const programStr =
        \\ double add(double a, double b){ return a + b;}
        \\ int main(){
        \\    double k = add(2.0,3.2);
        \\    if(k > 5.21){ return 1; }
        \\    return 0;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "float to uint conversion" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/floatToUInt.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/floatToUInt.s", .{})).writer();
    const programStr =
        \\ double add(double a, double b){ return a + b;}
        \\ int main(){
        \\    double k = add(2.0,3.2);
        \\    unsigned int l = k;
        \\    if(l == 5){return 0;}
        \\    return 1;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "calling external library" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/fabs.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/fabs.s", .{})).writer();
    const programStr =
        \\ double sub(double a, double b){ return a - b;}
        \\ double fabs(double x);
        \\ int main(){
        \\    double k = sub(2.0,3.0);
        \\    unsigned int l = fabs(k);
        \\    if(l == 1){return 0;}
        \\    return 1;
        \\ }
    ;
    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "comparision big for floats" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/compBigFloats.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/compBigFloats.s", .{})).writer();
    const programStr =
        \\ double fiftyFive = 55e5;
        \\ double fiftyFour = 54e4;
        \\ double tiny = .00004;
        \\ double four = 4.;
        \\ double pointOne = 0.1;
        \\ 
        \\ int main() {
        \\ 
        \\     if (fiftyFive < fiftyFour) {
        \\         return 1;
        \\     }
        \\ 
        \\     if (four > 4.0) {
        \\         return 2;
        \\     }
        \\ 
        \\     if (tiny <= 0.0) {
        \\         return 3;
        \\     }
        \\ 
        \\     if (fiftyFour >= fiftyFive) {
        \\         return 4;
        \\     }
        \\ 
        \\     if (tiny == 0.0) {
        \\         return 5;
        \\     }
        \\ 
        \\     if (pointOne != pointOne) {
        \\         return 6;
        \\     }
        \\ 
        \\ 
        \\     if ((tiny <= 00.000005))  {
        \\         return 7;
        \\     }
        \\ 
        \\     if ((-.00004 >= four)) {
        \\         return 8;
        \\     }
        \\ 
        \\     if ((tiny > tiny)) {
        \\         return 9;
        \\     }
        \\ 
        \\     if ((fiftyFive < fiftyFive)) {
        \\         return 10;
        \\     }
        \\ 
        \\     if ((0.1 != pointOne)) {
        \\         return 11;
        \\     }
        \\ 
        \\     if ((tiny == .00003)) {
        \\         return 12;
        \\     }
        \\ 
        \\     if (0.00003 < 0.000000000003) {
        \\         return 13;
        \\     }
        \\ 
        \\     return 0;
        \\ 
        \\ }
    ;

    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}

test "big test for float arith" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    const cFileWriter = (try std.fs.cwd().createFile("./cFiles/C/floatArithBig.c", .{})).writer();
    const sFileWriter = (try std.fs.cwd().createFile("./cFiles/S/floatArithBig.s", .{})).writer();
    const programStr =
        \\ double pointOne = 0.1;
        \\ double pointTwo = 0.2;
        \\ double pointThree = 0.3;
        \\ 
        \\ double two = 2.0;
        \\ double three = 3.0;
        \\ double four = 4.0;
        \\ double twelveThirty = 12e30;
        \\ 
        \\ int addition() {
        \\     return (pointOne + pointTwo == 0.30000000000000004);
        \\ }
        \\ 
        \\ int subtraction() {
        \\     return (four - 1.0 == 3.0);
        \\ }
        \\ 
        \\ int multiplication() {
        \\     return (0.01 * pointThree == 0.003);
        \\ }
        \\ 
        \\ int division() {
        \\     return (7.0 / two == 3.5);
        \\ }
        \\ 
        \\ int negation() {
        \\     double neg = -twelveThirty;
        \\     return (12e30 + neg) == 0;
        \\ }
        \\ 
        \\ int compExpr() {
        \\     double compExpr = (two + three) - 127.5 * four;
        \\     return compExpr == -505.0;
        \\ }
        \\ 
        \\ int main() {
        \\ 
        \\     if (addition() == 0) {
        \\         return 1;
        \\     }
        \\ 
        \\     if (subtraction() == 0){
        \\         return 2;
        \\     }
        \\ 
        \\     if (multiplication() == 0) {
        \\         return 3;
        \\     }
        \\ 
        \\     if (division() == 0) {
        \\         return 4;
        \\     }
        \\ 
        \\     if (negation() == 0) {
        \\         return 5;
        \\     }
        \\ 
        \\     if (compExpr() == 0) {
        \\         return 5;
        \\     }
        \\ 
        \\     return 0;
        \\ }
    ;

    const l = try lexer.Lexer.init(allocator, @as([]u8, @constCast(programStr)));
    var p = try parser.Parser.init(allocator, l);
    const program = try p.parseProgram();
    const varResolver = try ast.VarResolver.init(allocator);
    try varResolver.resolve(program);
    const typechecker = try semantic.Typechecker.init(allocator);
    const hasTypeErr = try typechecker.check(program);
    if (hasTypeErr) |typeError| {
        std.log.warn("\x1b[33mError\x1b[0m: {s}\n", .{typeError});
        std.debug.assert(false);
    }
    try ast.loopLabelPass(program, allocator);
    const tacRenderer = try ast.TACRenderer.init(allocator, typechecker.symbolTable);
    const tacProgram = try tacRenderer.render(program);
    const asmRenderer = try tac.AsmRenderer.init(allocator, tacRenderer.asmSymbolTable);
    const asmProgram = try asmRenderer.render(tacProgram);
    try asmProgram.stringify(sFileWriter, allocator, tacRenderer.asmSymbolTable);
    try cFileWriter.writeAll(programStr);
}
