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
        \\  long l = 2147483653;
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
        \\    long l = 2147483653;
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
        \\    long l = 2147483653;
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
