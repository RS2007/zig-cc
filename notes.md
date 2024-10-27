## Parser

## TAC

- converting current AST to three address code
- tree rewriting
- statement gets converted to instructions:

  - emit inner expression instructions first

- Normal AST Representation:

```
Return(Unary(Negate,
	     Unary(Complement,
		  Unary(Negate, Constant(8)))))
```

- TACKY representation(instructions):

```
[
   Unary(Negate,Constant(8),Var("tmp.0")),
   Unary(Complement,Var("tmp.0"),Var("tmp.1")),
   Unary(Negate,Var("tmp.1"),Var("tmp.2")),
   Return(Var("tmp.2"))
]
```

### Handling short circuiting operations

- Logic is something along the lines of `if (firstOp == shortValue)` then don't evaluate the next and move firstOp to dest

  - else evaluate the secondOp, compute the operation and store into register

- Move the firstOp to dest, cmp and jump. if no jump, compute and secondOp,dest.

- Handle this in TAC. Don't complicate assembly generation.

## If conditions in assembly

- Example of if(x==y) return x; else return y;
- compare x and y, if true dont jump, if false jump
- jumpIfNotZero to false
- insert end label after false
- jump to end label unconditionally after execution of then statement instructions

## ternary in assembly

- Eventually move TAC to SSA form to aid optimization.
- cmp on the condition,jmpIfNotZero to false label.
- insert end label after false.
- jmp to end label after true unconditionally.

## TODO:

- [x] Gotos and labelled
- [x] Compound statements
- [x] Loops
- [ ] Functions
  - [ ] Typechecker(void lexing + writing a pass)
  - [ ] Unit tests for error messages (parser + semantic analysis level)
- [ ] Storage specifiers
- [ ] long
- [ ] uint
- [ ] floating point
- [ ] pointers
- [ ] arrays + pointer arithmetic
- [ ] chars and strings
- [ ] dynamic memory allocation (can we support garbage collection?)
- [ ] TAC to CFG
- [ ] CFG to SSA
- [ ] Dead code Elimination
- [ ] Conditional Constant Propogation
- [ ] Strength Reduction and Global Value Numbering
- [ ] Peephole optimizations
- [ ] Register Allocation
- [ ] Support for function inlining (call graph construction)

### Can we support garbage collection?

- A two fold approach
- Using escape analysis to figure out whether the allocation can be on the stack
- If not use an internal malloc and keep track
- Use mark and sweep

### Notes while investigating a bug

- the flow works till the parser and tac generation.
- Issue somewhere around assembly + fixup passes.
- One issue with the current implementation is that some fixups have been implemented
  in the codegeneration itself.
- Should seperate it to the fixed pass and check all the assembly codegen unit tests(TASK 1)

#### Catch with the multiply instruction

- move left to R11
- imul R11, rhs
- mov R11 to left

### Notes on GNU asm syntax

- Suffixes
  - based on the size of the operand
  - mostly used l and q (for 32 and 64 bit integers)
- Prefixes
  - All registers prefixed with a `%`.
  - All numbers prefixed with `$`.
- Mov src into dest (Move semantics)

### Moving from LL1 to LL2

- support for labels
- problem in the design of the lexer.
- lex the whole file store in an array, peek ahead as require

## How do do while loops and while loops look in assembly?

### Do while

- generate a label doWhileStart
- generate body
- check condition
- jump to doWhileStart label if condition is not zero

### While

-
-

> [!NOTE]
> Loop inversion can be done here. This clearly shows why the do while loop is
> better.

- Should we make this the first optimization?

### For loops in assembly

- initialization rendered
- forStart label
- condition check, if zero jump to forLoopEnd
- body start
- update
- unconditional jump to forStart
- label forLoopEnd

## Support for functions in the parser

- Can I deviate from the book here?
  - Have a program with external declaration, either variable declaration or a function declaration/definition
  - Compound statements have BlockStatement
- Current plan is to follow the book till the CFG construction pass
- Then don't directly register allocate, convert to SSA and start writing the SSA passes

### Idea dump

- High level loop optimizations and inlining.
- TAC -> CFG -> SSA -> Sparse Conditional Constant Propogation ->
  Dead code elimination -> Strength Reduction -> Induction Variable Elimination -> Value numbering
- Garbage collector? (GC_malloc implementation?)

> [!NOTE]
> Introducing function abstraction for TAC and Assembly

- genTAC implementation in all the tests should be breaking now

## Function call in assembly

- Handling Function:
  - pushing rbp
  - moving rbp to rsp
  - stack sub for local variables
  - move arguments to the specific registers
  - How do arguments get resolved?
  - assign args place in the stack during the resolve pass
  - leaveq and retq in the end
- Handling FunctionCall
  - move arguments to the specific registers
  - call to the function

## Typechecking

- scope as a linked list of hashmaps
- lookup function that traverses it
- Probably make something like a result type.
- the expression typeerrors are not being printed properly
- Later typechecker design:
    - redesign parser to not use strings at all
    - instead use tokens to give line number errors
    - print the line and put a ^ at the error
    - ansii colors to make it look pretty?

## Global compile time evaluator pass

- All globals can be evaluated at compile time
- All global var declarations
- writing a pass that visits all global varDecls
- compileTimExprEval()
- assert that the expression if exists is assignment
- throw error for function calls (non constant)
- throw error for identifiers that are not part of global symbol table 
- extract from typechecker? All vars with external linkage can be used


## Global variable stuff

* handle globals    
* global evaluator pass
* 
