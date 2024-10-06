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

* Example of if(x==y) return x; else return y;
* compare x and y, if true dont jump, if false jump
* jumpIfNotZero to false
* insert end label after false
* jump to end label unconditionally after execution of then statement instructions

## ternary in assembly

* Eventually move TAC to SSA form to aid optimization.
* cmp on the condition,jmpIfNotZero to false label.
* insert end label after false.
* jmp to end label after true unconditionally. 





## TODO:

- [x] Gotos and labelled
- [ ] Compound statements 
- [ ] Loops
- [ ] Functions
- [ ] Storage specifiers
- [ ] long
- [ ] uint
- [ ] floating point
- [ ] pointers
- [ ] arrays + pointer arithmetic
- [ ] chars and strings
- [ ] dynamic memory allocation (can we support garbage collection?)

### Can we support garbage collection?

- A two fold approach
- Using escape analysis to figure out whether the allocation can be on the stack    
- If not use an internal malloc and keep track
- Use mark and sweep

### Notes while investigating a bug

* the flow works till the parser and tac generation.
* Issue somewhere around assembly + fixup passes.
* One issue with the current implementation is that some fixups have been implemented
in the codegeneration itself.
* Should seperate it to the fixed pass and check all the assembly codegen unit tests(TASK 1)


#### Catch with the multiply instruction

- move left to R11 
- imul R11, rhs
- mov R11 to left


### Notes on GNU asm syntax

* Suffixes 
  * based on the size of the operand
  * mostly used l and q (for 32 and 64 bit integers)
* Prefixes
  * All registers prefixed with a `%`.
  * All numbers prefixed with `$`.
* Mov src into dest (Move semantics)

### Moving from LL1 to LL2
- support for labels
- problem in the design of the lexer.
- lex the whole file store in an array, peek ahead as require 
