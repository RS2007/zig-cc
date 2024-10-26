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
* Can I deviate from the book here?  
    * Have a program with external declaration, either variable declaration or a function declaration/definition
    * Compound statements have BlockStatement 
* Current plan is to follow the book till the CFG construction pass
* Then don't directly register allocate, convert to SSA and start writing the SSA passes



### Idea dump
* High level loop optimizations and inlining.
* TAC -> CFG -> SSA -> Sparse Conditional Constant Propogation ->
    Dead code elimination -> Strength Reduction -> Induction Variable Elimination -> Value numbering
* Garbage collector? (GC_malloc implementation?)


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


### Enhancements

- VarResolutionPass is shady within scopes
- When there is no return in int main, make it return 0.

## Storage specifiers 

- Static, Extern and None.  
- These have different meanings based on where they are used (file scope or block scope)
- In assembly:   


### Modifications to semantic analyzer

* Symbol table redesign
* Typechecker should run before the symbol table resolution
- Typechecker can weed out a lot of trivial issues, so that we can maintain a single table for scope resolutions
- maintaing a scope within a typechecker
- global scope, at every compound copy the hashmap and run it within the children
- currently there are two types: `Int` and `Function`
- TypeCheckerTableEntry should have a `isDefined` for functions to weed out multiple definitions.

- Final goal is to have a single table that encapsulates all the necessary info about the AST.

### General cleanup

* Some functions are very large as of now, shorten them by creating helpers 

### On linkage

* Interal linkage entries never refer to entries in different files.
* Global variables and functions have external linkage.
 
* Just have a pass to check if the function returns have the right types?  
* loss of the typechecker
- The API's require a lot of work to be clean
* The craziest blunder in this whole business was to implement a typechecker right now.  
* Esoteric behaviour will be handled later


* Polymorphic typecheck table for varDecl and fnDecl is fine
* Resolve Expression  

> [!NOTE]
> Not supporting extern for now

* Check types, in your var resolution pass dont rename globals
* In genTAC, the tac global symbols  should be stored in another array
* Convert to assembly globals array
* This should ideally work

> [!NOTE]
> When the functions are getting two many arguments, you probably need a class to store state


### Plan

* Why does assemby have a program abstraction?     
* Assembly is linear: labels, loads, stores, arith etc
* Different sections in assembly can be made into different buffers
* Renderer should have state (too many args to functions)

* ASMRenderer:
    * pass in a function that takes in a tac program and tac global symbols
    * two buffers internally, a data buffer and a text buffer
    * push the glob symbols with inits into the data buffer
    *  
