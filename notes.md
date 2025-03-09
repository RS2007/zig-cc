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
- [x] Functions
  - [x] Typechecker(void lexing + writing a pass)
  - [x] Unit tests for error messages (parser + semantic analysis level)
- [x] Storage specifiers
- [x ] long
- [x ] uint
- [x ] floating point
- [x ] pointers
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

> [!IMPORTANT]
> The todo has been moved to `TODO.md`


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

- handle globals
- global evaluator pass
-

> [!NOTE]
> Qualifier should always proceed the type in this C subset.

- Storage specifier behaviour
- static variable in file scope
- static function in file scope
- static variable in block scope

- At file scope, the static specifier indicates that the
  variable/function has internal linkage.
- At block scope, static specifier indicates the storage
  duraction.
- If a var is declared extern at a point where the exiting
  var already has an internal/external linkage, new declaration
  will have the same linkage as the existing one.

### Errors

- Conflicting declarations

```c
int main(){
    extern int k;
    return k;
}
static int k = 4;
```

- same variable can't have two different linkage types
- no linkage available for extern, hence will be external
- later it becomes internal

```c

```

- no two entities can have the same type (redeclared as a different kind of
  symbol)

```c
int foo = 3;
int foo(int);
int main(){
    return foo(3);
}
```

- initializer for a static variable must be a constant expression
- no specifiers in for loop header or function arguments list

### Symbol table

- Variable resolution level (Stack of hashmaps):
  - Need it to maintain variable names for that scope
- At typechecker level
  - To maintain the types and initial values of symbols
  - Later used to generate TAC:
    - static variables: top level tac symbols
    - tac instructions(function bodies)
- At assembly level:

  - How are types kept in assembly?

- A bug in variable scope resolution
- An Id has to be created for each scope
- function args need to be renamed as well

- In TAC, while generating instructions, we need to know the type of the tac
  symbol
- Hence a TAC symbol table needs to be constructed

### Adding unsigned

#### In parser

- unsigned long as an entirely different type
  - just handle it within the switches for different types (compiler will aid
    refactoring in this case)
- add unsigned as a modifier (have to keep track of this everywhere, lots of
  places can be missed in the code)

#### Type conversions

- In asm, integers are integers, neither signed nor unsigned(cause they are
  represented as 2s complements)
- Addition,subtraction and multiplication? remains the same, no casting required between integer
  and unsigned

  - `CF` and `OF` flags

- division and compare operations change:

  - Idiv vs div:
    - Need for a different div instructions
    -
  - cmp using the `CF` flag instead of the `ZF` flag
    - different set of conditional codes

- writing a pass to replace all longs in the AST with a temp variable,


- To fix:
  - [x] Lexing doubles of the form `.\[0-9]*`
  - [x] Lexing doubles of the form `[0-9]*e[0-9]*`
  - [x] Expressions of this form returning a type error: `(-0.0005 > <var-name>)`
  - [x] UIntToFloat
  - [x] Test case with floats and integer arguments

### Handling pointers 

#### Parser

* Handling precedence
* `int var;`
* `int *var;` => can be written as `int *(var);`
* `int **var;` => can be written as `int *(*(var))`
* Pointer to Function declarators:
  * `int (*foo)(int,int)`
* Arrays:
  * `int arr[3]`
  * `int *arr[3]` => `int * (arr[3]);`
  * The last is different from `int (*arr)[3];`
  * `[]` has higher precedence compared to `*`
*


#### Semantic passes

#### TAC generation


#### Assembly generation


## Declarators messing up the parsing
- Patched the AST Nodes to fix this
- Added helper methods to unbox the function and identifier declarator
- Currently functions and variable declarations are assigned types while
parsing, declarators mess this up and hence all types should be resolved at the
typechecking stage.
- Or the types can be kept as tentative and during the typechecking stage can be
  rewritten to the actual type
* Eventually would have to implement function pointers: and that will make me
want to shoot myself, cause that would involve a good chunk of rewriting parser,
AST and the semanalyzer.

- During typechecking: 
* All functions and variable declaration types are resolved from their
declarators.
* For var declaration:
    * if an expression is found 
    * if the declaration type is different from the expression type, a type
    interchange is discovered:
        * Cast Instruction
        * For global pointer declarations: check if what's inside is null, else
          throw


## TAC for address of:
* These pointers need to be lvalue converted.
* `*k = 4`
* Lets try converting this to TAC:
    * generate lhs and copy rhs to it
        * LOAD tempForDerefK FROM k
        * COPY 4 to tempForDerefK 
    * This is obviously wrong, we are just overriding a stack variable, not the
      actual location pointed by k. 
    * Adding an indirection to generating tacky: 
        * Dereferenced pointer:
            * This is dereferenced, might have to operate on the pointer during
              assignment
        * Plain Value


### Array literal

- accomodating the array syntax within declarator?
- New grammar:

```ebnf

```

