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
* jump to false label unconditionally after execution of then statement instructions



## TODO:

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
