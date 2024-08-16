# Codegeneration for expressions

- Take in a allocator, hashmap and the expression struct
- store the intermediaries in the hashmap with its offset   
- total number of locals need to be determined before that

### Count Locals
```c
int main(){
    return 42;
}
```

- Count locals goes to program, goes to function def, goes to return statements, goes to expression, expression is an integer, returns 1.


- GNU asm syntax
```asm
   push %rbp
   mov %rsp,%rbp
   sub $16,%rsp
   movq $42,-16(%rbp)
   movq $0x3c,%rax
   movq -16(%rbp),%rdi
   syscall
   leaveq
   retq
```



## TAC
* converting current AST to three address code
* tree rewriting
* statement gets converted to instructions:
   * emit inner expression instructions first

* Normal AST Representation:

```
Return(Unary(Negate,
	     Unary(Complement,
		  Unary(Negate, Constant(8)))))
```

* TACKY representation(instructions):

```
[
   Unary(Negate,Constant(8),Var("tmp.0")),
   Unary(Complement,Var("tmp.0"),Var("tmp.1")),
   Unary(Negate,Var("tmp.1"),Var("tmp.2")),
   Return(Var("tmp.2"))
]
```

