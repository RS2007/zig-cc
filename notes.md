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


