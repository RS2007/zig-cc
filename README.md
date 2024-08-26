- Implementing a compiler for a subset for c.
- Trying to follow the book chapterwise, hence the implementations are not robust

### To test the current implementation

```bash
zig build
./zig-out/bin/zig-cc test.c # This generates a temp.s asm file
zig cc -o exec temp.s
./exec
```
