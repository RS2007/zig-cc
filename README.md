- Implementing a compiler for a subset for c.
- Trying to follow the book chapterwise, hence the implementations are not robust

### To test the current implementation

```bash
zig build
./zig-out/bin/zig-cc <c-file-path> # generates assembly to stdout
```
- providing `tacDump` as `argv[2]` for zig-cc will dump the TAC structs into a file.
