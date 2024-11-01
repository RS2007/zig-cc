![Unit Tests](https://github.com/RS2007/zig-cc/actions/workflows/test.yml/badge.svg)

- Implementing a compiler for a subset for c.
- Trying to follow the book chapterwise, hence the implementations are not robust

### To test the current implementation

```bash
zig build
./zig-out/bin/zig-cc <c-file-path> # generates assembly to stdout
```
- providing `tacDump` as `argv[2]` for zig-cc will dump the TAC structs into a file.

## Current Progress:
- Can compile these programs:
```c
int recurse(int n){
        static int accum = 0;
        static int k = 0;
        if(k < n){
                k = k +1;
                accum = accum + k;
                recurse(n);
        }
        return accum;
}

int main(){
    return recurse(10);
}
```
```c
int four = 4; 
int add(int x, int y){ return x+y; }
int main(){
    return add(four, 5); 
}
```
