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
 double calculateCompoundInterest(double* initialAmount, 
                                double* monthlyContribution,
                                double* interestRate,
                                int* numyears) {
     int months = *numyears * 12;
     double rate = *interestRate / 1200;
     double balance = *initialAmount;
     
     for (int i = 0; i < months; i = i + 1) {
         balance = balance * (1 + rate);
         balance = balance + *monthlyContribution;
     }
     
     return balance;
 }
 
 int main() {
     double initial = 1000.0;
     double monthly = 100.0;
     double rate = 7.0;
     int years = 10;
     
     return calculateCompoundInterest(&initial, &monthly, &rate, &years);
 }
```

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
