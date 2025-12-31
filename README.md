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

### 2d arrays

```C
int bsearch2d(int (*arr)[4], int rows, int target) {
    int s = 0;
    int e = rows * 4 - 1;
    while (s <= e) {
        int m = s + (e - s) / 2;
        int r = m / 4;
        int c = m % 4;
        if (arr[r][c] == target) {
            return 1;
        }
        if (arr[r][c] < target) {
            s = m + 1;
        } else {
            e = m - 1;
        }
    }
    return 0;
}
int main() {
    int grid[3][4] = {
        {1, 3, 5, 7},
        {9, 11, 13, 15},
        {17, 19, 21, 23}
    };
    if (bsearch2d(grid, 3, 13) == 0) return 1;
    if (bsearch2d(grid, 3, 14) != 0) return 2;
    return 0;
}
```

### 1d arrays - search

```c
 int binarysearch(int *arr, int arrLen, int target) {
     int s = 0;
     int e = arrLen - 1;

     while (s <= e) {
         int m = s + (e-s)/2;
         if (arr[m] == target) return m;
         if (arr[m] > target) e = m - 1;
         else s = m + 1;
     }
     return -1;
 }
 int main() {
     int arr[4] = {1,2,3,4};
     int rc = binarysearch(arr, 4, 3);
     return rc == 2 ? 0: -1;
 }
```

### 1d arrays - sorting

```c
 int bubblesort(int *arr, int arrLen) {
     for (int i = 0; i < arrLen - 1; i = i + 1) {
         for (int j = 0; j < arrLen - i - 1; j = j + 1) {
             if (arr[j] > arr[j+1]) {
                 int tmp = arr[j];
                 arr[j] = arr[j+1];
                 arr[j+1] = tmp;
             }
         }
     }
     return 0;
 }
 int main() {
     int arr[5] = {5,1,4,2,8};
     bubblesort(arr, 5);
     if (arr[0] != 1) return -1;
     if (arr[1] != 2) return -1;
     if (arr[2] != 4) return -1;
     if (arr[3] != 5) return -1;
     if (arr[4] != 8) return -1;
     return 0;
 }
```

### pointers

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

### static variables mutability across function calls

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

### functions 

```c
int four = 4;
int add(int x, int y){ return x+y; }
int main(){
    return add(four, 5);
}
```
