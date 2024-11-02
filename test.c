int binary = 0;
int constant = 1;
typedef struct {
  int valKind;
} valHeader;

typedef struct {
  valHeader header;
  valHeader *a;
  valHeader *b;
} bin;

typedef struct {
  valHeader header;
  int k;
} atom;

int eval(valHeader *s) {
  switch (s->valKind) {
  case 0: {
    int a = eval(((bin *)s)->a);
    int b = eval(((bin *)s)->b);
    return a + b;
  }
  case 1: {
    return ((atom *)s)->k;
  }
  }
  return 0;
}

int main() {
  valHeader header1 = {0};
  valHeader header2 = {1};
  atom op2 = {header2, 3};
  atom op3 = {header2, 5};
  bin op1 = {header1, &(op2.header), &(op3.header)};
  return eval(&(op1.header));
}
