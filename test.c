int func(int k) {
  int num = 1;
  int count = 0;
  while (num <= k) {
    if (num % 2 == 0) {
      num = num + 1;
      count = count + 1;
      continue;
    }
    num = num + 1;
  }
  return count;
}
int main() { return func(10); }
