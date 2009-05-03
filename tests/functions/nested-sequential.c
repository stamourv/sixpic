/* Dummy test, just to see what happens with nested/sequential calls */

int g (int x){
  return x+1;
}

int f (int x){
  int n;
  n = 0;
  while (n < 10){
    n = g(n); // nested calls look ok, but recursion isn't, and we don't care
    n = n + x;
  }
  return n;
}

int x;
x = f(3);
g(6); // sequential calls look ok
x + 3;
