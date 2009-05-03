// iterative fibonacci

int fib (byte i){
  int a = 0;
  int b = 1;
  int t;
  while(i){
    t = b;
    b = b + a;
    a = t;
    i--;
  }
  return a;
}

fib(6);
