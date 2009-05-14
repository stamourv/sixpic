void g(int x){
 baz:
  x+3;
 qux:
  0;
}

void f(int x, int n)
{
  return 0; // or else, would do an infinite loop, bad for automated testing
 foo:
  x = x + 3;
  goto baz;
 bar:
  n = n + 2;
  goto foo;
 baz:
  x = x + 1;
  goto bar;
}

f(0, 6);
