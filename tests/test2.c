/* File: test2.c */

/* Test function calls and while loops */

int x;
int n;

void f()
{
  while (n) // TODO doesn't work with > 0
    {
    foo:
      x = x+n;
      n--;
    bar:
      x--;
    }
}

x = 0;
n = 5;

f();

n = x;
