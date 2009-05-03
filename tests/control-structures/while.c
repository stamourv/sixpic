/* Test function calls and while loops */

int x;
int n;

void f()
{
  while (n > 0)
    {
      x = x+n;
      n--;
      x--;
    }
}

x = 0;
n = 5;

f();

n = x;
