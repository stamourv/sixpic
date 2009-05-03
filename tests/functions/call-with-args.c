/* Test function calls with arguments and while loops */

void f(int x, int n)
{
  while (n > 0)
    {
      x = x+n;
      n = n-1;
    }
}

f(0, 6);
