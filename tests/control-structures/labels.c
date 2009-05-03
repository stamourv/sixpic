/* Now with labels */

void f(int8 x, int8 n)
{
  while (n > 0)
    {
    foo:
      x = x+n;
      x = x+1;
    case 2:
      x = x;
    default:
      n = n-1;
    }
}

f(0, 6);
