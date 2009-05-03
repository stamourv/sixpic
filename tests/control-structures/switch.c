/* Switch */

void f(int x, int n)
{
  while (n > 0)
    {
      switch (n)
	{
	case 0 :
	  n = n+1;
	  //	  break;
	case 1 :
	  return;
	default :
	  x = x+n;
	  n = n-1;
	  break;
	}
    }
}

f(0, 6);
