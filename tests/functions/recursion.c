/* Dummy test, just to see what happens with recursion (recursion fails) */

int rec (int x){
  if (x > 0){
    rec(x-1);
    x++;
    x++;
    return x;
  }
}

rec(4);
