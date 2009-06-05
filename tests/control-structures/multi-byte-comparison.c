// test for < and > on multiple byte values

int16 f(int16 x, int16 y){
  while (x < 300){
    x++;
    y++;
  }
  while (x > 260){
    x--;
    y--;
  }
  return y;
}
f(0, 0);
