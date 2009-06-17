// test for < and > on multiple byte values

int16 f(int16 x){
  while (x <= 300){
    x++;
  }
  while (x >= 256){
    x--;
  }
  return x;
}
f(0);
