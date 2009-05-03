// test for multibyte comparisons

int16 x = 512 + 12;
byte y = 0;

void f (int16 x){
  if (x == 524) y = y + 1; // works
  if (x > 500) y = y + 2; // works
  if (x < 1000) y = y + 4; // works
  return;
}

f(x);

y;
