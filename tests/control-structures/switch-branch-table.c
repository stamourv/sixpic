byte f(){
  byte x = 3;
  byte y = 0;
  switch (x){
  case 0:
    y += 1;
    break; // works with and without fall throughs, including in the last case
  case 1:
    y += 2;
    break;
  case 2:
    y += 4;
    break;
  case 3:
    y += 8;
    break;
  }
  return y;
}
f();
