// this used to be a tricky case. since y was not live before the if, it didn't interfere with x, and was allocated at the same place, which cleared x. this is now solved.
byte f(){
  byte x = 2;
  byte y = 0;
  if (x) y = 4;
  else y = 3;
  return y;
}
f();
