// test for forward declarations TODO syntax not supported by six

byte f (byte x);

byte y = 0;

byte f (byte x){
  return x + 1;
}

f(y);
