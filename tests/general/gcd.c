// GCD

byte gcd (byte x, byte y){
  byte a = x;
  byte b = y;
  byte t;
  while(1){
    if (b == 0)
      return a;
    while (a >= b)
      a = a - b;
    t = b;
    b = a;
    a = t;
  }
}

gcd(6,2);
