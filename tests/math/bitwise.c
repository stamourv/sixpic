// bitwise operations

int16 x1 = #x5335;
int16 x = x1 & #x3553;
byte y1 = 4;
byte y = y1 | 2;
byte z1 = 5;
byte z = z1 ^ 3;

// these should be eliminated
byte w = z | 0;
byte k = y & #xff;

z;
