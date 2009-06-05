// shift by literal multiples of 8
int16 x = 15;
int16 y = x << 8;
int32 z = y + 2;
z = z << 16;
z = z + 259;
z;
