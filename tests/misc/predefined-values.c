// test to see how/if predefined values work

X = 3;
int x; x = 17;
int y; y = 23;
int z; z = x + y;
int w; w = y - x + X;
// ok, it seems register allocation allocates around predefined values, good
