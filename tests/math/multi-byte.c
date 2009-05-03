// to test values larger than 8 bits, to see how they are handled

int z = 250;
int w = 120;
z = z + w;
w = z;
w = w + 300;
z = z + 3;
