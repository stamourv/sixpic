int SIXPIC_MEMORY_DIVIDE = 6; // equivalent, but with no FSR var
int16 x = SIXPIC_MEMORY_DIVIDE;
x[0] = 3;
x[1] = 4;
byte a = x[0];
a + 2;
