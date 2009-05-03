// to test ints and bytes together

int x = 312;
byte y = 3;
// x = x + y;
y = x + y;
x = x + 4;

x = x - (y + 2); // no borrow
x = x - 4; // borrow
