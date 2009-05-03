// test for simple dereference, without FSR variables

byte a = 5;
*a = 3;
*(a+1) = 4;
a = a + 2;
*a = 5;
a;
