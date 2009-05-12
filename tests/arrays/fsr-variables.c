// test for special variables in FSR0, with simple dereference syntax
SIXPIC_FSR0 = 4;
*SIXPIC_FSR0 = 3;
SIXPIC_FSR0 = SIXPIC_FSR0 + 1;
*SIXPIC_FSR0 = 4;
SIXPIC_FSR0 = SIXPIC_FSR0 - 1;
byte a = *SIXPIC_FSR0;
a + 2;
