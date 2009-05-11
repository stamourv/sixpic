// simple, but rather complete, virtual machine

#define pc SIXPIC_FSR1
#define mem SIXPIC_FSR2

byte SIXPIC_MEMORY_DIVIDE = 24;

// program goes from 8 to 11
pc = 8;
mem = 36;

byte loop = 1;

*pc = 1;
*(pc + 1) = 2;
*(pc + 2) = 3;
*(pc + 3) = 0;

while (loop){
  switch(*pc){
  case 0: // halt
    loop = 0;
    break;
  case 1: // store 1 in mem[0]
    *mem = 1;
    break;
  case 2: // store 3 in mem[1]
    *(mem + 1) = 3;
    break;
  case 3: // store the sum of mem[0] and mem[1] in mem[2]
    *(mem + 2) = *mem + *(mem + 1);
    break;
  }
  pc = pc + 1;
 }
*(mem + 2);
