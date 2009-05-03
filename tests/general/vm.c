// little virtual machine
int acc1;
int acc2;
acc1 = 0;
acc2 = 0;

void dispatch (int instr){
  switch (instr){
  case 0:
    acc1 = 0;
    acc2 = 0;
    break;
  case 1:
    acc1++;
    break;
  case 2:
    acc2++;
    break;
  case 3:
    acc2 = acc1;
    break;
  case 4:
    acc1 = acc1 + acc2;
    break;
  }
}

dispatch(1);
dispatch(1);
dispatch(2);
dispatch(4);
dispatch(3);
