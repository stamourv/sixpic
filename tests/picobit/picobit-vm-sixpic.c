// TODO at least, use #include and #define, but #define would require me to start all over...
// no typedefs
// no forward declarations
// Scheme-style hex literals
// no explicit casts (intermediate variables instead)
// removed functions that interact with the firmware, and with rom
// had some case labels that were not literals (but could have been constant folded) in the dispatch

/* typedef char int8; */
/* typedef short int16; */
/* typedef long int32; */
/* typedef unsigned char uint8; */
/* typedef unsigned short uint16; */
/* typedef unsigned long uint32; */

/* typedef uint8 word; */

/* typedef uint16 ram_addr; */
/* typedef uint16 rom_addr; */

int16 arg1;
int16 arg2;
int16 arg3;
int16 arg4;
int16 arg5;
int16 cont;
int16 env;

int8 na;
int16 pc;
int8 glovars;
int16 entry;
int8 bytecode;
int8 bytecode_hi4;
int8 bytecode_lo4;
int16 a1;
int16 a2;
int16 a3;

void halt_with_error (){
  uart_write(101); // e
  uart_write(114); // r
  uart_write(114); // r
  uart_write(13);
  uart_write(10);
  exit();
}


/* typedef int16 obj; */
/* int8 ram_get_gc_tags (int16 o); */
/* int8 ram_get_gc_tag0 (int16 o); */
/* int8 ram_get_gc_tag1 (int16 o); */
/* void ram_set_gc_tags (int16 o, int8 tags); */
/* void ram_set_gc_tag0 (int16 o, int8 tag); */
/* void ram_set_gc_tag1 (int16 o, int8 tag); */
/* int8 ram_get_field0 (int16 o); */
/* int8 ram_get_field1 (int16 o); */
/* int8 ram_get_field2 (int16 o); */
/* int8 ram_get_field3 (int16 o); */
/* int8 ram_get_fieldn (int16 o, int8 n); */
/* void ram_set_field0 (int16 o, int8 val); */
/* void ram_set_field1 (int16 o, int8 val); */
/* void ram_set_field2 (int16 o, int8 val); */
/* void ram_set_field3 (int16 o, int8 val); */
/* void ram_set_fieldn (int16 o, int8 n, int8 val); */
/* int8 rom_get_field0 (int16 o); */
/* int8 rom_get_field1 (int16 o); */
/* int8 rom_get_field2 (int16 o); */
/* int8 rom_get_field3 (int16 o); */
int8 ram_get_gc_tags (int16 o) {
  int16 t2 = o - 512;
  return (*(((t2 << 2))+#x200) & #x60); // TODO not sure these shifts really save on code space, maybe multiplications (which would be 2 additions) would be better
}
int8 ram_get_gc_tag0 (int16 o) {
  int16 t2 = o - 512;
  return (*(((t2 << 2))+#x200) & #x20);
}
int8 ram_get_gc_tag1 (int16 o) {
  int16 t2 = o - 512;
  return (*(((t2 << 2))+#x200) & #x40);
}
void ram_set_gc_tags (int16 o, int8 tags) {
  int16 t2 = (o - 512) << 2; // TODO optimized a couple of things
  (*((t2)+#x200) = ((*((t2)+#x200) & #x9f) | (tags))); // TODO if we could use bst and bcf, would be better
}
void ram_set_gc_tag0 (int16 o, int8 tag) {
  int16 t2 = (o - 512) << 2; // TODO same here
  *(t2+#x200) = ((*(t2+#x200) & #xdf) | (tag));
}
void ram_set_gc_tag1 (int16 o, int8 tag) {
  int16 t2 = (o - 512) << 2; // TODO same here
  *(t2+#x200) = ((*(t2+#x200) & #xbf) | (tag));
}
int8 ram_get_field0 (int16 o) { int16 t2 = o - 512; return *(((t2 << 2) + (0))+#x200); }
int8 ram_get_field1 (int16 o) { int16 t2 = o - 512; return *(((t2 << 2) + (1))+#x200); }
int8 ram_get_field2 (int16 o) { int16 t2 = o - 512; return *(((t2 << 2) + (2))+#x200); }
int8 ram_get_field3 (int16 o) { int16 t2 = o - 512; return *(((t2 << 2) + (3))+#x200); }
int8 ram_get_fieldn (int16 o, int8 n) {
  switch (n) {
  case 0: return ram_get_field0 (o);
  case 1: return ram_get_field1 (o);
  case 2: return ram_get_field2 (o);
  case 3: return ram_get_field3 (o);
  }
}
void ram_set_field0 (int16 o, int8 val) { int16 t2 = o - 512; *(((t2 << 2) + (0))+#x200) = (val); }
void ram_set_field1 (int16 o, int8 val) { int16 t2 = o - 512; *(((t2 << 2) + (1))+#x200) = (val); }
void ram_set_field2 (int16 o, int8 val) { int16 t2 = o - 512; *(((t2 << 2) + (2))+#x200) = (val); }
void ram_set_field3 (int16 o, int8 val) { int16 t2 = o - 512; *(((t2 << 2) + (3))+#x200) = (val); }
void ram_set_fieldn (int16 o, int8 n, int8 val) {
  switch (n) {
  case 0: ram_set_field0 (o, val); break;
  case 1: ram_set_field1 (o, val); break;
  case 2: ram_set_field2 (o, val); break;
  case 3: ram_set_field3 (o, val); break;
  }
}
int8 rom_get_field0 (int16 o) { int16 t2 = (o) - (3 +255 - -1 +1); return rom_get (((t2 << 2) + (#x8000 + 4 + (0)))); }
int8 rom_get_field1 (int16 o) { int16 t2 = (o) - (3 +255 - -1 +1); return rom_get (((t2 << 2) + (#x8000 + 4 + (1)))); }
int8 rom_get_field2 (int16 o) { int16 t2 = (o) - (3 +255 - -1 +1); return rom_get (((t2 << 2) + (#x8000 + 4 + (2)))); }
int8 rom_get_field3 (int16 o) { int16 t2 = (o) - (3 +255 - -1 +1); return rom_get (((t2 << 2) + (#x8000 + 4 + (3)))); }


/* int16 ram_get_car (int16 o); */
/* int16 rom_get_car (int16 o); */
/* int16 ram_get_cdr (int16 o); */
/* int16 rom_get_cdr (int16 o); */
/* void ram_set_car (int16 o, int16 val); */
/* void ram_set_cdr (int16 o, int16 val); */

/* int16 ram_get_entry (int16 o); */
/* int16 rom_get_entry (int16 o); */

int16 ram_get_car (int16 o)
{ int16 tmp = (ram_get_field0 (o) & #x1f); return (tmp << 8) | ram_get_field1 (o); }
int16 rom_get_car (int16 o)
{ int16 tmp = (rom_get_field0 (o) & #x1f); return (tmp << 8) | rom_get_field1 (o); }
int16 ram_get_cdr (int16 o)
{ int16 tmp = (ram_get_field2 (o) & #x1f); return (tmp << 8) | ram_get_field3 (o); }
int16 rom_get_cdr (int16 o)
{ int16 tmp = (rom_get_field2 (o) & #x1f); return (tmp << 8) | rom_get_field3 (o); }

void ram_set_car (int16 o, int16 val) {
  ram_set_field0 (o, (val >> 8) | (ram_get_field0 (o) & #xe0));
  ram_set_field1 (o, val & #xff);
}
void ram_set_cdr (int16 o, int16 val) {
  ram_set_field2 (o, (val >> 8) | (ram_get_field2 (o) & #xe0));
  ram_set_field3 (o, val & #xff);
}


int16 ram_get_entry (int16 o) {
  int16 tmp  = (ram_get_field0 (o) & #x1f);
  int16 tmp2 = ram_get_field1 (o); // TODO this cast is necessary because literals are of the smallest type possible in SIXPIC, not always int (not standard)
  return ((tmp << 11)
	  | (tmp2 << 3)
	  | (ram_get_field2 (o) >> 5));
}
int16 rom_get_entry (int16 o){
  int16 tmp = (rom_get_field0 (o) & #x1f);
  return ((tmp << 11)
   | (rom_get_field1 (o) << 3)
   | (rom_get_field2 (o) >> 5));
}


/* int16 get_global (int8 i); */
/* void set_global (int8 i, int16 o); */

int16 get_global (int8 i) {

  if (i & 1)
    return ram_get_cdr (512 + (i >> 1));
  else
    return ram_get_car (512 + (i >> 1));
}
void set_global (int8 i, int16 o) {
  if (i & 1)
    ram_set_cdr (512 + (i >> 1), o);
  else
    ram_set_car (512 + (i >> 1), o);
}

int16 free_list;
int16 free_list_vec;

void mark (int16 temp) {


  int16 stack;
  int16 visit;

  if ((!((temp) >= 1280) && ((temp) >= 512))) {
    visit = 0;

  push:

    stack = visit;
    visit = temp;

    ;

    if (((((ram_get_field0 (visit) & #x80) == #x80) || ((ram_get_field0 (visit) & #xc0) == #x40) || ((ram_get_field0 (visit) & #xc0) == 0)) && ram_get_gc_tag0 (visit))
 || (((((ram_get_field0 (visit) & #x80) == #x80) && ((ram_get_field2 (visit) & #xe0) == 0)) || (((ram_get_field0 (visit) & #x80) == #x80) && ((ram_get_field2 (visit) & #xe0) == #x80)))
     && (ram_get_gc_tags (visit) != (0<<5))))
      ;
    else {
      if (((((ram_get_field0 (visit) & #x80) == #x80) && ((ram_get_field2 (visit) & #xe0) == 0)) || (((ram_get_field0 (visit) & #x80) == #x80) && ((ram_get_field2 (visit) & #xe0) == #x80)))) {
 ;

      visit_field2:

 temp = ram_get_cdr (visit);

 if ((!((temp) >= 1280) && ((temp) >= 512))) {
   ;
   int16 tmp = 2;
   ram_set_gc_tags (visit, (tmp<<5));
   ram_set_cdr (visit, stack);
   goto push;
 }

 ;

 goto visit_field1;
      }

      if ((((ram_get_field0 (visit) & #x80) == #x80) || ((ram_get_field0 (visit) & #xc0) == #x40) || ((ram_get_field0 (visit) & #xc0) == 0))) {
 ;

      visit_field1:


 if (((ram_get_field0 (visit) & #xc0) == #x40))
   temp = ram_get_cdr (visit);
 else
   temp = ram_get_car (visit);

 if ((!((temp) >= 1280) && ((temp) >= 512))) {
   ;
   int16 tmp = 1;
   ram_set_gc_tag0 (visit, (tmp<<5));
   if (((ram_get_field0 (visit) & #xc0) == #x40))
     ram_set_cdr (visit, stack);
   else
     ram_set_car (visit, stack);

   goto push;
 }

 ;
      }
      else
 ;
      int16 tmp = 1;
      ram_set_gc_tag0 (visit, (tmp<<5));
    }

  pop:

    ;

    if (stack != 0) {
      if (((((ram_get_field0 (stack) & #x80) == #x80) && ((ram_get_field2 (stack) & #xe0) == 0)) || (((ram_get_field0 (stack) & #x80) == #x80) && ((ram_get_field2 (stack) & #xe0) == #x80))) && ram_get_gc_tag1 (stack)) {
 ;

 temp = ram_get_cdr (stack);
 ram_set_cdr (stack, visit);
 visit = stack;
 stack = temp;

 ram_set_gc_tag1(visit, (0<<5));


 goto visit_field1;
      }

      if (((ram_get_field0 (stack) & #xc0) == #x40)) {

 ;

 temp = ram_get_cdr (stack);
 ram_set_cdr (stack, visit);
 visit = stack;
 stack = temp;

 goto pop;
      }

      ;

      temp = ram_get_car (stack);
      ram_set_car (stack, visit);
      visit = stack;
      stack = temp;

      goto pop;
    }
  }
}





void sweep () {






  int16 visit = 1279;

  free_list = 0;

  while (visit >= (512 + ((glovars + 1) >> 1))) {

    int16 tmp = 1;
    if ((((ram_get_field0 (visit) & #x80) == #x80)
  && (ram_get_gc_tags (visit) == (0<<5)))
 || !(ram_get_gc_tags (visit) & (tmp<<5))) {

      if ((((ram_get_field0 (visit) & #x80) == #x80) && ((ram_get_field2 (visit) & #xe0) == #x60))) {

 int16 o = ram_get_cdr (visit);
 int16 i = ram_get_car (visit);
 ram_set_car (o, free_list_vec);
 ram_set_cdr (o, (i + 3) >> 2);
 free_list_vec = o;

      }
      ram_set_car (visit, free_list);
      free_list = visit;
      }
    else {
      if (((ram_get_field0 (visit) & #x80) == #x80))
 ram_set_gc_tags (visit, (0<<5));
      else
 ram_set_gc_tag0 (visit, (0<<5));



    }
    visit--;
  }
}

void gc () {

  /*   uart_write(10); */ // TODO for debugging
/*   uart_write(13); */
/*   uart_write(103); // g */
/*   uart_write(99);  // c */
/*   uart_write(10); */
/*   uart_write(13); */
  
  int8 i;

  ;

  ;
  mark (arg1);
  ;
  mark (arg2);
  ;
  mark (arg3);
  ;
  mark (arg4);
  ;
  mark (arg5);
  ;
  mark (cont);
  ;
  mark (env);

  ;
  for (i=0; i<glovars; i++)
    mark (get_global (i));

  sweep ();
}


int16 alloc_ram_cell () {
  int16 o;





  if (free_list == 0) {

    gc ();
    if (free_list == 0)

      halt_with_error();
  }

  o = free_list;

  free_list = ram_get_car (o);
  /*   uart_write(65); */ // TODO for debugging
/*   uart_write(108); */
/*   uart_write(108); */
/*   uart_write((free_list>>12)+65); */
/*   uart_write(((free_list>>8)&#xf)+65); */
/*   uart_write(((free_list>>4)&#xf)+65); */
/*   uart_write((free_list & #xf) + 65); */
/*   uart_write(10); */
/*   uart_write(13); */
  

  return o;
}

int16 alloc_ram_cell_init (int8 f0, int8 f1, int8 f2, int8 f3) {
  int16 o = alloc_ram_cell ();

  ram_set_field0 (o, f0);
  ram_set_field1 (o, f1);
  ram_set_field2 (o, f2);
  ram_set_field3 (o, f3);

  return o;
}

int16 alloc_vec_cell (int16 n) {
  int16 o = free_list_vec;
  int16 prec = 0;
  int8 gc_done = 0;






  while ((ram_get_cdr (o) * 4) < n) {
    if (o == 0) {
      if (gc_done)
 halt_with_error();

      gc ();
      gc_done = 1;

      o = free_list_vec;
      prec = 0;
      continue;
    }
    prec = o;
    o = ram_get_car (o);
  }



  if (((ram_get_cdr(o) * 4) - n) < 4) {
    if (prec)
      ram_set_car (prec, ram_get_car (o));
    else
      free_list_vec = ram_get_car (o);
  }


  else {
    int16 new_free = o + ((n + 3) >> 2);
    if (prec)
      ram_set_car (prec, new_free);
    else
      free_list_vec = new_free;
    ram_set_car (new_free, ram_get_car (o));
    ram_set_cdr (new_free, ram_get_cdr (o) - ((n + 3) >> 2));
  }

  return o;
}



/* typedef int16 integer; */
/* typedef int16 digit; */
/* typedef int32 two_digit; */
/* int16 make_integer (int16 lo, int16 hi); */
/* int16 integer_hi (int16 x); */
/* int16 integer_lo (int16 x); */
int16 make_integer (int16 lo_make_integer, int16 hi_make_integer) { // TODO changed name
  return alloc_ram_cell_init (0 | (hi_make_integer >> 8), hi_make_integer, lo_make_integer >> 8, lo_make_integer);
}

int16 integer_hi (int16 x) {
  if ((!((x) >= 1280) // bb 0
       && ((x) >= 512))) // bb 4
    return ram_get_car (x); // bb 2
  else if ((!((x) >= 1280) // bb 3
	    && !(!((x) >= 1280) // bb 10
		 && ((x) >= 512)) // bb 11
	    && ((x) >= (3 +255 - -1 +1)))) // bb 9
    return rom_get_car (x); // bb 7
  else if (x < (3 - -1)){ // bb 8
    return ((0 + (3 - -1))-1); // bb 14
  }
  else {
    return (0 + (3 - -1)); // bb 15
  }
}

int16 integer_lo (int16 x) {
  if ((!((x) >= 1280) && ((x) >= 512))) {
    int16 t = ram_get_field2 (x);
    return (t << 8) + ram_get_field3 (x);
  }
  else if ((!((x) >= 1280) && !(!((x) >= 1280) && ((x) >= 512)) && ((x) >= (3 +255 - -1 +1)))) {
    int16 t = rom_get_field2 (x);
    return (t << 8) + rom_get_field3 (x);
  }
  else
    return x - (3 - -1);
}


/* int16 norm (int16 prefix, int16 n); */
/* int8 negp (int16 x); */
/* int8 cmp (int16 x, int16 y); */
/* int16 integer_length (int16 x); */
/* int16 shr (int16 x); */
/* int16 negative_carry (int16 carry); */
/* int16 shl (int16 x); */
/* int16 shift_left (int16 x, int16 n); */
/* int16 add (int16 x, int16 y); */
/* int16 invert (int16 x); */
/* int16 sub (int16 x, int16 y); */
/* int16 neg (int16 x); */
/* int16 scale (int16 n, int16 x); */
/* int16 mulnonneg (int16 x, int16 y); */
/* int16 divnonneg (int16 x, int16 y); */

int16 decode_int (int16 o) {
  int8 result;
  if (o < 3)
    halt_with_error();

  if (o <= (3 + (255 - -1)))
    return (o - (3 - -1));

  if ((!((o) >= 1280) && ((o) >= 512))) {
    if (!((ram_get_field0 (o) & #xc0) == 0))
      halt_with_error();
    return ram_get_field3 (o);
  }
  else if ((!((o) >= 1280) && !(!((o) >= 1280) && ((o) >= 512)) && ((o) >= (3 +255 - -1 +1)))) {
    if (!((rom_get_field0 (o) & #xc0) == 0))
      halt_with_error();
    return rom_get_field3 (o);
  }
  else
    halt_with_error();
}

/* int16 decode_int (int16 o); */
/* int16 encode_int (int16 n); */

int16 norm (int16 prefix, int16 n_norm) { // TODO arg changed



  while (prefix != 0) { // bbs 1 and 4
    int16 d = integer_lo (prefix); // bb 3
    int16 temp = prefix;

    prefix = integer_hi (temp);

    if (((n_norm) == ((0 + (3 - -1))))) { // bb 3 and 8
      if (d <= 255) { // bb 6
	n_norm = (d + (3 - -1)); // bb 10
	continue;
      }
    }
    else if (((n_norm) == (((0 + (3 - -1))-1)))) { // bbs 7 and 13
      int16 tmp = 1; // bb 12
      if (d >= (tmp<<16) - 1) { // bb 12 // TODO was + MIN_FIXNUM, but -1 is not a valid literal
	int16 t = d - (tmp << 16); // bb 15
	n_norm = (t + (3 - -1));
	continue;
      }
    }

    ram_set_car (temp, n_norm); // bb 5
    n_norm = temp;
  }

  return n_norm; // bb 2
}

int8 negp (int16 x_negp) {


  do {
    x_negp = integer_hi (x_negp); // bb 1
    if (((x_negp) == ((0 + (3 - -1))))) // bbs 1 and 6
      return 0; // bb 5
  } while (!((x_negp) == (((0 + (3 - -1))-1)))); // bbs 2 and 8

  return 1; // bb 3
}

int8 cmp (int16 x_cmp, int16 y_cmp) { // TODO changed. used to return -1, 0 and 1, now is 0, 1, 2


  int8 result = 1;
  int16 xlo;
  int16 ylo;

  for (;;) { // bb 2
    if (((x_cmp) == ((0 + (3 - -1)))) // bbs 2 and 8
	|| ((x_cmp) == (((0 + (3 - -1))-1)))) { // bbs 7 and 9
      if (!((x_cmp) == (y_cmp))) // bbs 6 and 12
	{ if (negp (y_cmp)) // bb 11
	    result = 2; // bb 14
	  else result = 0; } // bb 15
      break; // bb 10
    }

    if (((y_cmp) == ((0 + (3 - -1)))) // bbs 5 and 19
	|| ((y_cmp) == (((0 + (3 - -1))-1)))) { // bbs 18 and 20
      if (negp (x_cmp)) // bb 17
	result = 0; // bb 22
      else result = 2; // bb 23
      break; // bb 21
    }
    
    xlo = integer_lo (x_cmp); // bb 16
    ylo = integer_lo (y_cmp);
    x_cmp = integer_hi (x_cmp);
    y_cmp = integer_hi (y_cmp);
    if (xlo != ylo) // bb 16 and 26
      { if (xlo < ylo) // bb 25
	  result = 0; // bb 28
	else result = 2; } // bb 29
  }
  return result; // bb 4
}

int16 integer_length (int16 x) {



  int16 result = 0;
  int16 next;
  int16 d;

  while (!(((next = integer_hi (x))) == ((0 + (3 - -1))))) {
    result += 16;
    x = next;
  }

  d = integer_lo (x);

  while (d > 0) {
    result++;
    d >>= 1;
  }

  return result;
}

int16 shr (int16 x) {


  int16 result = 0;
  int16 d;

  for (;;) {
    if (((x) == ((0 + (3 - -1)))) || ((x) == (((0 + (3 - -1))-1)))) {
      result = norm (result, x);
      break;
    }

    d = integer_lo (x);
    x = integer_hi (x);
    int16 tmp = 1;
    result = make_integer ((d >> 1) |
			   ((integer_lo (x) & 1) ? (tmp<<(16-1)) : 0),
			   result);
  }

  return result;
}

int16 negative_carry (int16 carry) {
  if (carry)
    return ((0 + (3 - -1))-1);
  else
    return (0 + (3 - -1));
}

int16 shl (int16 x) {


  int16 negc = (0 + (3 - -1));
  int16 temp;
  int16 result = 0;
  int16 d;

  for (;;) {
    if (((x) == (negc))) {
      result = norm (result, x);
      break;
    }

    d = integer_lo (x);
    x = integer_hi (x);
    temp = negc;
    int16 tmp = 1;
    negc = negative_carry (d & (tmp<<15));
    result = make_integer ((d << 1) | ((temp) == ((0 + (3 - -1))-1)), result);
  }

  return result;
}

int16 shift_left (int16 x, int16 n) {


  if (((x) == ((0 + (3 - -1)))))
    return x;

  while (n & (16 -1)) {
    x = shl (x);
    n--;
  }

  while (n > 0) {
    x = make_integer (0, x);
    n -= 16;
  }

  return x;
}

int16 add (int16 x, int16 y) {


  int16 negc = (0 + (3 - -1));
  int16 result = 0;
  int16 dx;
  int16 dy;

  for (;;) { // bb 2
    if (((x) == (negc))) { // bbs 2 and 7
      result = norm (result, y); // bb 6
      break;
    }

    if (((y) == (negc))) { // bbs 5 and 10
      result = norm (result, x); // bb 9
      break;
    }

    dx = integer_lo (x); // bb 8
    dy = integer_lo (y);
    dx = dx + dy;

    if (((negc) == ((0 + (3 - -1))))) // bbs 8 and 14
      negc = negative_carry (dx < dy); // bbs 12, 15, 16, 17
    else { // bb 13
      dx++;
      negc = negative_carry (dx <= dy); // bbs 13, 18, 19, 20
    }

    x = integer_hi (x); // bb 11
    y = integer_hi (y);

    result = make_integer (dx, result);
  }

  return result; // bb 4
}

int16 invert (int16 x) {
  if (((x) == ((0 + (3 - -1)))))
    return ((0 + (3 - -1))-1);
  else
    return (0 + (3 - -1));
}

int16 sub (int16 x, int16 y) {

  int16 negc = ((0 + (3 - -1))-1);
  int16 result_sub = 0; // TODO name changed
  int16 dx_sub; // TODO changed
  int16 dy_sub;

  for (;;) { // bb 2
    if (((x) == (negc)) // bbs 2 and 8
	&& (((y) == ((0 + (3 - -1)))) // bbs 7 and 10
	    || ((y) == (((0 + (3 - -1))-1))))) { // bbs 9 and 11
      result_sub = norm (result_sub, invert (y)); // bb 6
      break;
    }

    if (((y) == (invert (negc)))) { // bbs 5 and 14
      result_sub = norm (result_sub, x); // bb 13
      break;
    }

    dx_sub = integer_lo (x); // bb 12
    dy_sub = ~integer_lo (y);
    dx_sub = dx_sub + dy_sub;

    if (((negc) == ((0 + (3 - -1))))) // bbs 12 and 18
      negc = negative_carry (dx_sub < dy_sub); // bbs 16, 19, 20, 21
    else { // bb 17, 22, 23, 24
      dx_sub++;
      negc = negative_carry (dx_sub <= dy_sub);
    }

    x = integer_hi (x); // bb 15
    y = integer_hi (y);

    result_sub = make_integer (dx_sub, result_sub);
  }

  return result_sub; // bb 4
}

int16 neg (int16 x) {


  return sub ((0 + (3 - -1)), x);
}

int16 scale (int16 n, int16 x) {


  int16 result;
  int16 carry;
  int32 m;

  if ((n == 0) || ((x) == ((0 + (3 - -1)))))
    return (0 + (3 - -1));

  if (n == 1)
    return x;

  result = 0;
  carry = 0;

  for (;;) {
    if (((x) == ((0 + (3 - -1))))){
      if (carry <= 255)
	result = norm (result, (carry + (3 - -1)));
      else
	result = norm (result, make_integer (carry, (0 + (3 - -1))));
      break;
    }

    if (((x) == (((0 + (3 - -1))-1)))) {
      carry = carry - n;
      int16 tmp = 1;
      if (carry >= ((tmp<<16) - 1)) // TODO -1 not a good literal
	result = norm (result, ((carry & #xff) + (3 - -1)));
      else
	result = norm (result, make_integer (carry, ((0 + (3 - -1))-1)));
      break;
    }

    int32 tmp1 = integer_lo (x);
    m = tmp1 * n + carry;

    x = integer_hi (x);
    carry = m >> 16;
    int16 tmp2 = m;
    result = make_integer (tmp2, result);
  }

  return result;
}

int16 mulnonneg (int16 x, int16 y) {



  int16 result = 0;
  int16 s = scale (integer_lo (x), y);

  for (;;) {
    result = make_integer (integer_lo (s), result);
    s = integer_hi (s);
    x = integer_hi (x);

    if (((x) == ((0 + (3 - -1)))))
      break;

    s = add (s, scale (integer_lo (x), y));
  }

  return norm (result, s);
}


int16 divnonneg (int16 x, int16 y) {



  int16 result = (0 + (3 - -1)); // bb 0
  int16 lx = integer_length (x);
  int16 ly = integer_length (y);

  if (lx >= ly) { // bb 0
    lx = lx - ly; // bb 2

    y = shift_left (y, lx);

    do {
      result = shl (result); // bb 3
      if (cmp (x, y) >= 1) {
	x = sub (x, y); // bb 7
	result = add (((0 + (3 - -1))+1), result);
      }
      y = shr (y); // bb 6 ?
    } while (lx-- != 0); // bbs 4 and 8
  }

  return result; // bb 1
}

int16 bitwise_ior (int16 x, int16 y) {


  int16 result = 0;

  for (;;){
    if (((x) == ((0 + (3 - -1)))))
      return norm(result, y);
    if (((x) == (((0 + (3 - -1))-1))))
      return norm(result, x);
    result = make_integer(integer_lo(x) | integer_lo(y),
			  result);
    x = integer_hi(x);
    y = integer_hi(y);
  }
}

int16 bitwise_xor (int16 x, int16 y) {


  int16 result = 0;

  for (;;){
    if (((x) == ((0 + (3 - -1)))))
      return norm(result, y);
    if (((x) == (((0 + (3 - -1))-1))))
      return norm(result, x);
    result = make_integer(integer_lo(x) ^ integer_lo(y),
     result);
    x = integer_hi(x);
    y = integer_hi(y);
  }
}



int16 encode_int (int16 n) {
  if (/* n >= -1 && */ n <= 255) { // TODO should be n >= -1, but -1 as a literal is not good. since only primitives (i.e. not the bignum code) uses it, shouldn't be a problem
    return (n + (3 - -1));
  }

  return alloc_ram_cell_init (0, (0 + (3 - -1)), n >> 8, n);
}
void decode_2_int_args () {
  a1 = decode_int (arg1);
  a2 = decode_int (arg2);
}





void prim_numberp () {
  if (arg1 >= 3
      && arg1 <= (3 + (255 - -1)))
    arg1 = 1;
  else {
    if ((!((arg1) >= 1280) && ((arg1) >= 512))){
      arg1 = (ram_get_field0 (arg1) & #xc0) == 0;
    }
    else if ((!((arg1) >= 1280) && !(!((arg1) >= 1280) && ((arg1) >= 512)) && ((arg1) >= (3 +255 - -1 +1))))
      arg1 = (rom_get_field0 (arg1) & #xc0) == 0;
    else
      arg1 = 0;
  }
}
void prim_add () {

  arg1 = add (arg1, arg2);




  arg2 = 0;
}

void prim_sub () {

  arg1 = sub (arg1, arg2);




  arg2 = 0;
}

void prim_mul () {

  a1 = negp (arg1);
  a2 = negp (arg2);
  arg1 = mulnonneg (a1 ? neg(arg1) : arg1,
      a2 ? neg(arg2) : arg2);
  if (a1 + a2 == 1)
    arg1 = neg(arg1);




  arg2 = 0;
}

void prim_div () {

  if (((arg2) == (((0) + (3 - -1)))))
    halt_with_error();
  a1 = negp (arg1);
  a2 = negp (arg2);
  arg1 = divnonneg (a1 ? neg(arg1) : arg1,
      a2 ? neg(arg2) : arg2);
  if (a1 + a2 == 1)
    arg1 = neg(arg1);






  arg2 = 0;
}

void prim_rem () {

  if (((arg2) == (((0) + (3 - -1)))))
    halt_with_error();
  if (negp(arg1) || negp(arg2))
    halt_with_error();


  arg3 = divnonneg (arg1, arg2);
  arg4 = mulnonneg (arg2, arg3);
  arg1 = sub(arg1, arg4 );
  arg3 = 0;
  arg4 = 0;






  arg2 = 0;
}

void prim_neg () {

  arg1 = neg (arg1);




}

void prim_eq () {

  arg1 = ((cmp (arg1, arg2) == 1));




  arg2 = 0;
}

void prim_lt () {

  arg1 = ((cmp (arg1, arg2) < 1));




  arg2 = 0;
}

void prim_gt () {

  arg1 = ((cmp (arg1, arg2) > 1));




  arg2 = 0;
}

void prim_leq () {

  arg1 = ((cmp (arg1, arg2) <= 1));




  arg2 = 0;

}

void prim_geq () {

  arg1 = ((cmp (arg1, arg2) >= 1));




  arg2 = 0;
}

void prim_ior () {

  arg1 = bitwise_ior(arg1, arg2);




  arg2 = 0;
}

void prim_xor () {

  arg1 = bitwise_xor(arg1, arg2);




  arg2 = 0;
}







void prim_pairp () {
  if ((!((arg1) >= 1280) && ((arg1) >= 512)))
    arg1 = (((((ram_get_field0 (arg1) & #x80) == #x80) && ((ram_get_field2 (arg1) & #xe0) == 0))));
  else if ((!((arg1) >= 1280) && !(!((arg1) >= 1280) && ((arg1) >= 512)) && ((arg1) >= (3 +255 - -1 +1))))
    arg1 = (((((rom_get_field0 (arg1) & #x80) == #x80) && ((rom_get_field2 (arg1) & #xe0) == 0))));
  else
    arg1 = 0;
}

int16 cons (int16 car, int16 cdr) {
  return alloc_ram_cell_init (#x80 | (car >> 8),
         car & #xff,
         0 | (cdr >> 8),
         cdr & #xff);
}

void prim_cons () {
  arg1 = cons (arg1, arg2);
  arg2 = 0;
}

void prim_car () {
  if ((!((arg1) >= 1280) && ((arg1) >= 512))) {
    if (!(((ram_get_field0 (arg1) & #x80) == #x80) && ((ram_get_field2 (arg1) & #xe0) == 0)))
      halt_with_error();
    arg1 = ram_get_car (arg1);
  }
  else if ((!((arg1) >= 1280) && !(!((arg1) >= 1280) && ((arg1) >= 512)) && ((arg1) >= (3 +255 - -1 +1)))) {
    if (!(((rom_get_field0 (arg1) & #x80) == #x80) && ((rom_get_field2 (arg1) & #xe0) == 0)))
      halt_with_error();
    arg1 = rom_get_car (arg1);
  }
  else
    halt_with_error();
}

void prim_cdr () {
  if ((!((arg1) >= 1280) && ((arg1) >= 512))) {
    if (!(((ram_get_field0 (arg1) & #x80) == #x80) && ((ram_get_field2 (arg1) & #xe0) == 0)))
      halt_with_error();
    arg1 = ram_get_cdr (arg1);
  }
  else if ((!((arg1) >= 1280) && !(!((arg1) >= 1280) && ((arg1) >= 512)) && ((arg1) >= (3 +255 - -1 +1)))) {
    if (!(((rom_get_field0 (arg1) & #x80) == #x80) && ((rom_get_field2 (arg1) & #xe0) == 0)))
      halt_with_error();
    arg1 = rom_get_cdr (arg1);
  }
  else
    halt_with_error();
}

void prim_set_car () {
  if ((!((arg1) >= 1280) && ((arg1) >= 512))) {
    if (!(((ram_get_field0 (arg1) & #x80) == #x80) && ((ram_get_field2 (arg1) & #xe0) == 0)))
      halt_with_error();

    ram_set_car (arg1, arg2);
    arg1 = 0;
    arg2 = 0;
  }
  else
    halt_with_error();
}

void prim_set_cdr () {
  if ((!((arg1) >= 1280) && ((arg1) >= 512))) {
    if (!(((ram_get_field0 (arg1) & #x80) == #x80) && ((ram_get_field2 (arg1) & #xe0) == 0)))
      halt_with_error();

    ram_set_cdr (arg1, arg2);
    arg1 = 0;
    arg2 = 0;
  }
  else
    halt_with_error();
}

void prim_nullp () {
  arg1 = ((arg1 == 2));
}





void prim_u8vectorp () {
  if ((!((arg1) >= 1280) && ((arg1) >= 512)))
    arg1 = (((((ram_get_field0 (arg1) & #x80) == #x80) && ((ram_get_field2 (arg1) & #xe0) == #x60))));
  else if ((!((arg1) >= 1280) && !(!((arg1) >= 1280) && ((arg1) >= 512)) && ((arg1) >= (3 +255 - -1 +1))))
    arg1 = (((((rom_get_field0 (arg1) & #x80) == #x80) && ((rom_get_field2 (arg1) & #xe0) == #x60))));
  else
    arg1 = 0;
}

void prim_make_u8vector () {
  decode_2_int_args ();

  if (a2 > 255)
    halt_with_error();

  arg3 = alloc_vec_cell (a1);
  arg1 = alloc_ram_cell_init (#x80 | (a1 >> 8),
         a1 & #xff,         #x60 | (arg3 >> 8),
         arg3 & #xff);

  a1 = (a1 + 3) >> 2;
  while (a1--) {
    ram_set_field0 (arg3, a2);
    ram_set_field1 (arg3, a2);
    ram_set_field2 (arg3, a2);
    ram_set_field3 (arg3, a2);
    arg3++;
  }
}

void prim_u8vector_ref () {
  a2 = decode_int (arg2);

  if ((!((arg1) >= 1280) && ((arg1) >= 512))) {
    if (!(((ram_get_field0 (arg1) & #x80) == #x80) && ((ram_get_field2 (arg1) & #xe0) == #x60)))
      halt_with_error();
    if ((ram_get_car (arg1) <= a2) || (a2 < 0))
      halt_with_error();
    arg1 = ram_get_cdr (arg1);
  }
  else if ((!((arg1) >= 1280) && !(!((arg1) >= 1280) && ((arg1) >= 512)) && ((arg1) >= (3 +255 - -1 +1)))) {
    if (!(((rom_get_field0 (arg1) & #x80) == #x80) && ((rom_get_field2 (arg1) & #xe0) == #x60)))
      halt_with_error();
    if ((rom_get_car (arg1) <= a2) || (a2 < 0))
      halt_with_error();
    arg1 = rom_get_cdr (arg1);
  }
  else
    halt_with_error();

  if (((arg1) >= 1280)) {
    arg1 += (a2 >> 2);
    a2 %= 4;

    arg1 = encode_int (ram_get_fieldn (arg1, a2));
  }
  else {
    while (a2--)
      arg1 = rom_get_cdr (arg1);


    arg1 = rom_get_car (arg1);
  }

  arg2 = 0;
  arg3 = 0;
  arg4 = 0;
}

void prim_u8vector_set () {
  a2 = decode_int (arg2);
  a3 = decode_int (arg3);

  if (a3 > 255)
    halt_with_error();

  if ((!((arg1) >= 1280) && ((arg1) >= 512))) {
    if (!(((ram_get_field0 (arg1) & #x80) == #x80) && ((ram_get_field2 (arg1) & #xe0) == #x60)))
      halt_with_error();
    if ((ram_get_car (arg1) <= a2) || (a2 < 0))
      halt_with_error();
    arg1 = ram_get_cdr (arg1);
  }
  else
    halt_with_error();

  arg1 += (a2 >> 2);
  a2 %= 4;

  ram_set_fieldn (arg1, a2, a3);

  arg1 = 0;
  arg2 = 0;
  arg3 = 0;
}

void prim_u8vector_length () {
  if ((!((arg1) >= 1280) && ((arg1) >= 512))) {
    if (!(((ram_get_field0 (arg1) & #x80) == #x80) && ((ram_get_field2 (arg1) & #xe0) == #x60)))
      halt_with_error();
    arg1 = encode_int (ram_get_car (arg1));
  }
  else if ((!((arg1) >= 1280) && !(!((arg1) >= 1280) && ((arg1) >= 512)) && ((arg1) >= (3 +255 - -1 +1)))) {
    if (!(((rom_get_field0 (arg1) & #x80) == #x80) && ((rom_get_field2 (arg1) & #xe0) == #x60)))
      halt_with_error();
    arg1 = encode_int (rom_get_car (arg1));
  }
  else
    halt_with_error();
}

void prim_u8vector_copy () {



  a1 = decode_int (arg2);
  a2 = decode_int (arg4);
  a3 = decode_int (arg5);


  if ((!((arg1) >= 1280) && ((arg1) >= 512)) && (!((arg3) >= 1280) && ((arg3) >= 512))) {
    if (!(((ram_get_field0 (arg1) & #x80) == #x80) && ((ram_get_field2 (arg1) & #xe0) == #x60)) || !(((ram_get_field0 (arg3) & #x80) == #x80) && ((ram_get_field2 (arg3) & #xe0) == #x60)))
      halt_with_error();
    if ((ram_get_car (arg1) < (a1 + a3)) || (a1 < 0) ||
 (ram_get_car (arg3) < (a2 + a3)) || (a2 < 0))
      halt_with_error();


    arg1 = ram_get_cdr (arg1);
    arg1 += (a1 >> 2);
    a1 %= 4;
    arg3 = ram_get_cdr (arg3);
    arg3 += (a2 >> 2);
    a2 %= 4;


    while (a3--) {
      ram_set_fieldn (arg3, a2, ram_get_fieldn (arg1, a1));

      a1++;
      arg1 += (a1 >> 2);
      a1 %= 4;
      a2++;
      arg3 += (a2 >> 2);
      a2 %= 4;
    }
  }

  else if ((!((arg1) >= 1280) && !(!((arg1) >= 1280) && ((arg1) >= 512)) && ((arg1) >= (3 +255 - -1 +1))) && (!((arg3) >= 1280) && ((arg3) >= 512))) {
    if (!(((rom_get_field0 (arg1) & #x80) == #x80) && ((rom_get_field2 (arg1) & #xe0) == #x60)) || !(((ram_get_field0 (arg3) & #x80) == #x80) && ((ram_get_field2 (arg3) & #xe0) == #x60)))
      halt_with_error();
    if ((rom_get_car (arg1) < (a1 + a3)) || (a1 < 0) ||
 (ram_get_car (arg3) < (a2 + a3)) || (a2 < 0))
      halt_with_error();

    arg1 = rom_get_cdr (arg1);
    while (a1--)
      arg1 = rom_get_cdr (arg1);

    arg3 = ram_get_cdr (arg3);
    arg3 += (a2 >> 2);
    a2 %= 4;

    while (a3--) {
      ram_set_fieldn (arg3, a2, decode_int (rom_get_car (arg1)));

      arg1 = rom_get_cdr (arg1);
      a2++;
      arg3 += (a2 >> 2);
      a2 %= 4;
    }
  }
  else
    halt_with_error();

  arg1 = 0;
  arg2 = 0;
  arg3 = 0;
  arg4 = 0;
  arg5 = 0;
}





void prim_eqp () {
  arg1 = ((arg1 == arg2));
  arg2 = 0;
}

void prim_not () {
  arg1 = ((arg1 == 0));
}

void prim_symbolp () {
  if ((!((arg1) >= 1280) && ((arg1) >= 512)))
    arg1 = (((((ram_get_field0 (arg1) & #x80) == #x80) && ((ram_get_field2 (arg1) & #xe0) == #x20))));
  else if ((!((arg1) >= 1280) && !(!((arg1) >= 1280) && ((arg1) >= 512)) && ((arg1) >= (3 +255 - -1 +1))))
    arg1 = (((((rom_get_field0 (arg1) & #x80) == #x80) && ((rom_get_field2 (arg1) & #xe0) == #x20))));
  else
    arg1 = 0;
}

void prim_stringp () {
  if ((!((arg1) >= 1280) && ((arg1) >= 512)))
    arg1 = (((((ram_get_field0 (arg1) & #x80) == #x80) && ((ram_get_field2 (arg1) & #xe0) == #x40))));
  else if ((!((arg1) >= 1280) && !(!((arg1) >= 1280) && ((arg1) >= 512)) && ((arg1) >= (3 +255 - -1 +1))))
    arg1 = (((((rom_get_field0 (arg1) & #x80) == #x80) && ((rom_get_field2 (arg1) & #xe0) == #x40))));
  else
    arg1 = 0;
}

void prim_string2list () {
  if ((!((arg1) >= 1280) && ((arg1) >= 512))) {
    if (!(((ram_get_field0 (arg1) & #x80) == #x80) && ((ram_get_field2 (arg1) & #xe0) == #x40)))
      halt_with_error();

    arg1 = ram_get_car (arg1);
  }
  else if ((!((arg1) >= 1280) && !(!((arg1) >= 1280) && ((arg1) >= 512)) && ((arg1) >= (3 +255 - -1 +1)))) {
    if (!(((rom_get_field0 (arg1) & #x80) == #x80) && ((rom_get_field2 (arg1) & #xe0) == #x40)))
      halt_with_error();

    arg1 = rom_get_car (arg1);
    }
  else
    halt_with_error();
}

void prim_list2string () {
  arg1 = alloc_ram_cell_init (#x80 | ((arg1 & #x1f00) >> 8),
         arg1 & #xff,         #x40,
         0);
}

void prim_booleanp () {
  arg1 = ((arg1 < 2));
}





void prim_print () {




  arg1 = 0;
}

int32 read_clock () {
  int32 now = 0;


  /* now = from_now( 0 ); */ // TODO
  return now;
}

void prim_clock () {
  arg1 = encode_int (read_clock ());
}

void prim_motor () {
  decode_2_int_args ();

  if (a1 < 1 || a1 > 2 || a2 < -100 || a2 > 100)
    halt_with_error();


  /*   MOTOR_set( a1, a2 ); */ // TODO







  arg1 = 0;
  arg2 = 0;
}


void prim_led () {
  decode_2_int_args ();
  a3 = decode_int (arg3);

  if (a1 < 1 || a1 > 3 || a2 < 0 || a3 < 0)
    halt_with_error();


  /*   LED_set( a1, a2, a3 ); */ // TODO







  arg1 = 0;
  arg2 = 0;
  arg3 = 0;
}


void prim_led2_color () {
  a1 = decode_int (arg1);

  if (a1 < 0 || a1 > 1)
    halt_with_error();


  /*   LED2_color_set( a1 ); */ // TODO







  arg1 = 0;
}


void prim_getchar_wait () {
  decode_2_int_args();
  a1 = read_clock () + a1;

  if (a1 < 0 || a2 < 1 || a2 > 3)
    halt_with_error();


  arg1 = 0;
  /*   { */ // TODO
/*     serial_port_set ports; */
/*     ports = serial_rx_wait_with_timeout( a2, a1 ); */
/*     if (ports != 0) */
/*       arg1 = encode_int (serial_rx_read( ports )); */
/*   } */
}


void prim_putchar () {
  decode_2_int_args ();

  if (a1 < 0 || a1 > 255 || a2 < 1 || a2 > 3)
    halt_with_error();


  /*   serial_tx_write( a2, a1 ); */ // TODO
  uart_write(a1);






  arg1 = 0;
  arg2 = 0;
}


void prim_beep () {
  decode_2_int_args ();

  if (a1 < 1 || a1 > 255 || a2 < 0)
    halt_with_error();


/*   beep( a1, from_now( a2 ) ); */







  arg1 = 0;
  arg2 = 0;
}


void prim_adc () {
  int16 x;

  a1 = decode_int (arg1);

  if (a1 < 1 || a1 > 3)
    halt_with_error();


/*   x = adc( a1 ); */
  arg1 = encode_int (x);
}

void prim_sernum () {
  int16 x;


/*   x = serial_num (); */






  arg1 = encode_int (x);
}





void prim_network_init () {





}

void prim_network_cleanup () {



}

void prim_receive_packet_to_u8vector () {

  if (!(((ram_get_field0 (arg1) & #x80) == #x80) && ((ram_get_field2 (arg1) & #xe0) == #x60)))
    halt_with_error();
}

void prim_send_packet_from_u8vector () {



  if (!(((ram_get_field0 (arg1) & #x80) == #x80) && ((ram_get_field2 (arg1) & #xe0) == #x60)))
    halt_with_error();

  a2 = decode_int (arg2);
  a1 = 0;


  if (ram_get_car (arg1) < a2)
    halt_with_error();

  arg1 = ram_get_cdr (arg1);
  arg2 = 0;
}

/* void push_arg1 (); */
/* int16 pop (); */
/* void pop_procedure (); */
/* void handle_arity_and_rest_param (); */
/* void build_env (); */
/* void save_cont (); */
/* void interpreter (); */

void push_arg1 () {
  env = cons (arg1, env);
  arg1 = 0;
}

int16 pop () {
  int16 o = ram_get_car (env);
  env = ram_get_cdr (env);
  return o;
}

void pop_procedure () {
  arg1 = pop();

  if ((!((arg1) >= 1280) && ((arg1) >= 512))) {
    if (!((ram_get_field0 (arg1) & #xc0) == #x40))
      halt_with_error();

    entry = ram_get_entry (arg1) + #x8000;
  }
  else if ((!((arg1) >= 1280) && !(!((arg1) >= 1280) && ((arg1) >= 512)) && ((arg1) >= (3 +255 - -1 +1)))) {
    if (!((rom_get_field0 (arg1) & #xc0) == #x40))
      halt_with_error();

    entry = rom_get_entry (arg1) + #x8000;
  }
  else
    halt_with_error();
}

void handle_arity_and_rest_param () {
  int8 np;

  np = rom_get (entry++);

  if ((np & #x80) == 0) {
    if (na != np)
      halt_with_error();
  }
  else {
    np = ~np;

    if (na < np)
      halt_with_error();

    arg3 = 2;

    while (na > np) {
      arg4 = pop();

      arg3 = cons (arg4, arg3);
      arg4 = 0;

      na--;
    }

    arg1 = cons (arg3, arg1);
    arg3 = 0;
  }
}

void build_env () {
  while (na != 0) {
    arg3 = pop();

    arg1 = cons (arg3, arg1);

    na--;
  }

  arg3 = 0;
}

void save_cont () {

  arg3 = alloc_ram_cell_init (#x40 | (pc >> 11),
         (pc >> 3) & #xff,
         ((pc & #x0007) << 5) | (env >> 8),
         env & #xff);
  cont = alloc_ram_cell_init (#x80 | (cont >> 8),
                              cont & #xff,         #x80 | (arg3 >> 8),
                              arg3 & #xff);
  arg3 = 0;
}

void init_ram_heap () {
  int8 i;
  int16 o = 1279;

  free_list = 0;

  int16 tmp = (512 + ((glovars + 1) >> 1)); // TODO optimization
  while (o > tmp) {


    ram_set_gc_tags (o, (0<<5));
    ram_set_car (o, free_list);
    free_list = o;
    o--;
  }

  free_list_vec = 1280;
  ram_set_car (free_list_vec, 0);



  ram_set_cdr (free_list_vec, ((2047 - 1280 + 1)*4) >> 2);

  for (i=0; i<glovars; i++)
    set_global (i, 0);

  arg1 = 0;
  arg2 = 0;
  arg3 = 0;
  arg4 = 0;
  cont = 0;
  env = 2;
}


void interpreter () {
  int16 tmp = rom_get (#x8000 +2);
  pc = (#x8000 + 4) + (tmp << 2);

  glovars = rom_get (#x8000 +3);

  init_ram_heap ();

 dispatch: ;
  bytecode = rom_get (pc++);
  bytecode_hi4 = bytecode & #xf0;
  bytecode_lo4 = bytecode & #x0f;
  switch (bytecode_hi4 >> 4) {;


  case 0: // push-constant // TODO used to be #x00 >> 4

  ;

  arg1 = bytecode_lo4;

  push_arg1 ();

  ; goto dispatch;;


  case 1:;; // push-constant

  ;
  arg1 = bytecode_lo4+16;

  push_arg1 ();

  ; goto dispatch;;


  case 2:;; // push-stack

  ;

  arg1 = env;

  while (bytecode_lo4 != 0) {
    arg1 = ram_get_cdr (arg1);
    bytecode_lo4--;
  }

  arg1 = ram_get_car (arg1);

  push_arg1 ();

  ; goto dispatch;;


  case 3:;; // push-stack

  ;

  bytecode_lo4 += 16;

  arg1 = env;

  while (bytecode_lo4 != 0) {
    arg1 = ram_get_cdr (arg1);
    bytecode_lo4--;
  }

  arg1 = ram_get_car (arg1);

  push_arg1 ();

  ; goto dispatch;;


  case 4:;; // push-global

  ;

  arg1 = get_global (bytecode_lo4);

  push_arg1 ();

  ; goto dispatch;;


  case 5:;; // set-global

  ;

  set_global (bytecode_lo4, pop());

  ; goto dispatch;;


  case 6:;; // call

  ;

  na = bytecode_lo4;

  pop_procedure ();
  handle_arity_and_rest_param ();
  build_env ();
  save_cont ();

  env = arg1;
  pc = entry;

  arg1 = 0;

  ; goto dispatch;;


  case 7:;; // jump

  ;

  na = bytecode_lo4;

  pop_procedure ();
  handle_arity_and_rest_param ();
  build_env ();

  env = arg1;
  pc = entry;

  arg1 = 0;

  ; goto dispatch;;


  case 8:;;

  switch (bytecode_lo4) {
  case 0: // call-toplevel
    bytecode = rom_get (pc++);
    arg2 = bytecode;

    bytecode = rom_get (pc++);

    ;


    entry = (arg2 << 8) + bytecode + #x8000;
    arg1 = 2;

    na = rom_get (entry++);

    build_env ();
    save_cont ();

    env = arg1;
    pc = entry;

    arg1 = 0;
    arg2 = 0;

    break;

  case 1: // jump-toplevel
    bytecode = rom_get (pc++);
    arg2 = bytecode;

    bytecode = rom_get (pc++);

    ;


    entry = (arg2 << 8) + bytecode + #x8000;
    arg1 = 2;

    na = rom_get (entry++);

    build_env ();

    env = arg1;
    pc = entry;

    arg1 = 0;
    arg2 = 0;

    break;

  case 2: // goto
    bytecode = rom_get (pc++);
    arg2 = bytecode;

    bytecode = rom_get (pc++);

    ;


    pc = (arg2 << 8) + bytecode + #x8000;

    break;

  case 3: // goto-if-false
    bytecode = rom_get (pc++);
    arg2 = bytecode;

    bytecode = rom_get (pc++);

    ;


    if (pop() == 0)
      pc = (arg2 << 8) + bytecode + #x8000;

    break;

  case 4: // closure
    bytecode = rom_get (pc++);
    arg2 = bytecode;

    bytecode = rom_get (pc++);

    ;

    arg3 = pop();

    entry = (arg2 << 8) | bytecode;

    int16 tmp = (bytecode & #x07);
    arg1 = alloc_ram_cell_init (#x40 | (arg2 >> 3),
				((arg2 & #x07) << 5) | (bytecode >> 3),
				(tmp << 5) |((arg3 &#x1f00) >>8),
    arg3 & #xff);

    push_arg1 ();

    arg2 = 0;
    arg3 = 0;

    break;

    /*   case 5: */ // TODO useless, they don't work in the regular PICOBIT
/*     bytecode = rom_get (pc++); */

/*     ; */


/*     entry = pc + bytecode + #x8000; */
/*     arg1 = 2; */

/*     na = rom_get (entry++); */

/*     build_env (); */
/*     save_cont (); */

/*     env = arg1; */
/*     pc = entry; */

/*     arg1 = 0; */

/*     break; */

/*   case 6: */
/*     bytecode = rom_get (pc++); */

/*     ; */


/*     entry = pc + bytecode + #x8000; */
/*     arg1 = 2; */

/*     na = rom_get (entry++); */

/*     build_env (); */

/*     env = arg1; */
/*     pc = entry; */

/*     arg1 = 0; */

/*     break; */

/*   case 7: */
/*     bytecode = rom_get (pc++); */

/*     ; */

/*     pc = pc + bytecode + #x8000; */

/*     break; */

/*   case 8: */
/*     bytecode = rom_get (pc++); */

/*     ; */


/*     if (pop() == 0) */
/*       pc = pc + bytecode + #x8000; */

/*     break; */

/*   case 9: */
/*     bytecode = rom_get (pc++); */

/*     ; */

/*     arg3 = pop(); */

/*     entry = pc + bytecode; */

/*     arg1 = alloc_ram_cell_init (#x40 | (arg2 >> 3), */
/*     ((arg2 & #x07) << 5) | (bytecode >> 3), */
/*     ((bytecode &#x07) <<5) |((arg3 &#x1f00) >>8), */
/*     arg3 & #xff); */

/*     push_arg1 (); */

/*     arg3 = 0; */

/*     break; */
  case 14: // push-global [long]
    bytecode = rom_get (pc++);

    ;

    arg1 = get_global (bytecode);

    push_arg1 ();

    break;

  case 15: // set-global [long]
    bytecode = rom_get (pc++);

    ;

    set_global (bytecode, pop());

    break;
  }

  ; goto dispatch;;


  case 9:;; // push-constant [long]



  bytecode = rom_get (pc++);

  ;

  int16 tmp = bytecode_lo4; // TODO ugly patch. not needed with gcc since the 8 is an int, and would make all the other operands ints, whereas we keep using int8, and truncate the value
  arg1 = (tmp << 8) | bytecode;
  push_arg1 ();

  ; goto dispatch;;


  case 10:;; 

  ; goto dispatch;;


  case 11:;;

  ; goto dispatch;;


  case 12:;;

  ;

  switch (bytecode_lo4) {
  case 0:
    arg1 = pop(); prim_numberp (); push_arg1 (); break;
  case 1:
    arg2 = pop(); arg1 = pop(); prim_add (); push_arg1 (); break;
  case 2:
    arg2 = pop(); arg1 = pop(); prim_sub (); push_arg1 (); break;
  case 3:
    arg2 = pop(); arg1 = pop(); prim_mul (); push_arg1 (); break;
  case 4:
    arg2 = pop(); arg1 = pop(); prim_div (); push_arg1 (); break;
  case 5:
    arg2 = pop(); arg1 = pop(); prim_rem (); push_arg1 (); break;
  case 6:
    arg1 = pop(); prim_neg (); push_arg1 (); break;
  case 7:
    arg2 = pop(); arg1 = pop(); prim_eq (); push_arg1 (); break;
  case 8:
    arg2 = pop(); arg1 = pop(); prim_lt (); push_arg1 (); break;
  case 9:
    arg2 = pop(); arg1 = pop(); prim_leq (); push_arg1 (); break;
  case 10:
    arg2 = pop(); arg1 = pop(); prim_gt (); push_arg1 (); break;
  case 11:
    arg2 = pop(); arg1 = pop(); prim_geq (); push_arg1 (); break;
  case 12:
    arg1 = pop(); prim_pairp (); push_arg1 (); break;
  case 13:
    arg2 = pop(); arg1 = pop(); prim_cons (); push_arg1 (); break;
  case 14:
    arg1 = pop(); prim_car (); push_arg1 (); break;
  case 15:
    arg1 = pop(); prim_cdr (); push_arg1 (); break;
  }

  ; goto dispatch;;


  case 13:;;

  ;

  switch (bytecode_lo4) {
  case 0:
    arg2 = pop(); arg1 = pop(); prim_set_car (); break;
  case 1:
    arg2 = pop(); arg1 = pop(); prim_set_cdr (); break;
  case 2:
    arg1 = pop(); prim_nullp (); push_arg1 (); break;
  case 3:
    arg2 = pop(); arg1 = pop(); prim_eqp (); push_arg1 (); break;
  case 4:
    arg1 = pop(); prim_not (); push_arg1 (); break;
  case 5: // get-cont

    arg1 = cont;
    push_arg1 ();
    break;
  case 6: // graft-to-cont


    arg1 = pop();
    cont = pop();

    push_arg1 ();

    na = 0;

    pop_procedure ();
    handle_arity_and_rest_param ();
    build_env ();

    env = arg1;
    pc = entry;

    arg1 = 0;

    break;
  case 7: // return-to-cont


    arg1 = pop();
    cont = pop();

    arg2 = ram_get_cdr (cont);

    pc = ram_get_entry (arg2);

    env = ram_get_cdr (arg2);
    cont = ram_get_car (cont);

    push_arg1 ();
    arg2 = 0;

    break;
  case 8: // halt

    return;
  case 9:

    arg1 = pop(); prim_symbolp (); push_arg1 (); break;
  case 10:

    arg1 = pop(); prim_stringp (); push_arg1 (); break;
  case 11:

    arg1 = pop(); prim_string2list (); push_arg1 (); break;
  case 12:

    arg1 = pop(); prim_list2string (); push_arg1 (); break;
  case 13:

    arg2 = pop(); arg1 = pop(); prim_make_u8vector (); push_arg1 (); break;
  case 14:

    arg2 = pop(); arg1 = pop(); prim_u8vector_ref (); push_arg1 (); break;
  case 15:

    arg3 = pop(); arg2 = pop(); arg1 = pop(); prim_u8vector_set (); break;
  }

  ; goto dispatch;;


  case 14:;;

  ;

  switch (bytecode_lo4) {
  case 0:

    arg1 = pop();
    prim_print ();
    break;
  case 1:

    prim_clock (); push_arg1 (); break;
  case 2:

    arg2 = pop(); arg1 = pop(); prim_motor (); break;
  case 3:

    arg3 = pop(); arg2 = pop(); arg1 = pop(); prim_led (); ;break;
  case 4:

    arg1 = pop(); prim_led2_color (); break;
  case 5:

    arg2 = pop(); arg1 = pop(); prim_getchar_wait (); push_arg1 (); break;
  case 6:

    arg2 = pop(); arg1 = pop(); prim_putchar (); break;
  case 7:

    arg2 = pop(); arg1 = pop(); prim_beep (); break;
  case 8:

    arg1 = pop(); prim_adc (); push_arg1 (); break;
  case 9:

    arg1 = pop(); prim_u8vectorp (); push_arg1 (); break;
  case 10:

    prim_sernum (); push_arg1 (); break;
  case 11:

    arg1 = pop(); prim_u8vector_length (); push_arg1 (); break;
  case 12:

    arg5 = pop(); arg4 = pop(); arg3 = pop(); arg2 = pop(); arg1 = pop();
    prim_u8vector_copy (); break;
    break;
  case 13: // shift

    arg1 = pop();
    pop();
    push_arg1 ();
    break;
  case 14: // pop

    pop();
    break;
  case 15: // return

    arg1 = pop();
    arg2 = ram_get_cdr (cont);
    pc = ram_get_entry (arg2);
    env = ram_get_cdr (arg2);
    cont = ram_get_car (cont);
    push_arg1 ();
    arg2 = 0;
    break;
  }

  ; goto dispatch;;



  case 15:;;

  ;

  switch (bytecode_lo4) {
  case 0:

    arg1 = pop(); prim_booleanp (); push_arg1 (); break;
  case 1:

    prim_network_init (); break;
  case 2:

    prim_network_cleanup (); break;
  case 3:

    arg1 = pop(); prim_receive_packet_to_u8vector (); push_arg1 (); break;
  case 4:

    arg2 = pop(); arg1 = pop(); prim_send_packet_from_u8vector ();
    push_arg1 (); break;
  case 5:
    arg2 = pop(); arg1 = pop(); prim_ior (); push_arg1 (); break;
    break;
  case 6:
    arg2 = pop(); arg1 = pop(); prim_xor (); push_arg1 (); break;
    break;
  }

  ; goto dispatch;;



  };
}

interpreter();
