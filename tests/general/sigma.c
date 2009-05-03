int sigma (int n){
  int res = 0;
  while (n < n+1){
    switch (n){
    case 0:
      return res;
    case 1:
      res++;
      /* break; */
      n--;
      continue;
    default:
      res = res + n;
    }
    n--;
  }
  return res;
}

sigma(5);
