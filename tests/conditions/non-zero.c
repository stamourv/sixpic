// simpler sigma, to test the new condition system
int sigma (int n){
  int res = n;
  while (n--){
    res = res + n;
  }
  return res;
}

sigma(5);
