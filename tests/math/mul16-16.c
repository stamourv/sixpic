// 16x16 multiplication
int32 x; x = 1026 * 515;
int16 x2; x2 = x; // truncation, to use it in the next multiplication (or else would need 8-32, which is not implemented)
int32 y; y = x2 * 1600;
int32 y2; y2 = x2 * 16;
