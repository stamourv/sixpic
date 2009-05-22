// to test rom_get, the data is contained in dummy.hex
byte x = 2 + 4;
int16 y = rom_get(#x5000);
int16 z = (y << 8) | rom_get(#x5001);
z;
