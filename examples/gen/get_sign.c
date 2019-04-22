int get_sign(int x, int z, int r)
{
  int y = z - x;
  if (x == 0)
    x = 0;
  if (y < 0){
    x = -x;
    r = x + 7;
    return r;
  }
  else
    x++;
}