// -*- c -*-

// from https://web.archive.org/web/20160418004149/http://freespace.virgin.net/hugo.elias/graphics/x_water.htm
#define TILE(f, deltax, deltay) (f[(pos.x+(deltax) + ((pos.y+(deltay))*size.x))])

kernel void reset(int2 size, float u, global float *dst) {
  int2 pos = (int2)(get_global_id(0), get_global_id(1));
  if(pos.x >= size.x || pos.y >= size.y) return;
  TILE(dst, 0, 0) = u;
}

kernel void ripple(int2 size, global const float *src, global float *dst) {
  int2 pos = (int2)(get_global_id(0), get_global_id(1));
  if(pos.x >= size.x-1 || pos.y >= size.y-1) return;
  if(pos.x <= 1 || pos.y <= 1) return;

  float result = ((TILE(src, -1, 0) +
                   TILE(src, +1, 0) +
                   TILE(src, 0, -1) +
                   TILE(src, 0, +1)) / 2) - TILE(dst, 0, 0);
  TILE(dst, 0, 0) = result * 0.9f;
}

kernel void render(int2 size, global const float *src, global uint *image) {
  int2 pos = (int2)(get_global_id(0), get_global_id(1));
  if(pos.x >= size.x || pos.y >= size.y) return;
  
  int color = 0xff000000;
  float2 offset = (float2)(TILE(src, -1, 0) - TILE(src, +1, 0),
                           TILE(src, 0, -1) - TILE(src, 0, +1));
  color |= (int)(fmax(0.5 + offset.x, 0) * 120);
  
  image[pos.x + pos.y*size.x] = color;
}


