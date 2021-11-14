// -*- c -*-
#define Q 9
// TODO: seprate into bigass
// void collide(float omega, private float *n0, private float *nN, ... )

typedef struct lattice {
  float n, E, W, N, S,
    NE, SE, NW, SW;
} lattice;

float Rho(lattice *F) {
  return F->n + F->N + F->S + F->E + F->W + F->NW + F->NE + F->SW + F->SE;
}

lattice equilibrium(float2 u, float rho) {
  float ux3 = 3 * u.x;
  float uy3 = 3 * u.y;
  float ux2 = u.x * u.x;
  float uy2 = u.y * u.y;
  float uxuy2 = 2 * u.x * u.y;
  float u2 = ux2 + uy2;
  float u215 = 1.5 * u2;

  float one9th = 1/9.0, one36th = 1/36.0;
  
  return (lattice){
    .n=   (4.0/9) * rho * (1                              - u215),
    .E=    one9th  * rho * (1 + ux3       + 4.5*ux2        - u215),
    .W=    one9th  * rho * (1 - ux3       + 4.5*ux2        - u215),
    .N=    one9th  * rho * (1 - uy3       + 4.5*uy2        - u215),
    .S=    one9th  * rho * (1 + uy3       + 4.5*uy2        - u215),
    .NE=   one36th * rho * (1 + ux3 - uy3 + 4.5*(u2-uxuy2) - u215),
    .SE=   one36th * rho * (1 + ux3 + uy3 + 4.5*(u2+uxuy2) - u215),
    .NW=   one36th * rho * (1 - ux3 - uy3 + 4.5*(u2+uxuy2) - u215),
    .SW=   one36th * rho * (1 - ux3 + uy3 + 4.5*(u2-uxuy2) - u215)
  };
}

kernel void collide(int2 size, float omega, global lattice *f) {
  uint x = get_global_id(0);
  uint y = get_global_id(1);
  if(x >= size.x || y >= size.y) return;
  uint i = (x + y*size.x);
  lattice F = f[i];
  float rho = Rho(&F);
  float2 u = (float2)((F.E + F.NE + F.SE - F.W - F.NW - F.SW) / rho,
                      (F.S + F.SE + F.SW - F.N - F.NE - F.NW ) / rho);

  if(x == 1) { // inlet
    f[i] = equilibrium((float2)(0.01f, 0.0f), rho);
  }
  else {
    lattice eq = equilibrium(u, rho);
    f[i].n  = /*fmin(*/fmax(F.n  + (omega * (eq.n  - F.n )), 0.0001f);//, 2.0f);
    f[i].E  = /*fmin(*/fmax(F.E  + (omega * (eq.E  - F.E )), 0.0001f);//, 2.0f);
    f[i].W  = /*fmin(*/fmax(F.W  + (omega * (eq.W  - F.W )), 0.0001f);//, 2.0f);
    f[i].N  = /*fmin(*/fmax(F.N  + (omega * (eq.N  - F.N )), 0.0001f);//, 2.0f);
    f[i].S  = /*fmin(*/fmax(F.S  + (omega * (eq.S  - F.S )), 0.0001f);//, 2.0f);
    f[i].NE = /*fmin(*/fmax(F.NE + (omega * (eq.NE - F.NE)), 0.0001f);//, 2.0f);
    f[i].SE = /*fmin(*/fmax(F.SE + (omega * (eq.SE - F.SE)), 0.0001f);//, 2.0f);
    f[i].NW = /*fmin(*/fmax(F.NW + (omega * (eq.NW - F.NW)), 0.0001f);//, 2.0f);
    f[i].SW = /*fmin(*/fmax(F.SW + (omega * (eq.SW - F.SW)), 0.0001f);//, 2.0f);
  }
}

#define CELL(f, deltax, deltay) (&f[(pos.x+(deltax) + ((pos.y+(deltay))*size.x))])
#define BARRIER(deltax, deltay) ((   pos.x+(deltax))<=0 || (pos.x+(deltax))>=size.x-1             \
                                 || (pos.y+(deltay))<=0 || (pos.y+(deltay)) >= size.y-1 \
                                 || barrier[((pos.x+(deltax)) + (pos.y+(deltay))*size.x)])
kernel void stream(int2 size, global const lattice *f, global lattice *f0, global uchar *barrier) {
  int2 pos = (int2)(get_global_id(0), get_global_id(1));
  if(pos.x >= size.x || pos.y >= size.y) return;

  if(BARRIER(0, 0)) {
    *CELL(f0, 0, 0) = *CELL(f, 0, 0);
    return;
  }

  char xp = (pos.x >= size.x-1) ? 0 : +1;
  char xm = (pos.x <=     0   ) ? 0 : -1;
  char yp = (pos.y >= size.y-1) ? 0 : +1;
  char ym = (pos.y <=     0   ) ? 0 : -1;

  CELL(f0, 0, 0)->n  = CELL(f, 0, 0)->n;
  CELL(f0, 0, 0)->E  = CELL(f, xm,  0)->E;
  CELL(f0, 0, 0)->S  = CELL(f,  0, ym)->S;
  CELL(f0, 0, 0)->N  = CELL(f,  0, yp)->N;
  CELL(f0, 0, 0)->W  = CELL(f, xp,  0)->W;
  CELL(f0, 0, 0)->SE = CELL(f, xm, ym)->SE;
  CELL(f0, 0, 0)->SW = CELL(f, xp, ym)->SW;
  CELL(f0, 0, 0)->NE = CELL(f, xm, yp)->NE;
  CELL(f0, 0, 0)->NW = CELL(f, xp, yp)->NW;

  if(pos.x == size.x-2) return; // outlet

  if(BARRIER(xm,  0)) { CELL(f0, 0, 0)->E  = CELL(f, xp,  0)->W;}
  if(BARRIER( 0, ym)) { CELL(f0, 0, 0)->S  = CELL(f,  0, yp)->N;}
  if(BARRIER( 0, yp)) { CELL(f0, 0, 0)->N  = CELL(f,  0, ym)->S;}
  if(BARRIER(xp,  0)) { CELL(f0, 0, 0)->W  = CELL(f, xm,  0)->E;}
  if(BARRIER(xm, ym)) { CELL(f0, 0, 0)->SE = CELL(f, xp, yp)->NW;}
  if(BARRIER(xp, ym)) { CELL(f0, 0, 0)->SW = CELL(f, xm, yp)->NE;}
  if(BARRIER(xm, yp)) { CELL(f0, 0, 0)->NE = CELL(f, xp, ym)->SW;}
  if(BARRIER(xp, yp)) { CELL(f0, 0, 0)->NW = CELL(f, xm, ym)->SE;}
}

kernel void rho(int2 size, global const lattice *f, global float *rho, global float2 *u) {
  uint x = get_global_id(0);
  uint y = get_global_id(1);
  uint i = x + y*size.x;
  lattice F = f[i];
  float rho_ = Rho(&F);

  if(rho != 0) {
    rho[x + y*size.x] = rho_;
  }
  if(u != 0) {
    u[x + y*size.x] = (float2)((F.E + F.NE + F.SE - F.W - F.NW - F.SW) / rho_,
                               (F.N + F.NE + F.W  - F.S - F.SE - F.SW) / rho_);
  }
}

kernel void speed(int2 size, global const float *F, global float *speed) {
  uint x = get_global_id(0);
  uint y = get_global_id(1);
  if(x >= size.x || y >= size.y) return;
  
  uint i = x*Q + y*size.x*Q;
  float  n0 = F[i + 0],
    nN = F[i + 1], nS = F[i + 2], nE = F[i + 3], nW = F[i + 4],
    nNW = F[i + 5], nNE = F[i + 6], nSW = F[i + 7], nSE = F[i + 8],
    rho_ = n0 + nN + nS + nE + nW + nNW + nNE + nSW + nSE;

  speed[x + y*size.x] = (fabs(nE + nNE + nSE - nW - nNW - nSW) +
                         fabs(nN + nNE + nNW - nS - nSE - nSW)) / rho_;
}

kernel void corner(int2 size, global lattice *f) {
  int2 pos = (int2)(get_global_id(0), get_global_id(1));
  if(pos.x >= size.x || pos.y >= size.y) return;
  int i = pos.x + pos.y*size.x;
  lattice F = f[i];
  if(pos.x == 0) {
    f[i] = equilibrium((float2)(0.05, 0), Rho(&F));
  }
  /* if(pos.x == size.x/8 && pos.y == size.y/2) { */
  /*   lattice *F = &f[pos.x + pos.y*size.x]; */
  /*   float rho = Rho(F); */
  /*   *F = equilibrium(right, rho); */
  /* } */
  /* else if(pos.x == size.x/2 && pos.y >= 3*size.y/8 && pos.y <= 5*size.y/8) { */
  /*   lattice *F = &f[pos.x + pos.y*size.x]; */
  /*   float rho = Rho(F); */
  /*   *F = equilibrium((float2)(-0.1,0), rho); */
  /* } */
}

kernel void initialize(int2 size, float2 u, float rho, global lattice *f) {
  int2 pos = (int2)(get_global_id(0), get_global_id(1));
  if(pos.x >= size.x || pos.y >= size.y) return;
  int i = pos.x + pos.y*size.x;
  lattice F = f[i];
  f[i] = equilibrium((float2)(u.x, u.y), rho);
}

kernel void visualize(int2 size, float2 r_range, float sscale,
                      global const lattice *f, global const uchar *barrier, global uint *image) {
  int2 pos = (int2)(get_global_id(0), get_global_id(1));
  if(pos.x >= size.x || pos.y >= size.y) return;
  lattice F = f[pos.x + size.x*pos.y];
  float rho = Rho(&F);
  float2 u = (float2)((F.E + F.NE + F.SE - F.W - F.NW - F.SW),
                      (F.N + F.NE + F.NW - F.S - F.SE - F.SW)) / rho;
  //float u = u2.x + u2.y;

  float b = barrier[pos.x + size.x*pos.y];
  int color = 0xff000000;

  if(b) {
    color |= 0xa0a0a0;
  } else {
    color |= (min(255, ((int)(255.0f*(rho-r_range.s0)/(r_range.s1 - r_range.s0)))))<<16;
    // color |= (min(255, ((int)(255.0f*(rho/4.0f)))));

    if(u.x > 0) {
      color |= min(255, ((int)(u.x*sscale)))<<8;
    } else {
      color |= min(255, ((int)(-u.x*sscale)))<<0;
    }
  }

  image[pos.x + pos.y*size.x] = color;
}
