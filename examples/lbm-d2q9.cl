// -*- c -*-
#define Q 9
// TODO: seprate into bigass
// void collide(float omega, private float *n0, private float *nN, ... )

typedef struct lattice {
  float n, E, W, N, S,
    NE, SE, NW, SW;
} lattice;

kernel void collide(int2 size, float omega, global lattice *f) {
  uint x = get_global_id(0);
  uint y = get_global_id(1);
  if(x >= size.x || y >= size.y) return;
  uint i = (x + y*size.x);
  lattice *F = &f[i];//  n0 = F[i + 0], nE = F[i + 1], nW = F[i + 2], nN = F[i + 3], nS = F[i + 4], nNE = F[i + 5], nSE = F[i + 6], nNW = F[i + 7], nSW = F[i + 8];
  float rho = F->n + F->N + F->S + F->E + F->W + F->NW + F->NE + F->SW + F->SE;
  float2 u = (float2)((F->E + F->NE + F->SE - F->W - F->NW - F->SW) / rho,
                      (F->N + F->NE + F->NW - F->S - F->SE - F->SW) / rho);

  float one9thrho = rho / 9.0;
  float one36thrho = rho / 36.0;
  float ux3 = 3 * u.x;
  float uy3 = 3 * u.y;
  float ux2 = u.x * u.x;
  float uy2 = u.y * u.y;
  float uxuy2 = 2 * u.x * u.y;
  float u2 = ux2 + uy2;
  float u215 = 1.5 * u2;
  F->n  += omega * ((4.0/9.0)*rho * (1                             - u215) - F->n);
  F->E  += omega * (   one9thrho * (1 + ux3       + 4.5*ux2        - u215) - F->E);
  F->W  += omega * (   one9thrho * (1 - ux3       + 4.5*ux2        - u215) - F->W);
  F->N  += omega * (   one9thrho * (1 + uy3       + 4.5*uy2        - u215) - F->N);
  F->S  += omega * (   one9thrho * (1 - uy3       + 4.5*uy2        - u215) - F->S);
  F->NE += omega * (  one36thrho * (1 + ux3 + uy3 + 4.5*(u2+uxuy2) - u215) - F->NE);
  F->SE += omega * (  one36thrho * (1 + ux3 - uy3 + 4.5*(u2-uxuy2) - u215) - F->SE);
  F->NW += omega * (  one36thrho * (1 - ux3 + uy3 + 4.5*(u2-uxuy2) - u215) - F->NW);
  F->SW += omega * (  one36thrho * (1 - ux3 - uy3 + 4.5*(u2+uxuy2) - u215) - F->SW);
}

#define CELL(f, vec) (&f[((vec).x + (vec).y*size.x)])
kernel void stream(int2 size, global const lattice *f, global lattice *f0) {
  int2 pos = (int2)(get_global_id(0), get_global_id(1));
  if(pos.x >= size.x || pos.y >= size.y) return;

  float xp = (pos.x >= size.x-1) ? 0 : +1;
  float xm = (pos.x <=     0   ) ? 0 : -1;
  float yp = (pos.y >= size.y-1) ? 0 : +1;
  float ym = (pos.y <=     0   ) ? 0 : -1;

  lattice *F = CELL(f0, pos);
  F->n  = CELL(f, pos)->n;
  F->E  = CELL(f, (pos + (int2)(xm, 0)))->E;
  F->S  = CELL(f, (pos + (int2)( 0,ym)))->S;
  F->N  = CELL(f, (pos + (int2)( 0,yp)))->N;
  F->W  = CELL(f, (pos + (int2)(xp, 0)))->W;
  F->SE = CELL(f, (pos + (int2)(xm,ym)))->SE;
  F->SW = CELL(f, (pos + (int2)(xp,ym)))->SW;
  F->NE = CELL(f, (pos + (int2)(xm,yp)))->NE;
  F->NW = CELL(f, (pos + (int2)(xp,yp)))->NW;
}

kernel void rho(int2 size, global const float *F, global float *rho, global float2 *u) {
  uint x = get_global_id(0);
  uint y = get_global_id(1);
  uint i = x*Q + y*size.x*Q;
  float  n0 = F[i + 0];
  float  nN = F[i + 1];
  float  nS = F[i + 2];
  float  nE = F[i + 3];
  float  nW = F[i + 4];
  float nNW = F[i + 5];
  float nNE = F[i + 6];
  float nSW = F[i + 7];
  float nSE = F[i + 8];
  float rho_ = n0 + nN + nS + nE + nW + nNW + nNE + nSW + nSE;

  if(rho != 0) {
    rho[x + y*size.x] = rho_;
  }
  if(u != 0) {
    u[x + y*size.x] = (float2)((nE + nNE + nSE - nW - nNW - nSW) / rho_,
                               (nN + nNE + nNW - nS - nSE - nSW) / rho_);
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

lattice equilibrium(float2 u, float newrho) {
  float ux3 = 3 * u.x;
  float uy3 = 3 * u.y;
  float ux2 = u.x * u.x;
  float uy2 = u.y * u.y;
  float uxuy2 = 2 * u.x * u.y;
  float u2 = ux2 + uy2;
  float u215 = 1.5 * u2;

  float one9th = 1/9.0, one36th = 1/36.0;
  
  return (lattice){
     .n=   (4.0/9) * newrho * (1                              - u215),
     .E=    one9th * newrho * (1 + ux3       + 4.5*ux2        - u215),
     .W=    one9th * newrho * (1 - ux3       + 4.5*ux2        - u215),
     .N=    one9th * newrho * (1 + uy3       + 4.5*uy2        - u215),
     .S=    one9th * newrho * (1 - uy3       + 4.5*uy2        - u215),
    .NE=   one36th * newrho * (1 + ux3 + uy3 + 4.5*(u2+uxuy2) - u215),
    .SE=   one36th * newrho * (1 + ux3 - uy3 + 4.5*(u2-uxuy2) - u215),
    .NW=   one36th * newrho * (1 - ux3 + uy3 + 4.5*(u2-uxuy2) - u215),
    .SW=   one36th * newrho * (1 - ux3 - uy3 + 4.5*(u2+uxuy2) - u215)};
}

constant float2 right = (float2)(-0.5, 0);

kernel void corner(int2 size, global lattice *f) {
  int2 pos = (int2)(get_global_id(0), get_global_id(1));
  if(pos.x >= size.x || pos.y >= size.y) return;
  
  if(pos.x == size.x/2 && pos.y == size.y/2) {
    lattice *F = &f[pos.x + pos.y*size.x];
    float rho = F->n + F->E + F->W + F->N + F->S + F->NE + F->SE + F->NW + F->SW;
    printf("rho %F\n", rho);
    *F = equilibrium(right, rho);
  }
}
