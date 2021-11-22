(import srfi-1 srfi-4 opencl nrepl srfi-18 chicken.gc
        (only chicken.condition print-error-message condition->list))
(foreign-declare "

#include <stdlib.h>
#include <stdio.h>
#include <sys/stat.h>

#include <raylib.h>
#include <rlgl.h>

int WIDTH = -1;
int HEIGHT = -1;

RenderTexture2D t0 = {};
unsigned long frame = 0;

int myinit() {
  SetConfigFlags(FLAG_WINDOW_RESIZABLE);
  InitWindow(1024, 800 + 21, \"lbm-d2q9 [floating]\");
  SetWindowPosition(880, 80);
  SetTargetFPS(60);
}

int myloop(float scale) {
  if(WindowShouldClose()) return 0;
  
  BeginDrawing(); {
    ClearBackground(BLACK);
    Vector2 pos = {0, 0};
    DrawTextureEx(t0.texture, pos, 0, scale, WHITE);
    // DrawText(FormatText(\"fps %d f%d\", GetFPS(), frame++), 1, HEIGHT*scale, 20, MAGENTA);
  } EndDrawing();

  return 1;
}

int myend() {
  UnloadRenderTexture(t0);
  CloseWindow();
}

void equilibrium(float *dest, float ux, float uy, float newrho) {
  float ux3 = 3 * ux;
  float uy3 = 3 * uy;
  float ux2 = ux * ux;
  float uy2 = uy * uy;
  float uxuy2 = 2 * ux * uy;
  float u2 = ux2 + uy2;
  float u215 = 1.5 * u2;

  float one9th = 1/9.0, one36th = 1/36.0;
  

 dest[0]/*n */ =  (4.0/9) * newrho * (1                              - u215);
 dest[1]/*E */ =   one9th * newrho * (1 + ux3       + 4.5*ux2        - u215);
 dest[2]/*W */ =   one9th * newrho * (1 - ux3       + 4.5*ux2        - u215);
 dest[3]/*N */ =   one9th * newrho * (1 - uy3       + 4.5*uy2        - u215);
 dest[4]/*S */ =   one9th * newrho * (1 + uy3       + 4.5*uy2        - u215);
 dest[5]/*NE*/ =   one36th * newrho * (1 + ux3 - uy3 + 4.5*(u2-uxuy2) - u215);
 dest[6]/*SE*/ =   one36th * newrho * (1 + ux3 + uy3 + 4.5*(u2+uxuy2) - u215);
 dest[7]/*NW*/ =   one36th * newrho * (1 - ux3 - uy3 + 4.5*(u2+uxuy2) - u215);
 dest[8]/*SW*/ =   one36th * newrho * (1 - ux3 + uy3 + 4.5*(u2-uxuy2) - u215);
}
")

((foreign-lambda bool "myinit"))

(define (equilibrium u rho)
  (let ((dest (make-f32vector 9)))
   ((foreign-lambda void "equilibrium" f32vector float float float)
    dest (real-part u) (imag-part u) rho)
   dest))

(define myloop (foreign-lambda bool "myloop" float))
(define mypixels (foreign-lambda* bool ((int w) (int h) (u32vector pixels)) "
 if(WIDTH != w || HEIGHT != h) {
   WIDTH = w;
   HEIGHT = h;
   UnloadRenderTexture(t0);
   t0 = LoadRenderTexture(WIDTH, HEIGHT);
 }
 UpdateTexture(t0.texture, pixels);
"))

(define fps (foreign-lambda int "GetFPS"))
(define mouse-x               (foreign-lambda int "GetMouseX"))
(define mouse-y               (foreign-lambda int "GetMouseY"))
(define mouse-button-pressed? (foreign-lambda bool "IsMouseButtonPressed" int))
(define mouse-button-down?    (foreign-lambda bool "IsMouseButtonDown" int))

(define draw-text (foreign-lambda* void ((c-string txt) (int x) (int y) (int size) (int32 c32))
                                   "Color color = {.r= (c32>>0)&0xFF,.g= (c32>>8)&0xFF,.b= (c32>>16)&0xFF,.a= (c32>>24)&0xFF};"
                                   "DrawText(txt, x, y, size, color);"))

(define key-down?      (foreign-lambda bool "IsKeyDown" int))
(define key-pressed?   (foreign-lambda bool "IsKeyPressed" int))

(thread-start! (lambda ()
                 (eval ` (import srfi-1 srfi-4 opencl nrepl srfi-18 chicken.gc))
                 (eval ` (import srfi-4 opencl fmt clojurian.syntax test srfi-18
                                 chicken.string
                                 chicken.port
                                 chicken.file.posix
                                 chicken.time
                                 chicken.condition
                                 chicken.process-context
                                 stb-image-write stb-image))
                 (nrepl 1234)))

(define scale 1)
(define (game-loop) #f)

(include "lbm-d2q9.scm")

(let loop ()
  (handle-exceptions
      e (begin (fmt #t (pretty (print-error-message e)))
               (set! game-loop (lambda () #f)))
      (game-loop))
  (thread-yield!)
  (when (myloop scale)
    (loop)))

((foreign-lambda bool "myend"))
