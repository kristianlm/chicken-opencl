// taken from https://www.intel.com/content/www/us/en/support/programmable/support-resources/design-examples/horizontal/mandelbrot.html

// Copyright (C) 2013-2018 Altera Corporation, San Jose, California, USA. All rights reserved.
// Permission is hereby granted, free of charge, to any person obtaining a copy of this
// software and associated documentation files (the "Software"), to deal in the Software
// without restriction, including without limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to
// whom the Software is furnished to do so, subject to the following conditions:
// The above copyright notice and this permission notice shall be included in all copies or
// substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
// WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
// OTHER DEALINGS IN THE SOFTWARE.
//
// This agreement shall be governed in all respects by the laws of the State of California and
// by the laws of the United States of America.

#pragma OPENCL EXTENSION cl_khr_fp64 : enable

__kernel void mandelbrot (
  const double x0, const double y0,
  const double stepSize,
  const uint maxIterations,
  __global uchar *restrict framebuffer,
  // __constant const unsigned int *restrict colorLUT,
  const uint windowWidth) {

  const size_t windowPosX = get_global_id(0);
  const size_t windowPosY = get_global_id(1);
  const double stepPosX = x0 + (windowPosX * stepSize);
  const double stepPosY = y0 - (windowPosY * stepSize);

  double x = 0.0;
  double y = 0.0;
  double xSqr = 0.0;
  double ySqr = 0.0;
  uint iterations = 0;

#pragma unroll 20
  while (xSqr + ySqr < 4.0 && ++iterations < maxIterations) {
    xSqr = x*x;
    ySqr = y*y;

    y = 2*x*y + stepPosY;
    x = xSqr - ySqr + stepPosX;
  }

  framebuffer[windowWidth * windowPosY + windowPosX] =
    (iterations >= maxIterations) ? 0 : min((uint)255, iterations);
}
