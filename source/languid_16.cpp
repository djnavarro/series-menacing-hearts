#include <Rcpp.h>
using namespace Rcpp;

// functions to be used internally
double heart_x(double t) {
  double s = sin(t);
  double x = (16.0 * s * s * s)/17.0;
  return x;
}

double heart_y(double t) {
  double c1 = cos(t);
  double c2 = cos(2.0 * t);
  double c3 = cos(3.0 * t);
  double c4 = cos(4.0 * t);
  double y = (13.0 * c1 - 5.0 * c2 - 2.0 * c3 - c4)/17.0;
  return -y;
}

// function to be called from R
// [[Rcpp::export]]
NumericMatrix raster_data(int iter, int layers, int pixels, double zoom, double alpha) {
  
  NumericMatrix image(pixels, pixels); // initially zero
  NumericMatrix coeffs(9, layers);
  
  // set coefficients
  for(int i = 0; i < 9; i++) {
    for(int j = 0; j < layers; j++) {
      coeffs(i, j) = R::runif(-.5, .5);
    }
  }
  
  // set image matrix to zeros
  for(int r = 0; r < pixels; r++) {
    for(int c = 0; c < pixels; c++) {
      image(c, r) = 0;
    }
  }
  
  // iterate
  int layer;
  int variant;

  // convenience variables
  double s = 0;
  double u = 0;

  // plot
  double x_plot = 0;
  double y_plot = 0;
  
  // variables needed for polygon transform [this can probably be deleted]
  double n_sides = 100;
  double radial_step = 6.2832 / n_sides;
  NumericVector x_vertices(n_sides + 1);
  NumericVector y_vertices(n_sides + 1);
  for(int i = 0; i <= n_sides; i++) {
    x_vertices(i) = cos(radial_step * i + radial_step / 2);
    y_vertices(i) = sin(radial_step * i + radial_step / 2);
  }
  int v;
  double n_steps;
  double partial;
  
  // store polar coordinates
  double radians;
  double radius;
  
  // indices for storing coordinates
  int x_ind;
  int y_ind;

  // indices for transform
  double a;
  double b;
  
  // values for current state
  double x = 0;
  double y = 0;
  double z = 0;
  
  // values for previous state
  double x_old = R::runif(-1, 1);
  double y_old = R::runif(-1, 1);
  double z_old = R::runif(-1, 1);
  
  // value for r
  double r = R::runif(1, 1.5);
  
  // iterate...
  for(int it = 1; it < iter; it++) {
    
    layer = rand() % layers;   // which affine transform to use?
    variant = rand() % 5;      // which variant function to use?
    
    // coordinates after random transform
    x = x_old + coeffs(0, layer) * x_old + coeffs(1, layer) * y_old + coeffs(2, layer);
    y = y_old + coeffs(3, layer) * x_old + coeffs(4, layer) * y_old + coeffs(5, layer);
    z = z_old + coeffs(6, layer) * x_old + coeffs(7, layer) * y_old + coeffs(8, layer);

    // apply function to the transformed coordinates
    if(variant == 0) {
      s = x*x + y*y + z*z;
      x = x/s;
      y = y/s;
      z = z/s;
    } else if (variant == 1) {
      s = sin(x + y) * 3.14 * 2;
      z = sin(x + y);
      x = heart_x(s) * r * (iter - it)/iter;
      y = heart_y(s) * r * (iter - it)/iter;
    } else if (variant == 2){
      s = sin(x + y) * 3.14 * 2;
      u = x / (x + y);
      x = cos(s) * 2 * u;
      y = sin(s) * 2 * u;
      z = sin(z) * 2;
    } else if (variant == 3) {
      x = cos(x) * 2;
      y = cos(y) * 2;
      z = cos(z) * 3;
    } else {
      s = sin(x + y) * 3.14 * 2;
      x = heart_x(s) * r * (iter - it)/iter / 2;
      y = heart_y(s) * r * (iter - it)/iter / 2;
      z = sin(z);
    }

    // simple
    x_plot = x;
    y_plot = y;

    // compute indices to be updated
    x_ind = int (x_plot * pixels * zoom) + pixels/2;
    y_ind = int (y_plot * pixels * zoom) + pixels/2;
    
    // store results if they fall within the range
    if(x_ind >= 0 & x_ind < pixels) {
      if(y_ind >= 0 & y_ind < pixels) {
        image(x_ind, y_ind) = alpha * z + (1- alpha) * image(x_ind, y_ind);
      }
    }
    
    // move new to old
    x_old = x;
    y_old = y;
    z_old = (z + z_old)/2; 
  }
  
  return image;
}


