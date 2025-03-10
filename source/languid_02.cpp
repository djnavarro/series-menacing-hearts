#include <Rcpp.h>
using namespace Rcpp;

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

  // plot
  double x_plot = 0;
  double y_plot = 0;
  
  // variables needed for polygon transform
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
  
  // iterate...
  for(int it = 1; it < iter; it++) {
    
    layer = rand() % layers;   // which affine transform to use?
    variant = rand() % 4;      // which variant function to use?
    
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
      x = sin(x);
      y = sin(y);
      z = sin(z);
    } else if (variant == 2){
      x = sin(x) * 2;
      y = sin(y) * 2;
      z = sin(z) * 2;
    } else {
      x = cos(x) * 3;
      y = cos(y) * 3;
      z = cos(z) * 3;
    }
    
    // conversion to polar coordinates
    radians = 2 * atan(y / x); 
    if(fabs(x) > fabs(y)) {
      radius = fabs(x);
    } else {
      radius = fabs(y);
    }
    
    // project onto polygon
    while(radians < 0) {
      radians = radians + 6.2832;
    }
    n_steps = radians / radial_step;
    partial = n_steps - floor(n_steps);
    v = int (floor(n_steps));
    
  
    // determine how to plot based on polar coords and variant
    if(variant == 0) {
      if(x*x + y*y < 1) {
        x_plot = partial * x_vertices[v + 1] + (1 - partial) * x_vertices[v];
        y_plot = partial * y_vertices[v + 1] + (1 - partial) * y_vertices[v];
        x_plot = radius * x_plot;
        y_plot = radius * y_plot;
      } else {
        x_plot = x_plot * 1.5;
        y_plot = y_plot * 1.5;
      }
    } else {
      x_plot = partial * x_vertices[v + 1] + (1 - partial) * x_vertices[v];
      y_plot = partial * y_vertices[v + 1] + (1 - partial) * y_vertices[v];
      x_plot = radius * x_plot;
      y_plot = radius * y_plot;
    }

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


