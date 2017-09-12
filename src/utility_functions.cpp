#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

//' @importFrom Rcpp sourceCpp
//' @useDynLib esback
//' @keywords internal
// [[Rcpp::export]]
Rcpp::NumericMatrix stationary_bootstrap_indices(int n, double avg_block_size, int B) {
  Rcpp::NumericMatrix indices(n, B);
  int start_index, block_size;

  for (int b; b < B; b++) {
    int idx = 0;
    while (idx < n) {
      // Draw a random starting index
      start_index = Rcpp::as<int>(Rcpp::runif(1, 0, n));

      // Draw the block length
      block_size = 1 + Rcpp::as<int>(Rcpp::rgeom(1, 1 / avg_block_size));

      // Add the indices
      for (int i = start_index; i < start_index + block_size; i++) {
        // Wrap in a circle
        if (idx < n) {
          if (i < n) {
            indices(idx, b) = i;
          } else {
            indices(idx, b) = i - n;
          }
        }
        idx += 1;
      }
    }
  }
  return indices;
}
