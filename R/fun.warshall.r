# compute the warshall closure for a matrix
# the code is in C for max efficiency
require('inline')
require('Rcpp')

CODE  = '   
  Rcpp::IntegerVector M(M_); 
  int n = Rcpp::as<int> (n_);
	
  int i,j,k;

  for (i=0; i<n; i++) {
   for (j=0; j<n; j++) {
    if (M[ i + j*n]>0) {
     for (k=0; k<n; k++) {
       M[ i + k*n ] = M[ i +j*n ] + M[ j + k*n];
     }
    }	    
   }
  }

  return M;
'

warshall.closure.tmp = cxxfunction(signature(M_ = "int",n_ = "int") , CODE, verbose=TRUE, plugin = 'Rcpp')

warshall.closure <- function(M) {
  D = dim(M)
  R = warshall.closure.tmp(M,D[1])
  return( array(R,dim=D))
}






