# compute the warshall closure for a matrix
# the code is in C for max efficiency
require('inline')
require('Rcpp')

CODE  = '   
  Rcpp::IntegerVector M(M_); 
  int n = Rcpp::as<int> (n_);
	
  int i,j,k;

  for (j=0; j<n; j++) {
   for (i=0; i<n; i++) {
    if (M[ i + j*n]>0) {
     for (k=0; k<n; k++) {
       if ( M[ j + k*n]>0) {
         M[ i + k*n ] = 1;
       } 
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

warshall.closure.test() {
  n=4
  A = (array(0.5 - runif(n*n),dim=c(n,n)) <0)+0
  B = warshall.closure(A)

  B2 = A
  for (i in 1:n) {
    B2 = B2 %*% B2
  }
  mean(B2 == B)
}




