# the following 2 functions combine Clenshaw Curtis
# and Checbyshev interpolation.
# The function is computed at the Chebyshev-Gauss-Lobatto points
# which are not the roots but the extrema of the polynomial.
#
# see <http://mathdl.maa.org/images/upload_library/4/vol6/Sarra/Chebyshev.html>
#
# to get the nodes and weights for CC intregration use cc.quad
# Q = cc.quad(n)
# Q$nodes are the nodes and Q$weights are the weights
#
# given the value of a function f at the nodes of Q, you can easily
# compute the integral by doing:
# Q$weights %*% f
# ( note that %*% is just the matrix multiplication in R )
#
# the second thing you might want to do is interpolate f. To
# do so, you can precompute an evaluation matrix by calling
# cc.interp
#
# E = cc.interp(vs,Q)
# where vs is the set of interpolation points (Q comes from before)
# then whenever you want to evaluate a represenation f at those
# points just run 
# y = E %*% f
# this will give you your interpolation
#
# the first function is adapted from the Matlab code from 
# Copyright (c) 2009, Greg von Winckel
# <http://www.mathworks.com/matlabcentral/fileexchange/authors/11897>

cc.quad <- function(N1,a=-1,b=1) {
  N=N1-1; 
  bma=b-a;
  M=array(0,dim=c(N1,2))
  M[seq(1,N1,by=2),1]= t(2/c(1, 1-seq(2,N,by=2)^2 )); 
  M[2,2]=1;
  f1=Re(fft(c(M[1:N1,1],M[N:2,1]),inverse=TRUE));
  f2=Re(fft(c(M[1:N1,2],M[N:2,2]),inverse=TRUE));
  f1 = f1/length(f1)
  f2 = f2/length(f2)
  w=bma*(c(f1[1], 2*f1[2:N], f1[N1]))/2;
  x=0.5*((b+a)+N*bma*f2[1:N1]);

  return(list(weights=w,nodes=x,a=a,b=b))
}


# returns a matrix that multiplied
# by f at the nodes gives you 
# the interpolated values at vs
# the vs need to be between [-1,1]
cc.interp <- function(vs,Q) {
  
  r = length(vs)
  n = length(Q$weights)
  N = n-1
  # we have n points in our grid
  # and so we use poly up to dgree N=n-1
  # which gives us exactly n polynomials
  
  # we compute the value of the Chebyshev
  # polynomial at each of those new values
  
  # get the Chebychev polynomial
  P = chebyshev.t.polynomials(N)
  
  # evaluate them on the original grid
  nodes = 2*(Q$nodes -Q$a)/(Q$a-Q$b) -1
  E = polynomial.values(P, nodes)
  E = 2/N*t(array(unlist(E),dim=c(n,n)))
  E[,1]=E[,1]/2
  E[,n]=E[,n]/2

  # evaluate them on the original grid
  nodes = 2*(vs-Q$a)/(Q$a-Q$b) -1
  E2 = polynomial.values(P, nodes)
  E2 = array(unlist(E2),dim=c(r,n))
  E2[,1] = E2[,1]/2
  
  # finish by creating the matrix that evaluates
  # this should be r x n
  return(E2 %*% E)
}

cc.interp.test <-function() {
  
  n=5
  vs = seq(0,5,l=15)
  Q = cc.quad(n,0,8)
  f= exp(Q$nodes)
  E = cc.interp(vs,Q)

  # interpolate using E x f
  plot(E%*%f)

  # integrate using Qw x f
  Q$weights %*% f

  # done !
}


