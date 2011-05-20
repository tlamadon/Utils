# n must be odd
# computes Clenshaw Curtis quadrature nodes

cc.quad <-function(np) {
  N=np-1
  r = list()
  n = N/2
  
  # computing the nodes
  no = cos( (0:n) * pi / (2*n))
  r$nodes = c(-no ,no[(n):1])

  # computing the weights
  D = array(0,dim=c(n+1,n+1))
  for (i in 0:n) {
    for (j in 0:n) {
      D[i+1,j+1] = 2/N * cos( (i *j*pi)/(N/2))
    }
  }
  D[,1] = D[,1]/2
  D[,n+1] = D[,n+1]/2

  d = array(1,dim=c(n+1))
  for (i in 1:(n-1)) {
    d[i+1] = 2/(1-(2*i)^2)
  }
  d[n+1] = 1/(1-(2*n)^2)

  # need to mult by 1/2 for some weigths
  w = t(D)%*%d
  r$weights = c(w[1:n],w[n+1]*2,w[n:1]) 
  return(r)
}
