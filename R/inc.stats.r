
means <- function(x) {
  sfit = summary(lm(x~1))
  return(data.frame(mean=sfit$coef[1],sem=sfit$coef[2],pval=sfit$coef[4],sd=sd(x)))
}

wt.summary <- function(x, w, na.rm = FALSE) {
  if (na.rm) {
      w <- w[i <- !is.na(x)]
      x <- x[i]
  }
  sum.w <- sum(w)
  sum.w2 <- sum(w^2)
  mean.w <- sum(x * w) / sum(w)

  return(list(mean=mean.w,var= (sum.w / (sum.w^2 - sum.w2)) * sum(w * (x - mean.w)^2, na.rm =na.rm)))
}


wt.cor <- function(x,y,w) {
 # we first compute the rank
 x <- rank(x) / (length(x) + 1)  ## pseudo-observations
 y <- rank(y) / (length(y) + 1)  ## pseudo-observations
 
 r  = cov.wt( cbind(x,y),w,cor=TRUE)
 return(r$cor[2,1])
}

wt.hist <- function(x,w,b) {
  # compute the ECDF
  I = order(x)
  F = cumsum(w[I])

  # find the bins
  bs = seq(min(x),max(x),l=b)
  i=1
  


}

getNormCop <- function(rho,n,Qn= seq(1/n,1-1/n,l=n),cond=FALSE) {

  require(copula)
  cop = normalCopula(rho) 

  #create the grid
  vals = expand.grid(p=Qn,p2=Qn)
  
  # apply the copula
  vals$v = dcopula(cop,as.matrix(vals))
  G = array(vals$v,dim=c(n,n)) 

  # making it conditional
  if (cond) {
    G = t(apply(G,1,function(v) { return(v/sum(v)) }))
  }

  return(G)
}

getNormPdf <- function(sigma, n) {
  Z  = as.array(qnorm(seq(1/n , 1-1/n,l=n),sd=sigma))
  G  = as.array(dnorm(Z),sd=sigma); G=G/sum(G)
  return(list(vals=Z,mass=G))
}


# copied from MSBVAR
rmultnorm <- function (n, mu, vmat, tol = 1e-10) 
{
    p <- ncol(vmat)
    if (mu==0) mu = rep(0,p);

    if (length(mu) != p) 
        stop(paste("mu vector is the wrong length:", length(mu)))
    vs <- La.svd(vmat)
    vsqrt <- t(t(vs$vt) %*% (t(vs$u) * sqrt(vs$d)))
    ans <- matrix(rnorm(n * p), nrow = n) %*% vsqrt
    ans <- sweep(ans, 2, mu, "+")
    dimnames(ans) <- list(NULL, dimnames(vmat)[[2]])
    ans
}

