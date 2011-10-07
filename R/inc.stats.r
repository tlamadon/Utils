
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

