
means <- function(x) {
  sfit = summary(lm(x~1))
  return(data.frame(mean=sfit$coef[1],sem=sfit$coef[2],pval=sfit$coef[4],sd=sd(x)))
}



