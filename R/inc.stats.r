
means <- function(x) {
  sfit = summary(lm(x~1))
  return(data.frame(mean=sfit$coef[1],sem=sfit$coef[2],sd=sd(x)))
}



