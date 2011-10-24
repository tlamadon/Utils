
means <- function(x) {
  sfit = summary(lm(x~1))
  return(list(mean=sfit$coef[1],sem=sfit$coef[2],sd=sd(x)))
}

list2df <- function(ll) {
 return(ldply(ll,function(l){ return(data.frame(rbind(unlist(l))))}))
}

