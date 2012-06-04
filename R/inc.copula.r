# fitting a copula


# data should have the list of values for
# which the copula should be evaluated

myMvd <- mvdc(copula = ellipCopula(family = "normal", param = 0.5),
              margins = c("gamma", "gamma"), 
              paramMargins = list(list(shape = 2,scale = 1), 
                                  list(shape = 3,scale = 2)))

mm <- apply(dat, 2, mean)
vv <- apply(dat, 2, var)
b1.0 <- c(mm[1]^2/vv[1], vv[1]/mm[1])
b2.0 <- c(mm[2]^2/vv[2], vv[2]/mm[2])
a.0 <- sin(cor(dat[, 1], dat[, 2], method = "kendall") * pi/2)
start <- c(b1.0, b2.0, a.0)
fit <- fitMvdc(dat, myMvd, start = start, optim.control = list(trace = TRUE, maxit = 2000))

gumbel.cop <- normalCopula(0.6, dim=2)

n <- 200
x <- rcopula(gumbel.cop, n)       ## true observations
u <- apply(x, 2, rank) / (n + 1)  ## pseudo-observations

## inverting Kendall's tau
fit.tau <- fitCopula(gumbel.cop, u, method="itau")
fit.tau

## inverting Spearman's rho
fit.rho <- fitCopula(gumbel.cop, u, method="irho")
fit.rho

## maximum pseudo-likelihood
fit.mpl <- fitCopula(gumbel.cop, u, method="mpl")
fit.mpl

## maximum likelihood
fit.ml <- fitCopula(gumbel.cop, x, method="ml")
fit.ml

