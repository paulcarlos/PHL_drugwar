# quasi-Akaike Information Criterion (q-AIC) and quasi-Bayesian Information Criterion (q-BIC) 
# for quasi-Poisson models

# QAIC & QBIC function
qaicbic <- function(model) {
  phi <- summary(model)$dispersion
  logll <- sum(dpois(model$y, model$fitted.values, log=TRUE))
  #logll <- -deviance(model)
  # qaic
  cbind((-2*logll + 2*phi*summary(model)$df[3]),
        # qbic
        (-2*logll + log(length(resid(model)))*phi*summary(model)$df[3]))
}