RUMresulting <- function(X, Y, coef) {
  Yprob <- X %*% coef[2:(p+2)] + coef[1]
  LDT <- X[,1:p] %*% coef[2:(p+1)] + coef[1]
  Ypred <- ifelse(Yprob > 0.5, 1, 0)
  Result <- data.frame(Yreal = Y, LDT = -LDT, Yprob = Yprob, Ypred = Ypred)
  return(Result) }

LDTresulting <- function(X, R, Y, coef) {
  LDT <- X %*% coef[2:(p+1)] + coef[1]
  Ypred <- ifelse(R>LDT, 1, 0)
  Result <- data.frame(Yreal = Y, LDT = LDT, Ypred = Ypred) 
  return (Result)}