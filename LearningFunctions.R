library(CVXR)

# Input: Data, the dataset for training, col 1~p are the attributes x, p+1 is Y, an column named ID
RUMs <- function(Data,N,p){
  print("Learning indiviudal RUM models")
  CoefRUM <- matrix(0, nrow=N, ncol=(p+1))
  colnames(CoefRUM) <- c('Intercept', colnames(Data)[1:p])
  for (i in 1:N){
    iTrain <- subset(Data, Data$ID==i)[,1:(p+1)]
    iTrain$Y[iTrain$Y== -1]<- 0
    mRUM <- glm(Y~., data=iTrain, family=binomial(link="logit"))
    CoefRUM[i,] <- coefficients(mRUM) }
  CoefRUM[is.na(CoefLog)] <- 0
  return(CoefRUM) }

# Input:
LDT <- function(X,R,Y,C) {
  xi <- Variable(tr)
  bt <- Variable(p)
  bt0<- Variable(1)
  objective<- Minimize(sum(bt^2)+C*sum(xi))
  classification <- (Y*(R-bt0-X%*%bt)>=1-xi)
  nonnegative <- (xi>=0)
  prob <-Problem(objective,list(classification,nonnegative))
  result <-solve(prob)
  return(c(result$getValue(bt0),result$getValue(bt)))}

LDTs <- function(Data,N,p,C){
  print("Learning LDT models")
  CoefLDT <- matrix(0,nrow=N,ncol=(p+1))
  colnames(CoefLDT) <- c('Intercept', colnames(Data)[1:p])
  for ( i in 1:N ) {
    iTrain <- subset(Data, Data$ID ==i)
    X<-as.matrix(iTrain[,1:p])
    R<-as.vector(iTrain[,(p+1)])
    Y<-as.vector(iTrain[,(p+2)])
    Y[Y==0]<- -1
    CoefLDT <- LDT(X,R,Y,C) }
  CoefLDT[is.na(CoefLDT)]<-0 
  return(CoefLDT) }

LDTresulting <- function(X, R, Y, coef) {
  LDT <- X %*% coef[2:(p+1)] + coef[1]
  Ypred <- ifelse(R>LDT, 1, 0)
  Result <- data.frame(Yreal = Y, LDT = LDT, Ypred = Ypred) 
  return (Result)}

LDTcv <- function(Data,N,p,Clist){
  print("Learning LDT models")
  CoefLDT <- matrix(0,nrow=N,ncol=(p+1))
  colnames(CoefLDT) <- c('Intercept', colnames(Data)[1:p])
  for ( i in 1:N ) {
    iTrain <- subset(Data, Data$ID ==i)
    Tr <- dim(iTrain)[1]
    shuffledindex <- sample(Tr)
    groupsize <- round(Tr/5)
    g <- vector("list")
    g[[1]] <- shuffledindex[1:groupsize]
    g[[2]] <- shuffledindex[(groupsize+1):(2*groupsize)]
    g[[3]] <- shuffledindex[(2*groupsize+1):(3*groupsize)]
    g[[4]] <- shuffledindex[(3*groupsize+1):(4*groupsize)]
    g[[5]] <- shuffledindex[(4*groupsize+1):length(shuffledindex)]
    CVresult <- matrix(0,nrow=Clist,ncol=5)
    for (j in 1:length(Clist)){
      C <- Clist[j]
      for (k in 1:5) {
        validating <- g[[k]]
        training <- setdiff(shuffledindex,validating)
        X <- as.matrix(iTrain[training,1:p])
        R <- as.vector(iTrain[training,(p+1)])
        Y <- as.vector(iTrain[training,(p+2)])
        Y[Y==0]<- -1
        coef <- LDT(C/length(Y))
        Y <- as.vector(iTrain[validating,(p+2)])
        Y[Y==0] <- -1
        Result <- LDTresulting(as.matrix(iTrain[validating,1:p]), as.vector(iTrain[validating,(p+1)]), Y, coef)
        CVresult[j,k] <- mean(Result$Yreal==Result$Ypred) } }
    valid <- apply(CVresult,1,mean)
    Copt <- round(median(which(valid==max(valid))))
    CoefLDT[i,] <- LDT(C/Tr) }
   return(CoefLDT) }

