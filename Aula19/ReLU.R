ReLU <- function(X){
  n <- dim(X)[1]
  m <- dim(X)[2]
  Xout <- matrix(data=0,nrow=n,ncol=m)
  for (i in 1:n){
    for (j in 1:m){
      if (X[i,j] > 0){
        Xout[i,j] <- X[i,j]
      } else {
        Xout[i,j] <- 0
      }
    }
  }
  return(Xout)
}