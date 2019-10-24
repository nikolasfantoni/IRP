#Convolucao

convolucao <- function(X,tf,f){
  dimx <- dim(X)[1]
  dimy <- dim(X)[2]
  M <- matrix(0,nrow=(dimx-2),ncol=(dimy-2))
  
  for (n in 1:(dimx-tf+1)){
    for (m in 1:(dimy-tf+1)){
      M[n,m] <- sum(X[n:(n+2),m:(m+2)] * f)
    }
  }
  return(M)
}