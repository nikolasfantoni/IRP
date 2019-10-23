#Convolucao

convolucao <- function(X,tf,f){
  img <- MostraImagem(X)
  dimx <- dim(img)[1]
  dimy <- dim(img)[2]
  M <- matrix(0,nrow=(dimx-2),ncol=(dimy-2))
  
  for (n in 1:(dimx-tf)){
    for (m in 1:(dimy-tf)){
      M[n,m] <- sum(img[n:(n+2),m:(m+2)] * f)
    }
  }
  return(M)
}