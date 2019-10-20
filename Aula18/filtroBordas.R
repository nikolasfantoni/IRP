#Filtro bordas

filtroBordas <- function(X,tf){
  img <- MostraImagem(X)
  dimx <- dim(img)[1]
  dimy <- dim(img)[2]
  f <- matrix(c(-1,-1,-1,-1,8,-1,-1,-1,-1),nrow=3, ncol = 3)
  M <- matrix(0,nrow=(dimx-2),ncol=(dimy-2))
  
  for (n in 1:(dimx-tf)){
    for (m in 1:(dimy-tf)){
      M[n,m] <- sum(img[n:(n+2),m:(m+2)] * f)
    }
  }
  return(M)
}