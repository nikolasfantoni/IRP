#Função de Pooling Máximo com stride 2

maxPool <- function(X,sizeofpool){

  if (dim(X)[1]%%sizeofpool!=0){
    aux <- matrix(0,ncol=dim(X)[2],nrow=dim(X)[1]%%sizeofpool)
    X <- rbind(X,aux)
  }
  if (dim(X)[2]%%sizeofpool!=0){
    aux <- matrix(0,ncol=dim(X)[2]%%sizeofpool,nrow=dim(X)[1])
    X <- cbind(X,aux)
  }
  n <- dim(X)[1]
  m <- dim(X)[2]

  Xout <- NULL
  iseq <- seq(1,n,2)
  jseq <- seq(1,m,2)
  for (i in iseq){
      for (j in jseq){
        newm <- matrix(X[i:(i+sizeofpool-1),j:(j+sizeofpool-1)],
                       nrow=sizeofpool,ncol=sizeofpool)
        Xout <- c(Xout,max(max(newm)))
    }
  }
  Xout <- matrix(data=Xout,nrow=sqrt(length(Xout)),ncol=sqrt(length(Xout)),byrow=TRUE)
  return(Xout)
}