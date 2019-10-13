#Função KDE
kde <- function(x1, data, h){
  n <- length(data[1,])
  N <- length(data[,1])
  tempall <- NULL
  for (j in 1:N){
    temp <- NULL
    for (i in 1:n){
      temp <- c(temp, (1/(sqrt(2*pi)*h))*exp(-((x1[i]-data[j,i])^2)/(2*(h*h))))
    }
    tempall <- c(tempall,(prod(temp)/N))
  }
  temp <- (sum(tempall))
  return(temp)
}