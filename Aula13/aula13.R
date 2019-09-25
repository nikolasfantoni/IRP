#Universidade Federal de Minas Gerais
#Introducao ao Reconhecimento de Padroes
#Nikolas Dias Magalhaes Fantoni
#AULA 13 - KDE
#2019/2

#Limpando o ambiente
rm(list=ls())

#Adicionando biblioteca
library('caret')
library('mlbench')
library('plot3D')

#Função KDE
kde <- function(x1, data, h){
  n <- length(data[1,])
  N <- length(data[,1])
  tempall <- NULL
  for (j in 1:N){
    temp <- NULL
    for (i in 1:n){
      temp <- c(temp, (1/(sqrt(2*pi)*h))*exp(-((x1[i]-data[j,i])^2)/(2*(h^2))))
    }
    tempall <- c(tempall,prod(temp))
  }
  temp <- ((1/N)*sum(tempall))
  return(temp)
}

#Criando dados e os folds
nc <- 1000
X<-mlbench.spirals(nc,cycles=1, sd=0.05)
X <- cbind(X$x, X$classes)
fl <- createFolds(X[,1], k = 10, list = TRUE, returnTrain = FALSE)

acuracia <- NULL
for (ii in 1:10){
  test <- X[fl[[ii]],]
  train <- X[-fl[[ii]],]

  C1 <- X[which(train[,3]==1),]
  C2 <- X[which(train[,3]==2),]
  k <- test[,1:2]
  C1 <- C1[,-3]
  C2 <- C2[,-3]
  Q3 <- as.numeric(quantile(C1))[4]
  Q1 <- as.numeric(quantile(C1))[2]
  h1 <- 0.9*(min(((Q3-Q1)/1.349),sd(C1)))*(length(C1[,1])^(-0.2))
  Q3 <- as.numeric(quantile(C2))[4]
  Q1 <- as.numeric(quantile(C2))[2]
  h2 <- 0.9*(min(((Q3-Q1)/1.349),sd(C2)))*(length(C2[,1])^(-0.2))

  classes <- NULL
  for (jj in 1:length(test[,1])){
    pc1 <- kde(x1 = test[jj,1:2],C1,h1)
    pc2 <- kde(x1 = test[jj,1:2],C2,h2)
    if ((pc1/pc2) >=1) classes <- c(classes,1)
    else classes <- c(classes,2)
  }
  erro <- 0
  for (jj in 1:length((test[,1]))){
    if (classes[jj] != test[jj,3]) erro <- erro +1
  }
  acuracia <- c(acuracia, (100*erro/length(classes)))
}
cat(acuracia,"\n")