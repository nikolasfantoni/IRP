#Universidade Federal de Minas Gerais
#Introducao ao Reconhecimento de Padroes
#Nikolas Dias Magalhaes Fantoni
#AULA 14 - SVM
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
      temp <- c(temp, (1/(sqrt(2*pi)*h))*exp(-((x1[i]-data[j,i])^2)/(2*(h*h))))
    }
    tempall <- c(tempall,(prod(temp)/N))
  }
  temp <- (sum(tempall))
  return(temp)
}

#Criando dados e os folds
nc <- 1000
X<-mlbench.spirals(nc,cycles=1, sd=0.05)
Y<-X
X <- cbind(X$x, X$classes)
fl <- createFolds(X[,1], k = 10, list = TRUE, returnTrain = FALSE)

acuracia <- NULL
for (ii in 1:10){
  test <- X[fl[[ii]],]
  train <- X[-fl[[ii]],]
  
  C1 <- train[which(train[,3]==1),]
  C2 <- train[which(train[,3]==2),]
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
  acuracia <- c(acuracia, (100*(1-(erro/length(classes)))))
}

#estimando as densidades de um grid
seqi<-seq(-0.995,1,0.005)
seqj<-seq(-0.9,1,0.1)
M <- matrix(0,nrow=length(seqi),ncol=length(seqj)) 
c<-NULL
ci <- 0
for (i in seqi){
  ci <- ci+1
  cj <- 0
  for (j in seqj){
    cj <- cj+1
    f1 <- NULL
    f2 <- NULL
    f1<- kde(c(i,j), C1, h1)
    f2<- kde(c(i,j), C2, h2)
    if (f2 == 0){
      c <- 1
    } else {
      c <- if ((f1/f2 >= 1) == TRUE) 1 else 2
    }
    M[ci,cj] <- c
  }
}

#plotando
#plot(X, main="Dados de Entrada", xlim = c(-1,1),ylim = c(-1,1), xlab="x1", ylab="x2")

#Plotando Superficie de Separacao
plot(Y, main="Superfície de Separação", xlim = c(-1,1),ylim = c(-1,1), xlab="x1", ylab="x2")
par(new=T)
contour(x=seqi, y=seqj, z=M, col='blue', xlim = c(-1,1),ylim = c(-1,1), xlab="x1", ylab="x2")

cat(acuracia,"\n")