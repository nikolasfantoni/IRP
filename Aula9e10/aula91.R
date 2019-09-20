#Universidade Federal de Minas Gerais
#Introducao ao Reconhecimento de Padroes
#Nikolas Dias Magalhaes Fantoni
#AULA 9 - Misturas (parte 2)
#2019/2

#Limpando o ambiente
rm(list=ls())

#Adicionando biblioteca
library('caret')
library('mlbench')
library('plot3D')

#Criando dados
data("BreastCancer")
Y <- as.matrix(na.omit(BreastCancer))[,-1]
Y[which(Y[,10]=="benign"),10] <- 1
Y[which(Y[,10]=="malignant"),10] <- 2
X <- matrix(0,nrow=length(Y[,1]),ncol=length(Y[1,])) 
for (i in 1:10) X[,i] <- as.numeric(Y[,i])
fl <- createFolds(X[,1], k = 10, list = TRUE, returnTrain = FALSE)

#Looping para os 10 testes
acuracia <- NULL
for (j in 1:10){
  test <- X[fl[[j]],]
  train <- X[-fl[[j]],]
  
  #Definindo constantes do teste
  errado <- 15
  k<-4
  
  #k-means
  while (errado > 3 && k < 100){
    errado <- 0
    k <- k+1
    
    w <- kmeans(train[,1:9],k,iter.max = 100)
    media <- NULL
    
    for (i in 1:k){
      newseq <- which(w$cluster==i)
      media <- mean(as.numeric(train[newseq,10]))
      if (media > as.numeric(train[newseq[1],10])*1.08 || media < as.numeric(train[newseq[1],10])*0.92) errado <- errado + 1
    }
  }
###############################################
  
  #Funcao estimativa densidade para n variáveis
  pdfnvar <- function(x, m, K, n) {
    if (det(K) == 0) 10^(50) else (1/(sqrt((2*pi)^(n)*(det(K)))))*exp(-0.5*(t(x-m)%*%(solve(K))%*%(x-m)))
  }
  
  #cáculo da média e desvio padrão das classes
  clusters <- list()
  c1 <- list()
  c2 <- list()
  count1 <- 1
  count2 <- 1
  for (i in 1:k){
    seqc <- which(w$cluster==i)
    clusters[[i]] <- train[seqc,]
    if (train[seqc[1],10]==1){
      c1[[count1]] <- train[seqc,]
      count1 <- count1+1
    } else {
      c2[[count2]] <- train[seqc,]
      count2 <- count2+1
    }
  }
  
  u1 <- list()
  u2 <- list()
    for (i in 1:length(c1)){
    u <- vector()
    for (ii in 1:9){
      u <- c(u,mean(c1[[i]][,ii]))
    }
    u1[[i]] <- u
  }
  
  for (i in 1:length(c2)){
    u <- vector()
    for (ii in 1:9){
      u <- c(u,mean(c2[[i]][,ii]))
    }
    u2[[i]] <- u
  }
  
  #Classificando as amostras de teste
  totalc1 <- NULL
  totalc2 <- NULL
  for (m in 1:length(c1)){
    totalc1 <- c(totalc1,length(c1[[m]][,1]))
  }
  for (m in 1:length(c2)){
    totalc2 <- c(totalc2,length(c2[[m]][,1]))
  }
  totalc1 <- sum(totalc1)
  totalc2 <- sum(totalc2)
  
  erro <- 0
  for (i in 1:length(test[,1])){
    c <- 0
    l <- test[i,1:9]
    f1 <- NULL
    f2 <- NULL
    for (m in 1:length(c1)){
      pir <- length(c1[[m]][,1])/totalc1
      f1 <- rbind(f1,pir*pdfnvar(l,u1[[m]],cov(c1[[m]][,1:9]),9))
    }
    for (m in 1:length(c2)){
      pir <- length(c2[[m]][,1])/totalc2
      f2<- rbind(f2,pir*pdfnvar(l,u2[[m]],cov(c2[[m]][,1:9]),9))
    }
    f1 <- sum(f1)
    f2 <- sum(f2)
    if (f2 == 0){
      c <- 1
    } else {
      c <- if ((f1/f2 > 1) == TRUE) 1 else 2
      if ((c-test[i,10]) != 0) erro <- erro +1
    }
  }
  
  acuracia <- c(acuracia,(100*(1-(erro/length(test[,3])))))
}


acuracia
mean(acuracia)
sd(acuracia)
