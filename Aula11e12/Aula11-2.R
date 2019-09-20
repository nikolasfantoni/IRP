#Universidade Federal de Minas Gerais
#Introducao ao Reconhecimento de Padroes
#Nikolas Dias Magalhaes Fantoni
#AULA 11 e 12 - Misturas (parte 2)
#2019/2

#Limpando o ambiente
rm(list=ls())

#Adicionando biblioteca
library('caret')
library('mlbench')
library('plot3D')
source('escolhefold.R')

#Criando dados
data("BreastCancer")
Y <- as.matrix(na.omit(BreastCancer))[,-1]
Y[which(Y[,10]=="benign"),10] <- 1
Y[which(Y[,10]=="malignant"),10] <- 2
X <- matrix(0,nrow=length(Y[,1]),ncol=length(Y[1,])) 
for (i in 1:10) X[,i] <- as.numeric(Y[,i])
fl <- createFolds(X[,1], k = 10, list = TRUE, returnTrain = FALSE)

#Escolhe o melhor fold, dentre os 10, para ser usado
a <- escolhefold(X,fl)

#Escolhido o melhor fold, refaz:
test <- X[fl[[a]],]
train <- X[-fl[[a]],]

#Definindo constantes do teste
errado <- 15
k<-4

#k-means
while (errado >= 2 && k < 100){
  errado <- 0
  k <- k+1
  
  w <- kmeans(train[,1:9],k,iter.max = 100)
  media <- NULL
  
  for (i in 1:k){
    newseq <- which(w$cluster==i)
    media <- mean(as.numeric(train[newseq,10]))
    if (media > as.numeric(train[newseq[1],10])*1.08 || media < as.numeric(train[newseq[1],10])*0.92) errado <- errado + 1
  }
  cat(errado,"\n")
}
###############################################

#Funcao estimativa densidade para n variáveis
pdfnvar <- function(x, m, K, n) {
  if (det(K) == 0 || det(K)<10^(-30)) 10^(50) else (1/(sqrt((2*pi)^(n)*(det(K)))))*exp(-0.5*(t(x-m)%*%(solve(K))%*%(x-m)))
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
    if (length(c1[[i]])/10 == 1){
      u <- c(u,c1[[i]][ii])
    }else{
    u <- c(u,mean(c1[[i]][,ii]))
    }
  }
  u1[[i]] <- u
}

for (i in 1:length(c2)){
  u <- vector()
  for (ii in 1:9){
    if (length(c2[[i]])/10 == 1){
      u <- c(u,c2[[i]][ii])
    }else{
      u <- c(u,mean(c2[[i]][,ii]))
    }
  }
  u2[[i]] <- u
}

#Classificando as amostras de teste
totalc1 <- NULL
totalc2 <- NULL
for (m in 1:length(c1)){
  totalc1 <- c(totalc1,lengths(c1)[m]/10)
}
for (m in 1:length(c2)){
  totalc2 <- c(totalc2,lengths(c2)[m]/10)
}
totalc1 <- sum(totalc1)
totalc2 <- sum(totalc2)

#Amostras de Teste
f1t <- NULL
f2t <- NULL
erro <- 0
for (i in 1:length(test[,1])){
  c <- 0
  l <- test[i,1:9]
  f1 <- NULL
  f2 <- NULL
  for (m in 1:length(c1)){
    pir <- (lengths(c1)[m]/10)/totalc1
    if (length(c1[[m]])/10 == 1){
      cov1 <- matrix(data=0,nrow=9,ncol=9)
      f1 <- rbind(f1,(10^50))
    }else{
      cov1 <- cov(c1[[m]][,1:9])
      f1 <- rbind(f1,pir*pdfnvar(l,u1[[m]],cov1,9))
    }
  }
  for (m in 1:length(c2)){
    pir <- (lengths(c2)[m]/10)/totalc2
    if (length(c2[[m]])/10 == 1){
      cov2 <- matrix(data=0,nrow=9,ncol=9)
      f2 <- rbind(f2,(10^50))
    }else{
      cov2 <- cov(c2[[m]][,1:9])
      f2 <- rbind(f2,pir*pdfnvar(l,u2[[m]],cov2,9))
    }
  }
  f1 <- sum(f1)*totalc1/(totalc1+totalc2)
  f2 <- sum(f2)*totalc2/(totalc1+totalc2)
  f1t <- c(f1t,f1)
  f2t <- c(f2t,f2)
    c <- if ((f1/f2 > 1) == TRUE) 1 else 2
    if ((c-test[i,10]) != 0) erro <- erro +1
}

#PLOTANDO RESULTADOS


#Espaço Verossemelhanca
for (i in 1:length(f1t)){
  if (f1t[i]<=f2t[i]) plot(f1t[i], f2t[i], xlim=c(10^(-18),6*10^(-4)),ylim=c(10^(-20), 6*10^(-9)), col="red", xlab="p(x|C1)", ylab="p(x|C2)")
  if (f2t[i]<f1t[i]) plot(f1t[i], f2t[i], xlim=c(10^(-18),6*10^(-4)),ylim=c(10^(-20), 6*10^(-9)), col="blue", xlab="p(x|C1)", ylab="p(x|C2)")
  par(new="T")
}
par(new="F")

#Curva de Separacao
iseq <- seq(0.0000001,0.0006,10^(-6))
jseq <- (totalc1/totalc2)*iseq*10^(-5)
for (i in 1:length(f1t)){
  if (f1t[i]<=f2t[i]) plot(f1t[i], f2t[i], col="red", xlim=c(10^(-18),6*10^(-4)),ylim=c(10^(-20), 6*10^(-9)), xlab="p(x|C1)", ylab="p(x|C2)")
  if (f2t[i]<f1t[i]) plot(f1t[i], f2t[i], col="blue", xlim=c(10^(-18),6*10^(-4)),ylim=c(10^(-20), 6*10^(-9)), xlab="p(x|C1)", ylab="p(x|C2)")
  par(new="T")
}
plot(iseq,jseq,xlim=c(10^(-18),6*10^(-4)),ylim=c(10^(-20), 6*10^(-9)), xlab="p(x|C1)", ylab="p(x|C2)")
par(new="F")
