#Universidade Federal de Minas Gerais
#Introducao ao Reconhecimento de Padroes
#Nikolas Dias Magalhaes Fantoni
#AULA 7 - K medias
#2019/2


#Limpando o ambiente
rm(list=ls())

#Adicionando biblioteca
#library(caret)

eucdist <- function(x,y){
  sqrt(sum((x-y)^2))
}

#Criando dados
s1 <- 0.3
s2 <- 0.3
s3 <- 0.3
s4 <- 0.3
nc <- 100
xc1 <- matrix(rnorm( nc*2 ) , ncol=2)*s1 +
  t (matrix( c ( 2 , 2 ) ,nrow=2,ncol=nc ) )
xc2 <- matrix(rnorm( nc*2 ) , ncol=2)*s2 +
  t (matrix( c ( 4 , 4 ) ,nrow=2,ncol=nc ) )
xc3 <- matrix(rnorm( nc*2 ) , ncol=2)*s3 +
  t (matrix( c ( 2 , 4 ) ,nrow=2,ncol=nc ) )
xc4 <- matrix(rnorm( nc*2 ) , ncol=2)*s4 +
  t (matrix( c ( 4 , 2 ) ,nrow=2,ncol=nc ) )
plot(c(xc1[,1], xc2[,1], xc3[,1], xc4[,1]), c(xc1[,2], xc2[,2], xc3[,2], xc4[,2]))

X <- rbind(xc1, xc2, xc3, xc4)
k <- 4
iseq <- sample(length(X[,1]), k)
centroid <- X[iseq,]
oldc <- centroid+1
distnow <- NULL
distancia <- NULL

iteracao <- 0
minimos <- NULL
oldminimos <- 1

  while(all.equal(minimos,oldminimos) != TRUE){
    iteracao <- iteracao+1
    cat(iteracao,"\n")
    if (iteracao == 1000) break
    for (i in 1:length(X[,1])){
     for (j in 1:length(centroid[,1])){
       distnow[j] <- eucdist(X[i,],centroid[j,])
     }
      distancia <- rbind(distancia,distnow)
    }
oldminimos <- minimos
for (i in 1:400){
minimos[i] <- which(distancia[i,] == min(distancia[i,]))
}

for (i in 1:k){
  pos <- NULL
  pos <- which(minimos == i)
  if (!is.null(pos)){
    for (j in 1:length(X[1,])){
      centroid[i,j] <- mean(X[pos,j])
    }
  } else {
    centroid[i] <- X[sample(1:length(X[,1]),1),]
  }
}

}

#Plotando resultados
pronto <- cbind(X,minimos)
classe1 <- pronto[which(pronto[,3] == 1),]
classe2 <- pronto[which(pronto[,3] == 2),]
classe3 <- pronto[which(pronto[,3] == 3),]
classe4 <- pronto[which(pronto[,3] == 4),]

plot(classe1[,1], classe1[,2], col="red",  xlim = c(0,6),ylim = c(0,6),xlab = '' ,ylab= '' )
par(new="T")
plot(classe2[,1], classe2[,2], col="blue",  xlim = c(0,6),ylim = c(0,6),xlab = '' ,ylab= '' )
par(new="T")
plot(classe3[,1], classe3[,2], col="orange",  xlim = c(0,6),ylim = c(0,6),xlab = '' ,ylab= '' )
par(new="T")
plot(classe4[,1], classe4[,2], col="green",  xlim = c(0,6),ylim = c(0,6),xlab = '' ,ylab= '' )
par(new="T")
plot(centroid[1,1], centroid[1,2], col="red",  xlim = c(0,6),ylim = c(0,6),xlab = '' ,ylab= '', pch=15 )
par(new="T")
plot(centroid[2,1], centroid[2,2], col="blue",  xlim = c(0,6),ylim = c(0,6),xlab = '' ,ylab= '', pch=15 )
par(new="T")
plot(centroid[3,1], centroid[3,2], col="orange",  xlim = c(0,6),ylim = c(0,6),xlab = '' ,ylab= '', pch=15 )
par(new="T")
plot(centroid[4,1], centroid[4,2], col="green",  xlim = c(0,6),ylim = c(0,6),xlab = '' ,ylab= '', pch=15 )
