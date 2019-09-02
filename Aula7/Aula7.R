#Universidade Federal de Minas Gerais
#Introducao ao Reconhecimento de Padroes
#Nikolas Dias Magalhaes Fantoni
#AULA 7 - K medias
#2019/2

#Limpando o ambiente
rm(list=ls())

#Funcao de distancia euclidiana
eucdist <- function(x,y){
  sqrt(sum((x-y)^2))
}

#Definindo constantes do teste
s <- 0.3 #desvio padrao
k <- 4

#Criando dados
nc <- 100
xc1 <- matrix(rnorm( nc*2 ) , ncol=2)*s +
  t (matrix( c ( 2 , 2 ) ,nrow=2,ncol=nc ) )
xc2 <- matrix(rnorm( nc*2 ) , ncol=2)*s +
  t (matrix( c ( 4 , 4 ) ,nrow=2,ncol=nc ) )
xc3 <- matrix(rnorm( nc*2 ) , ncol=2)*s +
  t (matrix( c ( 2 , 4 ) ,nrow=2,ncol=nc ) )
xc4 <- matrix(rnorm( nc*2 ) , ncol=2)*s +
  t (matrix( c ( 4 , 2 ) ,nrow=2,ncol=nc ) )
X <- rbind(xc1, xc2, xc3, xc4)

#Inicializando variaveis
iseq <- sample(length(X[,1]), k)
centroid <- X[iseq,]
oldc <- centroid+1
distnow <- NULL
distancia <- NULL
iteracao <- 0
minimos <- NULL
newc<-NULL

#Algoritmo k-means
while(all.equal(newc, centroid) != TRUE){
  iteracao <- iteracao+1
  cat(iteracao,"\n")
  if (iteracao == 100) break
  for (i in 1:length(X[,1])){
    for (j in 1:length(centroid[,1])){
      distnow[j] <- eucdist(X[i,],centroid[j,])
    }
    distancia <- rbind(distancia,distnow)
  }

  for (i in 1:(nc*4)){
    minimos[i] <- which(distancia[i,] == min(distancia[i,]))
  }
  newc <- centroid
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

plot(classe1[,1], classe1[,2], col="red",  xlim = c(0,6),ylim = c(0,6),
     xlab = '' ,ylab= '', main="Divisão em k clusters" )
par(new="T")
plot(classe2[,1], classe2[,2], col="green",  xlim = c(0,6),ylim = c(0,6),
     xlab = '' ,ylab= '' )
par(new="T")
plot(classe3[,1], classe3[,2], col="orange",  xlim = c(0,6),ylim = c(0,6),
     xlab = '' ,ylab= '')
par(new="T")
plot(classe4[,1], classe4[,2], col="grey",  xlim = c(0,6),ylim = c(0,6),
     xlab = '' ,ylab= '' )
par(new="T")
plot(centroid[1,1], centroid[1,2], col="red",  xlim = c(0,6),ylim = c(0,6),
     xlab = '' ,ylab= '', pch=16 )
par(new="T")
plot(centroid[2,1], centroid[2,2], col="green",  xlim = c(0,6),ylim = c(0,6),
     xlab = '' ,ylab= '', pch=16 )
par(new="T")
plot(centroid[3,1], centroid[3,2], col="orange",  xlim = c(0,6),ylim = c(0,6),
     xlab = '' ,ylab= '', pch=16 )
par(new="T")
plot(centroid[4,1], centroid[4,2], col="gray",  xlim = c(0,6),ylim = c(0,6),
     xlab = '' ,ylab= '', pch=16 )
par(new="T")
plot(centroid[1,1], centroid[1,2], col="black",  xlim = c(0,6),ylim = c(0,6),
     xlab = '' ,ylab= '', pch=3 )
par(new="T")
plot(centroid[2,1], centroid[2,2], col="black",  xlim = c(0,6),ylim = c(0,6),
     xlab = '' ,ylab= '', pch=3 )
par(new="T")
plot(centroid[3,1], centroid[3,2], col="black",  xlim = c(0,6),ylim = c(0,6),
     xlab = '' ,ylab= '', pch=3 )
par(new="T")
plot(centroid[4,1], centroid[4,2], col="black",  xlim = c(0,6),ylim = c(0,6),
     xlab = '' ,ylab= '', pch=3 )