#Universidade Federal de Minas Gerais
#Introducao ao Reconhecimento de Padroes
#Nikolas Dias Magalhaes Fantoni
#AULA 9 - Misturas
#2019/2

#Limpando o ambiente
rm(list=ls())

#Adicionando biblioteca
library('mlbench')

#Funcao para estimativa da densidade de 2 variaveis
pdf2var<-function(x,y,u1,u2,s1,s2,p) {(1/(2*pi*s1*s2*sqrt(1-(p^2))))*
    exp((-(1)/(2*(1-(p^2))))*((((x-u1)^2)/((s1)^2))+(((y-u2)^2)/((s2)^2))-
                                ((2*p*(x-u1)*(y-u2))/(s1*s2))))
}

#Funcao de distancia euclidiana
eucdist <- function(x,y){
  sqrt(sum((x-y)^2))
}

#Criando dados
nc <- 1000
X<-mlbench.spirals(nc,cycles=1, sd=0.05)
classes <- 2

#Definindo constantes do teste
errado <- 5
k<-4

while (errado > 2 && k < 40){
  errado <- 0
  k <- k+1
  
  #Inicializando variaveis
  iseq <- sample(length(X$x[,1]), k)
  centroid <- X$x[iseq,]
  oldc <- centroid+1
  distnow <- NULL
  distancia <- NULL
  iteracao <- 0
  minimos <- NULL
  newc<-NULL
  classe <- list()
  
  #Algoritmo k-means
  while(all.equal(newc, centroid) != TRUE){
    iteracao <- iteracao+1
    if (iteracao == 100) break
    for (i in 1:length(X$x[,1])){
      for (j in 1:k){
        distnow[j] <- eucdist(X$x[i,],centroid[j,])
      }
      distancia <- rbind(distancia,distnow)
    }
    
    for (i in 1:(nc)){
      minimos[i] <- which(distancia[i,] == min(distancia[i,]))
    }
    
    newc <- centroid
    for (i in 1:k){
      pos <- NULL
      pos <- which(minimos == i)
      if (!is.null(pos)){
        for (j in 1:length(X$x[1,])){
          centroid[i,j] <- mean(X$x[pos,j])
        }
      } else {
        centroid[i] <- X$x[sample(1:length(X$x[,1]),1),]
      }
    }
  }
  
  pronto <- cbind(X$x,minimos)
  
  classe <- list()
  cores <- rainbow(k)
  for (i in 1:k){
    classe[[i]] <- pronto[which(pronto[,3] == i),] 
  }
  
  pronto <- cbind(pronto, X$classes)
  
  for (i in 1:k){
    newseq <- which(pronto[,3]==i)
    media <- mean(pronto[newseq,4])
    cat("media: ",media,",era pra dar: ", as.numeric(X$classes[newseq[1]]), "\n")
    if (media > pronto[newseq[1],4]*1.05 || media < pronto[newseq[1],4]*0.95) errado <- errado + 1
  }
  cat("k = ", k,"e errados:",errado, "\n\n\n")
}
for (i in 1:k){
  plot(classe[[i]][,1], classe[[i]][,2], col=cores[i],  xlim = c(-1,1),ylim = c(-1,1),
       xlab = '' ,ylab= '' )
  par(new="T")
}
par(new="F")