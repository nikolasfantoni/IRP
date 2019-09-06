#Universidade Federal de Minas Gerais
#Introducao ao Reconhecimento de Padroes
#Nikolas Dias Magalhaes Fantoni
#AULA 8 - K medias 2
#2019/2

#Limpando o ambiente
rm(list=ls())

#Adicionando biblioteca
library('mlbench')
library('factoextra')


#Funcao de distancia euclidiana
eucdist <- function(x,y){
  sqrt(sum((x-y)^2))
}

#Criando dados
nc <- 1000
X<-mlbench.spirals(nc,cycles=1, sd=0.05)
classes <- 2

#Definindo constantes do teste
errado <- 1
k<-4

while (errado != 0 && k < 40){
  errado <- 0
  k <- k+1

w <- kmeans(X$x,k,iter.max = 10)
media <- NULL
as.numeric(X$classes)

for (i in 1:k){
  newseq <- which(w$cluster==i)
  media <- mean(as.numeric(X$classes[newseq]))
  cat("media: ",media,"era pra dar: ", X$classes[newseq[1]], "\n")
  if (media > as.numeric(X$classes[newseq[1]])*1.05 || media < as.numeric(X$classes[newseq[1]])*0.95) errado <- errado + 1
}
cat("k = ", k,"e errados:",errado, "\n\n\n")
}

cores <- rainbow(k)

for (i in 1:k){
  plot(X$x[which(w$cluster==i),1],X$x[which(w$cluster==i),2], col=cores[i],  xlim = c(-1,1),ylim = c(-1,1),
       xlab = '' ,ylab= '' )
  par(new="T")
}
par(new="F")
