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
library('plot3D')

#Criando dados
nc <- 1000
X<-mlbench.spirals(nc,cycles=1, sd=0.05)
classes <- 2
plot(X)

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
    if (media > as.numeric(X$classes[newseq[1]])*1.02 || media < as.numeric(X$classes[newseq[1]])*0.97) errado <- errado + 1
  }
}

cores <- rainbow(k)

for (i in 1:k){
  plot(X$x[which(w$cluster==i),1],X$x[which(w$cluster==i),2], col=cores[i],  xlim = c(-1,1),ylim = c(-1,1),
       xlab = '' ,ylab= '' )
  par(new="T")
}

for (i in 1:k){
  plot(w$centers[,1],w$centers[,2], col='black',  xlim = c(-1,1),ylim = c(-1,1), pch=3,
       xlab = '' ,ylab= '' )
  par(new="T")
}
#####################################

#funcao para estimativa da densidade de 2 variáveis
pdf2var<-function(x,y,u1,u2,s1,s2,p) {(1/(2*pi*s1*s2*sqrt(1-(p^2))))*
    exp((-(1)/(2*(1-(p^2))))*((((x-u1)^2)/((s1)^2))+(((y-u2)^2)/((s2)^2))-
                                ((2*p*(x-u1)*(y-u2))/(s1*s2))))
}

#cáculo da média e desvio padrão das classes
u <- list()
s <- list()
pc <- list()
for (i in 1:k){
  u[[i]] <- c(mean(X$x[which(w$cluster==i),1]),mean(X$x[which(w$cluster==i),2]))
  s[[i]] <- c(sd(X$x[which(w$cluster==i),1]),sd(X$x[which(w$cluster==i),2]))
  pc[[i]] <- (cov(X$x[which(w$cluster==i),1], X$x[which(w$cluster==i),2]))/
    sqrt(prod((c(sd(X$x[which(w$cluster==i),1]),sd(X$x[which(w$cluster==i),2])))^2))
}

#estimando as densidades em cada ponto de um grid
seqi<-seq(-0.99,1,0.01)
seqj<-seq(-0.99,1,0.01)
M <- matrix(0,nrow=length(seqi),ncol=length(seqj)) 
Mtotal <- list()
ci<-0
for (n in 1:k){
  ci <- 0
  for (i in seqi){
    ci<-ci+1
    cj<-0
    for(j in seqj)
    {
      cj<-cj+1
      M[ci,cj] <- pdf2var(i,j,u[[n]][1],u[[n]][2],s[[n]][1],s[[n]][2],pc[[n]])
      
    }
  }
  Mtotal[[n]] <- M
}
Mfim <- Reduce('+',Mtotal)

#plotando as densidades
persp3D(seqi,seqj,Mfim,counter=T,theta = -10, phi = 60, r = 100, d = 1, expand = 0.5,
        ltheta = 90, lphi = 180, shade = 0.4, ticktype = "detailed", nticks=5,
        xlab='', ylab='', zlab='')
