#Universidade Federal de Minas Gerais
#Introducao ao Reconhecimento de Padroes
#Nikolas Dias Magalhaes Fantoni
#AULA 15 - PCA
#2019/2

#Limpando o ambiente
rm(list=ls())

#Adicionando biblioteca
library('caret')
library('RnavGraphImageData')


# Carregando a Base de dados
data(faces)
faces <- t(faces)

#Mostra imagem
MostraImagem <- function( x ){
  rotate <- function(x) t( apply(x, 2, rev) )
  img <- matrix( x, nrow=64 )
 cor <- rev( gray(50:1/50) )
  image( rotate( img ), col=cor )
}

MostraImagem( faces[1,] )

#Gerando os rótulos
y <- NULL
for(i in 1:nrow(faces) ){
  y <- c( y, ((i-1) %/% 10) + 1 )
}

fl <- createFolds(X[,1], k = 10, list = TRUE, returnTrain = FALSE)
rm(i,classes,nc)


acuracia <- NULL
for (ii in 1:10){
  test <- X[fl[[ii]],]
  train <- X[-fl[[ii]],]
  
  svmtrain <- ksvm(x =train[,1:2],y=train[,3],type='C-bsvc',kernel='rbfdot',kpar=list(sigma=0.8),C=20)
  p <- NULL
  for (jj in 1:length(test[,1])){
    p <- predict(svmtrain,test[,-3])
  }
  d <-table(p,test[,3])
  acuracia <- c(acuracia,sum(diag(d))/sum(d))
}
acuracia <- 100*acuracia

#estimando as densidades de um grid
seqi<-seq(-0.995,1,0.005)
seqj<-seq(-0.995,1,0.005)
M <- matrix(0,nrow=length(seqi),ncol=length(seqj)) 
p <- NULL
for (jj in 1:length(seqi)){
  p <- rbind(p,predict(svmtrain,cbind(seqi[jj],seqj)))
}

#plotando

#entrada
plot(Y, xlim = c(-1,1),ylim = c(-1,1), xlab="x1", ylab="x2")

#Vetores de Suporte
indexes <- SVindex(svmtrain)
plot(Y, xlim = c(-1,1),ylim = c(-1,1), xlab="x1", ylab="x2")
par(new=T)
plot(train[indexes,1],train[indexes,2], col = 'green', xlim = c(-1,1),ylim = c(-1,1), xlab="x1", ylab="x2")

#Plotando Superficie de Separacao
plot(Y, xlim = c(-1,1),ylim = c(-1,1), xlab="x1", ylab="x2")
par(new=T)
contour(x=seqi, y=seqj, z=p, col='blue', xlim = c(-1,1),ylim = c(-1,1), xlab="x1", ylab="x2", levels = 1:2, labels = "", add = TRUE)

#Superficie de Separacao 3D
persp3D(seqi,seqj,p,counter=T,theta = 55, phi = 30, r = 40,
        d = 0.1, expand = 0.5, ltheta = 90, lphi = 180, shade = 0.4,
        ticktype = "detailed", nticks=5)

#cat(acuracia,"\n")
cat(mean(acuracia))