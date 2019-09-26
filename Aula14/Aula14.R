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
library('kernlab')
library('plyr')


#Criando dados e os folds
nc <- 1000
Y<-mlbench.spirals(nc,cycles=1, sd=0.05)
X <- list()
X$x <- Y$x
classes <- NULL
for (i in 1:length(X$x[,1]))
{
  if (Y$classes[i]==1){
    classes <- c(classes,-1)
  } else{
    classes <- c(classes,1)
  }
}
X$classes <- classes
X <- cbind(X$x,X$classes)
fl <- createFolds(X[,1], k = 10, list = TRUE, returnTrain = FALSE)
rm(i)


acuracia <- NULL
for (ii in 1:10){
  test <- X[fl[[ii]],]
  train <- X[-fl[[ii]],]

svmtrain <- ksvm(x =train[,1:2],y=train[,3],type='C-bsvc',kernel='rbfdot',kpar=list(sigma=0.5),C=2)
p <- NULL
for (jj in 1:length(test[,1])){
  p <- predict(svmtrain,test[,-3])
}
d <-table(p,test[,3])
acuracia <- c(acuracia,sum(diag(d))/sum(d))
}

#estimando as densidades de um grid
seqi<-seq(-0.995,1,0.005)
seqj<-seq(-0.995,1,0.005)
M <- matrix(0,nrow=length(seqi),ncol=length(seqj)) 
p <- NULL
for (jj in 1:length(seqi)){
  p <- rbind(p,predict(svmtrain,cbind(seqi[jj],seqj)))
}

#plotando
#plot(X, main="Dados de Entrada", xlim = c(-1,1),ylim = c(-1,1), xlab="x1", ylab="x2")

#Plotando Superficie de Separacao
plot(Y, main="Superfície de Separação", xlim = c(-1,1),ylim = c(-1,1), xlab="x1", ylab="x2")
par(new=T)
contour(x=seqi, y=seqj, z=p, col='blue', xlim = c(-1,1),ylim = c(-1,1), xlab="x1", ylab="x2")

#cat(acuracia,"\n")