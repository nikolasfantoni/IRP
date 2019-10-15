#Universidade Federal de Minas Gerais
#Introducao ao Reconhecimento de Padroes
#Nikolas Dias Magalhaes Fantoni
#AULA 16 - PCA-SVM
#2019/2

#Limpando o ambiente
rm(list=ls())

#Importando Bibliotecas
library('mlbench')
library('caret')
library('kernlab')

#Carregando dados
data(BreastCancer) # carrega dados
db <- na.omit(BreastCancer) # elimina dados faltantes
db$Label[db$Class == "benign"] <- -1 # muda labels
db$Label[db$Class == "malignant"] <- 1 # muda labels
X <- data.matrix(db[,2:10])
y <- data.matrix(db[,12])
rm(BreastCancer,db)

#PCA
xm <- colMeans(X)
Xinnorm <- X - matrix(xm,nrow = nrow(X),ncol=ncol(X),byrow=T)
S <- cov(Xinnorm)
eigS <- eigen(S)
u <- eigS$vectors
v <- eigS$values
s <- 0
k<- 0
while (s < 0.60){
  k <- k+1
  s <- sum(v[1:k])/sum(v)
}
autovetores <- u[,-((k+1):9)]
pcadata <- X %*% autovetores
rm(xm,Xinnorm,eigS,S,s,u, X, autovetores,k,v)

#Criando os Folds
fl <- createFolds(pcadata[,1], k = 10, list = TRUE, returnTrain = FALSE)

#SVM para 10 folds
acuracia <- NULL
for (ii in 1:10){
datatest <- pcadata[fl[[ii]],]
datatrain <- pcadata[-fl[[ii]],]
datatrainclass <- y[-fl[[ii]]]
datatestclass <- y[fl[[ii]]]
svmtrain <- ksvm(x =datatrain,y=datatrainclass,type='C-bsvc',kernel='rbfdot',kpar=list(sigma=0.8),C=20)
p <- predict(svmtrain,datatest)
d <-table(p,datatestclass)
acuracia <- c(acuracia,100*(sum(diag(d))/sum(d)))
}