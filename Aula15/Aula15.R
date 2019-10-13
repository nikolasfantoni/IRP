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
library('naivebayes')
library('e1071')
source('kde.R')

# Carregando a Base de dados
data(faces)
datain <- t(matrix(unlist(faces), ncol=400, byrow=TRUE))
faces <- t(faces)

#Mostra imagem
MostraImagem <- function( x ){
  rotate <- function(x) t( apply(x, 2, rev) )
  img <- matrix( x, nrow=64 )
 cor <- rev( gray(50:1/50) )
  image( rotate( img ), col=cor )
}

#Gerando os rótulos
y <- NULL
for(i in 1:nrow(faces) ){
  y <- c( y, ((i-1) %/% 10) + 1 )
}
rm(i)

#Dividindo os dados
fl <- createFolds(faces[,1], k = 2, list = TRUE, returnTrain = FALSE)
datatest <- datain[fl[[1]],]
datatrain <- datain[-fl[[1]],]
datatrainclass <- y[-fl[[1]]]
datatestclass <- y[fl[[1]]]
colnames(datatrain) <- 1:4096
colnames(datatest) <- 1:4096
rm(fl, datain, y, faces)

#PCA
xm <- colMeans(datatrain)
Xinnorm <- datatrain - matrix(xm,nrow = nrow(datatrain),ncol=ncol(datatrain),byrow=T)
S <- cov(Xinnorm)
eigS <- eigen(S)
u <- eigS$vectors
v <- eigS$values
rm(v,eigS,S,Xinnorm, xm)

for (m in 51:100){
#Preparação para os classificadores
autovetores <- u[,-(m:4096)]
datatrain <- datatrain - matrix(colMeans(datatrain),nrow = nrow(datatrain),ncol=ncol(datatrain),byrow=T)
pca_datatrain <- datatrain %*% autovetores
datatest <- datatest - matrix(colMeans(datatest),nrow = nrow(datatest),ncol=ncol(datatest),byrow=T)
pca_datatest <- datatest %*% autovetores

#PCA com KDE
C <- list()
h <- list()
for (i in 1:nlevels(factor(datatrainclass))){
  C[[i]] <- pca_datatrain[which(datatrainclass == i),]
  Q3 <- as.numeric(quantile(C[[i]]))[4]
  Q1 <- as.numeric(quantile(C[[i]]))[2]
  if (is.null(dim(C[[i]][1]))){
    dimensao <- length(C[[i]])
  } else {
    dimensao <- dim(C[[i]][1])
  }
  h[[i]] <- 0.9*(min(((Q3-Q1)/1.349),sd(C[[i]])))*(dimensao^(-0.2))
}


classificacao <- vector()
for (i in 1:dim(pca_datatest)[1]){
  pc <- NULL
  for (jj in 1:nlevels(factor(datatrainclass))){
    pc <- c(pc,kde(x1 = pca_datatest[i,],C[[jj]],h[[jj]]))
  }
  classificacao <- c(classificacao,min(which(pc == min(max(pc)))))
}

k <- table(factor(datatestclass),factor(classificacao))
acuracia <- sum(diag(k)/length(datatestclass))
cat("m: ",m, "Acuracia: ", acuracia*100,"%.\n")
}

#PCA sem pacote
#model <- naiveBayes(pca_datain,factor(datatrainclass))
#found <- predict(model,pca_datatest)
#acuracia <- sum(diag(table(found,factor(datatestclass))))/length(datatestclass)
#cat("Acuracia: ", acuracia*100,"%.\n")

#PCA com pacote
#pca <- prcomp(datatrain,center = TRUE, scale. = FALSE, tol=0.1)
#pca_train <- predict(pca,datatrain)
#pca_test <- predict(pca,datatest)
#model <- naive_bayes(pca_train,factor(datatrainclass))
#found <- predict(model,pca_test)
#acuracia <- sum(diag(table(found,factor(datatestclass))))/length(datatestclass)
#cat( "Acuracia: ", acuracia*100,"%.\n")

