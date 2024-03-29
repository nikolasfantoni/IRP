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

act <- NULL
for (l in 1:10){
#Dividindo os dados
fl <- createFolds(faces[,1], k = 4, list = TRUE, returnTrain = FALSE)
datatest <- datain[fl[[1]],]
datatrain <- datain[-fl[[1]],]
datatrainclass <- y[-fl[[1]]]
datatestclass <- y[fl[[1]]]
colnames(datatrain) <- 1:4096
colnames(datatest) <- 1:4096

#PCA
xm <- colMeans(datatrain)
Xinnorm <- datatrain - matrix(xm,nrow = nrow(datatrain),ncol=ncol(datatrain),byrow=T)
S <- cov(Xinnorm)
eigS <- eigen(S)
u <- eigS$vectors
v <- eigS$values
s <- 0
k<- 0
while (s < 0.98){
k <- k+1
  s <- sum(v[1:k])/sum(v)
}

#Preparando pra Classificação
autovetores <- u[,-((k+1):4096)]
datatrain <- datatrain - matrix(colMeans(datatrain),nrow = nrow(datatrain),ncol=ncol(datatrain),byrow=T)
pca_datain <- datatrain %*% autovetores
datatest <- datatest - matrix(colMeans(datatest),nrow = nrow(datatest),ncol=ncol(datatest),byrow=T)
pca_datatest <- datatest %*% autovetores
C <- list()
h <- list()
for (i in 1:40){
  C[[i]] <- pca_datain[which(datatrainclass==i),]
  Q3 <- as.numeric(quantile(C[[i]]))[4]
  Q1 <- as.numeric(quantile(C[[i]]))[2]
  h[[i]] <- 0.9*(min(((Q3-Q1)/1.349),sd(C[[i]])))*(length(C[[i]][,1])^(-0.2))
}

#Classificador KDE
pc <- vector(length = 40)
classificacao <- vector()
for (jj in 1:length(pca_datatest[,1])){
  for (i in 1:40){
    pc[i] <- kde(x1=pca_datatest[jj,],C[[i]],h[[i]])
  }
  classificacao <- c(classificacao,min(which(pc==min(max(pc)))))
}
erro <- length(which((classificacao-datatestclass)!=0))
acuracia <- 1-erro/length(datatestclass)
act <- c(act,acuracia)
cat("K vale: ", k, "Acuracia: ", acuracia*100,"%.\n")
}

#Criando matriz de confusão e exportando
cm <- confusionMatrix(factor(classificacao),factor(datatestclass))
write.csv(as.table(cm),file="cm.csv")
tocsv <- data.frame(cbind(t(cm$overall),t(cm$byClass)))
write.csv(tocsv,file="cm.csv")

#Classificador Bayesiano
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

