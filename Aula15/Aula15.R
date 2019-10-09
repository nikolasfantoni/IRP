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
fl <- createFolds(faces[,1], k = 4, list = TRUE, returnTrain = FALSE)
datatest <- datain[fl[[1]],]
datatrain <- datain[-fl[[1]],]
datatrainclass <- y[-fl[[1]]]
datatestclass <- y[fl[[1]]]
colnames(datatrain) <- 1:4096
colnames(datatest) <- 1:4096
rm(fl)

#PCA
#xm <- colMeans(datatrain)
#Xinnorm <- datatrain - matrix(xm,nrow = nrow(datatrain),ncol=ncol(datatrain),byrow=T)
#S <- cov(Xinnorm)
#eigS <- eigen(S)
#u <- eigen$ve

#PCA com pacote
pca <- prcomp(datatrain,center = TRUE, scale. = FALSE, tol=0.1)
pca_train <- predict(pca,datatrain)
pca_test <- predict(pca,datatest)
model <- naive_bayes(pca_train,factor(datatrainclass))
found <- predict(model,pca_test)
acuracia <- sum(diag(table(found,factor(datatestclass))))/length(datatestclass)
cat( "Acuracia: ", acuracia*100,"%.\n")

