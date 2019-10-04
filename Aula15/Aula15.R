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
library('caTools')
library('Rfast')

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

split <-  sample.split(1:400, SplitRatio=0.5)
datatrain <- subset(datain,split==TRUE)
colnames(datatrain) <- 1:4096
datatrainclass <- y[which(split==TRUE)]
datatest <- subset(datain,split==FALSE)
colnames(datatest) <- 1:4096
datatestclass <- y[which(split==FALSE)]
pca <- preProcess(x = datatrain, method='pca', pcaComp=80)
pca_train <- predict(pca, datatrain)
pca_test <- predict(pca, datatest)

a <- gaussian.nb(xnew =pca_test, x = pca_train, ina = datatrainclass)
previsto <- a$est
l <- datatestclass - previsto
acuracia <- length(which(l==0))/2
acuracia
f <- t(t(datatestclass))
k <- t(a$est)
cm <- confusionMatrix(factor(k),factor(f))
overall <- cm$overall
overall[1]*100
