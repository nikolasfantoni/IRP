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
library('naivebayes')

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


#pca <- preProcess(datatrain, method=c("BoxCox", "center", "scale", "pca"))
pca<-preProcess(datatrain,method="pca",pcaComp=80)
#pca <- prcomp(x = datatrain,center = TRUE, scale. = TRUE)
pca_train <- predict(pca, datatrain)
pca_test <- predict(pca, datatest)
a <- gaussian_naive_bayes(x=pca_train, as.factor(datatrainclass))
previsto <- predict(a,pca_test)
cm <- confusionMatrix(previsto,as.factor(datatestclass))
overall <- cm$overall
cat("Thresh = ", mm, ", Acuracia: ", overall[1]*100, "\n")
