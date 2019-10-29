#Universidade Federal de Minas Gerais
#Introducao ao Reconhecimento de Padroes
#Nikolas Dias Magalhaes Fantoni
#AULA 19 - CNN-Backpropagation
#2019/2

#Limpando o ambiente
rm(list=ls())

#Adicionando biblioteca
library('caret')
library('kernlab')
library('plot.matrix')
source('ReLU.R')
source('maxPool.R')
source('CriaBase.R')
source('convolucao.R')

# Criando a base de dados
base <- CriaBase()

#Mostra imagem
MostraImagem <- function( x ){
  rotate <- function(x) t( apply(x, 2, rev) )
  img <- matrix( x, nrow=sqrt(length(x)) )
  cor <- rev( gray(0:50/100) )
  image( rotate( img ), col=cor )
  return(img)
}

#Cria Imagem de Teste X
imgteste <- matrix(-1,ncol=9,nrow=9)
i <- c(3,3,3,4,4,4,5,5,6,6,6,7,7,7)
j <- c(2,3,7,3,4,6,4,5,3,5,6,2,6,7)
for (n in 1:length(i)){
  imgteste[i[n],j[n]]<-1
}

#Cria Imagem de Teste C
imgteste <- matrix(-1,ncol=9,nrow=9)
i <- c(3,3,3,2,3,4,5,6,6,7,7,7,7,7)
j <- c(3,4,5,6,7,3,3,3,4,3,4,5,6,7)
for (n in 1:length(i)){
  imgteste[i[n],j[n]]<-1
}
imgteste <- MostraImagem(imgteste)

#Definição dos filtros
tf<-3
f <- list()
f[[1]] <-  matrix(c(1,-1,-1,-1,1,-1,-1,-1,1),nrow=tf, ncol = tf)
f[[2]] <-  matrix(c(1,-1,1,-1,1,-1,1,-1,1),nrow=tf, ncol = tf)
f[[3]] <-  matrix(c(1,1,1,1,-1,-1,1,-1,-1),nrow=tf, ncol = tf)
f[[4]] <-  matrix(c(1,1,1,-1,-1,1,-1,-1,1),nrow=tf, ncol = tf)


#Treina a CNN
modelo <- NULL
for (b in 1:length(base)){
  vetores <- NULL
  for (filtro in 1:length(f)){
    imgconv <- convolucao(base[[b]],tf,f[[filtro]])
    
    #Aplica o reLU
    imgrelu <- ReLU(imgconv)

    #Aplica o Maxpool com um tamanho 2 e um stride de 2
    imgmaxpool <- maxPool(imgrelu,2)

    #Vetoriza a imagem
    imgvetor <- c(imgmaxpool)
    
    vetores <- c(vetores,imgvetor)
  }
  modelo <- cbind(modelo,vetores)
}
y<-factor(c('x','x','x','x','x','c','c','c','c','c'))

#Treinamento da Rede SVM
svmtrain <- ksvm(x =t(modelo),y=y,type='C-bsvc',kernel='rbfdot',kpar=list(sigma=0.8),C=20)

#Passa a amostra de testes na CNN
vetores <- NULL
for (filtro in 1:length(f)){
  imgconv <- convolucao(imgteste,tf,f[[filtro]])
  
  #Aplica o reLU
  imgrelu <- ReLU(imgconv)
  
  #Aplica o Maxpool com um tamanho 2 e um stride de 2
  imgmaxpool <- maxPool(imgrelu,2)
  
  #Vetoriza a imagem
  imgvetor <- c(imgmaxpool)
  
  vetores <- c(vetores,imgvetor)
}

letra <- predict(svmtrain,matrix(vetores,nrow=1,ncol=64))
