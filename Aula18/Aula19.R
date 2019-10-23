#Universidade Federal de Minas Gerais
#Introducao ao Reconhecimento de Padroes
#Nikolas Dias Magalhaes Fantoni
#AULA 19 - CNN-Backpropagation
#2019/2

#Limpando o ambiente
rm(list=ls())

#Adicionando biblioteca
library('caret')
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

#Cria Imagem de Teste
teste <- matrix(-1,ncol=9,nrow=9)
i <- c(3,3,3,4,4,4,5,5,6,6,6,7,7,7)
j <- c(2,3,7,3,4,6,4,5,3,5,6,2,6,7)
for (n in 1:14){
  teste[i[n],j[n]]<-1
}

tf<-3
#Aplica a Convolucao
f <- matrix(c(1,-1,-1,-1,1,-1,-1,-1,1),nrow=tf, ncol = tf)
testeconv <- convolucao(base[[1]],tf,f)

#Aplica o reLU
K <- ReLU(M)
K <- MostraImagem(K)

#Aplica o Maxpool com um tamanho 2 e um stride de 2
L <- maxPool(K,2)
L <- MostraImagem(L)
