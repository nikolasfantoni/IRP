#Universidade Federal de Minas Gerais
#Introducao ao Reconhecimento de Padroes
#Nikolas Dias Magalhaes Fantoni
#AULA 18 - CNN
#2019/2

#Limpando o ambiente
rm(list=ls())

#Adicionando biblioteca
library('caret')
library('RnavGraphImageData')
source('ReLU.R')
source('maxPool.R')
source('filtroBordas.R')

# Carregando a Base de dados
data(faces)
datain <- t(matrix(unlist(faces), ncol=400, byrow=TRUE))
faces <- t(faces)
foto <- faces[100,]
  
#Mostra imagem
MostraImagem <- function( x ){
  rotate <- function(x) t( apply(x, 2, rev) )
  img <- matrix( x, nrow=sqrt(length(x)) )
  cor <- rev( gray(50:1/50) )
  image( rotate( img ), col=cor )
  return(img)
}

#Mostra Foto Original
foto <- MostraImagem(foto)

#Aplica o Filtro de Bordas
M <- filtroBordas(X = foto,tf=3)
M <- MostraImagem(M)

#Aplica o reLU
K <- ReLU(M)
K <- MostraImagem(K)

#Aplica o Maxpool com um tamanho 2 e um stride de 2
L <- maxPool(K,2)
L <- MostraImagem(L)
