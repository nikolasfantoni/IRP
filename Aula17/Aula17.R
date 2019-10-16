#Universidade Federal de Minas Gerais
#Introducao ao Reconhecimento de Padroes
#Nikolas Dias Magalhaes Fantoni
#AULA 17 - Filtros de Convolução
#2019/2

#Limpando o ambiente
rm(list=ls())

#Adicionando biblioteca
library('caret')
library('RnavGraphImageData')


# Carregando a Base de dados
data(faces)
datain <- t(matrix(unlist(faces), ncol=400, byrow=TRUE))
faces <- t(faces)

#Mostra imagem
MostraImagem <- function( x ){
  rotate <- function(x) t( apply(x, 2, rev) )
  img <- matrix( x, nrow=sqrt(length(x)) )
  cor <- rev( gray(50:1/50) )
  image( rotate( img ), col=cor )
  return(img)
}

tf <- 3

#Filtro bordas
img <- MostraImagem(faces[100,])
dimx <- dim(img)[1]
dimy <- dim(img)[2]
f <- matrix(c(-1,-1,-1,-1,8,-1,-1,-1,-1),nrow=3, ncol = 3)
M <- matrix(0,nrow=(dimx-2),ncol=(dimy-2))

for (n in 1:(dimx-tf)){
  for (m in 1:(dimy-tf)){
    M[n,m] <- sum(img[n:(n+2),m:(m+2)] * f)
  }
}
MostraImagem(M)

#Filtro Linhas Verticais
img <- MostraImagem(faces[100,])
dimx <- dim(img)[1]
dimy <- dim(img)[2]
f <- matrix(c(1,2,1,0,0,0,-1,-2,-1),nrow=3, ncol = 3)
M <- matrix(0,nrow=(dimx-2),ncol=(dimy-2))

for (n in 1:(dimx-tf)){
  for (m in 1:(dimy-tf)){
    M[n,m] <- sum(img[n:(n+2),m:(m+2)] * f)
  }
}
MostraImagem(M)

#Filtro Linha Horizontal
img <- MostraImagem(faces[100,])
dimx <- dim(img)[1]
dimy <- dim(img)[2]
f <- matrix(c(1,0,-1,2,0,-2,1,0,-1),nrow=3, ncol = 3)
M <- matrix(0,nrow=(dimx-2),ncol=(dimy-2))

for (n in 1:(dimx-tf)){
  for (m in 1:(dimy-tf)){
    M[n,m] <- sum(img[n:(n+2),m:(m+2)] * f)
  }
}
MostraImagem(M)

#Filtro Sharpen
img <- MostraImagem(faces[100,])
dimx <- dim(img)[1]
dimy <- dim(img)[2]
f <- matrix(c(0,-1,0,-1,5,-1,0,-1,0),nrow=3, ncol = 3)
M <- matrix(0,nrow=(dimx-2),ncol=(dimy-2))

for (n in 1:(dimx-tf)){
  for (m in 1:(dimy-tf)){
    M[n,m] <- sum(img[n:(n+2),m:(m+2)] * f)
  }
}
MostraImagem(M)