#Universidade Federal de Minas Gerais
#Introducao ao Reconhecimento de Padroes
#Nikolas Dias Magalhaes Fantoni
#AULA 7 - K medias
#2019/2


######CODIGO DO EX 6######
#Limpando o ambiente
rm(list=ls())

#Adicionando biblioteca
library(caret)

eucdist <- function(m, n){
  sqrt(sum((m-n)^2))
}

kmeans1 <- function(X, k, c){
  iseq <- sample(length(X[,1]), k)
  c <- X[iseq,]
  newc <- NULL
  while(newc != c){
    for (i in 1:length(X[iseq,])){
      eucdist(X[i,], c)
    }
  }
}

#Funcao estimativa densidade para n variáveis
pdfnvar <- function(x, m, K, n) {
  if (det(K) == 0) 999999999 else (1/(sqrt((2*pi)^(n)*(det(K)))))*exp(-0.5*(t(x-m)%*%(solve(K))%*%(x-m)))
}

#Lendo os dados
spam <- read.csv( "spambase.data", sep=",",header = FALSE)
ic1 <- which(spam$V58 == 1)
ic2 <- which(spam$V58 == 0)
spamc1 <- spam[ic1,]
spamc2 <- spam[ic2,]

#Criando os folds que separao os dados
fl1 <- createFolds(spamc1[,1], k = 10, list = TRUE, returnTrain = FALSE)
fl2 <- createFolds(spamc1[,1], k = 10, list = TRUE, returnTrain = FALSE)

#Looping para os 10 testes
acuraciaacumulada <- NULL
for (j in 1:10){
  test <- rbind(spamc1[fl1[[j]],],spamc1[fl1[[j]],])
  trainc1 <- spamc1[-fl1[[j]],]
  trainc2 <- spamc2[-fl1[[j]],]
  
  #Calculando as medias
  u1<- NULL
  u2<- NULL
  for (m in 1:57){
    u1 <- rbind(u1, mean(trainc1[,m]))
    u2 <- rbind(u2, mean(trainc2[,m]))
  }
  
  #Covariancias dos dados
  cov1 <- cov(trainc1[,1:57])
  cov2 <- cov(trainc2[,1:57])
  
  #Testando o algoritmo
  erro <- 0
  for (i in 1:length(test[,1])){
    l <- t(test[i,1:57])
    f1 <- pdfnvar(l, u1 , cov1, 57)
    f2 <- pdfnvar(l, u2 , cov2, 57)
    if (f2 == 0){
      c <- 1
    } else {
      c <- if ((f1/f2 >= 1) == TRUE) 1 else 0
      if ((c-test[i,58]) != 0) erro <- erro +1
    }
  }
  
  #Obtendo a acuracia 
  acerto <- 1 - erro/length(test[,1])
  acuraciaacumulada <- c(acuraciaacumulada, acerto)
}

#Imprimindo a saida
cat("\n acuracia: [", acuraciaacumulada, "]\n", "media: ", 
    mean(acuraciaacumulada)*100,"%", "\n", "desvio: ", sd(acuraciaacumulada)*100,"%","\n\n")
