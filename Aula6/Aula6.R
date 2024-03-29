#Universidade Federal de Minas Gerais
#Introducao ao Reconhecimento de Padroes
#Nikolas Dias Magalhaes Fantoni
#AULA 6 - Base Spam
#2019/2

#Limpando o ambiente
rm(list=ls())

#Adicionando biblioteca
library(caret)

#Funcao estimativa densidade para n variáveis
pdfnvar <- function(x, m, K, n) {
  if (det(K) == 0) 10^(50) else (1/(sqrt((2*pi)^(n)*(det(K)))))*exp(-0.5*(t(x-m)%*%(solve(K))%*%(x-m)))
}

#Lendo os dados
spam <- read.csv( "spambase.data", sep=",",header = FALSE)
ic1 <- which(spam$V58 == 1)
ic2 <- which(spam$V58 == 0)
spamc1 <- spam[ic1,]
spamc2 <- spam[ic2,]

#Criando os folds que separao os dados
fl1 <- createFolds(spamc1[,1], k = 10, list = TRUE, returnTrain = FALSE)
fl2 <- createFolds(spamc2[,1], k = 10, list = TRUE, returnTrain = FALSE)

#Looping para os 10 testes
acuracia1 <- NULL
acuracia2 <- NULL
for (j in 1:10){
  test1 <- spamc1[fl1[[j]],]
  test2 <- spamc2[fl2[[j]],]
  trainc1 <- spamc1[-fl1[[j]],]
  trainc2 <- spamc2[-fl2[[j]],]
  
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
  
  #Testando o algoritmo para classe 1
  erro <- 0
  for (i in 1:length(test1[,1])){
    l <- t(test1[i,1:57])
    f1 <- pdfnvar(l, u1 , cov1, 57)
    f2 <- pdfnvar(l, u2 , cov2, 57)
    if (f2 == 0){
      c <- 1
    } else {
      c <- if ((f1/f2 > 1) == TRUE) 1 else 0
      if ((c-test1[i,58]) != 0) erro <- erro +1
    }
  }
  #Obtendo a acuracia da classe 1
  acerto <- 1 - erro/length(test1[,1])
  acuracia1 <- c(acuracia1, acerto)
  
  #Testando o algoritmo para classe 2
  erro <- 0
  for (i in 1:length(test2[,1])){
    l <- t(test2[i,1:57])
    f1 <- pdfnvar(l, u1 , cov1, 57)
    f2 <- pdfnvar(l, u2 , cov2, 57)
    if (f2 == 0){
      c <- 1
    } else {
      c <- if ((f1/f2 >= 1) == TRUE) 1 else 0
      if ((c-test2[i,58]) != 0) erro <- erro +1
    }
  }
  
  #Obtendo a acuracia da classe 2
  acerto <- 1 - erro/length(test2[,1])
  acuracia2 <- c(acuracia2, acerto)
}

#Imprimindo a saida
cat("\n acuracia classe 1: [", acuracia1, "]\n", 
    "acuracia classe 2: [", acuracia2, "]\n\n", "media1: ", 
    mean(acuracia1)*100,"%  media2: ", mean(acuracia2)*100, "%",
    "\n", "desvio1: ", sd(acuracia1)*100,"%  desvio2: ", 
    sd(acuracia2)*100,"%", "\n\n")
cat("\n\nAcuracia total: ", mean(c(acuracia1,acuracia2))*100, 
    "% e Desvio Padrao Total: ", sd(c(acuracia1, acuracia2))*100, "%.\n\n")