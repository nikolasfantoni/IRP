#AULA 5 - Base Heart

#Limpando o ambiente
rm(list=ls())

#funcao estimativa densidade para n variáveis
pdfnvar <- function(x, m, K, n) {(1/(sqrt((2*pi)^(n)*(det(K)))))*exp(-0.5*(t(x-m)%*%(solve(K))%*%(x-m)))}

#percentual amostra
per <- 0.9

#Lendo os dados
heart <- read.csv( "heart.dat", sep=" ",header = FALSE)
ic1 <- which(heart$V14 == 1)
ic2 <- which(heart$V14 == 2)
heartc1 <- heart[ic1,]
heartc2 <- heart[ic2,]

#Separando em 90% treinamento e 10% teste
iseq1 <- sample(length(heartc1[,1]))
iseq2 <- sample(length(heartc2[,1]))
trainc1 <- heartc1[iseq1[1:(per*length(iseq1))],]
trainc2 <- heartc2[iseq2[1:(per*length(iseq2))],]
test <- rbind(heartc1[iseq1[(per*length(iseq1)+1):length(iseq1)],], heartc2[iseq2[(per*length(iseq2)+1):length(iseq2)],])

#media e desvio padrão
u1 <- rbind(mean(trainc1[,1]), mean(trainc1[,2]), mean(trainc1[,3]), mean(trainc1[,4]), mean(trainc1[,5]),
            mean(trainc1[,6]), mean(trainc1[,7]), mean(trainc1[,8]), mean(trainc1[,9]), mean(trainc1[,10]),
            mean(trainc1[,11]), mean(trainc1[,12]), mean(trainc1[,13]))
u2 <- rbind(mean(trainc2[,1]), mean(trainc2[,2]), mean(trainc2[,3]), mean(trainc2[,4]), mean(trainc2[,5]),
            mean(trainc2[,6]), mean(trainc2[,7]), mean(trainc2[,8]), mean(trainc2[,9]), mean(trainc2[,10]),
            mean(trainc2[,11]), mean(trainc2[,12]), mean(trainc2[,13]))

#covariancias e coeficientes de correlação
cov1 <- cov(trainc1[,1:13])
cov2 <- cov(trainc2[,1:13])
erro <- 0
for (i in 1:length(test[,1])){
  l <- t(test[i,1:13])
  f1 <- pdfnvar(l, u1 , cov1, 13)
  f2 <- pdfnvar(l, u2 , cov2, 13)
  c <- if ((f1/f2 >= 1) == TRUE) 1 else 2
  if ((c-test[i,14]) != 0) erro <- erro +1
}

acertoporcento <- 100 - erro/length(test[,1])*100
