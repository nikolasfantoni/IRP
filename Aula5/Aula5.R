#AULA 3 - Gaussiana no espaço R²

#Limpando o ambiente
rm(list=ls())

#funcao estimativa densidade para n variáveis
pdfnvar <- function(x, m, K, n) {(1/(sqrt((2*pi)^(n)*(det(K)))))*exp(-0.5*(t(x-m)%*%(solve(K))%*%(x-m)))}

#Lendo os dados
heart <- read.csv( "heart.dat", sep=" ",header = FALSE)
ic1 <- which(heart$V14 == 1)
ic2 <- which(heart$V14 == 2)
heartc1 <- heart[ic1,]
heartc2 <- heart[ic2,]

#Separando em 90% treinamento e 10% teste
iseq1 <- sample(length(heartc1[,1]))
iseq2 <- sample(length(heartc2[,1]))
trainc1 <- heartc1[iseq1[1:0.9*length(iseq1)],]
trainc2 <- heartc2[iseq2[1:0.9**length(iseq2)],]
test <- rbind(heartc1[iseq1[(0.9*length(iseq1)+1):length(iseq1)],], heartc1[iseq2[(0.9*length(iseq2)+1):length(iseq2)],])

#media e desvio padrão
u1 <- rbind(mean(heartc1[,1]), mean(heartc1[,2]), mean(heartc1[,3]), mean(heartc1[,4]), mean(heartc1[,5]),
            mean(heartc1[,6]), mean(heartc1[,7]), mean(heartc1[,8]), mean(heartc1[,9]), mean(heartc1[,10]),
          mean(heartc1[,11]), mean(heartc1[,12]), mean(heartc1[,13]))
u2 <- rbind(mean(heartc2[,1]), mean(heartc2[,2]), mean(heartc2[,3]), mean(heartc2[,4]), mean(heartc2[,5]),
            mean(heartc2[,6]), mean(heartc2[,7]), mean(heartc2[,8]), mean(heartc2[,9]), mean(heartc2[,10]),
            mean(heartc2[,11]), mean(heartc2[,12]), mean(heartc2[,13]))
s1 <- rbind(sd(heartc1[,1]), sd(heartc1[,2]), sd(heartc1[,3]), sd(heartc1[,4]), sd(heartc1[,5]),
            sd(heartc1[,6]), sd(heartc1[,7]), sd(heartc1[,8]), sd(heartc1[,9]), sd(heartc1[,10]),
          sd(heartc1[,11]), sd(heartc1[,12]), sd(heartc1[,13])) 
s2 <- rbind(sd(heartc2[,1]), sd(heartc2[,2]), sd(heartc2[,3]), sd(heartc2[,4]), sd(heartc2[,5]),
            sd(heartc2[,6]), sd(heartc2[,7]), sd(heartc2[,8]), sd(heartc2[,9]), sd(heartc2[,10]),
            sd(heartc2[,11]), sd(heartc2[,12]), sd(heartc2[,13])) 

#covariancias e coeficientes de correlação
cov1 <- cov(heartc1[,1:13])
cov2 <- cov(heartc2[,1:13])


              