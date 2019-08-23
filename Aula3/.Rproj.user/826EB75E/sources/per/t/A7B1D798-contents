#Aula Bayes - Ex2

rm(list=ls())
library('plot3D')

# definição dos dados duas dimensoes
s1<-0.3
s2<-0.3
nc<-200
xc1<-rbind(matrix(rnorm(nc*2),ncol=2)*s1 + t(matrix((c(4,4)),ncol=nc,nrow=2)),
           matrix(rnorm(nc*2),ncol=2)*s1 + t(matrix((c(2,2)),ncol=nc,nrow=2)))
xc2<-rbind(matrix(rnorm(nc*2),ncol=2)*s2 + t(matrix((c(2,4)),ncol=nc,nrow=2)),
           matrix(rnorm(nc*2),ncol=2)*s2 + t(matrix((c(4,2)),ncol=nc,nrow=2)))
xc1 <- cbind(xc1,rep(1,nc*2))
xc2 <- cbind(xc2,rep(-1,nc*2))

#separação de dados de treinamento e teste
iseq <- sample(nc*2)
xtest <- rbind(xc1[iseq[((0.9*nc*2)+1):(nc*2)],], xc2[iseq[((0.9*nc*2)+1):(nc*2)],])
xc1 <- xc1[iseq[1:(0.9*nc*2)],]
xc2 <-xc2[iseq[1:(0.9*nc*2)],]

#plotando dados finais de entrada
plot(xc1[,1],xc1[,2],col = ' red ' , xlim = c(0,6),ylim = c(0,6),xlab = ' x1 ' ,ylab= ' x2 ', lty=1,main = 'Dados Amostrados' )
par(new=T)
plot(xc2[,1],xc2[,2],col = ' blue ' , xlim = c(0,6),ylim = c(0,6),xlab = '' ,ylab= '' , pch=0)
par(new=T)
plot(xtest[,1],xtest[,2],col = ' green ' , xlim = c(0,6),ylim = c(0,6),xlab = '' ,ylab= '', pch=3)
legend("topleft", legend=c("C1", "C2", "Teste"),
       col=c("red", "blue", "green"), pch=c(1, 0, 3), cex=0.8)


#funcao para estimativa da densidade de 2 variáveis
pdf2var<-function(x,y,u1,u2,s1,s2,p) {(1/(2*pi*s1*s2*sqrt(1-(p^2))))*
    exp((-(1)/(2*(1-(p^2))))*((((x-u1)^2)/((s1)^2))+(((y-u2)^2)/((s2)^2))-
                                ((2*p*(x-u1)*(y-u2))/(s1*s2))))
}

#cáculo da média e desvio padrão das classes
u1<-rbind(mean(xc1[,1]),mean(xc1[,2]))
u2<-rbind(mean(xc2[,1]),mean(xc2[,2]))
s1<-rbind(sd(xc1[,1]),sd(xc1[,2]))
s2<-rbind(sd(xc2[,1]),sd(xc2[,2]))

#cálculo covariâncias e coeficientes de correlação
covc1 <- cov(xc1[,1],xc1[,2])
pc1 <- covc1/sqrt(prod(s1^2))
covc2 <- cov(xc2[,1],xc2[,2])
pc2 <- covc2/sqrt(prod(s2^2))

#classificando a amostra de teste
class1 <- pdf2var(xtest[,1],xtest[,2],mean(xc1[,1]),mean(xc1[,2]),sd(xc1[,1]),sd(xc1[,2]),pc1)
class2 <- pdf2var(xtest[,1],xtest[,2],mean(xc2[,1]),mean(xc2[,2]),sd(xc2[,1]),sd(xc2[,2]),pc2)
classfinal <- class1/class2
classfinal <- (classfinal>=1)
for (i in 1:length(classfinal)){
  if (classfinal[i] == TRUE) classfinal[i] <- 1
  if (classfinal[i] == FALSE) classfinal[i] <- -1
}

#cálculo do erro
erro <- xtest[,3]-classfinal
qtderro <- 0
for (i in 1:length(erro)){
  if (erro[i] != 0) qtderro <- qtderro+1
}
erropercentual <- qtderro/length(erro)*100
acertopercentual <- 100-erropercentual

#estimando as classes em cada ponto de um grid
seqi<-seq(0.06,6,0.06)
seqj<-seq(0.06,6,0.06)
M1 <- matrix(0,nrow=length(seqi),ncol=length(seqj)) 
ci<-0
for (i in seqi){
  ci<-ci+1
  cj<-0
  for(j in seqj)
  {
    cj<-cj+1
    f1<- pdf2var(i,j,mean(xc1[,1]),mean(xc1[,2]),sd(xc1[,1]),sd(xc1[,2]),pc1)
    f2<- pdf2var(i,j,mean(xc2[,1]),mean(xc2[,2]),sd(xc2[,1]),sd(xc2[,2]),pc2)
    classfinal <- (f1/f2>=1)
    if (classfinal == TRUE) classfinal <- 1
    if (classfinal == FALSE) classfinal <- -1
    M1[ci,cj] <-  classfinal
  }
}

#plotando as superfícies de contorno
par(new=T)
contour(seqi, seqj, M1, xlim = c(0,6),ylim = c(0,6))
