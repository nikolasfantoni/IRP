#AULA 3 - Gaussiana no espaço R²

rm(list=ls())
library('plot3D')

# definição dos dados duas dimensoes
s1<-0.6
s2<-0.8
s3<-0.2
s4<-1
nc<-100
xc1<-matrix(rnorm(nc*2),ncol=2)*s1 + t(matrix((c(2,2)),ncol=nc,nrow=2))
xc2<-matrix(rnorm(nc*2),ncol=2)*s2 + t(matrix((c(4,4)),ncol=nc,nrow=2))
xc3<-matrix(rnorm(nc*2),ncol=2)*s3 + t(matrix((c(2,4)),ncol=nc,nrow=2))
xc4<-matrix(rnorm(nc*2),ncol=2)*s4 + t(matrix((c(4,2)),ncol=nc,nrow=2))


#plotando dados finais de entrada
plot(xc1[,1],xc1[,2],col = ' red ' , xlim = c(0,6),ylim = c(0,6),xlab = ' x1 ' ,ylab= ' x2 ', 
     lty=1,main = 'Dados Amostrados',xaxs="i",yaxs="i")
par(new=T)
plot(xc2[,1],xc2[,2],col = ' green ' , xlim = c(0,6),ylim = c(0,6),xlab = '' ,ylab= '' , pch=1,
     xaxs="i", yaxs="i")
par(new=T)
plot(xc3[,1],xc3[,2],col = ' yellow ' , xlim = c(0,6),ylim = c(0,6),xlab = '' ,ylab= '' , pch=1,
     xaxs="i", yaxs="i")
par(new=T)
plot(xc4[,1],xc4[,2],col = ' blue ' , xlim = c(0,6),ylim = c(0,6),xlab = '' ,ylab= '' , pch=1,
     xaxs="i", yaxs="i")
legend("topleft", legend=c("C1", "C2", "C3", "C4"),
       col=c("red", "green", "yellow", "blue"), pch=c(1, 1, 1, 1), cex=0.8)


#funcao para estimativa da densidade de 2 variáveis
pdf2var<-function(x,y,u1,u2,s1,s2,p) {(1/(2*pi*s1*s2*sqrt(1-(p^2))))*
    exp((-(1)/(2*(1-(p^2))))*((((x-u1)^2)/((s1)^2))+(((y-u2)^2)/((s2)^2))-
                                ((2*p*(x-u1)*(y-u2))/(s1*s2))))
}

#cáculo da média e desvio padrão das classes
u1<-rbind(mean(xc1[,1]),mean(xc1[,2]))
u2<-rbind(mean(xc2[,1]),mean(xc2[,2]))
u3<-rbind(mean(xc3[,1]),mean(xc3[,2]))
u4<-rbind(mean(xc4[,1]),mean(xc4[,2]))
s1<-rbind(sd(xc1[,1]),sd(xc1[,2]))
s2<-rbind(sd(xc2[,1]),sd(xc2[,2]))
s3<-rbind(sd(xc3[,1]),sd(xc3[,2]))
s4<-rbind(sd(xc4[,1]),sd(xc4[,2]))

#cálculo covariâncias e coeficientes de correlação
covc1 <- cov(xc1[,1],xc1[,2])
pc1 <- covc1/sqrt(prod(s1^2))
covc2 <- cov(xc2[,1],xc2[,2])
pc2 <- covc2/sqrt(prod(s2^2))
covc3 <- cov(xc3[,1],xc3[,2])
pc3 <- covc3/sqrt(prod(s3^2))
covc4 <- cov(xc4[,1],xc4[,2])
pc4 <- covc4/sqrt(prod(s4^2))

#estimando as densidades em cada ponto de um grid
seqi<-seq(0.06,6,0.06)
seqj<-seq(0.06,6,0.06)
M1 <- matrix(0,nrow=length(seqi),ncol=length(seqj)) 
M2 <- matrix(0,nrow=length(seqi),ncol=length(seqj))
M3 <- matrix(0,nrow=length(seqi),ncol=length(seqj))
M4 <- matrix(0,nrow=length(seqi),ncol=length(seqj))
ci<-0
for (i in seqi){
  ci<-ci+1
  cj<-0
  for(j in seqj)
  {
    cj<-cj+1
    M1[ci,cj]<- pdf2var(i,j,mean(xc1[,1]),mean(xc1[,2]),sd(xc1[,1]),sd(xc1[,2]),pc1)
    M2[ci,cj]<- pdf2var(i,j,mean(xc2[,1]),mean(xc2[,2]),sd(xc2[,1]),sd(xc2[,2]),pc2)
    M3[ci,cj]<- pdf2var(i,j,mean(xc3[,1]),mean(xc3[,2]),sd(xc3[,1]),sd(xc3[,2]),pc3)
    M4[ci,cj]<- pdf2var(i,j,mean(xc4[,1]),mean(xc4[,2]),sd(xc4[,1]),sd(xc4[,2]),pc4)
  }
}

#plotando as densidades
persp3D(seqi,seqj,M1,counter=T,theta = 55, phi = 40, r = 100, d = 1, expand = 0.5,
        ltheta = 90, lphi = 180, shade = 0.4, ticktype = "detailed", nticks=5)
persp3D(seqi,seqj,M2,counter=T,theta = 55, phi = 40, r = 100, d = 1, expand = 0.5,
        ltheta = 90, lphi = 180, shade = 0.4, ticktype = "detailed", nticks=5, add=T)
persp3D(seqi,seqj,M3,counter=T,theta = 55, phi = 40, r = 100, d = 1, expand = 0.5,
        ltheta = 90, lphi = 180, shade = 0.4, ticktype = "detailed", nticks=5, add=T)
persp3D(seqi,seqj,M4,counter=T,theta = 55, phi = 40, r = 100, d = 1, expand = 0.5,
        ltheta = 90, lphi = 180, shade = 0.4, ticktype = "detailed", nticks=5, add=T)

#plotando as superfícies de contorno
contour2D(M1,seqi,seqj,colkey = NULL)
par(new=T)
contour2D(M2,seqi,seqj,colkey = NULL)
par(new=T)
contour2D(M3,seqi,seqj,colkey = NULL)
par(new=T)
contour2D(M4,seqi,seqj,colkey = NULL)

#estimando as classes em cada ponto de um grid
seqi<-seq(0.03,6,0.03)
seqj<-seq(0.03,6,0.03)
Mk <- matrix(0,nrow=length(seqi),ncol=length(seqj)) 
ci<-0
for (i in seqi){
  ci<-ci+1
  cj<-0
  for(j in seqj)
  {
    cj<-cj+1
    f1<- pdf2var(i,j,mean(xc1[,1]),mean(xc1[,2]),sd(xc1[,1]),sd(xc1[,2]),pc1)
    f2<- pdf2var(i,j,mean(xc2[,1]),mean(xc2[,2]),sd(xc2[,1]),sd(xc2[,2]),pc2)
    f3<- pdf2var(i,j,mean(xc3[,1]),mean(xc3[,2]),sd(xc3[,1]),sd(xc3[,2]),pc3)
    f4<- pdf2var(i,j,mean(xc4[,1]),mean(xc4[,2]),sd(xc4[,1]),sd(xc4[,2]),pc4)
    if (max(f1, f2, f3, f4) == f1) Mk[ci,cj] <- 1
    if (max(f1, f2, f3, f4) == f2) Mk[ci,cj] <- 2
    if (max(f1, f2, f3, f4) == f3) Mk[ci,cj] <- 3
    if (max(f1, f2, f3, f4) == f4) Mk[ci,cj] <- 4
  }
}

#plotando as superfícies de contorno
par(new=T)
image2D(Mk, x = seqi, y=seqj, col=c("red", "green", "yellow", "blue"), 
        colkey=(plot=FALSE),xlab = '' ,ylab= '', xlim = c(0,6),ylim = c(0,6), xaxs='i', yaxs='i' )
