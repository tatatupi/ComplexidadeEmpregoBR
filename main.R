source("calc_ql.R")

filename1="2013.csv"
filename2="familiaxcnae2013.csv"


dados <- read.table(filename1, header=TRUE, sep=';',check.names=FALSE)

ql <- calc_ql(dados,0.8)


k0 <- rowSums(ql)
kp0 <- colSums(ql)


k1<-c()
kp1<-c()
for (i in 1:nrow(ql)) {
  k1[i]<-ql[i,]%*%kp0
}
for (j in 1:ncol(ql)) {
  kp1[j]<-ql[,j]%*%k0
}


k2<-c()
kp2<-c()
for (i in 1:nrow(ql)) {
  k2[i]<-ql[i,]%*%kp1
}
for (j in 1:ncol(ql)) {
  kp2[j]<-ql[,j]%*%k1
}


k<-cbind(k0,k1,k2)
kp<-rbind(kp0,kp1,kp2)


familia<-as.numeric(read.table(filename2, header=TRUE, sep=';',check.names=FALSE))

ocup<-c()
for (i in 1:nrow(ql)) {
  ocup[i]<-(ql[i,]%*%familia)/sum(ql[i,])
}

par(mfrow=c(1,3))
plot(k0,ocup)
plot(k1,ocup)
plot(k2,ocup)