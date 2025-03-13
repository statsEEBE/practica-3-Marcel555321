#Solucion Pregunta 1
x<-c(0,1)
fx<-c(0.68,0.32)
cbind(x,fx)
plot(x,fx,pch=16,col="red",ylim=c(0,1))
lines(x,fx,type="h",col="red")
mu<-sum(x*fx)
mu
sigmasq<-sum((x-mu)^2*fx)
sigmasq
sq<-sum(x^2*fx)-mu^2
sq
sample(x,43,prob=fx,replace=TRUE)
sum(sample(x,43,prob=fx,replace=TRUE))
y<-function(i){sum(sample(x,43,prob=fx,replace=TRUE))}
y
#bucle
m=400000
muestra<-sapply(1:m,y)
muestra
#frequencia
fi<-table(muestra)/m
fi
data.frame(fi)
barplot(fi)
#freqüencias relativas
data.frame(fi,Fi=cumsum(fi))

dbinom(13,43,0.32)
#tabla probabilidad
data.frame(y=0:43,Prob=dbinom(0:43,43,0.32))
y<-0:43
fy<-dbinom(0:43,43,0.32)
plot(y,fy,col="red")
lines(y,fy,type="h",col="red")


##########


y<-0:44
fi<-dbinom(0:44,44,0.32)
df<-data.frame(Y=y,Prob=fi)
df
Fi<-cumsum(df$Prob)
Fi
cbind(y,fi,Fi)
pbinom(16,44,0.32)
plot(y,Fi,type="s",col="red")
#
x<-0:24
fi<-dbinom(x,24,0.68)
sum(x*fi)
sum(x^2*fi)-(sum(x*fi))^2
qbinom(0.25,24,0.68)
