to.data = read.csv("occupazione_donne_da15a64_dal1977.csv",row.names=1)
head(to.data)
to = ts(to.data,frequency=4,start=c(1977))
to
ts.plot(to,xlab="anni",ylab="tasso di occupazione")
# Sembra che sia presente un trend ascendente, mentre non è molto evidente una stagionalità
# DECOMPOSIZIONE
acf(to,col="orange")
acf(to,50,col="orange")
# Anche dalla funzione di autocorrelazione, non emergono particolari periodicità. Sappiamo comunque che la funzione di autocorrelazione è di dubbia interpretazione se in presenza di un forte trend. Provando a visualizzare la serie al netto del trend:
plot(diff(to),xlab="anni",ylab="tasso di occupazione")
# Si vede già che ogni anno è visualizzato da un picco, sottolineando quindi la presenza di stagionalità. Questi picchi sono ben distinti per i primi anni, almeno fino al 2003, per poi cambiare andamento e suddividersi in picchi più confusi tra i vari trmiestri.
acf(diff(to),col="orange")
acf(diff(to),50,col="orange")
acf(diff(to),171,col="orange")
# Sembra che così una componente stagionale possa essere effettivamente presente: in corrispondenza dell'inizio di ogni periodo, vediamo che il valore di acf si staglia al di sopra degli altri valori, evento che era stato mascherato nei grafici precedenti dalla presenza di un trend accentuato.
par(bg="black")
m_to = t(matrix(to[1:168],42,4))
ts.plot(m_to,col=heat.colors(42))
lines(rowMeans(m_to),lwd=3,col="green3")
# Ogni anno i valori aumentano (dal rosso verso il bianco)
ts.plot(scale(m_to,scale=F),col=heat.colors(42))
# Evidenziamo il profilo medio:
lines(rowMeans(scale(m_to)),lwd=3,col="green3")
# in bianco si vede l'andamento annuale medio
par(bg="white")
to.da = decompose(to)
plot(to.da)
plot(to.da$random, col = "red")
lines(to.da$seasonal)
legend(1977,0.8,legend=c("seasonal","random"),col=c("black","red"),lty=1,cex=1,bty="n",ncol=2)

# ANALISI E PREVISIONE
# Smorzamento esponenziale
to.se=HoltWinters(to,beta=F,gamma=F)
to.se
plot(to.se)
summary(to.se)
predict(to.se,1)
plot(to.se,predict(se,1))
plot(to.se);points(HoltWinters(to,alpha=0.9,beta=F,gamma=F)$fitted,col="blue",type="l")
# Smorzamento esponenziale con trend
to.set=HoltWinters(to,gamma=F)
to.set
plot(to.set)
plot(to.set,predict(to.set,12))
# Confronto tra SE e SET:
layout(t(1:2))
plot(to.se,lwd=2,main="SE")
plot(to.set,lwd=2,main="SET")
plot(to.se,predict(se,5),main="SE")
plot(to.set,predict(to.set,5),main="SET")
layout(1)
plot(to.set);points(HoltWinters(to,alpha=0.9,beta=F,gamma=F)$fitted,col="blue",type="l")

# Smorzamento con trend e stagionalità
to.hw=HoltWinters(to)
to.hw
plot(to.hw,lwd=2)
layout(1:2)
plot(to.hw,predict(to.hw,4),main="Previsione a 1 anno",lwd=2)
# si ottiene una previsione decisamente attendibile
# Se si prova a effettuare una previsione di più anni:
plot(to.hw,predict(to.hw,12),main="Previsione a 3 anni",lwd=2)
layout(1)
# il risultato non è esaltante, visto che la previsione del primo anno viene ripetuta altre due volte, un po’ traslata verso l'alto, seguendo un trend del tutto lineare.
# RESIDUI:
to.hw.r=as.vector(residuals(to.hw))
plot(to.hw.r,pch=20)
1 - var(to.hw.r)/var(to)
acf(to.hw.r)
layout(t(1:2))
hist(to.hw.r,20,freq=F)
lines(density(to.hw.r),col="blue")
lines(sort(to.hw.r),dnorm(sort(to.hw.r),mean(to.hw.r),sd(to.hw.r)),col="red")
qqnorm(to.hw.r)
qqline(to.hw.r)
layout(1)
length(to.hw.r)
shapiro.test(to.hw.r)
shapiro.test(rnorm(167))
# Incertezza delle previsioni
quantile(to.hw.r,0.05)
quantile(to.hw.r,0.95)
predict(to.hw,1) + quantile(to.hw.r,0.05)
predict(to.hw,1) + quantile(to.hw.r,0.95)
ts.plot(predict(to.hw,4),
predict(to.hw,4)+quantile(to.hw.r,0.05),
predict(to.hw,4)+quantile(to.hw.r,0.95),
col=c("red","green3","green3"),lwd=c(2,1,1))
# Autovalidazione
l=length(to)
n=4
pred=rep(0,n)
for(i in 1:n){
ts<-ts(to[1:(l-i)],frequency=4,start=c(1977))
ts.hw<-HoltWinters(ts)
pred[n-i+1]=predict(ts.hw,1)
}
real=to.data[(l-n+1):l]
sqrt(var(pred-real)/var(real))
plot(c(1:4),y=c(49,49.5,50,51),col="white",xlab="Time",ylab="Value")
lines(real)
lines(pred,col="red")
legend(1,51.5,legend=c("real","prediction"),col=c("black","red"),lty=1,cex=1,bty="n",ncol=2)

