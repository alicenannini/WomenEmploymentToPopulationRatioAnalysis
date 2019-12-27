occ = read.csv("occupazione_donne_under25.csv",row.names=1)
head(occ)

# http://dati.istat.it/Index.aspx?DataSetCode=DCCV_TAXOCCUDE1#
#####################################################################################
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
# Ogni anno i valori diminuiscono (dal rosso verso il giallo)
ts.plot(scale(m_to,scale=F),col=heat.colors(42))
# casino: non si vede ancora la stagionalità ?
# Evidenziamo il profilo medio:
lines(rowMeans(scale(m_to)),lwd=3,col="green3")
# in bianco si vede l'andamento annuale medio
par(bg="white")
# nada de nada
to.da = decompose(to)
plot(to.da)
plot(to.da$random, col = "red")
lines(to.da$seasonal)
legend(1977,0.8,legend=c("seasonal","random"),col=c("black","red"),lty=1,cex=1,bty="n",ncol=2)

to.dm=decompose(to,type="multiplicative")
plot(to.dm)
plot(to.dm$random, col = "red")
lines(to.dm$seasonal)
legend(1977,0.8,legend=c("seasonal","random"),col=c("black","red"),lty=1,cex=1,bty="n",ncol=2)
# Non cambia niente:
plot(to.da$seasonal)
lines(mean(to.dm$trend,na.rm=T)*(to.dm$seasonal-1),col="red")
plot(to.da$random)
lines(mean(to.dm$trend,na.rm=TRUE)*(to.dm$random-1),col="red")
# Confronto i residui:
layout(t(1:2))
plot(to.da$random,xlab="Additive")
plot(to.dm$random,xlab="Multiplicative")
# Confronto sparsità dei residui:
to.dar=as.vector(window(to.da$random,c(1977,7),c(2019,1)))
plot(to.dar,pch=20,xlab="Additive")
to.dmr=as.vector(window(to.dm$random,c(1977,7),c(2019,1)))
plot(to.dmr,pch=20,xlab="Multiplicative")
# Residui additivi:
to.dar=as.vector(window(to.da$random,c(1977,7),c(2019,1))) #finestra per non considerare i valori NA
plot(to.dar,pch=20)
1 - var(to.dar)/var(window(to,c(1977,7),c(2019,1)))
acf(to.dar,col="orange")
# Analisi dei residui:
layout(t(1:2))
hist(to.dar,20,freq=F)
lines(density(to.dar),col="blue")
lines(sort(to.dar),dnorm(sort(to.dar),mean(to.dar),sd(to.dar)),col="red")
qqnorm(to.dar)
qqline(to.dar)
layout(1)
shapiro.test(to.dar)
# Si permutano le ascisse:
plot(sample(1:length(to.dar)),to.dar,pch=20)
acf(to.dar[sample(1:length(to.dar))],col="orange")
# Residui moltiplicativi:
to.dmr=as.vector(window(to.dm$random,c(1977,7),c(2019,1)))
plot(to.dmr,pch=20)
to.dmrl=log(to.dmr)
1 - var(to.dmrl)/var(window(log(to),c(1949,7),c(1960,6)))
acf(to.dmr,30,col="orange")
acf(to.dmrl,30,col="orange")
layout(t(1:2))
hist(to.dmrl,20,freq=F)
lines(density(to.dmrl),col="blue")
lines(sort(to.dmrl),dnorm(sort(to.dmrl),mean(to.dmrl),sd(to.dmrl)),col="red")
qqnorm(to.dmrl)
qqline(to.dmrl)
layout(1)
shapiro.test(to.dmrl)
sd(acf(to.dar,plot=F)$acf)
sd(acf(to.dmr,plot=F)$acf)

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
plot(to.se,lwd=2)
plot(to.set,lwd=2)
plot(to.se,predict(se,5))
plot(to.set,predict(to.set,5))
layout(1)
# Smorzamento con trend e stagionalità
to.hw=HoltWinters(to)
plot(to.hw,lwd=2)
layout(1:2)
plot(to.hw,predict(to.hw,4),main="Previsione a 1 anno")
# si ottiene una previsione decisamente attendibile
# Se si prova a effettuare una previsione di più anni:
plot(to.hw,predict(to.hw,12),main="Previsione a 3 anni")
layout(1)
# il risultato non è esaltante, visto che la previsione del primo anno viene ripetuta altre due volte, un po’ traslata verso l'alto, seguendo un trend del tutto lineare.
# Analisi della sottostruttura fitted:
plot(to.hw$fitted)
# Confronto con i dati della decomposizione:
to1 = ts(to.data[[1]],start=1977,frequency = 4)
to.stl=stl(to1,"periodic")
# confronto grafico (delle intercette) con il trend di stl
ts.plot(to.stl$time.series[,2],to.hw$fitted[,2],col=c("black","red"))
# confronto grafico con la stagionalità di stl
ts.plot(to.stl$time.series[,1],to.hw$fitted[,4],col=c("black","red"))
# RESIDUI:
to.hw.r=residuals(to.hw)
plot(to.hw,predict(to.hw,4))
lines(predict(to.hw,4)+quantile(to.hw.r,0.05),col="green3")
lines(predict(to.hw,4)+quantile(to.hw.r,0.95),col="green3")

#####################################################################################

