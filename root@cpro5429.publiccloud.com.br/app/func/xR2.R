xR2=function(m,n,y){
  if (n>1) {
    amplit=numeric(m) 	
    mat=matrix(y,m,n,byrow=T)
    media=apply(mat,1,mean)
    for (i in 1:m) amplit[i]=max(mat[i,])-min(mat[i,])
    x2barras=mean(media)
    Rbarra=mean(amplit) 
    d2=c(1.1296,1.6918,2.0535,2.3248,2.5404,2.7074,2.8501,
         2.9677,3.0737,3.1696)
    d3=c(.8541,.8909,.8800,.8674,.8508,.8326,.8209,
         .8102,.7978,.7890)
    LIC1=x2barras-3*Rbarra/(d2[n-1]*sqrt(n))
    LSC1=x2barras+3*Rbarra/(d2[n-1]*sqrt(n))
    LIC2=Rbarra-3*d3[n-1]*Rbarra/d2[n-1]
    LSC2=Rbarra+3*d3[n-1]*Rbarra/d2[n-1]
    if (LIC2 < 0) LIC2=0
    
  } else {
    amplit=numeric(m-1)
    media=y
    for (i in 2:m) amplit[i-1]=sqrt((y[i]-y[i-1])^2)
    x2barras=mean(media)
    Rbarra=mean(amplit) 
    d2=1.1296
    d3=.8541
    LIC1=x2barras-3*Rbarra/(d2*sqrt(n))
    LSC1=x2barras+3*Rbarra/(d2*sqrt(n))
    LIC2=Rbarra-3*d3*Rbarra/d2
    LSC2=Rbarra+3*d3*Rbarra/d2
    if (LIC2 < 0) LIC2=0
  }
  par(mfrow=c(2,1))
  amostra1=matrix(rep(seq(1,m),4),m,4,byrow=F)
  if (n>1) amostra2=amostra1 else amostra2=amostra1[-1,]
  xbar=cbind(rep(x2barras,m),rep(LIC1,m),rep(LSC1,m),media)
  if (n>1) {
    rbar=cbind(rep(Rbarra,m),rep(LIC2,m),rep(LSC2,m),amplit) 
  } else rbar=cbind(rep(Rbarra,m-1),rep(LIC2,m-1),
                    rep(LSC2,m-1),amplit)
  
  tab<-data.frame(n.amostra=seq_len(m),estatistica=xbar[,4],lc = xbar[,1], L.infer = xbar[,2], L.super=xbar[,3])
  
  p2<-ggplot(data=tab,aes(y=estatistica,x=n.amostra))+
    geom_line()+
    geom_point(aes(y=estatistica,x=n.amostra),col="blue")+
    geom_line(aes(y=lc,x=n.amostra),col="blue")+
    geom_line(aes(y=L.super,x=n.amostra),col="red")+
    geom_line(aes(y=L.infer,x=n.amostra),col="red")+
    geom_text(data=nomes,aes(y=y,x=x,label=nom))+
    labs(title="Método 2",x="Amostras",y="determinante")+
    theme_bw()+
    theme(plot.title=element_text(hjust=.5))
  return(ggplotly(p2, source = "source")%>%
    style(hoverinfo = "y")%>%
    layout(xaxis=list(
      range=c(0.8,max(tab$n.amostra)),
      tickvals= seq(0,max(tab$n.amostra)),
      ticktext = paste(seq(0,max(tab$n.amostra)))
    )))
  
  matplot(amostra2,rbar,type="o",ylab="Amplitude",
          col=c("black","red","red","blue"),
          ylim=c(min(rbar),max(rbar)),
          xlab="Amostras",main="Gráfico R",pch=20, lty=1, lwd=2)
  matplot(amostra1,xbar,type="o",ylab="Média",xlab="Amostras",
          main=expression(paste("Gráfico " * bar(x))),col=c("black","red","red",
                                                            "blue"),ylim=c(min(xbar),max(xbar)),pch=20, lty=1, lwd=2)
  out=rbind(round(xbar[1,1:3],2),round(rbar[1,1:3],2))
  colnames(out)=c("Referência","LIC","LSC")
  out
}
