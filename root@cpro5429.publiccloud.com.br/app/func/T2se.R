T2se=function(n,y,etapa){
  p=ncol(y);obs=nrow(y);m=obs/n
  if(n>1) {
    Sbarra=matrix(0,p,p); media=matrix(NA,m,p)
    amostra=matrix(NA,n,p); t2=numeric(m)	
    for (i in 1:m) {
      amostra=y[(n*i-n+1):(n*i),]
      media[i,]=apply(amostra,2,mean); S=cov(amostra)
      Sbarra=S+Sbarra
    }
    x2barras=apply(media,2,mean); Sinv=solve(Sbarra/m)
    for (i in 1:m) {
      z=media[i,]-x2barras; t2[i]=n*(z%*%Sinv%*%z)
    }
    if(etapa==1) {
      LSC=p*(m-1)*(n-1)*qf(0.997,p,m*n-m-p+1)/(m*n-m-p+1)
    } 
    if(etapa==2) {
      LSC=p*(m+1)*(n-1)*qf(0.997,p,m*n-m-p+1)/(m*n-m-p+1)
    }
    time=cbind(seq(1,m),seq(1,m)); W1=cbind(t2,rep(LSC,m))
    # par(mfrow=c(1,1))
    # matplot(time, W1, type="o", pch=20, col=c("black","red"),
    #         main=expression(paste("Gráfico " * T^2)), xlab="Amostras", ylab=expression(paste(T^2)),
    #         ylim=c(min(W1), max(W1)), lty=1,lwd=2)
    # 
    # 
    tab<-data.frame(n.amostra=time[,1],estatistica=W1[,1],L.super=W1[,2])
    p1<-ggplot(data=tab,aes(y=estatistica,x=n.amostra))+
      geom_line()+
      geom_point(aes(y=estatistica,x=n.amostra),col="blue")+
      geom_line(data=tab,aes(y=L.super,x=n.amostra),col="red")+
      geom_text(aes(y=tab[1,'L.super']+1,x=tab[5,'n.amostra']),label=paste("Limite Superior= ",round(tab[1,'L.super'],2)))+
      xlim(c(1,max(tab$n.amostra)))+
      labs(title="T² de Hotelling",x="Amostras",y="T²")+
      theme_bw()+
      theme(plot.title=element_text(hjust=.5))
    
    
    return(ggplotly(p1, source = "source3")%>%
      style(hoverinfo = "y")%>%
      layout(xaxis=list(
        range=c(0.8,max(tab$n.amostra)),
        tickvals= seq(0,max(tab$n.amostra)),
        ticktext = paste(seq(0,max(tab$n.amostra)))
      )))
    
    if (etapa==2) {
      lista=list(x2barras,Sbarra/m,LSC)
      names(lista)=c("Vetor de Médias",
                     "Matriz de Covariâncias", 
                     "Limite Superior de Controle")
      lista
    }
  } else {
    t2m1=numeric(m); t2m2=numeric(m); xbarra=apply(y,2,mean)
    Sinv1=solve(cov(y)); V=matrix(NA,m-1,p)
    for (i in 1:m-1) V[i,]=y[i+1,]-y[i,]
    S=(t(V)%*%V)/(2*(m-1));	Sinv2=solve(S)
    for (i in 1:m) {
      z=y[i,]-xbarra; t2m1[i]=z%*%Sinv1%*%z
      t2m2[i]=z%*%Sinv2%*%z
    }
    time=cbind(seq(1,m),seq(1,m))
    if(etapa==1) {
      LSC=((m-1)^2)*qbeta(0.997,p/2,(m-p-1)/2)/m
    }
    if(etapa==2) {
      LSC=p*(m+1)*(m-1)*qf(0.997,p,m-p)/(m*(m-p))
    } 
    W1=cbind(t2m1,rep(LSC,m)); W2=cbind(t2m2,rep(LSC,m))
    par(mfrow=c(1,2))
    matplot(time, W1, type="o", pch=20, col=c("black","red"),
            main=expression(paste("Gráfico " * T^2 * " - Estimador 1")), xlab="Amostras", ylab=expression(paste(T^2)),
            ylim=c(min(W1), max(W1)), lty=1, lwd=2)
    matplot(time, W2, type="o", pch=20, col=c("black","red"),
            main=expression(paste("Gráfico " * T^2 * " - Estimador 2")), xlab="Amostras", ylab=expression(paste(T^2)),
            ylim=c(min(W2), max(W2)), lty=1, lwd=2)
    if (etapa==2) {
      lista=list(xbarra,S,LSC)
      names(lista)=c("Vetor de Médias",
                     "Matriz de Covariâncias", 
                     "Limite Superior de Controle")
      lista
    }
  }
}
