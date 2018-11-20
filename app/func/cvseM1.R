cvseM1=function(n,y){
  p=ncol(y); obs=nrow(y);m=obs/n; f=p*n*(log(n)-1); S=cov(y)
  Sinv=solve(S); det1=det(S); amostra=matrix(0,n,p); W1=numeric(m) 
  W2=W1
  for (i in 1:m) {
    amostra=y[(n*i-n+1):(n*i),]; Si=cov(amostra); A=(n-1)*Si
    W1[i]=f+sum(diag(Sinv%*%A))-n*log(det(A)/det1)
    W2[i]=det(Si)
  }
  time1=cbind(seq(1,m),seq(1,m)); time2=cbind(time1,time1)	
  LSC1=qchisq(0.997,p*(p+1)/2); z1=cbind(W1,rep(LSC1,m))
  produto1=1; produto2=1; x=seq(1,m)
  for ( i in 1:p) produto1=(n-i)*produto1
  for ( i in 1:p) produto2=(n-i+2)*produto2
  b1=produto1/((n-1)^p)
  b2=produto1*(produto2-produto1)/((n-1)^(2*p)) 
  LIC2=det1*(b1-3*sqrt(b2))/b1; LSC2=det1*(b1+3*sqrt(b2))/b1
  if (LIC2<=0) LIC2=0
  z2=cbind(W2,rep(det1,m),rep(LIC2,m),rep(LSC2,m))
  
  
  tab<-data.frame(n.amostra=time1[,1],estatistica=z1[,1],L.super=z1[,2])
  p1<-ggplot(data=tab,aes(y=estatistica,x=n.amostra))+
    geom_line()+
    geom_point(aes(y=estatistica,x=n.amostra),col="blue")+
    geom_line(data=tab,aes(y=L.super,x=n.amostra),col="red")+
    geom_text(aes(y=tab[1,'L.super']+1,x=tab[5,'n.amostra']),label=paste("Limite Supeior= ",round(tab[1,'L.super'],2)))+
    xlim(c(1,max(tab$n.amostra)))+
    labs(title="Método 1",x="Amostras",y="W")+
    theme_bw()+
    theme(plot.title=element_text(hjust=.5))
  
  
  ggplotly(p1, source = "source")%>%
    style(hoverinfo = "y")%>%
    layout(xaxis=list(
      range=c(0.8,max(tab$n.amostra)),
      tickvals= seq(0,max(tab$n.amostra)),
      ticktext = paste(seq(0,max(tab$n.amostra)))
    ))
}