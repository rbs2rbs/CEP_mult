cvseM2=function(n,y){
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
  
  nomes<-data.frame(y=as.vector(z2[1,c(2,3,4)])-(max(z2[1,c(2,3,4)]
                                                     -min(z2[1,c(2,3,4)]))*.05),
                    x=rep(time2[5,2],3),
                    nom=c("",paste0("Limite Inferior = ",if (z2[1,3]==0) z2[1,3] else if (z2[1,3]<0.01) formatC(z2[1,3], format = "e", digits = 2) else round(z2[1,3],2))
                          ,paste0("Limite Superior = ",if (z2[1,4]<0.01) formatC(z2[1,4], format = "e", digits = 2) else round(z2[1,4],2) )))
  
  tab<-data.frame(n.amostra=time2[,1],estatistica=z2[,1],lc = z2[,2], L.infer = z2[,3], L.super=z2[,4])
  
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
  ggplotly(p2, source = "source")%>%
    style(hoverinfo = "y")%>%
    layout(xaxis=list(
      range=c(0.8,max(tab$n.amostra)),
      tickvals= seq(0,max(tab$n.amostra)),
      ticktext = paste(seq(0,max(tab$n.amostra)))
    ))
}