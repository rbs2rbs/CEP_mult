xS=function(m,n,y,nome,op){
  if (length(n)==1) n=rep(n,m)
  mat=matrix(NA,m,max(n)); k=1 
  for (i in 1:m) {
    for (j in 1:n[i]) {
      mat[i,j]=y[k]
      k=k+1
    }
  }
  media=apply(mat,1,mean,na.rm=T)
  desviop=apply(mat,1,sd,na.rm=T)
  desv<-sd(desviop);med<-mean(desviop)
  resp<-(desviop-med)/desv
  x2barras=sum(n*media)/sum(n)
  Sbarra=sqrt(sum((n-1)*desviop^2)/(sum(n)-m))
  LIC1=numeric(m)
  LSC1=numeric(m)
  LIC2=numeric(m)
  LSC2=numeric(m)
  c4=0	
  for (i in 1:m){ 
    c4=(gamma(n[i]/2)/gamma((n[i]-1)/2))*sqrt(2/(n[i]-1))
    LIC1[i]=x2barras-3*Sbarra/(c4*sqrt(n[i]))
    LSC1[i]=x2barras+3*Sbarra/(c4*sqrt(n[i]))
    LIC2[i]=Sbarra-3*Sbarra*sqrt((1-c4^2))/c4
    LSC2[i]=Sbarra+3*Sbarra*sqrt((1-c4^2))/c4
    if (LIC2[i] < 0) LIC2[i]=0
  }
  amostra=matrix(rep(seq(1,m),4),m,4,byrow=F)
  xbar=cbind(rep(x2barras,m),LIC1,LSC1,media,seq_len(m))
  rbar=cbind(rep(Sbarra,m),LIC2,LSC2,desviop,seq_len(m))
  if(op=="s"){
    rbar=as.data.frame(rbar)
    colnames(rbar)<-c('lc','L.infer','L.super','estatistica','n.amostra')
    
  ps<-ggplot(data=rbar,aes(y=estatistica,x=n.amostra))+
      geom_line()+
      geom_point(aes(y=estatistica,x=n.amostra),col="blue")+
      geom_line(aes(y=lc,x=n.amostra),col="blue")+
      geom_line(aes(y=L.super,x=n.amostra),col="red")+
      geom_line(aes(y=L.infer,x=n.amostra),col="red")+
      labs(title=sprintf("Grafico S da variável %s",op),x="Amostras",y="Estatistica")+
      theme_bw()+
      theme(plot.title=element_text(hjust=.5))
    return(ps)
    }else{
        if(op=="x"){
          xbar=as.data.frame(xbar)
          colnames(xbar)<-c('lc','L.infer','L.super','estatistica','n.amostra')
          
        px<-ggplot(data=xbar,aes(y=estatistica,x=n.amostra))+
            geom_line()+
            geom_point(aes(y=estatistica,x=n.amostra),col="blue")+
            geom_line(aes(y=lc,x=n.amostra),col="blue")+
            geom_line(aes(y=L.super,x=n.amostra),col="red")+
            geom_line(aes(y=L.infer,x=n.amostra),col="red")+
            labs(title=sprintf("Grafico X Barra da variável %s",op),x="Amostras",y="Estatistica")+
            theme_bw()+
            theme(plot.title=element_text(hjust=.5))
          return(px)
        }else stop("Opção de gráfico faltante")
      }
}