xS2=function(m,n,y,op){
  if (length(n)==1) n=rep(n,m)
  mat=matrix(NA,m,max(n)); k=1 
  for (i in 1:m) {
    for (j in 1:n[i]) {
      mat[i,j]=y[k]
      k=k+1
    }
  }
  media=apply(mat,1,mean,na.rm=T)
  desvX<-sd(media);medX<-mean(media)
  respX<-(media-medX)/desvX
  
  desviop=apply(mat,1,sd,na.rm=T)
  desvS<-sd(desviop);medS<-mean(desviop)
  respS<-(desviop-medS)/desvS
  
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
  xbar=cbind(rep(x2barras,m),LIC1,LSC1,media)
  rbar=cbind(rep(Sbarra,m),LIC2,LSC2,desviop)
  # ggplot()
  if(op=="s"){
    return(cbind(rbar,respS)) 
  }else{
    if(op=="x"){
      return(cbind(xbar,respX)) 
    }else{
      stop("Opção nao selecionada")
    } 
  }

  
  # matplot(amostra,rbar,type="o",ylab="Desvio-Padrão",
  #         col=c("black","red","red","blue"),
  #         ylim=c(min(rbar),max(rbar)),
  #         xlab="Amostras",main="Gráfico S",pch=20,lty=1,lwd=2)
  # matplot(amostra,xbar,type="o",ylab="Média",xlab="Amostras",
  #         main=expression(paste("Gráfico " * bar(x))), 
  #         col=c("black","red","red","blue"),
  #         ylim=c(min(xbar),max(xbar)),pch=20, lty=1, lwd=2)
}