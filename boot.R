setwd('/home/renan/Downloads/')
library("MSQC")
data("carbon1")
    res<-NULL
    for (i in 1:30){
      m<-cbind(t(as.data.frame(carbon1)[i,seq(1,24,by=3)]),
               t(as.data.frame(carbon1)[i,seq(2,25,by=3)]),
               t(as.data.frame(carbon1)[i,seq(3,26,by=3)]))
      res<-rbind(res,m)
    }
    colnames(res)<-c('inner' ,'thickness' ,"length")

# dados<-as.data.frame(readxl::read_excel('mangue.xlsx'))[,c(1,2,3)]
# PASSO 1
# dados<-apply(dados,2,as.numeric)
# dados<-res[sample(1:nrow(res),100),]
# colnames(dados)<-c("atr1","atr2","atr3")

n=400    
    
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
    z=media[i,]-x2barras; t2[i]=n*(z%*%Sinv%*%z);
  }
}
qt<-p*(m-1)*(n-1)*qf(0.95,p,m*n-m-p+1)/(m*n-m-p+1)

# PASSO 2 E 3
# graf<-NULL
# for(variacao in seq(100,10000,by=100)){  
  # print(variacao)
  tabX<-NULL
  s<-NULL
  reamos<-1000
  for(i in 1:reamos){
    x<-dados[sample(seq_len(nrow(dados)),n,replace = T),]
    media<-apply(x,2,mean)
    if(is.null(s)) s<-cov(x) else s<-s+cov(x)
    tabX<-rbind(tabX,media)
  }
  
  xbb<-apply(tab,2,mean)
  
  s<-s/reamos
  
  ### PASSO 4
  tHot<-NULL
  for(i in 1:nrow(tabX)){
    tHot[i]<-n*t(tabX[i,]-xbb)%*%solve(s)%*%(tabX[i,]-xbb)
  }
  

  LS=quantile(tHot,.95);print(LS)
#   
#   if(is.null(graf)) graf<-data.frame(reamos,LS-qt) else graf<-rbind(graf,data.frame(reamos,LS-qt))
# }
# 


  