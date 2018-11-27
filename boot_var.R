p=ncol(y); obs=nrow(y);m=obs/n; f=p*n*(log(n)-1); S=cov(y)
Sinv=solve(S); det1=det(S); amostra=matrix(0,n,p); W1<-W2<-numeric(m) 
produto1=1; produto2=1; x=seq(1,m)
for ( i in 1:p) produto1=(n-i)*produto1
for ( i in 1:p) produto2=(n-i+2)*produto2
b1=produto1/((n-1)^p)
b2=produto1*(produto2-produto1)/((n-1)^(2*p)) 

W3<-NULL
reamos=5000
for (i in 1:reamos) {
  repeat{
    amostra=y[sample(seq_len(nrow(dados)),n,replace = T),]
    Si=cov(amostra)
    A=(n-1)*Si
    W1[i]=f+sum(diag(Sinv%*%A))-n*log(det(A)/det1)
    if(!is.na(log(det(A)/det1))) break()
  }
  W2[i]<-det(Si)
  if(is.null(W3)) W3<-Si else W3<-W3+Si
}
LS1=quantile(W1,.95)

LS2=quantile(W2,c(.025,.975))

LS3=c(det(W3/reamos)*(b1-3*sqrt(b2))/b1,det(W3/reamos)*(b1+3*sqrt(b2))/b1)



p=ncol(y); obs=nrow(y);m=obs/n; f=p*n*(log(n)-1); S=cov(y)
Sinv=solve(S); det1=det(S); amostra=matrix(0,n,p); W1=numeric(m) 
W1=W2
for (i in 1:m) {
  amostra=y[(n*i-n+1):(n*i),]; Si=cov(amostra); A=(n-1)*Si
  W1[i]=f+sum(diag(Sinv%*%A))-n*log(det(A)/det1)
  W2[i]=det(Si)
}

LSC1=qchisq(0.997,p*(p+1)/2)
produto1=1; produto2=1; x=seq(1,m)
for ( i in 1:p) produto1=(n-i)*produto1
for ( i in 1:p) produto2=(n-i+2)*produto2
b1=produto1/((n-1)^p)
b2=produto1*(produto2-produto1)/((n-1)^(2*p)) 
LIC2=det1*(b1-3*sqrt(b2))/b1; LSC2=det1*(b1+3*sqrt(b2))/b1

LSC1=qchisq(0.997,p*(p+1)/2);LSC1;LS1;
LIC2;LSC2;LS2
LS3


library(ggplot2)
ggplot(data=data.frame(W1),aes(x="",y=W1))+
  geom_point(aes(x=seq_len(length(W1)),y=W1))+
  geom_hline(aes(yintercept=LSC1),color="red")+
  geom_hline(aes(yintercept=LS),color="blue")+
  theme_bw()


