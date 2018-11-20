library(shiny)
library(plotly)
library(shinythemes)
library(dplyr)
library('MSQC')
library('readxl')
server <- function(input, output,session){
  data("carbon1")
  tabela<-reactive({
    if(is.null(input$dados)){
      tabela<-NULL
      for (i in 1:30){
        m<-cbind(t(as.data.frame(carbon1)[i,seq(1,24,by=3)]),
                 t(as.data.frame(carbon1)[i,seq(2,25,by=3)]),
                 t(as.data.frame(carbon1)[i,seq(3,26,by=3)]))
        res<-rbind(tab,m)
      }
      colnames(res)<-c('inner' ,'thickness' ,"length")
      return(res)
    }else{
      return(as.data.frame(read_excel(input$dados)))
    }
  })

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
    print(sprintf("%f,%f",LIC2,LSC2))
    if (LIC2<=0) LIC2=0
    z2=cbind(W2,rep(det1,m),rep(LIC2,m),rep(LSC2,m))
    print(time1[,1])
    print(z2)
    
    tab<-data.frame(n.amostra=time1[,1],estatistica=z1[,1],L.super=z1[,2])
    p1<-ggplot(data=tab,aes(y=estatistica,x=n.amostra))+
      geom_line()+
      geom_point(aes(y=estatistica,x=n.amostra),col="blue")+
      geom_line(data=tab,aes(y=L.super,x=n.amostra),col="red")+
      geom_text(aes(y=tab[1,'L.super']+1,x=tab[5,'n.amostra']),label=paste("LIC= ",round(tab[1,'L.super'],2)))+
      xlim(c(1,max(tab$n.amostra)))+
      labs(title="Método 1",x="Amostras",y="W")+
      theme_bw()+
      theme(plot.title=element_text(hjust=.5))
    
    
    ggplotly(p1, source = "source")%>%
      style(hoverinfo = "y")%>%
      layout(xaxis=list(
        range=c(0.8,max(tab$n.amostra)),
        tickvals= seq(0,30),
        ticktext = paste(seq(0,30))
      ))
  }
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
    print(sprintf("%f,%f",LIC2,LSC2))
    if (LIC2<=0) LIC2=0
    z2=cbind(W2,rep(det1,m),rep(LIC2,m),rep(LSC2,m))
    print(time1[,1])
    print(z2)
    
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
        tickvals= seq(0,30),
        ticktext = paste(seq(0,30))
      ))
  }
  xS2=function(m,n,y){
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
    xbar=cbind(rep(x2barras,m),LIC1,LSC1,media)
    rbar=cbind(rep(Sbarra,m),LIC2,LSC2,desviop)
    # ggplot()
    return(cbind(rbar,resp))
    # matplot(amostra,rbar,type="o",ylab="Desvio-Padrão",
    #         col=c("black","red","red","blue"),
    #         ylim=c(min(rbar),max(rbar)),
    #         xlab="Amostras",main="Gráfico S",pch=20,lty=1,lwd=2)
    # matplot(amostra,xbar,type="o",ylab="Média",xlab="Amostras",
    #         main=expression(paste("Gráfico " * bar(x))), 
    #         col=c("black","red","red","blue"),
    #         ylim=c(min(xbar),max(xbar)),pch=20, lty=1, lwd=2)
  }
  todosPontos=function(m,n,tab)lapply(seq(1:ncol(tab)),function(x){
    xS2(m,n,tab[,x])
  })
  
  metodo<-eventReactive(input$metodo,{
    if(input$metodo==1){
      cvseM1(8,tabela())
    }else{
      if(input$metodo==2){
        cvseM2(8,tabela())
      }
    }
  })
  
  output$metodo1<-renderPlotly(
    metodo()
  )
  output$metodo2<-renderPlotly({
    eventdata <- event_data("plotly_click", source = "source")
    datapoint <- as.numeric(eventdata$pointNumber)[1]
    p<-as.data.frame(do.call("rbind",lapply(todosPontos(30,8,tabela()),function(x){
      x[datapoint+1,]
    })))
    p$atributo<-colnames(tabela())
    ggplotly(
      ggplot(p,aes(y=resp,x="",colour=factor(atributo),fill=factor(atributo)))+
      geom_point(size=5,shape=23,aes(text=sprintf("Atributo<br>%s",atributo)))+
      labs(title="Controle Univariado",x="",y="S\n Padronizada")+
      geom_hline(yintercept=0,col="blue")+
      geom_hline(yintercept = 3 ,col="red")+
      geom_hline(yintercept = -3,col="red")+
      scale_y_continuous(breaks = seq(-3,3))+
      theme_bw()+
      theme(legend.position = "none"), tooltip="text")
  })
}  

