library(shiny)
library(ggplot2)
library(plotly)
library(shinythemes)
library(dplyr)
library('MSQC')
library('readxl')
library('shinyBS')
library(shinyjs)
server <- function(input, output,session){
  data("carbon1")
  tabela<-reactive({
    if(is.null(input$dados)){
      res<-NULL
      for (i in 1:30){
        m<-cbind(t(as.data.frame(carbon1)[i,seq(1,24,by=3)]),
                 t(as.data.frame(carbon1)[i,seq(2,25,by=3)]),
                 t(as.data.frame(carbon1)[i,seq(3,26,by=3)]))
        res<-rbind(res,m)
      }
      colnames(res)<-c('inner' ,'thickness' ,"length")
      return(res)
    }else{
      return(apply(as.data.frame(read_excel(input$dados$datapath)),2,as.numeric))
    }
  })
  
  for(i in paste0('func/',list.files('func/'))){
    source(file.path(i),local=T)$value
  }

  metodo<-reactive({
    if(input$metodo==1){
      cvseM1(input$n,tabela())
    }else{
      if(input$metodo==2){
        cvseM2(input$n,tabela())
      }
    }
  })
#################  
  variavel<-reactive({
    if(!is.null(input$variavel)){ 
      dados<-tabela()
      fator<-factor(colnames(dados))
      grafDrop<-dados[,fator[(as.numeric(input$variavel[1])+1)]]
      nome<-paste(fator[1])
      xS((length(grafDrop)/input$n),input$n,grafDrop,nome,op=paste(input$variavel[2]))
    }
  })
  
  observe({
    datapoint <- eventdata <- event_data("plotly_click", source = "source2")
    session$sendCustomMessage("dropS", datapoint)
  })
  observe({
    datapoint <- eventdata <- event_data("plotly_click", source = "source4")
    session$sendCustomMessage("dropX", datapoint)
  })
  
  output$tex<-renderText(input$variavel[1])
  
  output$s<-renderPlot(
    variavel()
  )
###################  
  output$multiS<-renderPlotly(
    metodo()
  )
  output$univarS<-renderPlotly({
    eventdata <- event_data("plotly_click", source = "source")
    datapoint <- as.numeric(eventdata$pointNumber)[1]
    p<-as.data.frame(do.call("rbind",lapply(todosPontos((nrow(tabela())/input$n),input$n,tabela(),op="s"),function(x){
      x[datapoint+1,]
    })))
    p$atributo<-colnames(tabela())      
    p1<-ggplot(p,aes(y=respS,x="",colour=factor(atributo),fill=factor(atributo)))+
        geom_point(data=p,size=5,shape=23,aes(text=sprintf("Atributo: <br>%s",atributo)))+
        labs(title="Controle S barra Univariado",x="",y="S (Padronizada)")+
        geom_hline(yintercept=0,col="blue")+
        geom_hline(yintercept = 3 ,col="red")+
        geom_hline(yintercept = -3,col="red")+
        scale_y_continuous(breaks = seq(-3,3))+
        theme_bw()+
        theme(legend.position = "none")
    ggplotly(p1,source = "source2", tooltip="text")
  })
  
  output$multiX<-renderPlotly(
    T2se(input$n,tabela(),etapa=1)
  )
  output$univarX<-renderPlotly({
    eventdata <- event_data("plotly_click", source = "source3")
    datapoint <- as.numeric(eventdata$pointNumber)[1]
    p<-as.data.frame(do.call("rbind",lapply(todosPontos((nrow(tabela())/input$n),input$n,tabela(),op="x"),function(x){
      x[datapoint+1,]
    })))
    p$atributo<-colnames(tabela())      
    p2<-ggplot(p,aes(y=respX,x="",colour=factor(atributo),fill=factor(atributo)))+
      geom_point(data=p,size=5,shape=23,aes(text=sprintf("Atributo: <br>%s",atributo)))+
      labs(title="Controle X barra Univariado",x="",y="x barra\n (Padronizada)")+
      geom_hline(yintercept=0,col="blue")+
      geom_hline(yintercept = 3 ,col="red")+
      geom_hline(yintercept = -3,col="red")+
      scale_y_continuous(breaks = seq(-3,3))+
      theme_bw()+
      theme(legend.position = "none")
    ggplotly(p2,source = "source4", tooltip="text")
  })
}  



# setwd('/home/renan/Downloads/')
# summary(apply(as.data.frame(read_excel('mangue.xlsx')),2,as.numeric))
# dados<-as.data.frame(read_excel('mangue.xlsx'))