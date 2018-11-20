todosPontos=function(m,n,tab,op)lapply(seq(1:ncol(tab)),function(x){
  xS2(m,n,tab[,x],op)
})