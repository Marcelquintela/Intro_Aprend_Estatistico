#Entrada de dados
y <- c(2.07,0.38,11.77,3.66,4.02,8.20,1.88,2.92,2.83,1.76,
0.47,0.36,2.76,0.75,3.64,4.07,2.28,7.14,2.63,11.45)
#Visualização da distribui??o da amostra observada
hist(y, main="Histograma de y",right=FALSE, prob=TRUE)
#tamanho amostral
n <- length(y)
a <- NULL
b <- NULL
# -logverossimilhança
#theta<-c(a,b)
t1<-sum(y)
t2<-prod(y)
# 06/02/21 09:10 [MARCEL] para construção do gráfico preciso de decompor a função de VERO
# para estimar na grade de dados a, b
logvero<-function(a,b){n*(a*log(b)-log(gamma(a)))-t1*b+(a-1)*log(t2)} 

#Gráfico das curvas de nível
grade.a<-seq(0.1,5,0.01)
grade.b<-seq(0.01,1,0.01)
vero<-matrix(rep(0,length(grade.a)*length(grade.b)),nrow=length(grade.a))
for(i in 1:length(grade.a)){
  for(j in 1:length(grade.b)){
    a<-grade.a[i]
    b<-grade.b[j]
  vero[i,j]<-exp(logvero(a,b))
  }
  }
dim(vero)
contour(grade.a,grade.b,vero)


