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
theta<-c(a,b)
neglogvero<-function(theta){-sum(log(dgamma(y,theta[1],theta[2])))}

#resultado:
saida<-nlm(neglogvero,p=c(0.5,0.5))
saida$estimate #estimativas de maxima verossimilhanca

#obtenção das estimativas pelo método dos momentos:

ybar<-mean(y)
s2<- (n-1)*var(y)/n
b.hat<-ybar/s2
a.hat<-b.hat*ybar
b.hat
a.hat