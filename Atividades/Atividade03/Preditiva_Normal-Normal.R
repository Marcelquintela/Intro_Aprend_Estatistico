#####################################################
##Preditiva Normal-Normal
#####################################################
#Gerando artificialmente um conjunto de dados:
dados<-rnorm(100, 5, 0.5)
hist(dados)
t<-sum(dados)
phi<-1/0.5^2
xbarra<-mean(dados)
n<-length(dados)

#Priori:
m.0<-0
d.0<-10
phi.0<-1/d.0^2

#Posteriori:
phi.n<-phi.0+n*phi
mu.n<-(1/phi.n)*(phi.0*m.0+n*phi*xbarra)



#Primeiramente, vamos explorar, por amostragem, o comportamento da posteriori:
#Gerando uma amostra de 1000 valores theta, seguuindo o comportamento da distribuição a posteriori
theta.post<-rnorm(1000,mu.n,1/sqrt(phi.n))
hist(theta.post,prob=T)
#Veja que a amostra representa bem o comportamento da posteriori analítica,
#que é uma densidade Gama:
lines(sort(theta.post),dnorm(sort(theta.post),mu.n, sqrt(1/phi.n)))

#Podemos fazer estimação intervalar do parâmetro theta:
#Tomemos uma estimativa intervalar com nível de credibilidade 95%
int.theta<-quantile(theta.post,probs=c(0.025,0.975))
int.theta

#########################################################
# Predição de valores futuros
########################################################
# A estimação de parâmetros, em geral, é uma etapa intermediári na análise.
# Tipicamente, interesse é em prever valor futuro de Y (qtdd observável)
# Podemos aproximar a distribuição preditiva de Y por amostragem, notando que:
# Para cada valor de theta, Y~Poisson(theta).
# Então geraremos os valores preditos de Y "passeando pelo espaço de theta",
# com a posteriori servindo como guia sobre regiões mais ou menos importantes no espaço de theta
Y.pred<-NULL
for (i in 1:length(theta.post)){
  Y.pred[i]<-rnorm(1,theta.post[i],sqrt(1/phi))
}
#Formato da distribuição preditiva
#aquela que diz respeito ao comportamento de observações futuras de Y):
hist(Y.pred,prob=T)
phi.pred<-phi*phi.n/(phi.n+phi)
lines(sort(Y.pred),dnorm(sort(Y.pred),mu.n, sqrt(1/phi.pred)))


# Agora podemos usar a amostra da preditiva para fazer as
#inferências desejadas sobre realizações futuras de Y.
#Exemplos:
# Estimativa de P(Y.futuro>8)
#Basta tomar a proporção de valores preditos superiores entre 4.5 e 5.5:
p<-length(Y.pred[Y.pred>=4.5&Y.pred<=5.5])/length(Y.pred)
p

#Estimativa do valor esperado de valores futuros: E[Y.futuro]
#Basta tomar a média da amostra da preditiva:
Y.esp<-mean(Y.pred)
Y.esp

#Estimativa intervalar de Y.futuro (ao nível de credibilidade 95%)
int.Y<-quantile(Y.pred,probs=c(0.025,0.975))
int.Y
#Observe como a estimativa intervalar de valores futuros tem maior incerteza associada que a estimativa intervalar de parâmetro.
