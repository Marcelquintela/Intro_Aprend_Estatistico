#####################################################
##Vimos no slide 88: Posteriori theta~Gama(72,20.4)
#####################################################
#Primeiramente, vamos explorar, por amostragem, o comportamento da posteriori:
#Gerando uma amostra de 1000 valores theta, seguuindo o comportamento da distribuição a posteriori
theta.post<-rgamma(1000,72,rate=20.4)
hist(theta.post,prob=T)
#Veja que a amostra representa bem o comportamento da posteriori analítica,
#que é uma densidade Gama:
lines(sort(theta.post),dgamma(sort(theta.post),72, rate=20.4))

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
  Y.pred[i]<-rpois(1,theta.post[i])
}
#Formato da distribuição preditiva
#aquela que diz respeito ao comportamento de observações futuras de Y):
hist(Y.pred)

# Agora podemos usar a amostra da preditiva para fazer as
#inferências desejadas sobre realizações futuras de Y.
#Exemplos:
# Estimativa de             
#Basta tomar a proporção de valores preditos superiores a 8:
p<-length(Y.pred[Y.pred>8])/length(Y.pred)
p

#Estimativa do valor esperado de valores futuros: E[Y.futuro]
#Basta tomar a média da amostra da preditiva:
Y.esp<-mean(Y.pred)
Y.esp

#Estimativa intervalar de Y.futuro (ao nível de credibilidade 95%)
int.Y<-quantile(Y.pred,probs=c(0.025,0.975))
int.Y
#Observe como a estimativa intervalar de valores futuros tem maior incerteza associada que a estimativa intervalar de parâmetro.
