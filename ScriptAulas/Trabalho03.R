#####################################################
##Preditiva Normal-Normal
#####################################################

#Lendo o conjunto de dados:
base.dados<-matrix(scan(file="custos.txt"),ncol=2,byrow=T) #Leitura do arquivo
head(base.dados) #Visualização das primeiras linhas do arquivo lido
dim(base.dados) #Conferindo a dimensão dos do arquivo

#Explorando o conjunto de dados (log-custos)
#e calculando estatísticas que serão usadas adiante:
dados<-base.dados[,2]
hist(dados)
t<-sum(dados)
xbarra<-mean(dados)
n<-length(dados)

#Assumiremos precisão observacional conhecida:
sigma2<-0.01 #variância observacional
phi<-1/sigma2 #precisão observacional

#Priori:
m.0<-0
d.0<-10
phi.0<-1/d.0^2


# Etapa 1:

#Posteriori é uma distribuição Normal, com precisão e média dadas por:
phi.n<-phi.0+n*phi
mu.n<-(1/phi.n)*(phi.0*m.0+n*phi*xbarra)
#desvio padrão a posteriori:
d.n<-sqrt(1/phi.n)
# Observe quão baixo é o desvio-padrão a posteriori
# Note que a priori, que era bastante dispersa, praticamente não contribui
# para o comportamento da posteriori, que
# ficou centrada na média amostral observada
#(1.5 ponto)

# Gráfico da priori e da posteriori:
theta<-seq(-30,30,0.01)
priori<-dnorm(theta,m.0,d.0)
posteriori<-dnorm(theta,mu.n, sqrt(1/phi.n))
plot(theta, posteriori,color=2)
lines(theta,priori)
# a posteriori é tão concentrada que sua visualização, junto ao gráfico da priori, fica difícil.
#Plotando as duas curvas em gráficos separados, com grades distintas
#sobre o domínio de theta, para melhor visualização:
par(mfrow=c(1,2))
plot(theta, priori, ylab="Priori")
theta.post<-seq(mu.n-3*d.n,mu.n+3*d.n,0.0001)
posteriori<-dnorm(theta.post,mu.n, sqrt(1/phi.n))
plot(theta.post,posteriori,ylab="Posteriori")

# Como se pode ver, ganhamos muita informação
# a partir dos dados, modificando bastante a
#crença a priori: passamos de uma priori
#centrada em zero e bastante dispersa a uma
# posteriori dominada pela verossimilhança,
# centrada em 1.996, que é o valor da
# média amostral, e extremamente concentada
#(1.25 ponto)

#Estimativa intervalar para theta, ao nível
# de credibilidade :
lim.inf<-qnorm(0.025,mu.n,1/sqrt(phi.n))
lim.sup<-qnorm(0.975,mu.n,1/sqrt(phi.n))

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
