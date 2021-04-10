#Obtenção de EMV para taxa da Poisson

#Entrada de dados
y <- c(rep(0,60), rep(1,80), rep(2,120), rep(3,90),
rep(4, 50), rep(5,20), rep(6, 10))
#Visualização da distribuição da amostra observada
#opçes: right=FALSE -células fechadas à esquerda
#prob=TRUE: área total das barras é igual a 1
hist(y, main="Histograma de y",right=FALSE, prob=TRUE)
#tamanho amostral
n <- length(y)
theta<-NULL
# -logverossimilhança - expressão da verossimilhança (negativa)
neglogvero<-function(theta)
{ -sum(log(dpois(y,theta)))} #!!!!aqui mudar chaves
#resultado:
# A função nlm retorna o argumento mínimo. Por isso
#fornecemos a log verossimilhança negativa.
# p=c(0.5) ? um chute inicial para a estimativa pontual.
saida<-nlm(neglogvero,p=c(0.5))
saida$estimate

