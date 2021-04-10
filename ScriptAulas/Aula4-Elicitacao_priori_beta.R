### Exemplo:

# 1 - Considere Yi~Bernoulli(theta), i=1,...,n. Assuma que se deseje levar em conta
# opiniao subjetiva sobre theta, adotando-se para isso uma distribuicao a priori
# conjugada Beta(a,b).
# Os parametros da priori são especificados a partir das crencas de um especialista, que
# são: E[theta] = 0,25 e P(theta < 0.4) e aproximadamente 0,84.
# (Dica: a especificação do valor esperado dará uma relcao de proporcionalidade entre a e b.
# Por outro lado, o comando R pbeta(0.4, a, b) fornece a probabilidade acumulada ate o
# quantil 0,4.)


# Lembrete: media da beta = a/(a+b)

# 0.25 = a/(a+b) --> 3a = b ou a=b/3

#tentar fazer função para estimar o melhor valor com base na probabilidade
a <- 2
b <- 3*a
pbeta(0.4,a,b)

x <- seq(0,1,length=100)
par(mfrow=c(1,1),mar=c(4,4,2,1))
plot(x,dbeta(x,a,b),type='l', xlim=c(0,1),xlab=expression(theta),main="")
abline(h=0)
abline(v=0.4,col=2,lty=3)
abline(v=0.25,col=3,lty=2)
legend("topright",legend=c("média",expression(Q[84])),lty=c(2,3),col=c(3,2))







