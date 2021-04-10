#############################
# Geração dos dados simulados
#############################

xnorm<-rnorm(1000,2,0.1)
y<-exp(xnorm)
hist(y)
matriz<-cbind(y,xnorm)
write(t(matriz),"custos.txt",ncol=2)

#########################
#Lendo a base de dados
#########################

dados<-matrix(scan(file="custos.txt"),ncol=2,byrow=T)
head(dados)
dim(dados)

custo<-dados[,1]
log.custo<-dados[,2]

#############################
#Histograma dos custos
###########################
hist(custo)

#grade para mu
#mu<-seq(0,5,0.1)

###########################################
#Estimativa de maxima verossimilhança
###########################################

n <- length(log.custo)
a <- NULL
b <- NULL
# -logverossimilhança
theta<-c(a,b)
#neglogvero<-function(theta){-sum(log(dnorm(log.custo,theta[1],theta[2])))}


#resultado:
saida<-nlm(neglogvero,p=c(1,1))
saida$estimate #estimativas de maxima verossimilhanca

#######
1-pnorm(log(9),saida$estimate[1],saida$estimate[2])


###
Verossimilhança com respeito a mu:
mu<-seq(0,5,0.01)
t1<-sum(log.custo)
t2<-sum(log.custo^2)
sigma2<-0.1016071^2
vero.mu<-(-n/2)*log(sigma2)-((1/(2*sigma2))*(t2-2*mu*t1+n*mu^2))
plot(mu,vero.mu)



