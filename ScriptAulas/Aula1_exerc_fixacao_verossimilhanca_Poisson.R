#############################################################################
# Exercício de fixação - aula 1
# Construção do gráfico da função de verossimilhança Poisson(theta)
##############################################################################

# Para fazer o gráfico da função de verossimilhança,
# precisamos de uma grade de valores para theta
# a grade abaixo foi arbitrária, respeitando o fato de que theta>0

theta<-seq(0,10,0.01)

#Cenário 1:
n1<-10
t1<-20 # número de "cancelamentos" observados em 10 dias
# Cálculo do valor da função de verossimilhança, para cada theta da grade, à luz dos dados da amostra 1 (n=10, t=1)
vero1<-exp(-n1*theta)*theta^t1

#Cenário 2:
n2<-50
t2<-100 # número de "cancelamentos" observados em 10 dias
# Cálculo do valor da função de verossimilhança, para cada theta da grade, à luz dos dados da amostra 1 (n=10, t=1)
vero2<-exp(-n2*theta)*theta^t2

#Cenário 3:
n3<-10
t3<-60 # número de "cancelamentos" observados em 10 dias
# Cálculo do valor da função de verossimilhança, para cada theta da grade, à luz dos dados da amostra 1 (n=10, t=1)
vero3<-exp(-n3*theta)*theta^t3


##Preparação de uma janela gráfica particionada em duas linhas e duas colunas, para acomodar os 4 gráficos que seguem:
par(mfrow=c(1,3))

#Função de verossimilhança para theta com base na amostra 1:
plot(theta,vero1,type="l",ylab="n=10, t=20")
#Função de verossimilhança para theta com base na amostra 2:
plot(theta,vero2, type="l",ylab="n=50, t=100")
#Função de verossimilhança para theta com base na amostra 3:
plot(theta,vero3,type="l",ylab="n=10, t=60")

