#############################################################################
# Exemplo - aula 1
# Construção do gráfico da função de verossimilhança Bernoulli(theta)
##############################################################################

#Cenários 1 e 2: duas amostras de tamanho n=10:
n<-10
t1<-1 # número de "sucessos" observados na amostra 1
t2<-5 # número de "sucessos" bservados na amostra 2

# Para fazer o gráfico da função de verossimilhança,
# precisamos de uma grade de valores para theta
# a grade abaixo foi arbitrária, respeitando o fato de que 0<=theta<=1

theta<-seq(0,1,0.01)

# Cálculo do valor da função de verossimilhança, para cada theta da grade, à luz dos dados da amostra 1 (n=10, t=1)
vero1<-theta^t1*(1-theta)^(n-t1)

# Cálculo do valor da função de verossimilhança, para cada theta da grade, à luz dos dados da amostra 2 (n=10, t=5
vero2<-theta^t2*(1-theta)^(n-t2)

#Mais dois cenários, aom base em amostras de tamanho m=100:
m<-100
t3<-10 # número de "sucessos" observados na amostra 3
t4<-50 # número de "sucessos" observados na amostra 4

# Cálculo do valor da função de verossimilhança, para cada theta da grade, à luz dos dados da amostra 3 (n=100, t=10)
vero3<-theta^t3*(1-theta)^(m-t3)

# Cálculo do valor da função de verossimilhança, para cada theta da grade, à luz dos dados da amostra 4 (n=100, t=50)
vero4<-theta^t4*(1-theta)^(m-t4)

##Preparação de uma janela gráfica particionada em duas linhas e duas colunas, para acomodar os 4 gráficos que seguem:
par(mfrow=c(2,2))

#Função de verossimilhança para theta com base na amostra 1:
plot(theta,vero1,type="l",ylab="n=10, t=1")
#Função de verossimilhança para theta com base na amostra 2:
plot(theta,vero2, type="l",ylab="n=10, t=5")
#Função de verossimilhança para theta com base na amostra 3:
plot(theta,vero3,type="l",ylab="n=100, t=10")
#Função de verossimilhança para theta com base na amostra 4:
plot(theta,vero4, type="l",ylab="n=100, t=50")

