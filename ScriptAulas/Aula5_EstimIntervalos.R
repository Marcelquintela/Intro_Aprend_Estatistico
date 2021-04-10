qbeta(0.005,16.2,93)
qbeta(0.995,16.2,93)

theta<-seq(0,1,0.01)
post<-dbeta(theta,16.2,93)
plot(theta, post, type="l")
curve(dbeta(x,16.2,93))# identico ao plot

#aleatória de parametros
theta_rn<-rbeta(1000,16.2,93)
hist(theta_rn, prob=T)
lines(sort(theta_rn), dbeta(sort(theta_rn),16.2,93), col=2, lty=2)

#**************************************************

#*