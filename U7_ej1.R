
curve(dnorm(x,mean = 17.5, sd=1),from=16,to=19)

# gráficos para variable de interés: media de largos en n = 100
# por TCL sé que media = mu y var = sigma / raíz de n
# en este caso sigma vale 1, sqrt(100)=10 entonces var y desvío = 0.1 

plot(function(x){dnorm(x,mean=17.5, sd=0.1)}, 17.1, 18.1, lwd=2,col='blue',
     ylab = ' ',xlab='largo medio en n=100 (cm)')
plot(function(x){dnorm(x,mean=17.7, sd=0.1)}, 17.1, 18.1, add = T,lwd=2,col='red')
legend('topright',c('subsp m','subsp d'),lty=c(1,1),col = c('blue','red'),bty='n')
abline(v=17.6)

# c) si alpha fijo en 0.01
plot(function(x){dnorm(x,mean=17.5, sd=0.1)}, 17.1, 18.1, lwd=2,col='blue',
     ylab = ' ',xlab='largo medio en n=100 (cm)')
plot(function(x){dnorm(x,mean=17.7, sd=0.1)}, 17.1, 18.1, add = T,lwd=2,col='red')
legend('topright',c('subsp m','subsp d'),lty=c(1,1),col = c('blue','red'),bty='n')
abline(v=17.733)


# d) si mu de Cmm fuera 16.5 y alpha fijo en 0.01
plot(function(x){dnorm(x,mean=16.5, sd=0.1)}, 16.1, 18.1, lwd=2,col='blue',
     ylab = ' ',xlab='largo medio en n=100 (cm)')
plot(function(x){dnorm(x,mean=17.7, sd=0.1)}, 16.1, 18.1, add = T,lwd=2,col='red')
legend('topright',c('subsp m','subsp d'),lty=c(1,1),col = c('blue','red'),bty='n')
abline(v=16.733)

# e) si VC original (17.733), alpha fijo 0.01 y mu de Cmd variara:
# 17.8 rojo, 18 rosa, 18.1 bordó
plot(function(x){dnorm(x,mean=17.5, sd=0.1)}, 17.1, 18.3, lwd=2,col='blue',
     ylab = ' ',xlab='largo medio en n=100 (cm)')
plot(function(x){dnorm(x,mean=17.8, sd=0.1)}, 17.1, 18.3, add = T,lwd=2,col='red')
plot(function(x){dnorm(x,mean=18, sd=0.1)}, 17.1, 18.3, add = T,lwd=2,col='pink2')
plot(function(x){dnorm(x,mean=18.1, sd=0.1)}, 17.1, 18.3, add = T,lwd=2,col='red4')
#legend('topright',c('subsp m','subsp d'),lty=c(1,1),col = c('blue','red'),bty='n')
abline(v=17.733, lty=2)

# f) gráfico media de Cmd y beta
graf <- data.frame(media = 1:4,beta = 1:4)
graf$media <- c(17.7, 17.8, 18, 18.1)
graf$beta <- c(62, 25.14,0.38,0.01)

plot(graf$media,graf$beta,pch=16,type='b')

# graf para potencia
graf$poder <- 1-(graf$beta/100)
plot(graf$media,graf$poder,pch=16,type='b')
