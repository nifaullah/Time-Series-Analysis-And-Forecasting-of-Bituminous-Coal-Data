
ts1 = c(47730,46704,41535,41319,36962,32558,31995,32993,44834,29883,39611,
        40099,38051,36927,37272,39457,38097,40226,43589,39088,39409,37226,
        34421,34975,32710,31885,32106,30029,29501,31620,34205,32153,32764,
        33230,35636,35550,34529,37498,37229,36021,38281,36676,44541,40850,
        38404,37575,41476,42267,43062,45036,43769,42298,44412,40498,37830,
        42294,38330,43554,42579,36911,42541,42430,43465,44468,43597,40774,
        42573,41635,39030,41572,37027,34732,36817,34295,33218,32034,31417,
        35719,30001,33096,35196,36550,33463,37195,34748,36461,35754,36943,
        35854,37912,30095,28931,31020,31746,34613,37901)

myts <- ts(ts1, start=c(1952, 1), end=c(1959, 12), frequency=12)


plot(myts)
acf(ts1,ylim=c(-1,1), lag.max = 20)
pacf(ts1,ylim=c(-1,1), lag.max = 20)

library(lmtest)
coeftest(fit200)

fit200 = arima(myts,order =  c(2,0,0))
fit200


fit202 = arima(myts,order =  c(2,0,2))
fit202


fit101 = arima(myts,order =  c(1,0,1))
fit101

fit200 = arima(myts,order =  c(2,0,0))
fit202 = arima(myts,order =  c(2,0,2))
fit101 = arima(myts,order =  c(1,0,1))
AIC(fit200)
AIC(fit202)
AIC(fit101)
BIC(fit200)


BIC(fit202)
BIC(fit101)



plot(fit200$residuals)
acf(fit200$residuals,ylim=c(-1,1))
pacf(fit200$residuals,ylim=c(-1,1))
Box.test(resid(fit200), lag = 10, type = "Ljung-Box", fitdf = 2)

acf=acf(resid(fit200))

length(myts)*(length(myts)+2)*sum(acf$acf[2:11]^2/(length(myts)-1:10))


n = 100
MC=10000
Q_vec=rep(NA,MC)
for (it in 1:MC)
{
  z = rnorm(n)
  test = Box.test(z, lag = 10, type = "Ljung-Box", fitdf = 0)
  Q_vec[it]=test$statistic
}
hist(Q_vec,xlab="Q",breaks=30,main="Q's distribution")
abline(v=qchisq(0.95,df=10-0))

forecast(fit200,h=24)
plot(forecast(fit200,h=24))