
set.seed(1)

dd<-rnorm(100, 10, 1)
dd2<-rnorm(100, 10, 1)
# compare:
mean(dd/dd2)
sum(dd)/sum(dd2)

dd<-rgamma(100, 10, 1)
dd2<-rgamma(100, 10, 1)
# compare:
mean(dd/dd2)
sum(dd)/sum(dd2)

