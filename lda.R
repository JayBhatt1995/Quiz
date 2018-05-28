#LR-applied when depen. var is binary or dichotomous
ds<-read.csv("logistic.csv")
View(ds)
attach(ds)
dim(ds)
sum(is.na(ds))
lm.fit1<-glm(default~age+ed+employ+address+income+dbtinc+creddebt+otherdebt,family=binomial)
summary(lm.fit1)
lm.fit2<-glm(default~employ+address+dbtinc+creddebt,family = binomial)
summary(lm.fit2)
lm.fit2
pr1<-predict(lm.fit2,type="response")
pr1
table(default,pr1>0.5)#..81.42%
all<-sample(1:nrow(ds),0.8*nrow(ds))
train=ds[all,]
test=ds[-all,]
dim(train)
dim(test)
train
test
lm.fit3<-glm(train$default~train$employ+train$address+train$dbtinc+train$creddebt,family=binomial)
summary(lm.fit3)
pr2<-predict(lm.fit3,type="response")
pr2
table(train$default,pr2>0.5)
lm.fit4<-glm(test$default~test$employ+test$address+test$dbtinc+test$creddebt,family=binomial)
summary(lm.fit4)
pr3<-predict(lm.fit4,type="response")
pr3
table(test$default,pr3>0.5)


#LDA...applied when depen. var. is non-dichotomous(except 0/1)(eg.1.2 or 1,2,3)
library(MASS)
ds2<-read.csv("Iris.csv")
ds2=ds2[,1:6]
View(ds2)
dim(ds2)
sum(is.na(ds2))
attach(ds2)
lda<-lda(Group~sepal.length..X1.+sepal.width..X2.+petal.length..X3.+petal.width..X4.,data=ds2)
lda
pr<-predict(lda,data=ds2)
pr
lda$means
lda$svd
lda$terms
x<-cbind(pr$class,Group)
sum(x[,1]-x[,2]!=0)

#Hier. Clust.
d<-read.csv("ClusteringR.csv")
d
rownames(d)<-d$Case
matrix<-as.matrix(d[2:3])
#single
dists<-dist(d[-1],method="euclidean",diag=T)
models<-hclust(dists,method="single")
summary(models)
models$height
plot(models)
#complete
distc<-dist(d[-1],method="euclidean",diag=T)
modelc<-hclust(distc,method="complete")
summary(modelc)
modelc$height
plot(modelc)
#avg.
dista<-dist(d[-1],method="euclidean",diag=T)
modela<-hclust(dista,method="average")
summary(modela)
modela$height
plot(modela)
#centroid
distc<-dist(d[-1],method="euclidean",diag=T)
modelc<-hclust(((distc)^2),method="centroid")
summary(modelc)
modelc$height
plot(modelc)
#ward
distw<-dist(d[-1],method="euclidean",diag=T)
modelw<-hclust(((distw)^2),method="ward.D2")
summary(modelw)
modelw$height
plot(modelw)
#median
distm<-dist(d[-1],method="euclidean",diag=T)
modelm<-hclust(((distm)^2),method="median")
summary(modelm)
modelm$height
plot(modelm)




d2<-read.csv("hclust.csv")
d2
rownames(d2)<-d2$model
matrix<-as.matrix(d2[1:9])
#single
dists<-dist(d2[-10],method="euclidean",diag=T)
dists
models<-hclust(dists,method="single")
summary(models)
models$height
plot(models)
#complete
distc<-dist(d2[-10],method="euclidean",diag=T)
modelc<-hclust(distc,method="complete")
summary(modelc)
modelc$height
plot(modelc)
#avg.
dista<-dist(d2[-10],method="euclidean",diag=T)
modela<-hclust(dista,method="average")
summary(modela)
modela$height
plot(modela)
#centroid
distce<-dist(d2[-10],method="euclidean",diag=T)
modelce<-hclust(((distce)^2),method="centroid")
summary(modelce)
modelce$height
plot(modelce)
#ward
distw<-dist(d2[-10],method="euclidean",diag=T)
modelw<-hclust(((distw)^2),method="ward.D2")
summary(modelw)
modelw$height
plot(modelw)
#median
distm<-dist(d2[-10],method="euclidean",diag=T)
modelm<-hclust(((distm)^2),method="median")
summary(modelm)
modelm$height
plot(modelm)




#kmeans
d3<-read.csv("KM.csv")
d3
points.matrix<-cbind(x1=d3$x,x2=d3$y)
points.matrix
kmeans<-kmeans(points.matrix,2)
kmeans

d4<-read.csv("kclust.csv")
d4
points.matrix<-cbind(x1=d4$X1,x2=d4$X2)
kmns<-kmeans(points.matrix,3)
kmns


#KNN
library(class)
library(caret)
d5<-read.csv("KN.csv")
d5
KNN<-knn3Train(d5[,1:2],c(7,5),k=5,cl=d5[,3])
KNN


dm<-read.csv("M2_test.csv")
dm
dim(dm)
lm.fit1<-glm(dm$yes~dm$cost+dm$catch+dm$income+dm$employed+dm$educatio+dm$married+dm$sex+dm$age+dm$nc,family=binomial)
summary(lm.fit1)
lm.fit2<-glm(dm$yes~dm$cost+dm$catch+dm$employed,family=binomial)
summary(lm.fit2)
pr1<-predict(lm.fit2,type="response")
pr1
table(dm$yes,pr1>0.5)
all<-sample(1:nrow(dm),0.8*nrow(dm))
train=dm[all,]
test=dm[-all,]
dim(train)
dim(test)
train
test
lm.fit3<-glm(train$yes~train$cost+train$catch+train$employed,family=binomial)
summary(lm.fit3)
pr2<-predict(lm.fit3,type="response")
pr2
table(train$yes,pr2>0.5)
lm.fit4<-glm(test$yes~test$cost+test$catch+test$employed,family=binomial)
summary(lm.fit4)
pr3<-predict(lm.fit4,type="response")
pr3
table(test$yes,pr3>0.5)

gender<-factor(rep(c("M","F"),c(5,5)))
like2<-data.frame(Program.duration=c(35,25,41,33,30,55,35,62,93,34),like=c(0,1,0,1,0,1,1,1,0,1),gender)
like2
LDA<-lda(like2$gender~like2$Program.duration+like2$like,data=like2)
LDA
pr<-predict(LDA,data=like2)
pr
LDA$means
LDA$svd
LDA$terms
x<-cbind(pr$class,like2$gender)
x
sum(x[,1]-x[,2]!=0)

library(fpp)
library(TTR)
library(tseries)
library(forecast)
library(devtools)
AR2MA2<-arima.sim(n=100000,list(ar=c(0.75,-0.25),ma=c(1,0.5)),innov=rnorm(100000))
class(AR2MA2)#..checking time series
plot.ts(AR2MA2)
diff(AR2MA2)#..convet. stationary
adf.test(AR2MA2)#..checking stationarity
acf(AR2MA2)#...checking ma models
pacf(AR2MA2)#..checking ar models
arima(AR2MA2,order=c(2,0,2))


library(AER)
data<-UKNonDurables
data
class(data)
sum(is.na(data))
plot.ts(data)
adf.test(data)
acf(data)
pacf(data)
model<-auto.arima(data)
model
fore<-forecast(model,h=20)
fore

PepperPrice
class(PepperPrice)
sum(is.na(PepperPrice))
adf.test(PepperPrice)
acf(PepperPrice)
pacf(PepperPrice)
auto.arima(PepperPrice)
forecast(auto.arima(PepperPrice),h=60)


dmm<-read.csv("Unit Root.csv")
dmm
F<-dmm$FTSE100
class(F)
F2<-ts(F,start=02-01-1996,end=08-05-1997,frequency = 365)
plot.ts(F2)
class(F2)
F2
sum(is.na(F2))
adf.test(F2)
acf(F2)
pacf(F2)
G<-dmm$FX......
class(G)
G2<-ts(G,start=02-01-1996,end=08-05-1997,frequency = 365)
plot.ts(G2)
class(G2)
G2
sum(is.na(G2))
adf.test(G2)
acf(G2)
pacf(G2)

dmj<-read.csv("MWS.csv")
View(dmj)
a<-dmj$.MWS
class(a)
dmj2<-ts(a,start=1984,end=2007,frequency=12)
dmj2
class(dmj2)
sum(is.na(dmj2))
adf.test(dmj2)
acf(dmj2)
pacf(dmj2)
models<-auto.arima(dmj2)
models
fores<-forecast(models,h=48)
fores

library(fpp)
library(TTR)
library(tseries)
library(forecast)
library(devtools)
#RW
hdf<-rnorm(200)
hdf2<-cumsum(hdf)
hdf3=10+hdf2#..Rw
hdf4<-diff(hdf3)
adf.test(hdf4)
plot.ts(hdf4)
acf(hdf4)
pacf(hdf4)
#RW with drift
alp=0.1
hdf5<-alp+hdf3
hdf6<-diff(hdf5)
adf.test(hdf6)
plot.ts(hdf6)
acf(hdf6)
pacf(hdf6)
#deter. trend
time=1:200
hdf7=10+0.5*time+hdf2#...dt
hdf8<-diff(hdf7)
adf.test(hdf8)
plot.ts(hdf8)
acf(hdf8)
pacf(hdf8)
#deter. trend with drift
alp=0.1
hdf9<-alp+hdf7
hdf10<-diff(hdf9)
adf.test(hdf10)
plot.ts(hdf10)
acf(hdf10)
pacf(hdf10)



#RW
sen<-rnorm(200)
sen2<-cumsum(sen)
sen3=10+sen2#..Rw
sen4<-diff(sen3)
adf.test(sen4)
plot.ts(sen4)
acf(sen4)
pacf(sen4)
#RW with drift
alp=0.1
sen5<-alp+sen3
sen6<-diff(sen5)
adf.test(sen6)
plot.ts(sen6)
acf(sen6)
pacf(sen6)
#deter. trend
time=1:200
sen7=10+0.5*time+sen2#...dt
sen8<-diff(sen7)
adf.test(sen8)
plot.ts(sen8)
acf(sen8)
pacf(sen8)
#deter. trend with drift
alp=0.1
sen9<-alp+sen7
sen10<-diff(sen9)
adf.test(sen10)
plot.ts(sen10)
acf(sen10)
pacf(sen10)

#REgrr.
mm<-lm(hdf4~sen4)
summary(mm)
plot(mm)
 

AR3MA3<-arima.sim(n=1000000,list(ar=c(0.5,0.1,0.2),ma=c(0.6,0.4,0.3)),innov=rnorm(1000000))
class(AR3MA3)
AR3MA3<-diff(AR3MA3)
adf.test(AR3MA3)
acf(AR3MA3)
pacf(AR3MA3)
arima(AR3MA3,order=c(3,0,3))

MA2<-arima.sim(n=10000,list(ma=c(0.5,0.4)),innov=rnorm(10000))
class(MA2)
diff(MA2)
adf.test(MA2)
acf(MA2)
pacf(MA2)
arima(MA2,order=c(0,0,2))

dm3<-read.csv("Nifty5.csv")
View(dm3)
a<-dm3$Close
class(ts)
a2<-ts(a,start=2013,end=2018,frequency = 365)
a2
class(a2)
sum(is.na(a2))
adf.test(a2)
acf(a2)
pacf(a2)
modells<-auto.arima(a2)
modells
forres<-forecast(modells,h=365)
forres

dm4<-read.csv("final6.csv")
View(dm4)
am<-dm4$..TMRF
am
sum(is.na(am))
am2<-na.omit(am)
sum(is.na(am2))
class(am2)
am3<-ts(am2,start=1984,end=2007,frequency=12)
am3
class(am3)
adf.test(am3)
acf(am3)
pacf(am3)
modellls<-auto.arima(am3)
modellls
forrres<-forecast(modellls,h=36)
forrres


dmd<-read.csv("M.csv")
View(dmd)
M<-dmd$.MWS
M
sum(is.na(M))
M2<-na.omit(M)
sum(is.na(M2))
class(M2)
M3<-ts(M2,start=1984,end=2007,frequency=12)
M3
class(M3)
decompose(M3)
adf.test(M3)
plot.ts(M3)
f1<-meanf(M3,h=36)
f1
f2<-naive(M3,h=36)
f2
f3<-SMA(M3,h=36)
f3
f4<-ses(M3,initial="simple",h=36)
f4
f4<-holt(M3,h=36)
f4
f5<-hw(M3,h=36)
f5
f6<-hw(M3,damped = TRUE,h=36)
f6
decompose(f6)
??hw
f7<-hw(M3,seasonal = "additive",exponential = TRUE,h=36)
decompo
accuracy(f1)
accuracy(f2)
accuracy(f4)
accuracy(f5)


dd<-read.csv("PCAR.csv")
View(dd)
dd
sum(is.na(dd))
cov(dd)
Eigenvalues<-eigen(cov(dd))$values
Eigenvalues
Eigenvectors<-eigen(cov(dd))$vectors
Eigenvectors
PC<-as.matrix(dd)
cov(PC)
Eigenvalues[1:3]
print(round(Eigenvalues/sum(Eigenvalues)*100,digits=2))
PCAs2<-prcomp(dd)
PCAs2
pcas.var<-PCAs2$sdev^2
plot(PCAs2)
PCAs2$rotation

am<-read.csv("agg.csv")
am
dist<-as.dist(as.matrix(am[,2:5]),diag = TRUE)
dist
models<-hclust(((dist)^2),method="median")
summary(models)
plot(models)
models$height

dd2<-read.csv("CarsR.csv")
dd2
sum(is.na(dd2))
dd3<-na.omit(dd2)
sum(is.na(dd3))
dd3
cor(dd3)
Eigenvalues2<-eigen(cor(dd3))$values
Eigenvalues2
Eigenvectors2<-eigen(cor(dd3))$vectors
Eigenvectors2
PC<-as.matrix(dd3)
cor(PC)
Eigenvalues2[1:3]
print(round(Eigenvalues2/sum(Eigenvalues2)*100,digits=2))
PCASS<-prcomp(dd3)
PCASS
pca.var4<-PCASS$sdev^2
plot(PCASS)
PCASS$rotation

