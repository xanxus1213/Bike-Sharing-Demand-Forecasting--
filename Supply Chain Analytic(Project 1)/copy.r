rm(list=ls())
setwd("C:/Users/Ujjal/Documents/UIUC Teaching");
d<-read.csv("BikeDemandDaily.csv", header=TRUE)
View(d)

plot(d$Total, col=2, xlab="Day", ylab="Total Demand")
abline(lm(d$Total~d$Index), col=4)


m1<-lm(Total~Index, data=d)

fitted<-m1$fitted.values; 

plot(fitted)

plot(d$Index, d$Total, pch="*")
lines(d$Index, fitted)

detrend<-d$Total-fitted;

plot(d$Index, detrend, pch="*")

m2<-lm(Total~Index+as.factor(season), data=d);
summary(m2)
fitted2<-m2$fitted.values;

plot(d$Index, d$Total, pch="*", col=4)
lines(d$Index, fitted2, col=2, lwd=2)

m3<-lm(Total~Index+as.factor(season)+as.factor(holiday)+meanatemp+meanwindspeed+meanhumidity, data=d);
summary(m3)
fitted3<-m3$fitted.values;

plot(d$Index, d$Total, pch="*", col=4)
lines(d$Index, fitted3, col=2, lwd=2)


###################################################

#Prediction Error

n<-dim(d)[1];

ind<-sample.int(n, size=150)

dtrain<-d[-ind,];
dtest<-d[ind,]

plot(dtrain$Index, dtrain$Total, xlim=c(1,460), xlab="Day", ylab="Total Demand", col=2)
abline(lm(dtrain$Total~dtrain$Index), lty=2, col=1)
points(dtest$Index, dtest$Total,  col=4, pch="*");
abline(lm(dtest$Total~dtest$Index), lty=3, col=3);
legend("topleft", legend=c("Train Sample Trend Line", "Test Sample Trend Line", "Train Data", "Test Data"), lty=c(2,3,NA,NA), col=c(1,3,2,4), pch=c(NA,NA,"o","*"))

m0<-lm(Registered~Index+year+as.factor(month)+as.factor(day)+as.factor(season)+as.factor(holiday)+as.factor(workingday)+meanatemp+maxatemp+minatemp+sdatemp+meanhumidity+maxhumidity+minhumidity+sdhumidity+meanwindspeed+maxwindspeed+minwindspeed+sdwindspeed, data=dtrain)
p0<-predict(m0, newdata=dtest)
plot(dtest$Index, dtest$Registered, pch="*", col=2, xlab="Day", ylab="Registered Demand");
points(dtest$Index, p0, pch="o", col=4)
legend("topleft", legend=c("Observed Demand", "Predicted Demand"), pch=c("*","o"), col=c(2,4))

m0<-lm(Casual~Index+year+as.factor(month)+as.factor(day)+as.factor(season)+as.factor(holiday)+as.factor(workingday)+meanatemp+maxatemp+minatemp+sdatemp+meanhumidity+maxhumidity+minhumidity+sdhumidity+meanwindspeed+maxwindspeed+minwindspeed+sdwindspeed, data=dtrain)
p0<-predict(m0, newdata=dtest)
p0
plot(dtest$Index, dtest$Casual, pch="*", col=2, xlab="Day", ylab="Casual Demand");
points(dtest$Index, p0, pch="o", col=4)
legend("topleft", legend=c("Observed Demand", "Predicted Demand"), pch=c("*","o"), col=c(2,4))

m0<-lm(Total~Index+year+as.factor(month)+as.factor(day)+as.factor(season)+as.factor(holiday)+as.factor(workingday)+meanatemp+maxatemp+minatemp+sdatemp+meanhumidity+maxhumidity+minhumidity+sdhumidity+meanwindspeed+maxwindspeed+minwindspeed+sdwindspeed, data=dtrain)
summary(m0)
p0<-predict(m0, newdata=dtest)


plot(dtest$Index, dtest$Total, pch="*", col=2, xlab="Day", ylab="Total Demand");
points(dtest$Index, p0, pch="o", col=4)
legend("topleft", legend=c("Observed Demand", "Predicted Demand"), pch=c("*","o"), col=c(2,4))



m1<-lm(Total~Index, data=d)
p1<-predict(m1)
detrendedDemand<-d$Total-p1
plot(d$Index, detrendedDemand, xlab="Day", ylab="Detrended Demand", col=2, pch="*")
lines(c(0,460),c(0,0))

boxplot(d$Total~d$season, xlab="Seasons", ylab="Demand", col=c(2,3,4,5));

install.packages("car")
library(car)
library(MASS)

scatterplotMatrix(~Total+meanatemp+meanhumidity+maxwindspeed, data=d, pch=".")

m1<-lm(Total~Index, data=dtrain)
summary(m1)
p1<-predict(m1, newdata=dtest)
plot(dtest$Index, dtest$Total, pch="*", col=2, xlab="Day", ylab="Demand");
points(dtest$Index, p1, pch="o", col=4)
legend("topleft", legend=c("Observed Demand", "Predicted Demand"), pch=c("*","o"), col=c(2,4))




m2<-lm(Total~Index+as.factor(season), data=dtrain);
summary(m2)
p2<-predict(m2, newdata=dtest)
plot(dtest$Index, dtest$Total, pch="*", col=2, xlab="Day", ylab="Demand");
points(dtest$Index, p2, pch="o", col=4)
legend("topleft", legend=c("Observed Demand", "Predicted Demand"), pch=c("*","o"), col=c(2,4))


m3<-lm(Total~Index+as.factor(season)+as.factor(holiday)+meanatemp+meanwindspeed+meanhumidity, data=dtrain);
summary(m3)
p3<-predict(m3, newdata=dtest)
plot(dtest$Index, dtest$Total, pch="*", col=2, xlab="Day", ylab="Demand");
points(dtest$Index, p3, pch="o", col=4)
legend("topleft", legend=c("Observed Demand", "Predicted Demand"), pch=c("*","o"), col=c(2,4))


m4<-glm(Total~Index+as.factor(season)+as.factor(holiday)+meanatemp+meanwindspeed+meanhumidity, data=dtrain, family="gaussian");
summary(m4)
p4<-predict(m3, newdata=dtest, type="response")
plot(dtest$Index, dtest$Total, pch="*", col=2, xlab="Day", ylab="Demand");
points(dtest$Index, p4, pch="o", col=4)
legend("topleft", legend=c("Observed Demand", "Predicted Demand"), pch=c("*","o"), col=c(2,4))
RMSPE(dtest$Total, p4);
#762.2077

m5<-glm(Total~Index+as.factor(season)+as.factor(holiday)+meanatemp+meanwindspeed+meanhumidity, data=dtrain, family="poisson");
summary(m5)
p5<-predict(m5, newdata=dtest, type="response")
plot(dtest$Index, dtest$Total, pch="*", col=2, xlab="Day", ylab="Demand");
points(dtest$Index, p5, pch="o", col=4)
legend("topleft", legend=c("Observed Demand", "Predicted Demand"), pch=c("*","o"), col=c(2,4))
RMSPE(dtest$Total, p5);
#845.238

m6<-glm.nb(Total~Index+as.factor(season)+as.factor(holiday)+meanatemp+meanwindspeed+meanhumidity, data=dtrain);
summary(m6)
p6<-predict(m6, newdata=dtest, type="response")
plot(dtest$Index, dtest$Total, pch="*", col=2, xlab="Day", ylab="Demand");
points(dtest$Index, p6, pch="o", col=4)
legend("topleft", legend=c("Observed Demand", "Predicted Demand"), pch=c("*","o"), col=c(2,4))
RMSPE(dtest$Total, p6);
#889.2531



p2<-predict(m2, newdata=dtest)

p3<-predict(m3, newdata=dtest)


plot(dtest$Index, dtest$Total, pch="o")
points(dtest$Index, p2, pch="*", col=2)
points(dtest$Index, p3, pch="*", col=4);


RMSPE=function(d,p){

	y1=mean((d-p)^2);
	y2=sqrt(y1)

	return(y2);

}

RMSPE(dtest$Total, p2)
#995.4348

RMSPE(dtest$Total, p3)
#762.2077

library(car)

scatterplotMatrix(~Total+meanatemp+meanhumidity+meanwindspeed, data=d, pch=".")
#???????????????

#----------------------------------------------------------------------
#Linear Regression
m1<-glm(Total~1, data=dtrain, family="gaussian")
m2<-glm(Total~Index+year+as.factor(month)+as.factor(day)+as.factor(season)+as.factor(holiday)+as.factor(workingday)+meanatemp+maxatemp+minatemp+sdatemp+meanhumidity+maxhumidity+minhumidity+sdhumidity+meanwindspeed+maxwindspeed+minwindspeed+sdwindspeed, data=dtrain, family="gaussian")

summary(m1)
summary(m2)

mean(d$Total)

s1<-step(m1, scope=list(lower=m1, upper=m2), direction="forward")
s1
summary(s1)


p7<-predict(s1, newdata=dtest);
plot(dtest$Index, dtest$Total, pch="*", col=2, xlab="Day", ylab="Demand");
points(dtest$Index, p7, pch="o", col=4)
legend("topleft", legend=c("Observed Demand", "Predicted Demand"), pch=c("*","o"), col=c(2,4))
RMSPE(dtest$Total, p7);
#717.2872



plot(dtest$Index, dtest$Total, pch="o");
points(dtest$Index, p2, pch="*", col=2);
points(dtest$Index, p3, pch="*", col=4);
points(dtest$Index, p4, pch="*", col=3);

RMSPE(dtest$Total, p2);
#995.4348
RMSPE(dtest$Total, p3);
#762.2077
RMSPE(dtest$Total, p4);
# 762.2077

#-------------------------------------------------------
#Lasso
install.packages('glmnet')
library(glmnet)

for1<-as.formula(Total~Index+year+as.factor(month)+as.factor(day)+as.factor(season)+as.factor(holiday)+as.factor(workingday)+meanatemp+maxatemp+minatemp+sdatemp+meanhumidity+maxhumidity+minhumidity+sdhumidity+meanwindspeed+maxwindspeed+minwindspeed+sdwindspeed);

y<-dtrain$Total;
x<-model.matrix(for1, data=dtrain);
install.packages('glmnet')
library('glmnet')
l1<-cv.glmnet(x,y,alpha=1);

plot(l1)

print(log(l1$lambda.min));
#0.3346309
l2<-glmnet(x,y, lambda=l1$lambda.min);

xn<-model.matrix(for1, data=dtest)

p5<-predict(l2, newx=xn)
plot(dtest$Index, dtest$Total, pch="*", col=2, xlab="Day", ylab="Demand");
points(dtest$Index, p5, pch="o", col=4)
legend("topleft", legend=c("Observed Demand", "Predicted Demand"), pch=c("*","o"), col=c(2,4))
RMSPE(dtest$Total, p5);
#718.2931



plot(dtest$Index, dtest$Total, pch="o");
points(dtest$Index, p2, pch="*", col=2);
points(dtest$Index, p3, pch="*", col=4);
points(dtest$Index, p4, pch="*", col=3);
points(dtest$Index, p5, pch="*", col=5);

RMSPE(dtest$Total, p2);
#995.4348
RMSPE(dtest$Total, p3);
#762.2077
RMSPE(dtest$Total, p4);
#762.2077
RMSPE(dtest$Total, p5);
#718.2931

#----------------------------------------------------------
#randomForest
install.packages('randomForest')
library(randomForest)

dtrain$month=as.factor(dtrain$month);
dtrain$day=as.factor(dtrain$day);
dtrain$season=as.factor(dtrain$season);
dtrain$holiday=as.factor(dtrain$holiday);
dtrain$workingday=as.factor(dtrain$workingday);

r1<-randomForest(Total~Index+year+month+day+season+holiday+workingday
			+meanatemp+maxatemp+minatemp+sdatemp+meanhumidity+maxhumidity+minhumidity+sdhumidity+meanwindspeed
			+maxwindspeed+minwindspeed+sdwindspeed, data=dtrain, ntree=500,
			do.trace=1, importance=TRUE, proximity=TRUE)

dtest$month=as.factor(dtest$month);
dtest$day=as.factor(dtest$day);
dtest$season=as.factor(dtest$season);
dtest$holiday=as.factor(dtest$holiday);
dtest$workingday=as.factor(dtest$workingday);

p6<-predict(r1, newdata=dtest, type="response");
plot(dtest$Index, dtest$Total, pch="*", col=2, xlab="Day", ylab="Demand");
points(dtest$Index, p6, pch="o", col=4)
legend("topleft", legend=c("Observed Demand", "Predicted Demand"), pch=c("*","o"), col=c(2,4))
RMSPE(dtest$Total, p6);
#689.5276


plot(dtest$Index, dtest$Total, pch="o");
points(dtest$Index, p2, pch="*", col=2);
points(dtest$Index, p3, pch="*", col=4);
points(dtest$Index, p4, pch="*", col=3);
points(dtest$Index, p5, pch="*", col=6);
points(dtest$Index, p6, pch="*", col=7);


RMSPE(dtest$Total, p2);
#995.4348
RMSPE(dtest$Total, p3);
#762.2077
RMSPE(dtest$Total, p4);
#762.2077
RMSPE(dtest$Total, p5);
#718.2931
RMSPE(dtest$Total, p6);
#689.5276

varImpPlot(r1)

r2<-randomForest(Total~meanatemp+meanwindspeed, data=d, ntree=500);

T<-seq(min(d$meanatemp), max(d$meanatemp), 0.1)
W<-seq(min(d$meanwindspeed), max(d$meanwindspeed), 0.1)

dn<-expand.grid(T,W);
colnames(dn)<-c("meanatemp", "meanwindspeed");

p7<-predict(r2, newdata=dn, type="response");
z<-matrix(p7, nrow=length(T), ncol=length(W), byrow=FALSE);

image(T,W,z)
contour(T,W,z, add=TRUE)


#---------------------------------------

library(e1071)

s1<-svm(for1, data=dtrain)

p8<-predict(s1, newdata=dtest, type="response");
plot(dtest$Index, dtest$Total, pch="*", col=2, xlab="Day", ylab="Demand");
points(dtest$Index, p8, pch="o", col=4)
legend("topleft", legend=c("Observed Demand", "Predicted Demand"), pch=c("*","o"), col=c(2,4))
RMSPE(dtest$Total, p8);
#683.813

plot(dtest$Index, dtest$Total, pch="o");
points(dtest$Index, p2, pch="*", col=2);
points(dtest$Index, p3, pch="*", col=4);
points(dtest$Index, p4, pch="*", col=3);
points(dtest$Index, p5, pch="*", col=5);
points(dtest$Index, p6, pch="*", col=6);
points(dtest$Index, p8, pch="*", col=7);


RMSPE(dtest$Total, p2);
#995.4348
RMSPE(dtest$Total, p3);
#762.2077
RMSPE(dtest$Total, p4);
#762.2077
RMSPE(dtest$Total, p5);
#718.2931
RMSPE(dtest$Total, p6);
#689.5276
RMSPE(dtest$Total, p8);
#683.813

T<-seq(min(d$meanatemp), max(d$meanatemp), 0.1)
W<-seq(min(d$meanwindspeed), max(d$meanwindspeed), 0.1)

dn<-expand.grid(T,W);
colnames(dn)<-c("meanatemp", "meanwindspeed");

s2<-svm(Total~meanatemp+meanwindspeed, data=d);

p9<-predict(s2, newdata=dn, type="response");
z<-matrix(p9, nrow=length(T), ncol=length(W), byrow=FALSE);

image(T,W,z)
contour(T,W,z, add=TRUE)

#--------------------
#Ann
install.packages('nnet')
library(nnet)
n1 = nnet(Total~Index+month+season+meanatemp
          +holiday+maxatemp+minatemp+sdatemp
          +meanhumidity+maxhumidity+minhumidity
          +sdhumidity+meanwindspeed+maxwindspeed
          +minwindspeed+sdwindspeed,data=dtrain, size = 16, linout = TRUE)

p10<-predict(n1, newdata=dtest, type="raw");
plot(dtest$Index, dtest$Total, pch="*", col=2, xlab="Day", ylab="Demand");
points(dtest$Index, p10, pch="o", col=4)
legend("topleft", legend=c("Observed Demand", "Predicted Demand"), pch=c("*","o"), col=c(2,4))
RMSPE(dtest$Total, p10);
#1861.472

plot(dtest$Index, dtest$Total, pch="o");
points(dtest$Index, p2, pch="*", col=2);
points(dtest$Index, p3, pch="*", col=4);
points(dtest$Index, p4, pch="*", col=3);
points(dtest$Index, p5, pch="*", col=5);
points(dtest$Index, p6, pch="*", col=6);
points(dtest$Index, p8, pch="*", col=7);
points(dtest$Index, p10, pch="*",col=8);

RMSPE(dtest$Total, p2);
#995.4348
RMSPE(dtest$Total, p3);
#762.2077
RMSPE(dtest$Total, p4);
#762.2077
RMSPE(dtest$Total, p5);
#718.2931
RMSPE(dtest$Total, p6);
#689.5276
RMSPE(dtest$Total, p8);
#683.813
RMSPE(dtest$Total, p10);
#1861.472







