
d=read.csv("BikeDemandDaily.csv", header=TRUE)

> colnames(d)
 [1] "Index"         "year"          "month"         "day"          
 [5] "season"        "holiday"       "workingday"    "meanatemp"    
 [9] "maxatemp"      "minatemp"      "sdatemp"       "meanhumidity" 
[13] "maxhumidity"   "minhumidity"   "sdhumidity"    "meanwindspeed"
[17] "maxwindspeed"  "minwindspeed"  "sdwindspeed"   "Casual"       
[21] "Registered"    "Total"        

plot(d$Index, d$Total)

m=lm(Total~Index, data=d)

plot(d$Index, d$Total)
lines(d$Index, m$fitted.values, col=2)

DeTrended_Demand=d$Total-m$fitted.values

plot(d$Index, DeTrended_Demand)

m1=lm(Total~Index+month+season+meanatemp
		+holiday+maxatemp+minatemp+sdatemp
		+meanhumidity+maxhumidity+minhumidity
		+sdhumidity+meanwindspeed+maxwindspeed
		+minwindspeed+sdwindspeed, data=d)

plot(d$Index, d$Total)
points(d$Index, m$fitted.values, col=2, pch=".", cex=2)
points(d$Index, m1$fitted.values, col=4, pch=".", cex=2)


library(glmnet)

y=d$Total
X=model.matrix(m1)

m2=cv.glmnet(X,y)

m2$lambda.min

plot(m2)

m3=glmnet(X,y,lambda=m2$lambda.min)
m3$beta


17 x 1 sparse Matrix of class "dgCMatrix"
                       s0
(Intercept)      .       
Index            9.026593
month            .       
season         -29.876651
meanatemp       68.092206
holiday        -67.966999
maxatemp      -126.481973
minatemp       170.156684
sdatemp        455.963161
meanhumidity   -35.247431
maxhumidity      1.355405
minhumidity      9.546752
sdhumidity      49.580094
meanwindspeed  -16.726824
maxwindspeed   -25.347022
minwindspeed     8.352978
sdwindspeed     26.540699

################################################
#   COMPARE LM WITH LASSO 
################################################

n=dim(d)[1]

ind=sample.int(n, size=floor(0.8*n), replace=FALSE)

train=d[ind,]
test=d[-ind,]


m5=lm(Total~Index+month+season+meanatemp
		+holiday+maxatemp+minatemp+sdatemp
		+meanhumidity+maxhumidity+minhumidity
		+sdhumidity+meanwindspeed+maxwindspeed
		+minwindspeed+sdwindspeed, data=train)

p5=predict(m5, newdata=test)

rmspe=function(yp, yo){

	err=yp-yo
	errsq=err^2
	merrsq=mean(errsq)
	return(sqrt(merrsq))

}

mape=function(yp, yo){

	err=yp-yo
	aerr=abs(err)
	maerr=mean(aerr)
	return(maerr)

}

rmspe_lm=rmspe(p5, test$Total);
mape_lm=mape(p5, test$Total);

XTest=X[-ind,]
YTest=test$Total

YTrain=train$Total
XTrain=X[ind,]

m6=cv.glmnet(XTrain,YTrain)
m7=glmnet(X,y,lambda=m6$lambda.min)

p7=predict(m7, newx=XTest)

rmspe_lasso=rmspe(p7, YTest)
mape_lasso=mape(p7, YTest)  

#########################################################

library(randomForest)

r1=randomForest(Total~Index+month+season+meanatemp
		+holiday+maxatemp+minatemp+sdatemp
		+meanhumidity+maxhumidity+minhumidity
		+sdhumidity+meanwindspeed+maxwindspeed
		+minwindspeed+sdwindspeed, data=train, ntree=1000)

p1=predict(r1, newdata=test)

rmspe_rf=rmspe(p1, YTest)
mape_rf=mape(p1, YTest)

plot(r1)
varImpPlot(r1)
partialPlot(r1, pred.data=train, x.var="meanatemp")
partialPlot(r1, pred.data=train, x.var="maxatemp")
partialPlot(r1, pred.data=train, x.var="sdatemp")
partialPlot(r1, pred.data=train, x.var="maxhumidity")

############Neural networks

library(nnet)

n1=nnet(Total~Index+month+season+meanatemp
		+holiday+maxatemp+minatemp+sdatemp
		+meanhumidity+maxhumidity+minhumidity
		+sdhumidity+meanwindspeed+maxwindspeed
		+minwindspeed+sdwindspeed, data=train,
		size=16, linout=TRUE)

p1=predict(n1, newdata=test, type="raw")

rmspe_nnet=rmspe(p1, YTest)
mape_nnet=mape(p1, YTest)

#import function from Github
require(RCurl)
library(scales)

root.url<-'https://gist.githubusercontent.com/fawda123'
raw.fun<-paste(
  root.url,
  '5086859/raw/cc1544804d5027d82b70e74b83b3941cd2184354/nnet_plot_fun.r',
  sep='/'
  )
script<-getURL(raw.fun, ssl.verifypeer = FALSE)
eval(parse(text = script))
rm('script','raw.fun')

par(mar=numeric(4),mfrow=c(1,2),family='serif')
plot(n1,nid=F)
plot(n1)


