library(ggplot2)
######Q1 a###
set.seed(123)
sigma<-1
B<-10
plot1<-function(sigma,B){
M<-matrix(NA,ncol=200,nrow=B)
for(j in 1:B)
{
	m_1<-c()
	for(i in 1:200)
	{
		x<-rnorm(i,1,sigma)
		m_1<-c(m_1,mean(x))
	}
	M[j,]<-m_1
}
MSE<-c()
for(i in 1:200)
{
	m<-mean((M[,i]-1)^2)
	MSE<-c(MSE,m)
}
s_n<-sigma^2/(1:200)
type<-as.factor(rep(c("MSE","sigma^2/n"),each=200))
#data<-data.frame(x=c(MSE,s_n),n=1:200,type)
#ggplot(data=data,aes(n,x,color=type))+
#geom_line()+
#scale_y_log10()+
#ylab("")+
#scale_color_discrete(labels=c("MSE",expression(sigma^2/n)))+
#theme(legend.title=element_blank())+
#ggtitle(bquote(list(B==.(B),sigma==.(sigma))))
data1<-data.frame(MSE,s_n,n=1:200)
ggplot(data=data1,aes(n,MSE))+
geom_point(size=1,color="orange",alpha=0.5)+
geom_line(aes(x=n,y=s_n),linetype="dashed")+
scale_y_log10()+
ylab("MSE")+
ggtitle(bquote(list(B==.(B),sigma==.(sigma))))
}

data1<-data.frame(MSE,s_n,n=1:200)

ggplot(data=data1,aes(n,MSE))+
geom_point(aes(size=0.1))+
geom_line(aes(x=n,y=s_n))+
scale_y_log10()






set.seed(123)
n=100
B=1000
Z<-c()
for(i in 1:B)
{
	Z<-c(Z,sqrt(n)*(mean(rnorm(n,1,1))-1))
}
x<-density(Z)$x
nden<-dnorm(density(Z)$x)
y<-density(Z)$y
type<-as.factor(rep(c("estimated","normal"),each=length(x)))
data<-data.frame(x=rep(x,2),y<-c(y,nden),type)
ggplot(data=data,aes(x=x,y=y,color=type))+
geom_line()+
ylab("density")+
xlab("x")+
scale_color_manual(values=c("#990000","#FF9900"))+
theme(legend.title=element_blank())+
ggtitle(bquote(list(B==.(B),n==.(n),bandwidth==.(round(density(Z)$bw,3)))))


######Q2 b
set.seed(123)
xi<-(1:1000)/1000
r<-sqrt(xi*(1-xi))*sin(2.1*pi/(xi+0.05))
e<-rnorm(1000,0,0.1)
yi<-r+e
plot(xi,yi)


Risk<-c()
for( h in (5:50)/5000)
{
	R<-0
	n<-length(xi)
	for(i in 1:n)
	{
		x<-xi[i]
		W<-diag(exp(-((xi-x)/h)^2))/sum(exp(-((xi-x)/h)^2))
		X<-matrix(c(rep(1,length(yi)),xi-x),ncol=2)
		L<-c(1,0)%*%solve(t(X)%*%W%*%X)%*%t(X)%*%W
		R<-R+((yi[i]-L%*%yi)/(1-L[i]))^2
	}
	Risk<-c(Risk,R/n)
	print(h)
}

H<-min(((5:50)/5000)[which.min(Risk)])
H<-0.0032
n<-length(xi)
Yhat<-c()
for(i in 1:n)
{
	x<-xi[i]
	W<-diag(exp(-((xi-x)/H)^2))/sum(exp(-((xi-x)/H)^2))
	X<-matrix(c(rep(1,length(yi)),xi-x),ncol=2)
	L<-c(1,0)%*%solve(t(X)%*%W%*%X)%*%t(X)%*%W
	Yhat<-c(Yhat,L%*%yi)
}

data2<-data.frame(xi=(5:50)/5000,Risk)
ggplot(data=data2,aes(x=xi,y=Risk))+
geom_line()+
scale_x_continuous(breaks=c(0.001,0.0032,0.005,0.01))+
geom_vline(xintercept=H,linetype="longdash",color="#FFCC00")+
xlab("bandwidth")+
ylab("Risk")+
ggtitle("LOO Cross Validation")

data3<-data.frame(xi,yi,Yhat,r)

ggplot(data=data3,aes(x=xi))+
geom_point(aes(y=yi),color="#FFCC00",alpha=0.5)+
geom_line(aes(y=r,color="True curve"))+
geom_line(aes(y=Yhat,color="Estimated curve"))+
scale_color_manual("",breaks=c("True curve","Estimated curve"),values=c("#990000","#FF9900"))+
xlab("x")+
ylab("y")


######Q3
data(faithful)
E<-faithful$eruptions
W<-faithful$waiting
J1<-function(h)
{
	fhat<-Vectorize(function(x) density(E,from=x,to=x,bw=h,n=1,kernel="gaussian")$y)
	fhati<-Vectorize(function(i) density(E[-i],from=E[i],to=E[i],bw=h,n=1,kernel="gaussian")$y)
	F<-fhati(1:length(E))
	return(integrate(function(x) fhat(x)^2,-Inf,Inf)$value-2*mean(F))
}
H<-seq(0.02,0.6,by=0.005)
R<-Vectorize(J1)(H)
H0<-H[which.min(R)]
data<-data.frame(H,R)
ggplot(data=data, aes(x=H,y=R))+
geom_line()+
geom_vline(xintercept=H[which.min(R)],linetype="longdash",color="#FF9900")+
ylab("Risk")+
xlab("bandwidth")+
scale_x_continuous(breaks=c(0.0,H[which.min(R)],0.2,0.4,0.6))+
ggtitle("LOO Cross Validation for eruptions")




J2<-function(h)
{
	fhat<-Vectorize(function(x) density(W,from=x,to=x,n=1,bw=h)$y)
	fhati=Vectorize(function(i) density(W[-i],from=W[i],to=W[i],n=1,bw=h)$y)
	F=fhati(1:length(W))
	return(integrate(function(x) fhat(x)^2,-40,200)$value-2*mean(F))
}
h<-seq(1,6,by=.1)
r<-Vectorize(J2)(h)
h0<-h[which.min(r)]
data<-data.frame(h,r)
ggplot(data=data, aes(x=h,y=r))+
geom_line()+
geom_vline(xintercept=H[which.min(r)],linetype="longdash",color="#FF9900")+
ylab("Risk")+
xlab("bandwidth")+
scale_x_continuous(breaks=c(1,2,h[which.min(r)],4,6))+
ggtitle("LOO Cross Validation for waiting")


H1<-1.06*sd(E)/length(E)^0.2
x<-density(E,bw=H0,kernel="gaussian")$x
y1<-density(E,bw=H0,kernel="gaussian")$y
y2<-density(E,bw=H1,kernel="gaussian")$y
data<-data.frame(x,y1,y2)
ggplot(data=data,aes(x=x))+
geom_line(aes(y=y1,color="Bandwidth=0.105"))+
geom_line(aes(y=y2,color="Bandwidth=0.394"))+
ylab("density")+
xlab("eruptions")+
scale_color_manual("",breaks=c("Bandwidth=0.105","Bandwidth=0.394"),values=c("#990000","#FF9900"))+
ggtitle("Kernel density estimation for eruptions")

h1<-1.06*sd(W)/length(W)^0.2
x<-density(W,bw=h0,kernel="gaussian")$x
y1<-density(W,bw=h0,kernel="gaussian")$y
y2<-density(W,bw=h1,kernel="gaussian")$y
data<-data.frame(x,y1,y2)
ggplot(data=data,aes(x=x))+
geom_line(aes(y=y1,color="Bandwidth=2.600"))+
geom_line(aes(y=y2,color="Bandwidth=4.696"))+
ylab("density")+
xlab("waiting")+
scale_color_manual("",breaks=c("Bandwidth=2.600","Bandwidth=4.696"),values=c("#990000","#FF9900"))+
ggtitle("Kernel density estimation for waiting")


######5
train<-read.csv("~/Desktop/UChi/37400/train.csv",header=TRUE)
Train<-train[train$day<=15,]
Train_v<-train[train$day>15,]
Train1<-data.frame(daylabel=as.factor(Train$daylabel),day=as.factor(Train$day),year=as.factor(Train$year),year=as.factor(Train$year),month=as.factor(Train$month),hour=as.factor(Train$hour),season=as.factor(Train$season),holiday=as.factor(Train$holiday),workingday=as.factor(Train$workingday),lcount=log(Train$count+1),weather=Train$weather,temp=Train$temp,atemp=Train$atemp,humidity=Train$humidity,windspeed=Train$windspeed)
Train1_v<-data.frame(daylabel=as.factor(Train_v$daylabel),day=as.factor(Train_v$day),year=as.factor(Train_v$year),year=as.factor(Train_v$year),month=as.factor(Train_v$month),hour=as.factor(Train_v$hour),season=as.factor(Train_v$season),holiday=as.factor(Train_v$holiday),workingday=as.factor(Train_v$workingday),lcount=log(Train_v$count+1),weather=Train_v$weather,temp=Train_v$temp,atemp=Train_v$atemp,humidity=Train_v$humidity,windspeed=Train_v$windspeed)


ggplot(data=Train1)+
geom_point(aes(temp,atemp),alpha=0.1)


mod1<-lm(lcount~month+hour+workingday+weather+atemp+humidity+windspeed,data=Train1)
pred1<-predict(mod1,Train1_v)
sqrt(mean((pred1-Train1_v$lcount)^2))
mod2<-lm(lcount~month+hour+workingday+weather+temp+humidity+windspeed,data=Train1)
pred2<-predict(mod2,Train1_v)
sqrt(mean((pred2-Train1_v$lcount)^2))
mod3<-lm(lcount~month+hour+workingday+weather+temp+humidity+windspeed+humidity*windspeed,data=Train1)
pred3<-predict(mod3,Train1_v)
sqrt(mean((pred3-Train1_v$lcount)^2))
mod4<-lm(lcount~year+month+hour+workingday+weather+temp+humidity+windspeed,data=Train1)
mod5<-lm(lcount~(year+month+hour+weather+temp+humidity+windspeed)*workingday,data=Train1)
pred5<-predict(mod5,Train1_v)
sqrt(mean((pred5-Train1_v$lcount)^2))



###b
Train_2<-aggregate(lcount~daylabel,data=Train1, FUN=mean)
Train_2$daylabel<-as.numeric(as.character(Train_2$daylabel))
ggplot(data=Train_2,aes(x=daylabel,y=lcount))+
geom_point()+
ylab("transformed count")
library(locfit)

h<-seq(20,80,1); 
alphas <-cbind(rep(0,length(h)),h)
gcvs<-gcvplot(lcount~daylabel, data=Train_2, alpha=alphas, kern="gauss", deg=1)
cv<-data.frame(r=gcvs$values,h)
ggplot(data=cv, aes(x=h,y=r))+
geom_line()+
geom_vline(xintercept=h[which.min(cv$r)],linetype="longdash",color="#FF9900")+
ylab("Risk")+
xlab("bandwidth")+
scale_x_continuous(breaks=c(20,40,h[which.min(cv$r)],60,80))


bw<-h[which.min(cv$r)]
mod_1<-locfit(lcount~daylabel,data=Train_2,alpha=c(0,bw), deg=1)
fit_1<-fitted(mod_1)

Train_2$fit<-fit_1
ggplot(data=Train_2,aes(x=daylabel))+
geom_point(aes(y=lcount),alpha=0.7,color="#FF9900")+
geom_line(aes(y=fit))+
ylab("transformed count")
xlab("daylabel")


Train_rep<-aggregate(lcount~daylabel,data=Train1,length)
Train1$meanlcount<-rep(fit_1,Train_rep[,2])
Train1$res<-Train1$lcount-Train1$meanlcount 
moda<-lm(res~year+month+hour+workingday+weather+temp+humidity+windspeed,data=Train1)


Train1_v$daylabel<-as.numeric(as.character(Train1_v$daylabel))
locpred<-predict(mod_1,Train1_v)
respred<-predict(moda,Train1_v)
pred_1<-locpred+respred
sqrt(mean((pred_1-Train1_v$lcount)^2))


###c
library(gam)
modela<-gam(lcount~daylabel+year+month+hour+workingday+weather+temp+humidity+windspeed,data=Train1_v)
pred_a<-predict(modela,Train1_v)
sqrt(mean((pred_a-Train1_v$lcount)^2))

###d
Test<-read.csv("~/Desktop/UChi/37400/test.csv",header=TRUE)
test<-data.frame(daylabel=Test$daylabel,year=as.factor(Test$year),month=as.factor(Test$month),hour=as.factor(Test$hour),season=as.factor(Test$season),holiday=as.factor(Test$holiday),workingday=as.factor(Test$workingday),weather=Test$weather,temp=Test$temp,atemp=Test$atemp,humidity=Test$humidity,windspeed=Test$windspeed)



pred_test_lcount<-predict(modela,test)
pred_test_count<-exp(pred_test_lcount)-1
write.table(pred_test_count,file = "assn1-lijing.txt",row.names=FALSE,col.names=FALSE)




