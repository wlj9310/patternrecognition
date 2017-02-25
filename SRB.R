library(ggplot2)
data<-as.numeric(read.csv("~/Documents/Singapore_SRB.csv"))
data[,1]<-as.Date(paste(data[,1],"01",sep="")," %Y %b %d")

leapy<-c(31,29,31,30,31,30,31,31,30,31,30,31,31,28,31,30,31,30,31,31,30,31,30,31,31,28,31,30,31,30,31,31,30,31,30,31,31,28,31,30,31,30,31,31,30,31,30,31)
ndays<-c(rep(leapy,14),31,29,31,30,31,30,31,31,30,31,30,31)

mon_avg_total<-data.frame(month=rep(data[,1],3),avg=c(data$Total.Live.Births.By.Ethnic.Group,data$Total.Male.Live.Births.By.Ethnic.Group,data$Total.Female.Live.Births.By.Ethnic.Group)/ndays,type=as.factor(rep(c("total","male",'female'),each=nrow(data))))

ggplot(data=mon_avg_total,aes(x=month,y=avg,color=type))+
geom_line()


mon_total<-data.frame(month=rep(data[,1],3),avg=c(data$Total.Live.Births.By.Ethnic.Group,data$Total.Male.Live.Births.By.Ethnic.Group,data$Total.Female.Live.Births.By.Ethnic.Group),type=as.factor(rep(c("total","male",'female'),each=nrow(data))))
ggplot(data=mon_total,aes(x=month,y=avg,color=type))+
geom_line()



mon_avg_group1<-data.frame(month=rep(data[,1],8),avg=c(data$Malays.1,data$Malays.2,data$Chinese.1,data$Chinese.2,data$Indians.1,data$Indians.2,data$Other.Ethnic.Groups.1,data$Other.Ethnic.Groups.2)/ndays,group=as.factor(rep(c("Malays","Chinese",'Indians',"Others"),each=nrow(data)*2)),gender=rep(rep(as.factor(c('male','female')),each=nrow(data)),4),type=as.factor(rep(c('M_M','M_F','C_M','C_F','I_M','I_F','O_M','O_F'),each=nrow(data))))
ggplot(data=mon_avg_group1,aes(x=month,y=avg,color=type))+
geom_line()


mon_total_group<-data.frame(month=rep(data[,1],4),avg=c(data$Malays,data$Chinese,data$Indians,data$Other.Ethnic.Groups),type=as.factor(rep(c("Malays","Chinese",'Indians',"Others"),each=nrow(data))))
ggplot(data=mon_total_group,aes(x=month,y=avg,color=type))+
geom_line()

mon_avg_group<-data.frame(month=rep(data[,1],4),avg=c(data$Malays,data$Chinese,data$Indians,data$Other.Ethnic.Groups)/ndays,type=as.factor(rep(c("Malays","Chinese",'Indians',"Others"),each=nrow(data))))
ggplot(data=mon_avg_group,aes(x=month,y=avg,color=type))+
geom_line()

chi<-data$Chinese.1/data$Chinese.2
mal<-data$Malays.1/data$Malays.2
ind<-data$Indians.1/data$Indians.2
oth<-data$Other.Ethnic.Groups.1/data$Other.Ethnic.Groups.2
y_c<-c()
y_m<-c()
y_i<-c()
y_o<-c()
for(i in 1:57)
{
	y_c<-c(y_c,mean(chi[(i*12-11):(i*12)]))
	y_m<-c(y_m,mean(mal[(i*12-11):(i*12)]))
	y_i<-c(y_i,mean(ind[(i*12-11):(i*12)]))
	y_o<-c(y_o,mean(oth[(i*12-11):(i*12)]))
}


ycount_chi<-c()
ycount_mal<-c()
ycount_ind<-c()
ycount_oth<-c()
ycount_tot<-c()
for(i in 1:57)
{
	ycount_chi<-c(ycount_chi,sum(data$Chinese[(i*12-11):(i*12)]))	
	ycount_mal<-c(ycount_mal,sum(data$Malays[(i*12-11):(i*12)]))	
	ycount_ind<-c(ycount_ind,sum(data$Indians[(i*12-11):(i*12)]))	
	ycount_oth<-c(ycount_oth,sum(data$Other.Ethnic.Groups[(i*12-11):(i*12)]))	
	ycount_tot<-c(ycount_tot,sum(data$Total.Live.Births.By.Ethnic.Group[(i*12-11):(i*12)]))		
}

year<-data.frame(count=c(ycount_chi,ycount_mal,ycount_ind,ycount_oth,ycount_tot),year=rep(1960:2016,5),groups=as.factor(rep(c("Chinese","Malay","Indian","Other","Total"),each=57)))
ggplot(data=year,aes(x=year,y=count,group=groups,color=groups))+
geom_line()


peak_c<-c()
peak_m<-c()
peak_i<-c()
peak_o<-c()
for(i in 1:57)
{
	peak_c<-c(peak_c,which.max(data$Chinese[(i*12-11):(i*12)]/ndays[(i*12-11):(i*12)]))
	peak_m<-c(peak_m,which.max(data$Malays[(i*12-11):(i*12)]/ndays[(i*12-11):(i*12)]))
	peak_i<-c(peak_i,which.max(data$Indians[(i*12-11):(i*12)]/ndays[(i*12-11):(i*12)]))
	peak_o<-c(peak_o,which.max(data$Other.Ethnic.Groups[(i*12-11):(i*12)]/ndays[(i*12-11):(i*12)]))
}


bottom_c<-c()
bottom_m<-c()
bottom_i<-c()
bottom_o<-c()
for(i in 1:57)
{
	bottom_c<-c(bottom_c,which.min(data$Chinese[(i*12-11):(i*12)]/ndays[(i*12-11):(i*12)]))
	bottom_m<-c(bottom_m,which.min(data$Malays[(i*12-11):(i*12)]/ndays[(i*12-11):(i*12)]))
	bottom_i<-c(bottom_i,which.min(data$Indians[(i*12-11):(i*12)]/ndays[(i*12-11):(i*12)]))
	bottom_o<-c(bottom_o,which.min(data$Other.Ethnic.Groups[(i*12-11):(i*12)]/ndays[(i*12-11):(i*12)]))
}