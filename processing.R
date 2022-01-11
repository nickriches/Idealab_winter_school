
######
##Clean some non-believable subjects
length(mydata$id)
mydata<-mydata[!(mydata$age<16 & (mydata$education=="Graduate Degree" | mydata$education=="High School Degree (12-13 years)" | mydata$education=="Undergraduate Degree (3-5 years higher ed)" | mydata$education=="Some Undergrad (higher ed)" | mydata$education=="Some Graduate School")),]
mydata<-mydata[!(mydata$age<19 & (mydata$education=="Graduate Degree" | mydata$education=="Some Graduate School")),]
mydata<-mydata[mydata$age>=mydata$Eng_start,]
mydata<-mydata[mydata$age>=mydata$Eng_country_yrs | is.na(mydata$Eng_country_yrs),]
length(mydata$id)

quartz()
hist(mydata$age,breaks=c(0:100),main="",xlab="",ylab="",axes=FALSE,cex.lab=1.5,las=1,cex.axis=.9)
axis(1, at=seq(0,90,10),pos=0,cex.axis=1.3,label=c(seq(0,90,10)))
axis(1,label="age",at=40,pos=-2800,cex.axis=2.3,tick=FALSE)
axis(2,at=seq(0,35000,5000),label=c(seq(0,35000,5000)),pos=0,cex.axis=1.3)
axis(2,label="count",at=18000,pos=-8,cex.axis=2.3,tick=FALSE)
summaryBy(Eng_start~age,data=mydata,FUN=length)

mydata<-mydata[mydata$age>6 & mydata$age<90,]
length(mydata$id)

######
##need to define correct


natdial<-c("Australia","Canada","Ebonics","England","New Zealand","Ireland","Northern Ireland","Scotland","Singapore","South Africa","United States","Wales","India")

newcols<-c()
for (i in 1:8){
	newcols<-c(newcols,paste("q",i,sep=""))
}
for (i in 9:31){
	for (j in 1:4){
		newcols<-c(newcols,paste("q",i,"_",j,sep=""))
	}
}
for (i in 32:35){
	for (j in 1:8){
		newcols<-c(newcols,paste("q",i,"_",j,sep=""))
	}
}
classes<-summaryBy(q1~type,data=mydata[mydata$age>17 & mydata$age<71,],FUN=length,keep.names=TRUE)
classes<-classes[classes$type!="",]
classes<-unique(classes)
colnames(classes)<-c("type","N")
for (c in newcols){
	mydata[,c]<-as.numeric(mydata[,c])
	temp<-summaryBy(as.formula(paste(c,"~type",sep="")),data=mydata[mydata$age>17 & mydata$age<71,],FUN=mean,keep.names=TRUE)
	classes[,c]<-temp[c(2:length(temp$type)),2]
  print(paste(c,length(classes$type)))
}
natclasses<-classes[classes$type %in% natdial,]
qs<-data.frame(q=colnames(natclasses)[3:134],sd=0,use=0,correct=0)
for (c in 3:134){
  qs$sd[(c-2)]<-sd(natclasses[,c])
  if (max(natclasses[,c])<.25){
  	qs$use[(c-2)]<-1
  	qs$correct[(c-2)]<-0
  }
  if (min(natclasses[,c])>.75){
  	qs$use[(c-2)]<-1
  	qs$correct[(c-2)]<-1
  }
}
qs$use[qs$q=="q8" | qs$q=="q4"]<-0
qs$sd<-round(qs$sd,2)

correct<-mydata[,1:32]
for (c in qs$q[qs$use==1]){
	print(c)
	correct$temp<-0
	colnames(correct)[length(colnames(correct))]<-c
	correct[,c]<-as.numeric(qs$correct[qs$q==c]==mydata[,c])
}
correct$correct<-0
for (c in colnames(correct)[33:(length(colnames(correct))-1)]){
	correct$correct<-correct$correct+correct[,c]
}
correct$correct<-correct$correct/(length(colnames(correct)[33:(length(colnames(correct))-1)]))
nqs<-max(grep("q35_8",colnames(correct)))-min(grep("q1",colnames(correct)))+1
correct$elogit<-log((correct$correct*nqs+.5)/(nqs-correct$correct*nqs+.5))

correct$Eng_start<-as.numeric(as.character(correct$Eng_start))
correct$Eng_country_yrs<-as.numeric(as.character(correct$Eng_country_yrs))
correct$Lived_Eng_per<-correct$Eng_country_yrs/(correct$age-correct$Eng_start)
correct$Lived_Eng_per[!is.na(correct$Lived_Eng_per) & (correct$Lived_Eng_per<0 | correct$Lived_Eng_per>1.1)]<-NA

correct$natlangs[correct$natlangs==", English"]<-"English"
correct$Eng_little<-""
correct$natcon<-0
natcon<-c("Australia","Canada","United Kingdom","United States","New Zealand","Ireland (Republic of)","Singapore","South Africa","India")
for (n in natcon){
	correct$natcon[grep(n,correct$countries,ignore.case=TRUE)]<-1
}
correct$primeng<-0
correct$primeng[grep("English",correct$primelangs)]<-1
correct$natlangs[correct$natlangs=="NULL" & correct$natcon==1 & correct$primeng==1]<-"English"
correct$nat_Eng<-0
correct$nat_Eng[grep("English",correct$natlangs)]<-1
correct$Eng_little[correct$nat_Eng==1 & correct$Eng_start==0 & correct$natlangs=="English"]<-"monoeng"
correct$Eng_little[correct$nat_Eng==1 & correct$Eng_start==0 & correct$natlangs!="English"]<-"bileng"
correct$Eng_little[correct$Lived_Eng_per>.9 & !(is.na(correct$Lived_Eng_per)) & correct$Eng_start>0]<-"lot"
correct$Eng_little[correct$Lived_Eng_per<.1 & correct$Eng_country_yrs<2 & !(is.na(correct$Lived_Eng_per)) & correct$Eng_start>0]<-"little"

correct$edtype<-0
correct$edtype[correct$education=="Didn't Finish High School (less than 13 years ed)" | correct$education=="Haven't Finished High School (less than 13 years ed)" | correct$education=="High School Degree (12-13 years)"]<-1
correct$edtype[correct$education=="Some Undergrad (higher ed)" | correct$education=="Undergraduate Degree (3-5 years higher ed)"]<-2
correct$edtype[correct$education=="Some Graduate School" | correct$education=="Graduate Degree"]<-3
correct$edtype[correct$edtype==0]<-NA

write.csv(correct,file="data_for_analysis.csv")

