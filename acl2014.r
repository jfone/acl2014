acl2014<-read.csv(file="/media/jfone/disk/project/acl2014/acl2014e.csv",header=T)
acl2014
library(RMySQL)
attach(acl2014)
class(Ascore)
acl2014$TeamA<-as.character(TeamA)
acl2014$TeamB<-as.character(TeamB)
table1<-table(acl2014$TeamA)
n1<-length(table1)
n2<-2*(n1-1)

ST<-matrix(rep(0,n1*n2),nrow=n1,ncol=n2)
ST<-data.frame(ST)
rownames(ST)<-names(table1)

k<-c(1:12)
for (i in c(1:12) ){k[i]<-(acl2014$Ascore[i]-acl2014$Bscore[i])}
ST2<-matrix(rep(0,48),12,4)
colnames(ST2)<-c("P0","P1","P1.0","P3")


for (i in 1:12) 
  {
  if (acl2014$Ascore[i] > acl2014$Bscore[i])
     {ST2[i,4]<-acl2014$TeamA[i]
     ST2[i,1]<-acl2014$TeamB[i]}
    else if (acl2014$Ascore[i] == acl2014$Bscore[i])
            {ST2[i,2]<-acl2014$TeamA[i]
            ST2[i,3]<-acl2014$TeamB[i]}
            else 
              {ST2[i,4]<-acl2014$TeamB[i]
              ST2[i,1]<-acl2014$TeamA[i]}
}

for (r in 1:length(table1))
{
  kk<-ST2==rownames(ST)[r]
  kk[,1]<-kk[,1]*0
  kk[,2]<-kk[,2]*1
  kk[,3]<-kk[,3]*1
  kk[,4]<-kk[,4]*3
  for (i in 1:6) 
    { ST[r,i]<-sum(kk[(2*i-1):(2*i),])}
}

#show the points table

ST3<-ST
for (m in 2:dim(ST3)[2]){
  print (m)
  ST3[,m]<-apply(ST[,1:m],1,sum)}

plot(1:6,ST3[1,],col=1,type="b",ylim=c(1,18),ylab="Points",main="group")
lines(1:6,ST3[2,],col=2,type="b")
lines(1:6,ST3[3,],col=3,type="b")
lines(1:6,ST3[4,],col=4,type="b")
legend("topleft",col=1:4,lty=1,c("Buriram","COsaka","Pohang","ShanDong"))

#ST4 is rank

#connect to mysql, show the point table.

mydb = dbConnect(MySQL(), user='jfone', password='667686', dbname='2014acl', host='192.168.1.8')
dbListTables(mydb)
dbWriteTable(mydb,name='GroupE',value=ST3)
dbDisconnect(mydb)
