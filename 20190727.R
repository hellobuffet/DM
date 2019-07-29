set.seed(123)
x<-rnorm(100000,mean=20,sd=5)
#常態分布
hist(x)
summary(x)

x<-runif(60000,1,6)
#均勻分布
hist(x)
summary(x)

data("iris")
shapiro.test(iris$Petal.Length)
hist(iris$Petal.Length,prob=T)

curve(dnorm(x,mean(iris$Petal.Length),sd(iris$Petal.Length)),col="red",add=T)

shapiro.test(iris$Sepal.Width)
hist(iris$Sepal.Width.prob=T)
curve(dnorm(x,mean(iris$Sepal.Width),sd(iris$Sepal.Width)),col="red",add=T)


install.packages('C50')
library(C50)
?(churn)
data(churn)
str(churnTrain)
t.test(churnTrain$total_day_calls,mu=160,alternative = "two.sided")

var.test(churnTrain$total_day_calls[churnTrain$area_code=="area_code_408"], churnTrain$total_day_calls[churnTrain$area_code == "area_code_415"])
help(t.test)

Crd_amt_before <- rnorm(10000,mean=4032,sd=570)
Crd_amt_after <- rnorm(10000,mean=5661,sd=690)
t.test(Crd_amt_before,Crd_amt_after,mu=0,paired = T,var.equal = F)

#P197-卡方獨立性檢定
dt <-matrix(c(38,45,100,77),ncol=2)
chisq.test(dt)

#P197-相關係數檢定
cor(churnTrain$total_night_calls,churnTrain$total_day_calls)
cor.test(churnTrain$total_night_calls,churnTrain$total_day_calls)


#P198-單因子變異數檢定
df<-data.frame(
  group=c(rep(1,20),rep(2,20),rep(3,20)),
  GoInternet = floor(runif(n=60,min=1,max=10))
)
lm_df<-lm(GoInternet~group,data=df)
#lm()簡單線性回歸
anova(lm_df)

lm_df<-lm(group~GoInternet,data=df)
anova(lm_df)

#P205-實作--藥劑量與感冒痊癒天數的簡單回歸模型
x<-c(3,3,4,3,6,8,8,9)#藥劑量
y<-c(22,25,18,20,16,9,12,5)#感冒痊癒天數
New_x<-data.frame(x=5)
#預測X=5時的痊癒天數

#建立一個回歸模型
Train <-data.frame(x=x,y=y)
lmTrain<-lm(formula=y~x,data=Train)
predicted <- predict(lmTrain,newdata=New_x)

#模型摘要
summary(lmTrain)

plot(y~x,family="stheiti")
points(x=New_x,y=predicted,col="green",cex=2,pch=16)
abline(reg = lmTrain$coefficients,col="red",lwd=1)


#P207

x1<-c(3,3,4,3,6,8,8,9)
x2<-c(3,1,6,4,9,10,8,11)
y<-c(22,25,18,20,16,9,12,5)

New_x1<-5
New_x2<-7
New_data<-data.frame(x1=5,x2=7)

Train<-data.frame(x1=x1,x2=x2,y=y)
lmTrain<-lm(formula=y~.,data=Train)

predicted<-predict(lmTrain,newdata=New_data)
predicted

summary(lmTrain)

#P209
getwd()
setwd("E:/DM")
babyData=read.table("./DM/babies.csv",header=T,sep=",")


babyData=na.exclude(babyData)

n=0.3*nrow(babyData)
test.index=sample(1:nrow(babyData),n)
Train = babyData[-test.index,]
Test=babyData[test.index,]

par(mfrow=c(1,3))
hist(babyData$bwt)
hist(Train$bwt)
hist(Test$bwt)

install.packages("repart")
library(rpart)
baby.tree=rpart(bwt~.,data=Train)
baby.tree
plot(baby.tree)
text(baby.tree,cex=.8)



#P203-主成分分析-變數降維
install.packages("C50")
library("C50")
data(churn)
data<-churnTrain[,c(-1,-3,-4,-5,-20)]
pca_Traindt <-princomp(data,cor=T)
summary(pca_Traindt)


screeplot(pca_Traindt,type="lines")
p<-predict(pca_Traindt)
head(p,5)
p[,c(1:7)]


#P215
install.packages("class")
library(class)
data(iris)
set.seed(123)
n <-nrow(iris)
train_idx <-sample(seq_len(n),size=round(0.7*n))
traindata<-iris[train_idx,]
testdata<-iris[-train_idx,]
train_y <-traindata[,5]
test_y <-testdata[,5]
k_set <-as.integer(sqrt(n))
pred<-knn(train=traindata[-5],test=testdata[-5],cl=train_y,k=k_set)
message("精準度：",sum(diag(table(test_y,pred)))/sum(table(test_y,pred))*100,"%")
