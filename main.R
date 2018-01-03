# NOTES:
# gender: female=1 male=2 other=3
# age: 10 levels-> [0-10]-1, [10,20]-2
# admission_type_id-> num
# discharge_disposition_id-> num
# time_in_hospital-> num
# num_lab_procedures-> num
# num_medications-> num
# diag_1-> num
# AlCresult-> 1= >7 2= >8 3= None 4= Norm
# readmitted-> 1= <30 2= >30 3= NO

# any necessary libraries 
library(ggfortify)
library(nnet)
require(MASS)
library(caTools)
library(scatterplot3d)
require(class)
library(e1071)
#import datasets for class 1 and 2 
datac1<-read.csv('C:/Users/Camila/Documents/R/project/BME777Data_Class1.csv',header=TRUE,sep=",")
datac2<-read.csv('C:/Users/Camila/Documents/R/project/BME777Data_Class2.csv',head=TRUE)

#delete first column and diagnosis number- not useful 
drop<-c(names(datac1)[1],names(datac1)[3],names(datac1)[9])
datac1<-datac1[,!(names(datac1) %in% drop)]
datac2<-datac2[,!(names(datac2) %in% drop)]

# useful dimensions
ro1<-nrow(datac1)
ro2<-nrow(datac2)
numfeat<-ncol(datac1)
num_algo<-4
boot_it<-20

#numeric data frame 
feature_factors<-c(1,7,8)
ids<-matrix(list(),1,length(feature_factors))
k<-1
for (i in feature_factors){
  ids[[k]]<-levels(datac1[,i])
  datac1[,i]<-as.numeric(as.factor(datac1[,i]))
  datac2[,i]<-as.numeric(as.factor(datac2[,i]))
  k<-k+1
}

#create class labels 
datac1[,"class_labels"]<-rep(1,ro1)
datac2[,"class_labels"]<-rep(2,ro2)

dataset<-rbind(datac1,datac2)

# find unknowns
num_samples<-nrow(dataset)
unknowns1<-length(which(dataset[,2]==5|dataset[,2]==6|dataset[,2]==8))
missing_ad<-unknowns1/num_samples*100
if (missing_ad>=50) {
  drop<-c(names(dataset)[2])
  dataset<-dataset[,!(names(dataset) %in% drop)]
  numfeat=numfeat-1;
}

unknowns2<-length(which(dataset[,3]==25|dataset[,3]==26|dataset[,3]==18)) # HARD CODE
missing_dis<-unknowns2/num_samples*100
if (missing_dis>=50){
  drop<-c(names(dataset)[2])
  dataset<-dataset[,!names(dataset) %in% drop]
  numfeat=numfeat-1;
}

df<-dataset[seq(1,numfeat)]
#plot pca is two dimensions
autoplot(prcomp(df),data=dataset,colour='class_labels')

df.class<-dataset[,numfeat+1]
df.pca<-prcomp(df)
summary(df.pca)
# choose 3 features that have 99.773% of the variance
pca_weights<-df.pca$rotation[,c(1,2,3)]
df.mat<-data.matrix(df)
#reduce dimensions
df.reduced<-df.mat%*%pca_weights

# TRAIN & TEST 60:40
df.reduced<-cbind(as.data.frame(df.class),as.data.frame(df.reduced))
accuracies<-matrix(data=NA,num_algo,boot_it+1)
rownames(accuracies)<-c("Bayes","LDF","Neural Net","KNN")
colnames(accuracies)[boot_it+1]<-"Average"

# boot strap validation 20 times
for (i in 1:boot_it) {
  
  set.seed(i)
  boot<-sample(1:num_samples,num_samples,replace=TRUE)
  boot_data<-df.reduced[boot,]
  split <- sample.split(df.class, SplitRatio = 0.60)
  train <- subset(boot_data, split == TRUE)
  test <- subset(boot_data, split == FALSE)

  # BAYESIAN
  
  bayesian<-naiveBayes(as.factor(train[,1])~.,data=train)
  pred.bayes <- predict(bayesian, test[,2:4])
  accuracy.bayes<-length(which(as.numeric(as.factor(pred.bayes))==test[,1]))/length(test[,1])*100
  
  # LDF 
  
  ldf<-lda(train[,1]~.,data=train[,2:4])
  pred.ldf <- predict(ldf,test[,2:4])
  accuracy.ldf<-length(which(pred.ldf$class==test[,1]))/length(test[,1])*100
  # avg 57% accuracy
  
  # NN
  
  features <- names(train)
  # Response~Variable_1+Variable_2
  f <- paste(features[2:4],collapse=' + ')
  f <- paste('df.class~',f)
  f <- as.formula(f)
  
  nn <- nnet(f,data=train,size=3)
  pred.nn<-predict(nn,test[,2:4])
  accuracy.nn<-length(which(pred.nn==test[,1]))/length(test[,1])*100
  # 52.886% accuracy
  
  # KNN
  
  # classify using resulting centroids
  pred.knn<-knn(train[,2:4],test[,2:4],train[,1],k=2)
  pred.knn<-as.numeric(factor(pred.knn))
  accuracy.knn<-length(which(pred.knn==test[,1]))/length(test[,1])*100
  # 53% accuracy
  accuracies[,i]<-c(accuracy.bayes,accuracy.ldf,accuracy.nn,accuracy.knn)
}

# average final accuracies 
for (i in 1:num_algo) {
  accuracies[i,boot_it+1]<-mean(accuracies[i,1:boot_it])
}
  
################### BAYES Class Conditional Probability Graphs  #########################
# mean11<-mean(df.reduced[seq(1,ro1),1])
# mean12<-mean(df.reduced[seq(1,ro1),2])
# mean13<-mean(df.reduced[seq(1,ro1),3])
# 
# std11<-sd(df.reduced[seq(1,ro1),1])
# std12<-sd(df.reduced[seq(1,ro1),2])
# std13<-sd(df.reduced[seq(1,ro1),3])
# 
# mean21<-mean(df.reduced[seq(ro1+1,ro1+ro2),1])
# mean22<-mean(df.reduced[seq(ro1+1,ro1+ro2),2])
# mean23<-mean(df.reduced[seq(ro1+1,ro1+ro2),3])
# 
# std21<-sd(df.reduced[seq(ro1+1,ro1+ro2),1])
# std22<-sd(df.reduced[seq(ro1+1,ro1+ro2),2])
# std23<-sd(df.reduced[seq(ro1+1,ro1+ro2),3])
# 
# #class conditional probabilities
# 
# # feature 1
# x11<-seq(-4,4,length=100)*std11+mean11
# x21<-seq(-4,4,length=100)*std21+mean21
# dx11<-dnorm(x11,mean11,std11)
# dx21<-dnorm(x21,mean21,std21)
# 
# plot(x11,dx11,col="red",main="Feature 1 Class Conditional Probability",ylim=range(c(dx11,dx21)))
# par(new=TRUE)
# plot(x21,dx21,col="blue",ylim=range(c(dx11,dx21)),axes=FALSE)
# legend(0, 0.015, legend=c("Class 1", "Class 2"),
#        col=c("red", "blue"),lty=1:2, cex=0.8)
# # feature 2
# x12<-seq(-4,4,length=100)*std12+mean12
# x22<-seq(-4,4,length=100)*std22+mean22
# dx12<-dnorm(x12,mean12,std12)
# dx22<-dnorm(x22,mean22,std22)
# 
# plot(x12,dx12,col="red",main="Feature 2 Class Conditional Probability",ylim=range(c(dx12,dx22)))
# par(new=TRUE)
# plot(x22,dx22,col="blue",ylim=range(c(dx12,dx22)),axes=FALSE)
# legend(20, 0.04, legend=c("Class 1", "Class 2"),
#        col=c("red", "blue"),lty=1:2, cex=0.8)
# 
# # feature 3
# x13<-seq(-4,4,length=100)*std13+mean13
# x23<-seq(-4,4,length=100)*std23+mean23
# dx13<-dnorm(x13,mean13,std13)
# dx23<-dnorm(x23,mean23,std23)
# 
# plot(x13,dx13,col="red",main="Feature 3 Class Conditional Probability",ylim=range(c(dx13,dx23)))
# par(new=TRUE)
# plot(x23,dx23,col="blue",ylim=range(c(dx13,dx23)),axes=FALSE)
# legend(4, 0.15, legend=c("Class 1", "Class 2"),
#        col=c("red", "blue"),lty=1:2, cex=0.8)
