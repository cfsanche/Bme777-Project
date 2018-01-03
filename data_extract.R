# WRITE THIS IN THE TERMINAL TO ACCESS FILE:
##
#setwd("/home/student1/cfsanch/bme777/project/")
#source("sample.R")
###
Sys.setenv(JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64/jre")
Sys.setenv(HADOOP_HOME="/usr/local/hadoop")
Sys.setenv(HADOOP_CMD="/usr/local/hadoop/bin/hadoop")
Sys.setenv(HADOOP_STREAMING="/usr/local/hadoop/share/hadoop/tools/lib/hadoop-streaming-2.8.1.jar")
#/usr/local/hadoop/share/hadoop/tools/lib/hadoop-streaming-2.8")

library(rmr2)
library(rhdfs)

#message('hello')

#setwd("/home/student1/cfsanche/bme777/project")

hdfs.init()
#cars.values<-to.dfs(cars)

#setwd("/home/student1/cfsanch/bme777/project/") # initialize with the path to your working directory.

#car.map.fn<-function(k,v){ #function will define functionality of the map command
#	p<-which(v[4]>200) #check 4th column to find all indexs that are greater than 200
#	keyval(v[p,],v[p,4]) #key= all p indexes and all the rows 
	#val= index values and only the 4th column info 
#}

#car.reduce.fn<-function(k,v){
#	keyval(k,sum(unlist(v)))
#}

#carfeat<-mapreduce(input=cars.values,
#	map=car.map.fn,
#	reduce=car.reduce.fn)


databme777<-read.csv("/home/courses/bme777/diabetic_data_V2.csv") # study the read.csv command parameters and read the dataset csv file.

head(databme777)

#hdfs.init()
databme777.values <- to.dfs(databme777)

# write your own map() and reduce() functions based on your assigned queries.
# find respiratory patients -> 460-519,786
databme777.map.fn <- function(k,v) {
	temp<-as.numeric(paste(v[,8]))
	temp2<-as.numeric(factor(v[,2]))
	code_ind<-which((((temp<=519)&(temp>=460))|temp==786)&temp2<=7) #class1 
	#code_ind<-which((((temp<=519)&(temp>=460))|temp==786)&temp2>=8) #CHANGE FOR CLASS 2 AND RE-RUN
	v[,8]=temp
	#data<-v[code_ind,]
	#p<-as.numeric(factor(data[,2]))
	#less<-which(p<=7)
	
	keyval(v[code_ind,2],v[code_ind,])
}

databme777.reduce.fn <- function(k,v) {
	#while hasnext(
keyval(k,v)
}

# study mapreduce function and pass appropriate inputs and ouputs.

dataex <- mapreduce(input= databme777.values,
                   map = databme777.map.fn,
                   reduce = databme777.reduce.fn)

# FILE FACTORS
# 1. Female = 1, Male = 2
# 2. Factor by age (0-10,10-20, etc.)
# 3. 

new_var<-from.dfs(dataex)

# write appropriate code to format the data matrix you want to write to a csv file.

# WRITE THIS IN THE TERMINAL TO STORE FILE:
##
#k<-as.data.fram(new_var[2])
#write.csv(k,'BME777_BigData_Extract_Class1.csv')
###
