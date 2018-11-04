library(class) 
library(e1071)
library(gmodels)
library(tidyr)

setwd("C:\\Users\\Alex\\Documents\\UAI\\BI\\ACT3_BI\\Tarea3")
train_knn = read.table("train_knn.txt")
test_knn = read.table("test_knn.txt")
#Sampleo de Train_KNN con 260 observaciones
y<-train_knn[sample(nrow(train_knn), 260), ]


#KNN=1
emails_knn1<-knn(train_knn[,-c(1,2869)], test_knn[,-c(1,2869)],as.factor(train_knn[,2869]),k=1)
table(test_knn[,2869],emails_knn1)
#KNN=2
emails_knn1<-knn(train_knn[,-c(1,2869)], test_knn[,-c(1,2869)],as.factor(train_knn[,2869]),k=2)
table(test_knn[,2869],emails_knn1)
#KNN=3
emails_knn1<-knn(train_knn[,-c(1,2869)], test_knn[,-c(1,2869)],as.factor(train_knn[,2869]),k=3)
table(test_knn[,2869],emails_knn1)
#KNN=4
emails_knn1<-knn(train_knn[,-c(1,2869)], test_knn[,-c(1,2869)],as.factor(train_knn[,2869]),k=4)
table(test_knn[,2869],emails_knn1)
#KNN=5
emails_knn1<-knn(train_knn[,-c(1,2869)], test_knn[,-c(1,2869)],as.factor(train_knn[,2869]),k=5)
table(test_knn[,2869],emails_knn1)
#KNN=6
emails_knn1<-knn(train_knn[,-c(1,2869)], test_knn[,-c(1,2869)],as.factor(train_knn[,2869]),k=6)
table(test_knn[,2869],emails_knn1)
#KNN=7
emails_knn1<-knn(train_knn[,-c(1,2869)], test_knn[,-c(1,2869)],as.factor(train_knn[,2869]),k=7)
table(test_knn[,2869],emails_knn1)
#KNN=8
emails_knn1<-knn(train_knn[,-c(1,2869)], test_knn[,-c(1,2869)],as.factor(train_knn[,2869]),k=8)
table(test_knn[,2869],emails_knn1)
#KNN=9
emails_knn1<-knn(train_knn[,-c(1,2869)], test_knn[,-c(1,2869)],as.factor(train_knn[,2869]),k=9)
table(test_knn[,2869],emails_knn1)
#KNN=10
emails_knn1<-knn(train_knn[,-c(1,2869)], test_knn[,-c(1,2869)],as.factor(train_knn[,2869]),k=10)
table(test_knn[,2869],emails_knn1)
#KNN=11
emails_knn1<-knn(train_knn[,-c(1,2869)], test_knn[,-c(1,2869)],as.factor(train_knn[,2869]),k=11)
table(test_knn[,2869],emails_knn1)
#KNN=12
emails_knn1<-knn(train_knn[,-c(1,2869)], test_knn[,-c(1,2869)],as.factor(train_knn[,2869]),k=12)
table(test_knn[,2869],emails_knn1)
#KNN=13
emails_knn1<-knn(train_knn[,-c(1,2869)], test_knn[,-c(1,2869)],as.factor(train_knn[,2869]),k=13)
table(test_knn[,2869],emails_knn1)
#KNN=14
emails_knn1<-knn(train_knn[,-c(1,2869)], test_knn[,-c(1,2869)],as.factor(train_knn[,2869]),k=14)
table(test_knn[,2869],emails_knn1)
#KNN=15
emails_knn1<-knn(train_knn[,-c(1,2869)], test_knn[,-c(1,2869)],as.factor(train_knn[,2869]),k=15)
table(test_knn[,2869],emails_knn1)
#KNN=16
emails_knn1<-knn(train_knn[,-c(1,2869)], test_knn[,-c(1,2869)],as.factor(train_knn[,2869]),k=16)
table(test_knn[,2869],emails_knn1)
#KNN=17
emails_knn1<-knn(train_knn[,-c(1,2869)], test_knn[,-c(1,2869)],as.factor(train_knn[,2869]),k=17)
table(test_knn[,2869],emails_knn1)
#KNN=18
emails_knn1<-knn(train_knn[,-c(1,2869)], test_knn[,-c(1,2869)],as.factor(train_knn[,2869]),k=18)
table(test_knn[,2869],emails_knn1)
#KNN=19
emails_knn1<-knn(train_knn[,-c(1,2869)], test_knn[,-c(1,2869)],as.factor(train_knn[,2869]),k=19)
table(test_knn[,2869],emails_knn1)
#KNN=20
emails_knn1<-knn(train_knn[,-c(1,2869)], test_knn[,-c(1,2869)],as.factor(train_knn[,2869]),k=20)
table(test_knn[,2869],emails_knn1)


#Naive Bayes classification without data discretization 
bayes <- naiveBayes(Class~.,data=train_knn[,-1])
prediccion <- predict(bayes,test_knn[,-c(1,2869)],type="raw")
pred1 <- max.col(prediccion)
table(test_knn$Class,pred1)
#Naive Bayes classification without data discretization (LaPlace)
bayes <- naiveBayes(Class~.,data=train_knn[,-1], Laplace = 1)
prediccion <- predict(bayes,test_knn[,-c(1,2869)],type="raw")
pred1 <- max.col(prediccion)
table(test_knn$Class,pred1)


#for para cada variable, crea el vector ¨r¨
classvector<-as.vector(train_knn$X1)
r=c()
for(i in 1:2867){
  bayesl <- naiveBayes(Class~.,data=train_knn[,(i,2869)] , Laplace = 1)
  bayesl <- naiveBayes(Class~.,data=train_knn[,(classvector[i])] , Laplace = 1)
  prediccionl <- predict(bayesl,test_knn[,(i,-1,-2869)],type="raw")
  pred1l <- max.col(prediccionl)
  z<-table(test_knn$Class,pred1l)
  #La siguiente linea calcula Accuaracy
  x[i]=((z[1,1]+z[2,2])/(z[1,2]+z[2,1]+z[1,1]+z[2,2]))
  cat("loop: " , i, "\n")
}

#una vez finalizado el ciclo, nos quedamos con los valores mas altos
tail(sort(r),6)