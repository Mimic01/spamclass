library(class) # knn()
library(e1071)
library(gmodels) # CrossTable() 

setwd("C:\\Users\\Alex\\Documents\\UAI\\BI\\ACT3_BI")
emails = read.table("features-train.txt")
emails_class = read.table("train-labels.txt")
#De querer agregar una columna id a emails_class
#emails_class$ID <- seq.int(nrow(emails_class))
#Reshape dataframe
library(tidyr)
new_emails<-spread(emails, key = V2, value = V3)
#Llenamos de 0 los NA
new_emails[is.na(new_emails)]<-0
#Agrego columna clase a datframe principal
new_emails["Class"]<-emails_class$V1
#Check si columna Class existe
if("Class" %in% colnames(new_emails)){
       cat("Yep, it's in there!\n");
        }

sub<-sample(nrow(new_emails),floor(nrow(new_emails)*0.7))
train_emails<-new_emails[sub,] # 70% Train Set
test_emails<-new_emails[-sub,] # 30% Test Set

#knn K=1
emails_knn1<-knn(train_emails[,-2863], test_emails[,-2863],as.factor(train_emails[,2863]),k=1)
table(test_emails[,2863],emails_knn1)

#knn K=6
emails_knn1<-knn(train_emails[,-2863], test_emails[,-2863],as.factor(train_emails[,2863]),k=6)
table(test_emails[,2863],emails_knn1)

#knn K=9
emails_knn1<-knn(train_emails[,-2863], test_emails[,-2863],as.factor(train_emails[,2863]),k=9)
table(test_emails[,2863],emails_knn1)

#knn K=15
emails_knn1<-knn(train_emails[,-2863], test_emails[,-2863],as.factor(train_emails[,2863]),k=15)
table(test_emails[,2863],emails_knn1)

#knn K=20
emails_knn1<-knn(train_emails[,-2863], test_emails[,-2863],as.factor(train_emails[,2863]),k=20)
table(test_emails[,2863],emails_knn1)