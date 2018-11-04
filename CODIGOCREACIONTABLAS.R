library(class) # knn()
library(e1071)
library(gmodels) # CrossTable() 
library(tidyr)

setwd("/Users/FcoMerino/Documents/UNIVERSIDAD/inteligencia\ de\ negocios")
emails = read.table("features-train.txt")
emails_class = read.table("train-labels.txt")
emails2 = read.table("features-test.txt")
emails2_class = read.table("test-labels.txt")
#De querer agregar una columna id a emails_class
#emails_class$ID <- seq.int(nrow(emails_class))
#Reshape dataframe
new_emails<-spread(emails, key = V2, value = V3)
new_emails2<-spread(emails2, key = V2, value = V3)
#Llenamos de 0 los NA
new_emails[is.na(new_emails)]<-0
new_emails2[is.na(new_emails2)]<-0
#Agrego columna clase a datframe principal
#new_emails["Class"]<-emails_class$V1
#new_emails2["Class"]<-emails2_class$V1
train_emails<-new_emails
test_emails<-new_emails2

#plantilla para test
plantilla_test<-matrix(data= NA, nrow = 260, ncol=2867, byrow=FALSE)
plantilla_test<-as.data.frame(plantilla_test)
plantilla_test[is.na(plantilla_test)]<-0
plantilla_test<-cbind(X1=1:260, plantilla_test)
colnames(plantilla_test) <- c("V1", c(1:2866))

#plantilla para train
plantilla_train<-matrix(data= NA, nrow = 700, ncol=2867, byrow=FALSE)
plantilla_train<-as.data.frame(plantilla_train)
plantilla_train[is.na(plantilla_train)]<-0
plantilla_train<-cbind(X1=1:700, plantilla_train)
colnames(plantilla_test) <- c("V1", c(1:2866))

#ciclo para test
for(i in 1:260){
  for(j in 1:2740){
    if ((test_emails[i,j]!=0) && plantilla_test$V1[i]==test_emails$V1[i]){
      plantilla_test[i,j] <- test_emails[i,j]
    } else {
      plantilla_test[i,j] <- 0  
    }
  } 
  cat("loop: " , i, "\n")
}


#ciclo para train 
for(i in 1:700){
  for(j in 1:2862){
    if ((train_emails[i,j]!=0) && plantilla_train$V1[i]==train_emails$V1[i]){
      plantilla_train[i,j] <- train_emails[i,j]
    } else {
      plantilla_train[i,j] <- 0  
    }
  }
  #CAT NOS MUESTRA EL AVANCE EN LA CONSOLA
  cat("loop: " , i, "\n")
}

#Agrego columna clase a datframe final
plantilla_train["Class"]<-emails_class$V1
plantilla_test["Class"]<-emails2_class$V1

#GUARDAMOS
test_knn<-plantilla_test
train_knn<-plantilla_train
write.table(test_knn, "/Users/FcoMerino/Documents/UNIVERSIDAD/inteligencia\ de\ negocios/test_knn.txt", sep="\t")
write.table(train_knn, "/Users/FcoMerino/Documents/UNIVERSIDAD/inteligencia\ de\ negocios/train_knn.txt", sep="\t")
