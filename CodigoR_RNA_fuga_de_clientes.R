library(readxl)
library(dplyr)
library(ggplot2) 
library(fastDummies)
library(corrplot)
library(plotrix)
library(factoextra)
library(tidyverse)
library(caret)
options(scipen = 999)
#https://www.kaggle.com/datasets/shantanudhakadd/bank-customer-churn-prediction
#credit score=puntaje crédito
#geography=país donde vive 
#tenure=cuantos años tiene el cliente en el banco
#balance=saldo promedio 
#num products=numero de productos bancarios del cliente
data<-read.csv("C:/Users/ricar/Desktop/SEMESTRE 7/Machine Learning/3. Talleres/Taller 3/churn.csv",sep=",")
head(data)
dim(data)
names(data)
df<-rename(data, RowNumber = "ï..RowNumber")
names(df)
sum(is.na(df)) #no hay valores nan
#Las variables surname=apellido, ID del cliente y número de fila no 
#tienen un sentido práctico para aplicar modelos de clasificación
#por tanto, se eliminan:
df<-df[-c(1,2,3)]
names(df)
dim(df)
#se cambian palabras por números
df$Geography[df$Geography=="France"]<-0
df$Geography[df$Geography=="Germany"]<-1
df$Geography[df$Geography=="Spain"]<-2
df$Gender[df$Gender=="Female"]<-0
df$Gender[df$Gender=="Male"]<-1
df[c("Geography","Gender")]

lab1 <- paste0(round(c(sum(df["Gender"]==0),sum(df["Gender"]==1))/sum(c(sum(df["Gender"]==0),
                                                                   sum(df["Gender"]==1))) * 100, 2),"%")
lab2 <- paste0(round(c(sum(df["HasCrCard"]==0),sum(df["HasCrCard"]==1))/sum(c(sum(df["HasCrCard"]==0),
                                                                        sum(df["HasCrCard"]==1))) * 100, 2),"%")
lab3 <- paste0(round(c(sum(df["IsActiveMember"]==0),sum(df["IsActiveMember"]==1))/sum(c(sum(df["IsActiveMember"]==0),
                                                                        sum(df["IsActiveMember"]==1))) * 100, 2),"%")
lab4 <- paste0(round(c(sum(df["Exited"]==0),sum(df["Exited"]==1))/sum(c(sum(df["Exited"]==0),
                                                                        sum(df["Exited"]==1))) * 100, 2),"%")
pie3D(c(sum(df["Gender"]==0),sum(df["Gender"]==1)),theta=1,labels=lab1,explode=0.2,col=c("red","blue"),
      main = "Proporción del sexo")
legend("topright", c("Femenino","Masculino"), cex = 0.8,fill = c("red","blue"))

pie3D(c(sum(df["HasCrCard"]==0),sum(df["HasCrCard"]==1)),theta=1,labels=lab2,explode=0.1,col=c("green","orange"),
      main = "Proporción de clientes con tarjeta de crédito")
legend("topright", c("No tiene","Tiene"), cex = 0.8,fill = c("green","orange")) 

pie3D(c(sum(df["IsActiveMember"]==0),sum(df["IsActiveMember"]==1)),theta=1,labels=lab3,explode=0.5,col=c("purple","yellow"),
      main = "Proporción de miembros activos")
legend("topright", c("Miembro activo","Miembro no activo"), cex = 0.8,fill = c("purple","yellow"))  

pie3D(c(sum(df["Exited"]==0),sum(df["Exited"]==1)),theta=1,labels=lab4,explode=0.1,col=c("brown","cyan"),
      main = "Proporción de abandono de la empresa")
legend("topright", c("Abandonó","Sigue"), cex = 0.8,fill = c("brown","cyan"))

table(df$Geography)
table(df$Tenure)
table(df$NumOfProducts)
par(mfrow = c(2,2))
barplot(c(5014,2509,2477),
        main = "Frecuencias de la nacionalidad del cliente",
        xlab = "Nacionalidad",
        ylab = "Frecuencia",
        names.arg = c("Francia","Alemania","España"),
        col = c("red","blue","yellow"))
barplot(c(5084,4590,266,60),
        main = "Frecuencias de cantidad de productos del cliente",
        xlab = "Cantidad de productos",
        ylab = "Frecuencia",
        names.arg=c(1,2,3,4),
        col=c("purple","orange","green","cyan"))
barplot(c(413,1035,1048,1009,989,1012,967,1028,1025,984,490),
        main = "Frecuencias de los años de antigüedad del cliente",
        xlab = "Años de antiguedad",
        ylab = "Frecuencia",
        names.arg=c(0,1,2,3,4,5,6,7,8,9,10))


par(mfrow=c(2,1))
hist(df$Balance, main="Histograma de balance del cliente",
     xlab="Balance",ylab="Frecuencia",col="red")
boxplot(df$Balance, main ="Box-Plot de balance del cliente",
        horizontal = TRUE,xlab="Balance",col="red")

par(mfrow=c(2,1))
hist(df$EstimatedSalary,main="Histograma del salario estimado del cliente",
     xlab="Salario estimado",ylab="Frecuencia",col="green")
boxplot(df$EstimatedSalary, main ="Box-Plot de salario estimado del cliente",
        horizontal = TRUE,xlab="Salario estimado",col="green")

par(mfrow = c(2,1))
hist(df$CreditScore,main="Histograma del puntaje de crédito del cliente",
     xlab="Puntaje de crédito",ylab="Frecuencia",col="orange")
boxplot(df$CreditScore, main ="Box-Plot de puntaje de crédito del cliente",
        horizontal = TRUE,xlab="Puntaje de crédito",col="orange")

#reemplazo de outliers con R
df$CreditScore <- as.numeric(df$CreditScore)
outliersReplace <- function(data, lowLimit, highLimit){     ##FUNCIÓN PARA PODER REEMPLAZAR VALORES OUTLIERS 
  data[data < lowLimit] <- 650.5
  data[data > highLimit] <- 650.5
  data}
df["CreditScore"]<-outliersReplace(df["CreditScore"],383, 1000000000)

par(mfrow = c(2,1))
hist(df$CreditScore,main="Histograma del puntaje de crédito del cliente",
     xlab="Puntaje de crédito",ylab="Frecuencia",col="orange")
boxplot(df$CreditScore, main ="Box-Plot de puntaje de crédito del cliente",
        horizontal = TRUE,xlab="Puntaje de crédito",col="orange")

hist(df$Age,main="Histograma de la edad del cliente",
     xlab="Edad",ylab="Frecuencia",col="blue")

cor<-cor(x=df[c(1,4,6,10)],method="pearson") #Correlación
corrplot(cor,method="circle")

chi2 = chisq.test(df[c(7,8)], correct=F)
cat("El p-value es: ", chi2$p.value," con un estadístico de prueba de: ", chi2$statistic)
######### ESCALAMIENTO DE VARIABLES##############
df %>% sapply(class) %>% as.data.frame() %>% rename(tipo = 1) #tipos de variables
df$Geography <- as.numeric(df$Geography)
df$Gender <-as.numeric(df$Gender)
scale<-scale(df[c(1,4,5,6,10)]) #todas las variables numéricas
apply(X=scale,MARGIN=2,FUN=mean)
apply(X=scale,MARGIN=2,FUN=var)

binarias<-df[c(3,8,9,11)] #variables binarias

dummys<-dummy_cols(df,  select_columns = c("Geography", "NumOfProducts"))
names(dummys[12:18])
df2<-cbind(scale,binarias,dummys[12:18])
df2
#############   train y test #############
train <- createDataPartition(y = df2$Exited, p = 0.8, list = FALSE, times = 1)
df_train <- df2[train, ] 
df_test  <- df2[-train, ]

prop.table(table(df_train$Exited))
prop.table(table(df_test$Exited))

################ MODELOS con SOFTPLUS##################
library(neuralnet)
#función de activación
softplus <- function(x) { log(1 + exp(x)) }
#modelo 1 capa 1 neurona
nn1=neuralnet(Exited~CreditScore+Age+Tenure+Balance+EstimatedSalary+HasCrCard+
                IsActiveMember+Geography_0+Geography_1+Geography_2+
                NumOfProducts_1+NumOfProducts_2+NumOfProducts_3+NumOfProducts_4,
              data=df_train,hidden=1,act.fct=softplus,linear.output=FALSE)
plot(nn1,rep="best")

predict1=compute(nn1,df_test)
prob1<-predict1$net.result
pred1 <- ifelse(prob1>0.5, 1, 0)
tabla1<-table(true=df_test[,"Exited"],pred1) #matriz confusión
tabla1
acc1<-(tabla1[1,1]+tabla1[2,2])/(tabla1[1,1]+tabla1[2,2]+tabla1[1,2]+tabla1[2,1])
acc1 #0.849
ECM1<-mean((df_test$Exited - pred1)^2)
ECM1 #0.151

#################modelo 2, neurona=2
nn2=neuralnet(Exited~CreditScore+Age+Tenure+Balance+EstimatedSalary+HasCrCard+
                IsActiveMember+Geography_0+Geography_1+Geography_2+
                NumOfProducts_1+NumOfProducts_2+NumOfProducts_3+NumOfProducts_4,data=df_train,
                hidden=2,act.fct = softplus,linear.output = FALSE)
plot(nn2)

predict2=compute(nn2,df_test)
prob2<-predict2$net.result
pred2 <- ifelse(prob2>0.5, 1, 0)
tabla2<-table(true=df_test[,"Exited"],pred2) #matriz confusión
tabla2
acc2<-(tabla2[1,1]+tabla2[2,2])/(tabla2[1,1]+tabla2[2,2]+tabla2[1,2]+tabla2[2,1])
acc2 #0.857
ECM2<-mean((df_test$Exited - pred2)^2)
ECM2 #0.143

#modelo 3, neurona=3
nn3=neuralnet(Exited~CreditScore+Age+Tenure+Balance+EstimatedSalary+HasCrCard+
                IsActiveMember+Geography_0+Geography_1+Geography_2+
                NumOfProducts_1+NumOfProducts_2+NumOfProducts_3+NumOfProducts_4,data=df_train,
              hidden=3,act.fct = softplus,linear.output = FALSE)
plot(nn3)

predict3=compute(nn3,df_test)
prob3<-predict3$net.result
pred3 <- ifelse(prob3>0.5, 1, 0)
tabla3<-table(true=df_test[,"Exited"],pred3)
tabla3
acc3<-(tabla3[1,1]+tabla3[2,2])/(tabla3[1,1]+tabla3[2,2]+tabla3[1,2]+tabla3[2,1])
acc3 #0.8525
ECM3<-mean((df_test$Exited - pred3)^2)
ECM3 #0.1475

### modelo 4, capa=2, neurona 2x3
nn4=neuralnet(Exited~CreditScore+Age+Tenure+Balance+EstimatedSalary+HasCrCard+
                IsActiveMember+Geography_0+Geography_1+Geography_2+
                NumOfProducts_1+NumOfProducts_2+NumOfProducts_3+NumOfProducts_4,data=df_train,
              hidden=c(2,3),learningrate=0.001,act.fct = softplus,linear.output = FALSE)
plot(nn4)

predict4=compute(nn4,df_test)
prob4<-predict4$net.result
pred4 <- ifelse(prob4>0.5, 1, 0)
tabla4<-table(true=df_test[,"Exited"],pred4)
tabla4
acc4<-(tabla4[1,1]+tabla4[2,2])/(tabla4[1,1]+tabla4[2,2]+tabla4[1,2]+tabla4[2,1])
acc4 #0.859
ECM4<-mean((df_test$Exited - pred4)^2)
ECM4 #0.141

### modelo 5, capa=3 neurona 2x2x2
nn5=neuralnet(Exited~CreditScore+Age+Tenure+Balance+EstimatedSalary+HasCrCard+
                IsActiveMember+Geography_0+Geography_1+Geography_2+
                NumOfProducts_1+NumOfProducts_2+NumOfProducts_3+NumOfProducts_4,data=df_train,
              hidden=c(2,2,2),act.fct = softplus,linear.output = FALSE)
plot(nn5)

predict5=compute(nn5,df_test)
prob5<-predict5$net.result
pred5 <- ifelse(prob5>0.5, 1, 0)
tabla5<-table(true=df_test[,"Exited"],pred5)
tabla5
acc5<-(tabla5[1,1]+tabla5[2,2])/(tabla5[1,1]+tabla5[2,2]+tabla5[1,2]+tabla5[2,1])
acc5 #0.874
ECM5<-mean((df_test$Exited - pred5)^2)
ECM5 #0.126

#### modelo 6, capa=3 neurona=4x2x3
nn6=neuralnet(Exited~CreditScore+Age+Tenure+Balance+EstimatedSalary+HasCrCard+
                IsActiveMember+Geography_0+Geography_1+Geography_2+
                NumOfProducts_1+NumOfProducts_2+NumOfProducts_3+NumOfProducts_4,data=df_train,
              hidden=c(4,2,3),act.fct = relu,linear.output = FALSE)
plot(nn6)

predict6=compute(nn6,df_test)
prob6<-predict6$net.result
pred6 <- ifelse(prob6>0.5, 1, 0)
tabla6<-table(true=df_test[,"Exited"],pred6)
tabla6
acc5<-(tabla5[1,1]+tabla5[2,2])/(tabla5[1,1]+tabla5[2,2]+tabla5[1,2]+tabla5[2,1])
acc5 #0.914