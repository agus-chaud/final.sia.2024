base=read.table("phoneme.data",sep=",",header=TRUE)
#head(base)
dim(base)

base$speaker=NULL # borrar variable
base$row.names=NULL
dim(base)

for (i in 1:256){
  var=paste("x.",i,sep="")
  nuevo=paste("Frecuencia",i,sep="")
  names(base)[names(base)==var]=nuevo}

names(base)[names(base)=="g"]="Fonema"
base$Fonema=factor(base$Fonema) #renombrar variables con su fonema correspondiente

#head(base)
head(base[,c(1:4,254:257)])

table(base$Fonema) # Hay 695 fonemas de tipo aa, 1022 de tipo ao, 757 de tipo dcl, 1163 de tipo iy y 872 de tipo sh
# el fonema con mas elementos en el dataset es "iy"

plot(base$Fonema,col="steelblue4",main="Gráfico de Agustin Chaud")
#colors()

fonemaAsignado=base[253,]
fonemaAsignado # tiene el fonema "ao"


# parte B
library(caret)
set.seed(63253);particion=createDataPartition(y=base$Fonema,p=0.70,list=FALSE) 
entreno=base[particion,] 
testeo=base[-particion,] 

head(entreno[,c(1:3,255:257)])
summary(entreno[,c(1:3,255:257)])

head(testeo[,c(1:3,255:257)])
summary(testeo[,c(1:3,255:257)])

table(base$Fonema);table(entreno$Fonema);table(testeo$Fonema)

#---parte C--
library(e1071)

svm=svm(Fonema~.,entreno,kernel="polynomial")

svm #Aparecen 1397 vectores soporte... El SVM-type es C-classification
#library(caret)
pred=predict(svm,testeo)
confusionMatrix(pred,testeo$Fonema) #accuracy de 0.9059
predict(svm,fonemaAsignado) 


#---------parte D---------
library(e1071)
vm2=svm(Fonema~.,entreno,kernel="sigmoid")
vm2
pred=predict(vm2,testeo)

confusionMatrix(pred,testeo$Fonema) #accuracy de 0.8459
pred=predict(vm2,fonemaAsignado) # fonema ao

#-------parte E---------
library(e1071)
nb=naiveBayes(Fonema~.,entreno)

pred=predict(nb,testeo)
confusionMatrix(pred,testeo$Fonema) #accuracy de 0.8748

predict(nb,fonemaAsignado) #fonema ao

#------------ej 2------------
#EN REGRESIÓN NO SE PUEDE USAR MATRIZ DE CONFUSIÓN, PORQUE NUNCA VAMOS A PREDECIR EXACTAMENTE UN NUMERO (y además tendriamos demasiadas filas y columns)
# POR ENDE TAMPOCO HAY ACCURACY NI SENSIBILIDAD O ESPECIFIDAD)
# vamos a usar el error cuadrático medio como métrica
#tenemos que cumplir los supuestos para que el modelo de regresión lineal sea válido --> 1: altamente correlacionadas linealmente las var predictoras con la var a predecir
# 2: residuos distribuidos normalmente

#cheque el supuesto 1 graficando dos variables y viendo si los puntos se parecen a una tendencia lineal... otra opcion es el coef de correlación de Pearson
# en regresion lineal el equivalente a accuracy es el coef de detrminación R cuadrado ajustado (mas de 2 variables)
#significancia de las variables predictoras (cuanto mas estrellas tenga mejor) a través de test de hipotesis

#chequeo supuesto 2 --> al graficarlos no ver ningún patrón  plot(REGR)

# REGR= lm(rating~.,data=attitude) --> setea la regresión lineal
# summary(REGR) --> tira las variables con las estrellitas


base=read.csv("Advertising.csv",sep=",",header=TRUE)
head(base) 
base$X=NULL
head(base)

str(base)
dim(base) #200 registros y 4 columnas

plot(base$TV,base$sales,main= "Gráfico de Dispersión de Agusín Chaud", col="steelblue4", pch=21)
#----parte B
library(caret)
set.seed(63253);particion=createDataPartition(y=base$sales,p=0.70,list=FALSE)
entreno=base[particion,]
testeo=base[-particion,]

head(entreno)
summary(entreno)
dim(entreno)

head(testeo)
summary(testeo)
dim(testeo)
#--------parte C---
library(nnet)
set.seed(63253);red=nnet(sales~.,entreno,size=20,maxit=10000,linout=TRUE)
library(NeuralNetTools)
plotnet(red)   

pred=predict(red, testeo) #sin type=“class”
ECMRN=mean((pred-testeo$sales)^2) #0.1873

#ADD
library(rpart)
arbol=rpart(sales~., entreno) #IMPORTANTE SIN method=“class”!!!

library(rpart.plot)
rpart.plot(arbol,extra=1,type=5)

pred=predict(arbol, testeo) #sin type=“class”
ECMArbol=mean((pred-testeo$sales)^2) # 2.6932


#SVM
library(e1071)
svm=svm(sales~., entreno,kernel="polynomial")
svm

pred=predict(svm, testeo)
ECMSvm=mean((pred-testeo$sales)^2) #11.53646

#el mejor modelo es la red neuronal