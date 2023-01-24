library(tidyverse); library(caret)

training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")

# para mañana hay que creaar un set de validación y con este estimar el out-of-sample error

# sacar las variables con muchos NA, más del 80% de los datos 

retirar <- data.frame(row.names = c("numero","si_no"))
for(i in 1:dim(training)[2]){
  t <- as.numeric(table(is.na(training[,i]))["TRUE"])
  f <- as.numeric(table(is.na(training[,i]))["FALSE"])

  na_percent <- t/(t+f)
  

  if(is.na(na_percent)){
    retirar[i,1] <- i
    retirar[i,2] <- FALSE
  } else if(na_percent >= 0.8){
    retirar[i,1] <- i
    retirar[i,2] <- TRUE
  } else {
    retirar[i,1] <- i
    retirar[i,2] <- FALSE
  }
}
retirar <- retirar[retirar$V2 == TRUE,1]
train1 <- training[,-retirar]


# separar el set de entrenamiento y crear un set de validación, con el objetivo de obtener un estimado del error fuera de la muestra
index <- createDataPartition(training$classe,
                             p=0.7,
                             list = F)
validation <- train1[-index,]
train1 <- train1[index, ]

# retirar primeras 7 variables pues no entregan información relevante
# además se quita la variable classe para calcular los PCA
# finalmente se calculan los PC
train1 <- train1[,-c(1:7, 93)]
pca1 <- preProcess(train1, method = "pca")
training_pca <- predict(pca1, train1)
training_pca <- training_pca[,grepl("^PC", names(training_pca))]
training_pca$classe <- training[index, "classe"] # se agrega la variable dependiente

# entrenar el modelo, con todos los nucleos disponibles
library(parallel); library(doParallel)
cluster <- makeCluster(detectCores()-1) # se deja uno libre para que el computador funcione
registerDoParallel(cluster)

fitControl <- trainControl(method = "cv",
                           number = 5,
                           allowParallel = T) # este objeto entrega a train() las preferencias con que debe calcular el modelo

system.time(
model_1 <- train(classe~.,
                 data = training_pca,
                 method="rf",
                 trControl = fitControl)
)
stopCluster(cluster)
registerDoSEQ() 

# estimación del error

pred_train<- predict(model_1, training_pca)
aux <-  factor(training_pca$classe)
conf_train <- confusionMatrix(pred_train,aux) 
conf_train$overall

1 - max(model_1[["results"]][["Accuracy"]]) # Tasa de errores en la muestra


# estimación del error aut of sample con el set de validación
validation_pca <- predict(pca1, validation)
pred_validation <- predict(model_1, validation_pca)

aux_2 <- factor(training[-index, "classe"])
conf_val <- confusionMatrix(pred_validation, aux_2)
conf_val$overall
1- conf_val[["overall"]]["Accuracy"]






# crear los PC del set de testeo
# sacar las variables con muchos NA, más del 80% de los datos 

retirar <- data.frame("numero", "si_no")
for(i in 1:dim(testing)[2]){
  t <- as.numeric(table(is.na(testing[,i]))["TRUE"])
  f <- as.numeric(table(is.na(testing[,i]))["FALSE"])
  
  na_percent <- t/sum(t,f, na.rm = T)
  
  
  if(is.na(na_percent)){
    retirar[i,1] <- i
    retirar[i,2] <- FALSE
  } else if(na_percent >= 0.8){
    retirar[i,1] <- i
    retirar[i,2] <- TRUE
  } else {
    retirar[i,1] <- i
    retirar[i,2] <- FALSE
  }
}
retirar <- retirar[retirar$X.si_no. == TRUE,1]


test2 <- testing[,-as.numeric(retirar)]
test2 <- test2[,-c(1:7, 60)]
testing_pca <- predict(pca1, test2)

pred_test <- predict(model_1, testing_pca)

