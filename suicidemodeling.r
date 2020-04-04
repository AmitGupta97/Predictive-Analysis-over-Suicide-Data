library(ggplot2)
library(e1071)
library(Amelia)

# GETTING DATA
DATA=read.csv(file.choose(),header = T,dec = ',')

# DATA VIEWING
str(DATA)
summary(DATA)
is.na(DATA)
(DATA[!complete.cases(DATA),])

# DATA CLEANING AND PREPARATION
DATA$year=as.factor(DATA$year)
DATA$suicides.100k.pop=as.numeric(DATA$suicides.100k.pop)
DATA$HDI.for.year=as.numeric(DATA$HDI.for.year)
DATACLEANED=DATA
str(DATACLEANED)

# IMPUTING OUTLIERS AND FEEDING NAs TO THEM
OUTLIERS=NULL
for (i in 1:ncol(DATACLEANED))
{
  if(class(DATACLEANED[,i])=='numeric'||class(DATACLEANED[,i])=='integer')
  {
    if (length((boxplot(DATACLEANED[,i])$out))==0)
    {
      print ('NO OUTLIERS')
    }else {
      print ('OUTLIERS')
      OUTLIERS=boxplot(DATACLEANED[,i], plot=FALSE)$out
      DATACLEANED[which(DATACLEANED[,i] %in% OUTLIERS),i]=NA
      OUTLIERS=NULL
    }
  }else{
    print ("NOT NUMERIC")
  }
}
summary(DATACLEANED)
DATANOOUTLIER=DATACLEANED[c(-8,-10)]
View(DATANOOUTLIER)

#Visualizing the pattern of missing values
library(VIM)
mice_plot <- aggr(DATANOOUTLIER, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(DATANOOUTLIER), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

#Imputing the missing values
amelia_imputed_data=amelia(DATANOOUTLIER,m=5,parallel="multicore",noms=c(1:4,10))
amelia_imputed_data$imputations
amelia_imputed_data$imputations[[1]]
completedataamelia=as.data.frame(amelia_imputed_data$imputations[[1]])
summary(completedataamelia)
DATANORM=completedataamelia

# DATA SUBSETTING AND PREPARATION
set.seed(1234)
ind <- sample(2, nrow(DATANORM), replace = T, prob = c(0.7, 0.3))
TRAINING<- DATANORM[ind == 1,]
TESTING<- DATANORM[ind == 2,]
summary(TRAINING)


# CREATING AUTOMATIC SVM MODEL
MODEL=svm(generation~.,data=TRAINING)
PREDICTIONPROB=predict(MODEL,TESTING,type="prob")
head(cbind(PREDICTIONPROB,TESTING))
summary(MODEL)

# FINDING PREDICTION AND ACCURACY
SVMPREDICTION=predict(MODEL,TESTING)
(Tab=table(SVMPREDICTION,TESTING$generation))
(SVMACCURACY=sum(diag(Tab))/sum(Tab)*100)


# CREATING LINEAR SVM MODEL
MODEL1=svm(generation~.,data=TRAINING,kernel="linear")
PREDICTIONPROB1=predict(MODEL1,TESTING,type="prob")
head(cbind(PREDICTIONPROB1,TESTING))
summary(MODEL1)

# FINDING PREDICTION AND ACCURACY
SVMPREDICTION1=predict(MODEL1,TESTING)
(Tab1=table(SVMPREDICTION1,TESTING$generation))
(SVMACCURACY1=sum(diag(Tab1))/sum(Tab1)*100)


# CREATING POLYNOMIAL SVM MODEL
MODEL2=svm(generation~.,data=TRAINING,kernel="polynomial")
PREDICTIONPROB2=predict(MODEL2,TESTING,type="prob")
head(cbind(PREDICTIONPROB2,TESTING))
summary(MODEL2)

# FINDING PREDICTION AND ACCURACY
SVMPREDICTION2=predict(MODEL2,TESTING)
(Tab2=table(SVMPREDICTION2,TESTING$generation))
(SVMACCURACY2=sum(diag(Tab2))/sum(Tab2)*100)



# CREATING SIGMOID SVM MODEL
MODEL3=svm(generation~.,data=TRAINING,kernel="sigmoid")
PREDICTIONPROB3=predict(MODEL3,TESTING,type="prob")
head(cbind(PREDICTIONPROB3,TESTING))
summary(MODEL3)

# FINDING PREDICTION AND ACCURACY
SVMPREDICTION3=predict(MODEL3,TESTING)
(Tab3=table(SVMPREDICTION3,TESTING$generation))
(SVMACCURACY3=sum(diag(Tab3))/sum(Tab3)*100)


# CREATING RADIAL SVM MODEL
MODEL4=svm(generation~.,data=TRAINING,kernel="radial")
PREDICTIONPROB4=predict(MODEL4,TESTING,type="prob")
head(cbind(PREDICTIONPROB4,TESTING))
summary(MODEL4)

# FINDING PREDICTION AND ACCURACY
SVMPREDICTION4=predict(MODEL4,TESTING)
(Tab4=table(SVMPREDICTION4,TESTING$generation))
(SVMACCURACY4=sum(diag(Tab4))/sum(Tab4)*100)



ACCURACIES=c(SVMACCURACY,SVMACCURACY1,SVMACCURACY2,SVMACCURACY3,SVMACCURACY4)
NAMES_MODEL=c('MODEL','MODEL1','MODEL2','MODEL3','MODEL4')
ALGO_USED=c('DEFAULT','LINEAR','POLYNOMIAL','SIGMOID','RADIAL')
(ACCURACIES_COLUMNS=data.frame(NAMES_MODEL,ALGO_USED,ACCURACIES=round(ACCURACIES)))

