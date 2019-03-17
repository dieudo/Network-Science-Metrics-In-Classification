Net_Metrics_Class <- read.csv("Net_Metrics_Class.csv")
#Fit Random Forest Model
rf = randomForest(Classifier ~ .,  
                  ntree = 100,
                  data = Net_Metrics_Class)
plot(rf)  

# Variable Importance
varImpPlot(rf,  
           sort = T,
           main="Top - Variable Importance")

#Variable Importance
var.imp = data.frame(importance(rf,  
                                type=2))
# make row names as columns
var.imp$Variables = row.names(var.imp)  
print(var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),])





# Using decision trees we have this algorithm

predictClassifier <- function(clscoef=NA,
                              hubs=NA){
  if (is.na(clscoef)){
    return("Knn")
  }
  if (clscoef > 0.7921) {
    if (is.na(hubs)){
      return("NN")
    }
    if (hubs > 0.09946) {
      return("NN")
    }
    if (hubs <= 0.09946) {
      return("DT")
    }
  }
  if (clscoef <= 0.7921) {
    if (is.na(hubs)){
      return("Knn")
    }
    if (hubs > 0.27031) {
      if (hubs > 0.32238) {
        if (hubs > 0.37673) {
          if (hubs > 0.40154) {
            return("DT")
          }
          if (hubs <= 0.40154) {
            return("SVM")
          }
        }
        if (hubs <= 0.37673) {
          if (clscoef > 0.62497) {
            return("DT")
          }
          if (clscoef <= 0.62497) {
            return("Knn")
          }
        }
      }
      if (hubs <= 0.32238) {
        return("SVM")
      }
    }
    if (hubs <= 0.27031) {
      if (hubs > 0.16547) {
        if (clscoef > 0.54187) {
          return("Knn")
        }
        if (clscoef <= 0.54187) {
          return("LDA")
        }
      }
      if (hubs <= 0.16547) {
        return("SVM")
      }
    }
  }
  return(NA)
}

