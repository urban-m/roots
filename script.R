require(tidyverse)
require(ranger)
require(caret)
require(lattice)
require(glmnet)
require(missRanger)

#read data
rootsdata<-read.csv(file.choose(), header=T, row.names=1)

#code variables according to their respective scales
rootsdata[,"Roots"]=as.numeric(gsub(',','.',rootsdata[,"Roots"]))
rootsdata[,"Analyzability"]=as.numeric(gsub(',','.',rootsdata[,"Analyzability"]))
rootsdata[,"Consonants"]=as.numeric(rootsdata[,"Consonants"])
rootsdata[,"Vowels"]=as.numeric(rootsdata[,"Vowels"])
rootsdata[,"Ratio"]=as.numeric(rootsdata[,"Ratio"])
rootsdata[,"Syllables"]=as.ordered(rootsdata[,"Syllables"])
rootsdata[,"Tone"]=as.ordered(rootsdata[,"Tone"])
rootsdata[,"Stress.system"]=as.factor(rootsdata[,"Stress.system"])
rootsdata[,"Stress.alignment"]=as.factor(rootsdata[,"Stress.alignment"])
rootsdata[,"Rhythm"]=as.factor(rootsdata[,"Rhythm"])
rootsdata[,"Fusion"]=as.factor(rootsdata[,"Fusion"])
rootsdata[,"Exponence"]=as.factor(rootsdata[,"Exponence"])
rootsdata[,"Flexivity"]=as.factor(rootsdata[,"Flexivity"])
rootsdata[,"Synthesis"]=as.factor(rootsdata[,"Synthesis"])
rootsdata[,"Marking"]=as.factor(rootsdata[,"Marking"])
rootsdata[,"Word.order..SVO"]=as.factor(rootsdata[,"Word.order..SVO"])
rootsdata[,"Word.order..SV"]=as.factor(rootsdata[,"Word.order..SV"])
rootsdata[,"Word.order..SO"]=as.factor(rootsdata[,"Word.order..SO"])
rootsdata[,"Adpositions"]=as.factor(rootsdata[,"Adpositions"])
rootsdata[,"Case"]=as.factor(rootsdata[,"Case"])

#Impute NA's 
rootsdatatest<-select(rootsdata, c(1:20))
rootsdatatestimputed<-missRanger(rootsdatatest, pmm.k=3, num.trees=100)

#Estimation of the mtry parameter
#not 1000% clear yet what the tuneLength parameter is doing, check
mtry_est<-train(Roots~ ., data=rootsdatatestimputed, method = 'rf', tuneLength=14)
print(mtry_est)
#The final value used for the model was mtry = 8.

##the below code is from the supplementary materials of Tomaschek et al. (2018) adapted for use to the present data set
##To be checked!!!
# Get the existing method for the ranger() function
ranger_type = getModelInfo(model = "ranger")

# Change the parameters that may be tuned to include num.trees
ranger_type$ranger$parameters = data.frame("parameter" = c("mtry", "num.trees"),
                                           "class" = c("numeric", "numeric"),
                                           "label" = c("Selected Predictors", 
                                                       "Number of Trees"))

# Edit the model fit function to include a num.trees argument in the call to the
# ranger function()
ranger_type$ranger$fit = function (x, y, wts, param, lev, last, classProbs, 
                                   ...) {
  if ((!is.data.frame(x)) || dplyr::is.tbl(x)) 
    x <- as.data.frame(x)
  x$.outcome <- y
  if (!is.null(wts)) {
    out <- ranger::ranger(dependent.variable.name = ".outcome", 
                          data = x, mtry = param$mtry, num.trees = param$num.trees,
                          probability = classProbs, case.weights = wts, ...)
  }
  else {
    out <- ranger::ranger(dependent.variable.name = ".outcome", 
                          data = x, mtry = param$mtry, num.trees = param$num.trees, 
                          write.forest = TRUE, probability = classProbs, ...)
  }
  if (!last) 
    out$y <- y
  out
}
num_trees = round(exp(seq(1, 11.5, by = 0.5)))
num_trees
#  [1]     3     4     7    12    20    33    55    90   148   245   403   665
# [13]  1097  1808  2981  4915  8103 13360 22026 36316 59874 98716

# Define the search grid
tuneGrid = expand.grid(mtry = 10, num.trees = num_trees)

# Define the cross-validation parameters
control = trainControl(method = "cv", number = 10)


# Run the grid search using the train() function of the caret package
mod<-train(Roots~ ., data=rootsdatatestimputed, method = ranger_type$ranger, trControl = control, tuneGrid = tuneGrid, verbose = TRUE)
mod$bestTune

#Yields different results even when holding mtry constant. Why?
#with mtry = 8
#mtry num.trees
#6    8        33

#with mtry = 10
#mtry num.trees
#4   10        12

#rerun with mtry = 10
#mtry num.trees
#8    10        42

#rerun with mtry = 10 once more
#mtry num.trees
#12   10        51

#In this case, I work with optimal num trees = 51, so instead of setting the search space as in the script, I'm setting it to 30:80
num_trees = c(20:80)
tuneGrid <-  expand.grid(mtry = 10, num.trees = num_trees)
mod2<-train(Roots~ ., data=rootsdatatestimputed, method = ranger_type$ranger, trControl = control, tuneGrid = tuneGrid, verbose = TRUE)
mod2$bestTune

#mod2$bestTune
#mtry num.trees
#17    10       36

#so we continue with 36 trees...

##omit visualization of RMSE 

# Run a random forest with the ranger function
forest36=ranger(Roots ~ ., data=rootsdatatestimputed, num.trees=36, mtry=10, importance="permutation")
forest1500=ranger(Roots ~ ., data=rootsdatatestimputed, num.trees=1500, importance="permutation")


# Evaluate OOB prediction accuracy
rmse = sqrt(forest36$prediction.error)
rmse
#[1] 0.5314021

rmse_cv = min(mod2$results$RMSE)
rmse_cv
#[1] 0.4810242

##Is this difference acceptable?


varimps36 = round(importance(forest36), 3)
rev(sort(varimps36))

#0.086            0.024            0.013            0.013            0.012            0.008            0.006 
#Word.order..SO   Word.order..SV          Marking        Synthesis Stress.alignment           Fusion           Rhythm 
#0.003           -0.001           -0.004           -0.004           -0.004           -0.005           -0.005 
#Ratio        Exponence  Word.order..SVO        Flexivity    Stress.system 
#-0.005           -0.006           -0.007           -0.010           -0.012 

varimps1500 = round(importance(forest1500), 3)
rev(sort(varimps1500))

#Consonants             Tone      Adpositions        Syllables  Word.order..SVO             Case    Stress.system 
#0.044            0.014            0.013            0.009            0.006            0.005            0.005 
#Ratio   Word.order..SO    Analyzability   Word.order..SV        Synthesis           Vowels          Marking 
#0.005            0.004            0.004            0.003            0.001            0.001            0.000 
#Fusion        Exponence           Rhythm Stress.alignment        Flexivity 
#-0.001           -0.002           -0.003           -0.003           -0.004

# Define plot function
# Define plot function
plotCoefficientsRF.fnc = function(importances, color = "#CD5555",title="") {
  
  # Order importances
  importances = importances[order(importances)]
  
  # Define predictor names
  predictors = names(importances)
  
  # Define colors for non-zero and zero importances
  colors = c(color,paste0(color,"66"))
  
  # Plot
  dotplot(importances, labels = predictors,
          xlab = "variable importances", ylab = "predictors",
          col = colors[as.numeric(importances==0)+1], pch = 19,
          scales = list(x = list(at = seq(-0.1, 0.1, by = 0.01),
                                 labels = seq(-0.1, 0.1, by = 0.01))),
          panel = function(...) {
            panel.abline(v = 0, lty = "dotted", col = "black")
            panel.dotplot(...)
          },
          par.settings = list(fontsize = list(text = 12, points = 10))
  )
  
}

# Set plotting parameters
par(mfrow = c(1, 1))

# Plot
plotCoefficientsRF.fnc(importances = varimps60, color = "#CD5555", 
                       title = "segment duration")
plotCoefficientsRF.fnc(importances = varimps1500, color = "#CD5555", 
                       title = "segment duration")
