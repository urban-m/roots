#file:///C:/Users/Matthias/AppData/Local/Temp/Rar$EXa1.895/SupplementaryMaterial_P5_Random_Forests_V3.html
require(ranger)
require(party)
require(caret)
require(lattice)
require(glmnet)
require(missRanger)
rootsdata<-read.csv(file.choose(), header=T, row.names=1)
rootsdata[,"Roots"]=as.numeric(gsub(',','.',rootsdata[,"Roots"]))
rootsdata[,"Analyzability"]=as.numeric(gsub(',','.',rootsdata[,"Analyzability"]))
rootsdata["arb", "Roots"]=3.125
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

#The below is a test to learn RandomForests and make sure you can apply it. When trying to impute all
#NAs using missRanger there's an error message that I think is due to just too many NAs. So until I fill
#these, the below is to make sure I can run the randomforests analysis to not do everything in vain. This
#is meant to just take the first five columns and run the analysis, to be used as a model for the full dataset-

#Impute NA's (play around with parameters a bit, check package documentation. There's also the possibility to 
#create a kind of consensus interpretation)
rootsdatatest<-select(rootsdata, c(1:20))
rootsdatatestimputed<-missRanger(rootsdatatest, pmm.k=3, num.trees=100)
#Estimation of the mtry parameter
#not 1000% clear yet what the tuneLength parameter is doing, check
mtry_est<-train(Roots~ ., data=rootsdatatestimputed, method = 'rf', tuneLength=14)
print(mtry_est)

##the below code is from the suppl mats of Tomaschek et al. (2018)
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
tuneGrid = expand.grid(mtry = 8, num.trees = num_trees)

# Define the cross-validation parameters
control = trainControl(method = "cv", number = 10)

# Run the grid search using the train() function of the caret package
#mod = train(sDur ~ ., data = rf_data, method = ranger_type$ranger, 
#            trControl = control, tuneGrid = tuneGrid, verbose = TRUE)
# Load the results of the coarse grid search
#this is where it stops working, can't find the .rda in the supp mats
load("data/caret_ranger_coarse.rda")

# Retrieve the optimal parameter settings


mod<-train(Roots~ ., data=rootsdatatestimputed, method = ranger_type$ranger, trControl = control, tuneGrid = tuneGrid, verbose = TRUE)
mod$bestTune

#In this case, optimal num trees = 55, so instead of setting the search space as in the script, I'm setting it to 30:80
num_trees = c(10:80)
tuneGrid <-  expand.grid(mtry = 8, num.trees = num_trees)
mod2<-train(Roots~ ., data=rootsdatatestimputed, method = ranger_type$ranger, trControl = control, tuneGrid = tuneGrid, verbose = TRUE)
mod2$bestTune

#The output is that the optimal tree size is 1. This is getting weirder and weirder. You have to check this thoroughly.

#forest=ranger(Root.structure ~ ., data=rootsdatatestimputed, num.trees=1, mtry=10, importance="permutation")
#Error: mtry can not be larger than number of variables in data. Ranger will EXIT now.
#Fehler in ranger(Root.structure ~ ., data = rootsdatatestimputed, num.trees = 1,  : 
#                   User interrupt or internal error.
forest=ranger(Roots ~ ., data=rootsdatatestimputed, num.trees=66, mtry=8, importance="permutation")


# Evaluate OOB prediction accuracy
rmse = sqrt(forest$prediction.error)
rmse

rmse_cv = min(mod2$results$RMSE)
rmse_cv

#What I get here is quite different. Suggests a problem.
#rmse
#[1] 590.1706
#rmse_cv = min(mod2$results$RMSE)
# rmse_cv
#[1] 118.5394
rev(sort(varimps))
varimps = round(importance(forest), 3)


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
          scales = list(x = list(at = seq(-1, 1, by = 0.1),
                                 labels = round(seq(-1, 1, by = 0.1),1))),
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
plotCoefficientsRF.fnc(importances = varimps, color = "#CD5555", 
                       title = "segment duration")
