

library(rBayesianOptimization)
set.seed(13)



gau_fit_bayes <- function(Sigma) {
 ## Use the same model code but for a single (C, sigma) pair. 
  txt <- capture.output(
    mod <- caret::train(trainx, as.numeric(trainy),
                        method = "gaussprRadial",
                        metric = "RMSE",
                        trControl = ctrl,
                        tuneGrid = data.frame(sigma = Sigma))
    )
 ## The function wants to _maximize_ the outcome so we return 
 ## the negative of the resampled RMSE value. `Pred` can be used
 ## to return predicted values but we'll avoid that and use zero
  list(Score = -getTrainPerf(mod)[, "TrainRMSE"], Pred = 0)
  }


lasso_fit_bayes <- function(Lambda, Alpha) {
  ## Use the same model code but for a single (C, sigma) pair. 
  txt <- capture.output(
    mod <- caret::train(trainx, as.numeric(trainy),
                        method = "glmnet",
                        metric = "RMSE",
                        trControl = ctrl,
                        tuneGrid = data.frame(lambda = Lambda, alpha = Alpha))
  )
  ## The function wants to _maximize_ the outcome so we return 
  ## the negative of the resampled RMSE value. `Pred` can be used
  ## to return predicted values but we'll avoid that and use zero
  list(Score = -getTrainPerf(mod)[, "TrainRMSE"], Pred = 0)
}

xgboost_fit_bayes <- function(nrounds,
                              eta, 
                              max_depth, 
                              min_child_weight,
                              subsample) {
  ## Use the same model code but for a single (C, sigma) pair. 
  txt <- capture.output(
    mod <- caret::train(trainx, as.numeric(trainy),
                        method = "xgbTree",
                        metric = "RMSE",
                        trControl = ctrl,
                        tuneGrid = data.frame(nrounds= nrounds,
                                              eta = eta,
                                              max_depth = max_depth,
                                              min_child_weight = min_child_weight,
                                              subsample = subsample, 
                                              colsample_bytree = 0.3))
  )
  ## The function wants to _maximize_ the outcome so we return 
  ## the negative of the resampled RMSE value. `Pred` can be used
  ## to return predicted values but we'll avoid that and use zero
  list(Score = -getTrainPerf(mod)[, "TrainRMSE"], Pred = 0)
}




