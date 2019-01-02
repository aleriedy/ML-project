

library(rBayesianOptimization)
set.seed(13)

## Define the resampling method
ctrl <- trainControl(method = "repeatedcv", repeats = 5)
## Use this function to optimize the model. The two parameters are 
## evaluated on the log scale given their range and scope. 

gbm_rand <- caret::train(trainx, as.numeric(trainy)
                          , method="gaussprRadial"
                          , metric="RMSE"
                          , tunelength = 20
                          , trControl=ctrl1
                          , verbose=FALSE
)                  


gau_fit_bayes <- function(Sigma) {
 ## Use the same model code but for a single (C, sigma) pair. 
  txt <- capture.output(
    mod <- caret::train(trainx ~ as.numeric(trainy),
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

## Define the bounds of the search. 
lower_bounds <- c(Sigma = 0.00015)
upper_bounds <- c(Sigma = 1)
bounds <- list(Sigma = c(lower_bounds[1], 
                         upper_bounds[1]))

## Create a grid of values as the input into the BO code
initial_grid <- gbm_rand$results[, c("sigma", "RMSE")]
initial_grid$sigma <- initial_grid$sigma
initial_grid$RMSE <- -initial_grid$RMSE
names(initial_grid) <- c("Sigma", "Value")

## Run the optimization with the initial grid and do
## 30 iterations. We will choose new parameter values
## using the upper confidence bound using 1 std. dev. 
ba_search <- BayesianOptimization(gau_fit_bayes,
                                  bounds = bounds,
                                  init_grid_dt = initial_grid, 
                                  init_points = 10, 
                                  n_iter = 30,
                                  acq = "ucb", 
                                  kappa = 1, 
                                  eps = 0.0,
                                  verbose = TRUE)

