time_series_test_split <- function(data, y, fmla, test_years, model = 'lm', acc_metric = "mse"){
  ### function assumes year column is called year!
  
  all_test_metric <- rep(NA, length(test_years))
  
  for (i in c(1:length(test_years))){
    yr <- test_years[i]
    
    # Fit model
    if (model == 'lm'){
      partial_reg <- lm(fmla, data = filter(data, year < yr))
      predicted_res <- predict(partial_reg, filter(data, year == yr))
    } else if (model == 'pls') {
      partial_reg <- train(fmla, data = filter(data, year < yr),   method = "pls")
      predicted_res <- predict(partial_reg, filter(data, year == yr))
    } else if (model %in% c('ridge', 'lasso')){
      x <- as.matrix(data[ , names(data) != y])
      y_train <- data[data$year < yr ,  names(data) == y] %>% pull(1) #pull coerces to vector
      lambdas <- 10^seq(2, -3, by = -.1)
      
      if (model == 'ridge'){
        cv_ridge <- cv.glmnet(x[x[ , "year"] < yr, ], y_train, alpha = 0, lambda = lambdas, standardize = TRUE, nfolds = 5)
        optimal_lambda <- cv_ridge$lambda.min
        
        partial_reg <- glmnet(x[x[ , "year"] < yr, ], y_train, nlambda = 25, alpha = 0, family = 'gaussian', lambda = optimal_lambda)
        predicted_res <- predict(partial_reg, s = optimal_lambda, newx = x[x[ , "year"] == yr, ])
      } else {
        cv_lasso <- cv.glmnet(x[x[ , "year"] < yr, ], y_train, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)
        optimal_lambda <- cv_lasso$lambda.min 
        
        partial_reg <- glmnet(x[x[ , "year"] < yr, ], y_train, alpha = 1, lambda = optimal_lambda, standardize = TRUE)
        predicted_res <- predict(partial_reg, s = optimal_lambda, newx = x[x[ , "year"] == yr, ])
      }
      
      
    }
    
    
    else if (model == 'nnet'){
      nnetGrid <- expand.grid(.decay = c(0, 0.01, .1), .size = c(1:10))
      partial_reg <- train(fmla, data = filter(data, year < yr), method = "nnet",
                          tuneGrid = nnetGrid,
                          preProcess = c("center", "scale"), trace = FALSE)
    } else {
      partial_reg <- glm(fmla, data = filter(data, year < yr), family = 'binomial')
      predicted_res <- predict(partial_reg, filter(data, year == yr), type = 'response')
    }
    
    actual_res <- data[data$year == yr, y]
    
    if (acc_metric == "mse"){
      test_metric <- sum((actual_res - predicted_res)**2, na.rm = TRUE)
    } else {
      
      if (model %in% c('lm', 'pls', 'nnet')){
          predicted_res[predicted_res < 50] <- 0
          predicted_res[predicted_res >= 50] <- 1
          
          actual_res[actual_res < 50] <- 0
          actual_res[actual_res >= 50] <- 1
      } else {
        predicted_res[predicted_res < 0.5] <- 0
        predicted_res[predicted_res >= 0.5] <- 1
        
        actual_res[actual_res < 0.5] <- 0
        actual_res[actual_res >= 0.5] <- 1   
      }
    
      reg_error <- predicted_res - actual_res
      test_metric <- length(reg_error[reg_error == 0]) / length(reg_error[!is.na(reg_error)])
    }
    
    all_test_metric[i] <- test_metric
  }
  
  return(all_test_metric)
}

ts_full_selection <- function(data, y, current_preds, test_years, model = 'lm', n_iter = length(current_preds), acc_metric = "mse"){
  
  ## Output Vector
  best_metric <- rep(NA, n_iter)
  
  ## Removed Vars
  removed_var <- rep(NA, n_iter)
  
  ## For Every Iteration (Number of Variables to Remove)
  for (k in c(1: n_iter)){
    
    ## First Variable Setup
    if (k == 1){
      
      ## Remaining Predictors
      leftover_preds <- current_preds
      
      ## Finding Model Baseline
      fmla <- as.formula(paste(y, " ~ ", paste(current_preds, collapse = "+"))) #adjusting formula
      baseline <- time_series_test_split(data, y, fmla, test_years, model = model, acc_metric = acc_metric) #prediction 
      baseline <- mean(baseline, na.rm = TRUE) # time_series_test_split returns vector
    }
    
    # Calculating Improvement
    ## Output Vector
    metric <- rep(NA, length(leftover_preds))
    
    ## Calculating Improvement
    for (i in c(1:length(leftover_preds))){
      
      preds <- leftover_preds[leftover_preds != leftover_preds[i]] #removing variable i from predictors
      fmla <- as.formula(paste(y, " ~ ", paste(preds, collapse = "+"))) #adjusting formula
      
      
      new_metric <- time_series_test_split(data, y, fmla, test_years, model = model, acc_metric = acc_metric) #prediction
      metric[i] <- mean(new_metric, na.rm = TRUE)
    }
    
    if (acc_metric == 'mse'){
      location <- which(metric == min(metric, na.rm = TRUE)) #minimizing mse
    } else {
      location <- which(metric == max(metric, na.rm = TRUE)) #maximizing accuracy
    }
    
    if (length(location) >= 1){ # in case multiple models are optimal
      best_metric[k] <- metric[location[1]]
      new_var <- leftover_preds[location[1]]
      removed_var[k] <- new_var
    }
    
    
    ## Check whether Latest Metric is an Improvement
    if (acc_metric == 'mse'){
      if (best_metric[k] >= baseline){ 
        print("No variables left to remove.")
        break
      } else {
        ## Update Metric
        baseline <- best_metric[k]
        
        ## Save Most Improved Vars
        leftover_preds <- leftover_preds[leftover_preds != new_var] #update current preds
        
        assign("removed_var", removed_var, envir = globalenv())
        assign("leftover_preds", leftover_preds, envir = globalenv())
        assign("best_metric", best_metric, envir = globalenv())
      }     
    } else {
      if (best_metric[k] <= baseline){ 
        print("No variables left to remove.")
        ts_forward_selection(data, y, leftover_preds, current_preds[!current_preds %in% leftover_preds], test_years, 
                             model = model, acc_metric = acc_metric)
        break # or stuck in loop
      } else {
        ## Update Metric
        baseline <- best_metric[k]
        
        ## Save Most Improved Vars
        leftover_preds <- leftover_preds[leftover_preds != new_var] #update current preds
        
        assign("removed_var", removed_var, envir = globalenv())
        assign("leftover_preds", leftover_preds, envir = globalenv())
        assign("best_metric", best_metric, envir = globalenv())
      }
    }
  }
}

ts_backward_selection <- function(data, y, current_preds, test_years, model = 'lm', n_iter = length(current_preds), acc_metric = "mse"){
  
  ## Output Vector
  best_metric <- rep(NA, n_iter)
  
  ## Removed Vars
  removed_var <- rep(NA, n_iter)
  
  ## For Every Iteration (Number of Variables to Remove)
  for (k in c(1: n_iter)){
    
    ## First Variable Setup
    if (k == 1){
      
      ## Remaining Predictors
      leftover_preds <- current_preds
      
      ## Finding Model Baseline
      fmla <- as.formula(paste(y, " ~ ", paste(current_preds, collapse = "+"))) #adjusting formula
      baseline <- time_series_test_split(data, y, fmla, test_years, model = model, acc_metric = acc_metric) #prediction 
      baseline <- mean(baseline, na.rm = TRUE) # time_series_test_split returns vector
    }
    
    # Calculating Improvement
    ## Output Vector
    metric <- rep(NA, length(leftover_preds))
    
    ## Calculating Improvement
    for (i in c(1:length(leftover_preds))){
      
      preds <- leftover_preds[leftover_preds != leftover_preds[i]] #removing variable i from predictors
      fmla <- as.formula(paste(y, " ~ ", paste(preds, collapse = "+"))) #adjusting formula


      new_metric <- time_series_test_split(data, y, fmla, test_years, model = model, acc_metric = acc_metric) #prediction
      metric[i] <- mean(new_metric, na.rm = TRUE)
    }
    
    if (acc_metric == 'mse'){
      location <- which(metric == min(metric, na.rm = TRUE)) #minimizing mse
    } else {
      location <- which(metric == max(metric, na.rm = TRUE)) #maximizing accuracy
    }
    
    if (length(location) >= 1){ # in case multiple models are optimal
      best_metric[k] <- metric[location[1]]
      new_var <- leftover_preds[location[1]]
      removed_var[k] <- new_var
    }
    
    
    ## Check whether Latest Metric is an Improvement
    if (acc_metric == 'mse'){
      if (best_metric[k] >= baseline){ 
        print("No variables left to remove.")
        break
      } else {
        ## Update Metric
        baseline <- best_metric[k]
        
        ## Save Most Improved Vars
        leftover_preds <- leftover_preds[leftover_preds != new_var] #update current preds
        
        assign("removed_var", removed_var, envir = globalenv())
        assign("leftover_preds", leftover_preds, envir = globalenv())
        assign("best_metric", best_metric, envir = globalenv())
      }     
    } else {
    if (best_metric[k] <= baseline){ 
      print("No variables left to remove.")
      break
    } else {
      ## Update Metric
      baseline <- best_metric[k]
      
      ## Save Most Improved Vars
      leftover_preds <- leftover_preds[leftover_preds != new_var] #update current preds
      
      assign("removed_var", removed_var, envir = globalenv())
      assign("leftover_preds", leftover_preds, envir = globalenv())
      assign("best_metric", best_metric, envir = globalenv())
    }
    }
  }
}



ts_forward_selection <- function(data, y, current_preds, potential_preds, test_years, model = 'lm', n_iter = length(current_preds), acc_metric = "mse"){
  
  ## Output Vector
  best_metric <- rep(NA, n_iter)
  
  ## Removed Vars
  added_var <- rep(NA, n_iter)
  
  ## For Every Iteration (Number of Variables to Remove)
  for (k in c(1: n_iter)){
    
    ## First Variable Setup
    if (k == 1){
      
      ## Finding Model Baseline
      fmla <- as.formula(paste(y, " ~ ", paste(current_preds, collapse = "+"))) #adjusting formula
      baseline <- time_series_test_split(data, y, fmla, test_years, model = model, acc_metric = acc_metric) #prediction 
      baseline <- mean(baseline, na.rm = TRUE) # time_series_test_split returns vector
    }
    
    # Calculating Improvement
    ## Output Vector
    metric <- rep(NA, length(potential_preds))
    
    ## Calculating Improvement
    for (i in c(1:length(potential_preds))){
      
      preds <- c(current_preds, potential_preds[i]) #adding variable i to predictors
      fmla <- as.formula(paste(y, " ~ ", paste(preds, collapse = "+"))) #adjusting formula
      
      new_metric <- time_series_test_split(data, y, fmla, test_years, model = model, acc_metric = acc_metric) #prediction
      metric[i] <- mean(new_metric, na.rm = TRUE)
    }
    
    if (acc_metric == 'mse'){
      location <- which(metric == min(metric, na.rm = TRUE)) #minimizing mse
    } else {
      location <- which(metric == max(metric, na.rm = TRUE)) #maximizing accuracy
    }
    
    if (length(location) >= 1){ # in case multiple models are optimal
      best_metric[k] <- metric[location[1]]
      new_var <- leftover_preds[location[1]]
      added_var[k] <- new_var
    }
    
    
    ## Check whether Latest Metric is an Improvement
    if (acc_metric == 'mse'){
      if (best_metric[k] >= baseline){ 
        print("No variables left to add.")
        break
      } else {
        ## Update Metric
        baseline <- best_metric[k]
        
        ## Save Most Improved Vars
        current_preds <- c(current_preds, new_var) #update current preds
        
        assign("added_var", added_var, envir = globalenv())
        assign("current_preds", current_preds, envir = globalenv())
        assign("best_metric", best_metric, envir = globalenv())
      }     
    } else {
      if (best_metric[k] <= baseline){ 
        print("No variables left to add.")
        break
      } else {
        ## Update Metric
        baseline <- best_metric[k]
        
        ## Save Most Improved Vars
        current_preds <- c(current_preds, new_var) #update current preds
        
        assign("added_var", added_var, envir = globalenv())
        assign("leftover_preds", leftover_preds, envir = globalenv())
        assign("best_metric", best_metric, envir = globalenv())
      }
    }
  }
  
}

