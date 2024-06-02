iterative_performance <- function(observed,
                                  predicted, 
                                  ncomp = 18) {
  
  application_predict <- function(X,
                                  observed,
                                  predicted) {
    
    return(parameters(observed, as.matrix(predicted[, .SD, .SDcols = X])[,1]))
    
  }
  
  # Parallel processing
  complete <- mclapply(X = 1:ncol(predicted), 
                       FUN = application_predict, 
                       observed,
                       predicted,
                       mc.preschedule = TRUE, 
                       mc.set.seed = FALSE,
                       mc.silent = FALSE, 
                       mc.cores = threads,
                       mc.cleanup = TRUE, 
                       mc.allow.recursive = TRUE, 
                       affinity.list = NULL)
  
  complete <- data.table(do.call(rbind, complete))
  
  return(complete)
  
}

# Performance
parameters <- function(obs, pred) {
  
  linear <- lm(obs ~ pred)
  linear_summary <- summary(linear)
  
  R2 <- linear_summary$r.squared
  intercept <- linear_summary$coefficients[1,1]
  slope <- linear_summary$coefficients[2,1]
  BIAS <- mean(obs-pred)
  RMSE <- sqrt(sum(((obs-pred)^2))/length(obs))
  perRMSE <- sqrt(sum(((obs-pred)^2))/length(obs))/(quantile(obs, 0.99) - quantile(obs, 0.01))
  names(perRMSE) <- NULL
  
  
  return(round(data.table(R2 = R2,
                          BIAS = BIAS,
                          RMSE = RMSE,
                          perRMSE = perRMSE,
                          intercept = intercept,
                          slope = slope), 4))
  
}
