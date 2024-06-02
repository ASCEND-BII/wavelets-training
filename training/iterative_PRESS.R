iterative_PRESS <- function(models) {
  
  application_opt <- function(X,
                              models) {
    
    return(models[[X]]$validation$PRESS)
    
  }
  
  # Parallel processing
  complete <- mclapply(X = 1:length(models), 
                       FUN = application_opt, 
                       models = models,
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
