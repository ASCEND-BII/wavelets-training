iterative_predict <- function(models,
                              spectra, 
                              ncomp = 18) {
  
  application_predict <- function(X,
                                  models,
                                  spectra,
                                  ncomp = ncomp) {
    
    return(predict(models[[X]], newdata = spectra, ncomp = ncomp)[,,1])
    
  }
  
  # Parallel processing
  complete <- mclapply(X = 1:length(models), 
                       FUN = application_predict, 
                       models = models,
                       spectra = spectra,
                       ncomp = ncomp,
                       mc.preschedule = TRUE, 
                       mc.set.seed = FALSE,
                       mc.silent = FALSE, 
                       mc.cores = threads,
                       mc.cleanup = TRUE, 
                       mc.allow.recursive = TRUE, 
                       affinity.list = NULL)
  
  complete <- data.table(do.call(cbind, complete))
  
  return(complete)
  
}
