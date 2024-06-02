iterative_VIP <- function(models,
                          ncomp = 18) {
  
  application_VIP <- function(X,
                              models,
                              ncomp = ncomp) {
    
    return(VIP(models[[X]], opt.comp = ncomp))
    
  }
  
  # Parallel processing
  complete <- mclapply(X = 1:length(models), 
                       FUN = application_VIP, 
                       models = models,
                       ncomp = ncomp,
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
