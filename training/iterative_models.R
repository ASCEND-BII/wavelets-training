iterative_models <- function(trait, 
                             spectra, 
                             split,
                             icv,
                             ncomp_max = 30,
                             threads) {
  
  # Homogeneous segments
  frame <- cbind(trait_oi = trait, spectra)
  
  # Training
  frame_training <- frame[split, ]
  
  # Application loop
  application_tune <- function(X,
                               frame_training,
                               icv,
                               ncomp_max) {
    
    # Time
    t <- X
    
    # Subframe
    sub_frame <- frame_training[icv$samples[[t]],]
    folds <- icv$folds[[t]]
    
    # Model
    ml_model <- plsr(trait_oi ~ ., 
                     data = sub_frame,
                     validation = "CV",
                     segments = folds,
                     ncomp = ncomp_max,
                     center = TRUE,
                     method = "oscorespls",
                     maxit = 1000,
                     na.action = na.exclude)
    
    return(ml_model)
    
  }
  
  # Parallel processing
  complete <- mclapply(X = 1:length(icv$samples), 
                       FUN = application_tune, 
                       frame_training = frame_training,
                       icv = icv,
                       ncomp_max = ncomp_max,
                       mc.preschedule = TRUE, 
                       mc.set.seed = FALSE,
                       mc.silent = FALSE, 
                       mc.cores = threads,
                       mc.cleanup = TRUE, 
                       mc.allow.recursive = TRUE, 
                       affinity.list = NULL)
  
  return(complete)
  
}