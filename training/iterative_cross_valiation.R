iterative_cross_valiation <- function(samples,
                                      psamples = 0.8,
                                      k = 10,
                                      times = 30) {
  
  # Collectors
  samples_collector <- list()
  folds_collector <- list()
  
  for(i in 1:times) {
    
    sub_samples <- sample(1:length(samples), 
                          ceiling(length(samples)*psamples), 
                          replace = FALSE)
    
    
    sub_folds <- cvsegments(N = length(sub_samples),
                            k = k)
    
    
    samples_collector <- append(samples_collector, list(sub_samples))
    folds_collector <- append(folds_collector, list(sub_folds))
    
  }
  
  return(list(samples = samples_collector,
              folds = folds_collector))
  
}
