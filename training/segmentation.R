segmentation <- function(trait, 
                         nsegments = 50, 
                         lower.p = 0.5, 
                         proportion = 0.8) {
  
  densities <- density(trait, na.rm = T)
  
  # Divide data by quantiles
  x_bins <- cut(trait, 
                breaks = densities$x, 
                include.lowest = TRUE, 
                labels = FALSE)

  # Togeter data and quantiles
  frame <- data.table(x = x_bins, 
                      y = y_bins, 
                      z = z_bins,
                      row = 1:nrow(ordination))
  
  frame$total <- 0
  
  for(i in 1:nrow(frame)) {
    frame$total[i] <- densities$d[frame$x[i],frame$y[i], frame$z[i]]
  }
  
  frame$total <- 1 - frame$total/max(frame$total)
  
  # Normalize
  frame$total <- (frame$total - min(frame$total)) / (max(frame$total) - min(frame$total))
  frame$total <- (1 - lower.p) * frame$total + lower.p
  
  # Get sample
  set.seed(sample(1:1000, 1))
  sub_sample <- sample(frame$row, 
                       ceiling(nrow(frame)*proportion), 
                       prob = frame$total)
  
  #plot(ordination[,c(2, 3)])
  #plot(ordination[sub_sample,c(2, 3)])
  #points(ordination[!sub_sample,c(2, 3)], col = "red")
  
  return(sub_sample)
  
}