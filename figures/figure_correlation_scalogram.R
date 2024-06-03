figure_correlation_scalogram <- function(trait, 
                                         reflectance, 
                                         scales = 1:10,
                                         title) {
  
  # Clean NA
  reflectance_clean <- reflectance[!is.na(trait), ]
  trait_clean <- trait[!is.na(trait)]
  
  #Apply wavelet
  transformation <- cwt(reflectance_clean, 
                        scales = scales,
                        summed_wavelet = FALSE,
                        threads = 1L)
  
  #Creation of the matrix of correlation
  final <- matrix(NA, nrow = dim(transformation)[3], 
                  ncol = dim(transformation)[2])
  
  #Implementation of the correlation
  for(i in 1:dim(transformation)[3]) { #Scale
    for(ii in 1:dim(transformation)[2]) { #Band
      
      scale_wavelength <- as.numeric(transformation[,ii,i])
      correlation <- cor(scale_wavelength, trait_clean, method = c("pearson"))
      final[i,ii] <- correlation
      
    }
  }
  
  final <- as.data.table(final)
  colnames(final) <- colnames(reflectance)
  final <- cbind(scales = scales, final)
  final_melt <- melt(final, 
                     id.vars = c("scales"),
                     variable.name = "Wavelength",
                     value.name = "Correlation")
  final_melt$Wavelength <- as.numeric(as.character(final_melt$Wavelength))
  
  
  plot <- ggplot(data = final_melt, aes(x= Wavelength, y= scales, fill = Correlation)) + 
    geom_raster() +
    ylab("Scales") +
    scale_fill_distiller("Correlation", palette = "RdBu", limits = c(-1.00, 1.00), expand = c(0,0)) +
    scale_y_continuous(breaks = scales) +
    labs(title = title) +
    coord_cartesian(expand = FALSE) +
    theme_bw()
    
  return(plot)
  
}
