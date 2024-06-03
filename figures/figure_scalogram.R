figure_scalogram <- function(signal,
                             scales = 1:10,
                             wavelenghts = 400:2500) {
  
  
  #Apply wavelet
  transformation <- cwt(signal, 
                        scales = scales,
                        summed_wavelet = FALSE,
                        threads = 1L)
  
  transformation <- t(transformation[1,,])
  transformation <- as.data.table(transformation)
  colnames(transformation) <- as.character(wavelenghts)
  transformation <- cbind(scales = scales, transformation)
  
  #Melting 
  final_melt <- melt(transformation, 
                     id.vars = c("scales"),
                     variable.name = "Wavelength",
                     value.name = "Value")
  final_melt$Wavelength <- as.numeric(as.character(final_melt$Wavelength))
  values_max <- max(abs(range(final_melt$Value)))
  final_melt <- final_melt[Wavelength >= (min(wavelenghts) + max(scales)),]
  final_melt <- final_melt[Wavelength <= (max(wavelenghts) - max(scales)),]
  quantile <- final_melt
  
  plot <- ggplot(data = final_melt, aes(x= Wavelength, y= scales, fill = Value)) + 
    geom_raster() +
    ylab("Scales") +
    #scale_y_continuous(trans='log2') +
    #scale_fill_distiller("Value", palette = "RdBu", expand = c(0,0), limits = c(-values_max, values_max)) +
    scale_fill_viridis_c(option = "turbo", 
                         limits = quantile(final_melt$Value, c(0.05, 0.95)),
                         oob = scales::squish) +
    #scale_y_continuous(breaks = scales) +
    coord_cartesian(expand = FALSE) +
    theme_bw()
  
  return(plot)
  
}
