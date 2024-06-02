figure_VIP <- function(VIP_results,
                       Wavelength_range = c(424:2374),
                       VIP_range = range(VIP_results),
                       title) {
  
  VIP_means <- colMeans(VIP_results)
  VIP_sd <- apply(VIP_results, 2, sd)
  
  frame <- data.table(Wavelength = Wavelength_range,
                      VIP = VIP_means,
                      VIP_sd = VIP_sd)
  
  plot <- ggplot(frame, aes(x = Wavelength, y = VIP)) +
    geom_ribbon(aes(x = Wavelength, ymin = VIP-VIP_sd, ymax = VIP+VIP_sd), 
                fill = "darkgreen",
                alpha = 0.2) +
    geom_line(colour = "darkgreen") +
    coord_cartesian(xlim = range(Wavelength_range), 
                    ylim = VIP_range, 
                    expand = FALSE) +
    xlab("Wavelength (nm)") + 
    labs(title = title) +
    theme_bw(base_size = 12)
  
  return(plot)
  
}
