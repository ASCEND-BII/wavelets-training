figure_PRESS <- function(PRESS_results,
                         PRESS_range = range(PRESS_results),
                         title) {
  
  PRESS_means <- colMeans(PRESS_results)
  PRESS_sd <- apply(PRESS_results, 2, sd)
  min_opt <- apply(PRESS_results, 1, which.min) - 1 
  
  mode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  mode_opt <- mode(min_opt)
  
  frame <- data.table(Components = 1:length(PRESS_means),
                      PRESS = PRESS_means,
                      PRESS_sd = PRESS_sd)
  
  plot <- ggplot(frame, aes(x = Components, y = PRESS)) +
    geom_ribbon(aes(x = Components, ymin = PRESS-PRESS_sd, ymax = PRESS+PRESS_sd), 
                fill = "darkgreen",
                alpha = 0.2) +
    #geom_rect(aes(xmin= range(min_opt)[1], 
    #              xmax= range(min_opt)[2], 
    #              ymin= PRESS_range[1], 
    #              ymax= PRESS_range[2]), fill = "grey75", alpha = 0.2) +
    geom_line(colour = "darkgreen") +
    geom_vline(xintercept = mode_opt, linetype = "dashed") +
    coord_cartesian(xlim = c(1, length(PRESS_means)), 
                    ylim = PRESS_range, 
                    expand = FALSE) +
    labs(title = title,
         caption = paste0("The optimal is ", mode_opt, " components")) +
    theme_bw(base_size = 12)
    
  
  return(plot)
  
}
