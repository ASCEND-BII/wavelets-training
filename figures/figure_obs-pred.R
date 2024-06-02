figure_obs_pred <- function(observed,
                            predicted,
                            performance,
                            title) {
  
  mean_predicted <- rowMeans(predicted)
  frame <- data.table(Observed = observed,
                      Predicted = mean_predicted)
  
  plot <- ggplot(frame, aes(x = Predicted, y = Observed)) +
    geom_abline(aes(intercept = 0, slope = 1),
                colour = "grey", linetype = "dotted") +
    geom_point(shape = 21, col = "darkgreen") +
    geom_abline(data = performance,
                aes(intercept = mean(intercept), slope = mean(slope)),
                colour = "black") +
    coord_cartesian(xlim = range(frame), 
                    ylim = range(frame), 
                    expand = TRUE) +
    geom_text(#data = performance,
      aes(label = paste("R2 = ", format(round(mean(performance$R2), 2), nsmall = 2), "\n",
                        "BIAS = ", format(round(mean(performance$BIAS), 2), nsmall = 2), "\n",
                        "RMSE = ", format(round(mean(performance$RMSE), 2), nsmall = 2), "\n",
                        "%RMSE = ", format(round(mean(performance$perRMSE), 2), nsmall = 2)),
          x = Inf, 
          y = -Inf, 
          hjust = 1.1, 
          vjust = -0.2),
      colour = "black",
      size = 4,
      parse = FALSE) +
    labs(title = title)
    theme_bw()
    
    return(plot)
    
}
  