#' This function visualizes the distribution of the test-statistic of choice,
#' using the output of any of the 'simulate_...()' functions.
#' @input_data input data: output of any of the 'simulate_...()' functions
#' @ref_val Any value you want to compare with the distribution of the
#' test-statistic.This simply plots a vertical line at that value. If you don't
#' want to add this line, chose "none" (default)
#' @import ggplot2
#' @import dplyr
#' @export
plot_distribution <- function(
input_data = mydf,
ref_val = "none"){

  # Select data with only selected test statistic
  if(attributes(input_data)$model_specifications$test == "regression slope"){
    dat = input_data %>%
    select(stat = 4)
  } else if(attributes(input_data)$model_specifications$test == "difference between regression slopes") {
    dat = input_data %>%
      select(stat = 2)
  } else if(attributes(input_data)$model_specifications$test == "difference between means") {
    dat = input_data %>%
      select(stat = 3)
  }

  # Get mean and sd of the test-statistic
  meanstat <- mean(dat$stat)
  sdstat   <- sd(dat$stat)

  # Get reference data from meta data of input file
  # either zero (when input data is for null-hypothesis test) or
  # the difference in regression slopes in the model fitted on the original data
  ref <- ifelse(attributes(input_data)$model_specifications$procedure == "null-hypothesis",
                attributes(input_data)$test_stat,
                0)

  # calculate probability of finding the reference slope or less/more
  # from fitted normal distribution
  if (ref < meanstat){
    my_prob <- pnorm(ref, mean = meanstat, sd = sdstat, lower.tail = T)
  } else {
    my_prob <- pnorm(ref, mean = meanstat, sd = sdstat, lower.tail = F)
  }

  # calculating probabilities
  if (ref < meanstat){
    my_prob2 <- dat %>%
      filter(stat <= ref) %>%
      summarise(n()/nrow(dat)) %>%
      as.numeric()
  } else {
    my_prob2 <- dat %>%
      filter(stat >= ref) %>%
      summarise(n()/nrow(dat)) %>%
      as.numeric()
  }

  # x-axis title
  xtitle = attributes(input_data)$model_specifications$test

  # Creating the histogram and adding a normal distribution
    suppressMessages(
      p <- ggplot(dat, aes(stat)) +
      geom_histogram(aes(y = ..density..),
                     fill = "#EFC000FF",
                     color = "white",
                     alpha = 0.5) +
      stat_function(fun = dnorm,
                    args = list(mean = meanstat,
                                sd   = sdstat),
                    color = '#868686FF',
                    lwd = 1) +
      ylab("density") +
      xlab(xtitle) +
      theme_bw() +
      theme(axis.title.x = element_text(size=13, color = "#EFC000FF"),
            axis.text.x  = element_text(size=13, color = "#EFC000FF"),
            axis.title.y = element_text(size=13, color = "#EFC000FF"),
            axis.text.y  = element_text(size=13, color = "#EFC000FF"))
      )

    # Creating probability area below curve
    # If the reference value is outside the data range of the calculated normal
    # curve, this is ignored
    suppressMessages(
      if(ref >= min(ggplot_build(p)$data[[2]]$x) & ref <= max(ggplot_build(p)$data[[2]]$x)){

        if (ref < meanstat){
          p <- p +
            ggplot_build(p)$data[[2]] %>%
            filter(x <= ref) %>%
            geom_area(data = ., aes(x = x, y = y),
                      fill = "#868686FF",
                      alpha = 0.5)

        } else {
          p <- p +
            ggplot_build(p)$data[[2]] %>%
            filter(x >= ref) %>%
            geom_area(data = ., aes(x = x, y = y),
                      fill = "#868686FF",
                      alpha = 0.5)
        }
      }
    )

    if(ref_val != "none"){
      suppressMessages(
        p <- p +
      geom_vline(aes(xintercept = ref_val),
               color = "black",
               linetype = "dashed")
      )
    }

    # Position for plotting the p-value inside the graph
    suppressMessages(
      xpos <-0.95*max(ggplot_build(p)$data[[2]]$x)
      )
    suppressMessages(
      ypos <- 0.95 * max(ggplot_build(p)$data[[1]]$y,
              ggplot_build(p)$data[[2]]$y)
      )

    # Adding label with probability and some layout
    suppressMessages(
      p <- p +
      theme(panel.border = element_rect(colour = "#EFC000FF", size = 1.5),
            axis.ticks = element_blank()) +
      annotate("text", xpos, ypos,
               label = paste(100*round(my_prob,3), "%"),
               hjust = "inward", vjust = "inward",
               fontface = "plain", size = 4.5, color = "#868686FF") +
      annotate("text", xpos, ypos*0.9,
               label = paste(100*round(my_prob2,3), "%"),
               hjust = "inward", vjust = "inward",
               fontface = "plain", size = 4.5, color = "#EFC000FF")
      )

  return(p)

}
