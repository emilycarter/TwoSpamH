#' Visualize the 2SpamH algorithm
#'
#' This function visualizes the 2SpamH algorithm.
#'
#' @param TwoSpamH.output The output from the function TwoSpamH when the \code{plot.data} is set to TRUE
#' @param to.plot A length 2 vector that indicates which principle components are to be plotted
#' @param title The plot title
#' @param variable The variable that is being plotted
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_color_discrete
#' @importFrom ggplot2 scale_size_continuous
#' @importFrom ggplot2 theme_bw
#'
#' @return A ggplot object that visualize the algorithm's reduced feature space.
#'
#' @export

TwoSpamH.plot = function(TwoSpamH.output,
                              to.plot = c(1,2),
                              title = "Validity Distribition in the Constructed Feature Space",
                              variable = "Filtered Variable"){

  df = TwoSpamH.output$df
  threshes.low = TwoSpamH.output$thresholds[[1]][to.plot]
  threshes.high = TwoSpamH.output$thresholds[[2]][to.plot]
  plot.vec = paste("pc", to.plot, sep = ".")
  title.ex = TwoSpamH.output$subj
  df$x = df %>% pull(plot.vec[1])
  df$y = df %>% pull(plot.vec[2])

  ggplot() +
    geom_point(data = df, aes(x = x, y = y, col = filter.new, size = filtered.var), alpha = 0.4) +
    annotate("rect",xmin=-Inf,xmax=threshes.low[[1]],ymin=-Inf, ymax=threshes.low[[2]], fill = "red",alpha = 0.2) +
    annotate("rect",xmin=threshes.high[[1]],xmax=Inf,ymin=threshes.high[[2]], ymax=Inf, fill = "#00BFC4",alpha = 0.2) +
    labs(title = title, x = paste("PC",to.plot[1]),
         y = paste("PC",to.plot[2])) +
    scale_color_manual(name = "",
                       values = c("filtered" = "red",
                                  "not filtered" = "#00BFC4"),
                       labels = c("filtered" = "Invalid",
                                  "not filtered" = "Valid")) +
    scale_size_continuous(name = variable) +
    theme_bw()

}
