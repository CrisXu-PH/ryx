#' @title Computes correlation of a data frame
#'
#' @description
#' \code{plot.ryx} plots magnitude and direction of each correlation.
#'
#' @param x an object of class \code{ryx}
#'
#' @return a line segment ggplot
#' @method plot ryx
#' @export
#'
#' @examples
#' \dontrun{
#' x <- ryx(MASS::Boston, y = "medv")
#' plot(x)
#' }

plot.ryx <- function(x){
  if(!inherits(x, "ryx")) stop("Must be class 'ryx'")

  df <- x$df[order(abs(as.numeric(x$df$r)), decreasing = TRUE),]
  df$direction <- factor(ifelse(df$r>=0, "positive", "negative"))
  df$r <- abs(df$r)

  ggplot2::ggplot(df, aes(x = r, y = reorder(df$variable, r)))+
    geom_segment(aes(xend = 0, yend = variable),
                 color = "grey")+
    geom_point(aes(color = direction), size = 3) +
    scale_x_continuous(breaks = seq(0.0, 1.0, 0.1),
                       limits = c(0, 1))+
    scale_color_manual(values = c("negative" = "red",
                                  "positive" = "blue"))+
    ylab("Variable")+
    xlab("Correlation (absolute value)")+
    labs(color = "Direction")+
    ggtitle(paste("Correlations with",x$y))+
    theme_bw()+
    theme(element_line(linetype = "dashed"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank())
}

