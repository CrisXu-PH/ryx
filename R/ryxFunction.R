#' @title ryx function
#'
#' @description
#' This package deals with correlation among variables.
#'
#' \code{ryx} calculates correlations between one outcome variable and a group
#' of independent variables, respectively, as well as significance of the correlations.
#' The final result is reported in the form of a correlation table.
#'
#' @details
#' Three generic functions (\code{print.ryx},\code{summary.ryx},\code{plot.ryx}) are
#' implemented. These functions can be used to report table of ryx class.
#'
#' @param data data frame
#' @param y variable used as the outcome variable in correlations
#' @param x a vector of independent variables
#'
#' @import stringr
#' @import ggplot2
#'
#' @export
#' @return a list of y, x, and a data frame reporting correlations of y with each variable in x respectively
#'
#' @examples
#' \dontrun{
#' # create a ryx object
#' x <- ryx(MASS::Boston, y = medv)
#'
#' # generic functions
#' print(x)
#' summary(x)
#' plot(x)
#'
#' # report error message if imput is not class ryx
#' print.ryx(mtcars)
#' }
#'

ryx <- function(data, y, x){
  if(missing(x)){
    x <- names(data)[sapply(data, class)=="numeric"]
    x <- setdiff(x, y)
  }
  df <- data.frame()
  for (var in x){
    res <- cor.test(data[[y]], data[[var]])
    df_temp <- data.frame(variable = var,
                          r = res$estimate,
                          p = res$p.value)
    df <- rbind(df, df_temp)
    df <- df[order(-abs(df$r)),]
  }

  df$sigif <- ifelse(df$p < .001, "***",
                     ifelse(df$p < .01, "**",
                            ifelse(df$p < .05, "*", " ")))
  results <- list(y=y, x=x, df=df)
  class(results) <- "ryx"
  return(results)
}

print.ryx <- function(x){
  if(!inherits(x, "ryx")) stop("Must be class 'ryx'")

  cat("Correlations of ",x$y," with \n")

  varNames <- x$df$variable
  rVal <- format(round(x$df$r,digits = 3), nsmall = 3)
  pVal <- ifelse(x$df$p < 2e-16, "< 2e-16", formatC(x$df$p, format = "e", digits = 2))
  sigif <- x$df$sigif

  df <- as.data.frame(cbind(varNames, rVal, pVal, sigif))
  colnames(df) <- c("variable", "r","p","sigif")

  df
}

summary.ryx <- function(x){
  if(!inherits(x, "ryx")) stop("Must be class 'ryx'")

  x_vars <- ""

  for (name in x$x){
    x_vars<- stringr::str_trim(paste(x_vars, name, sep = " "))
  }

  strOutput <- cat(
    "Correlating ",
    x$y,
    " with ",
    x_vars,
    "\nThe median absolute correlation was ",
    round(median(abs(x$df$r)), digits = 3),
    " with a range from ",
    round(min(x$df$r),digits = 3),
    " to ",
    round(max(x$df$r),digits = 3),
    "\n",
    length(subset(x$df, p < 0.05)$p),
    " out of ",
    length(x$x),
    " variables were significant at the p < 0.05 level."
  )

}

plot.ryx <- function(x){
  if(!inherits(x, "ryx")) stop("Must be class 'ryx'")

  df <- x$df[order(abs(as.numeric(x$df$r)), decreasing = TRUE),]
  df$direction <- factor(ifelse(df$r>=0, "positive", "negative"))
  df$r <- abs(df$r)

  ggplot2::ggplot(df, aes(x = r, y = reorder(variable, r)))+
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



