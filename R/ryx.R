#' @title Computes correlation of a data frame.
#'
#' @description
#' \code{ryx} calculates correlations between specified outcome variable and independent variables.
#'
#' @param data data frame
#' @param y variable used as the outcome variable in correlations
#' @param x a vector of independent variables
#'
#' @return a data frame with 3 components:
#' \describe{
#' \item{y}{name of the outcome variable}
#' \item{x}{a group of independent variables, each is paired with y to calculate correlation}
#' \item{df}{data frame for overall correlations}
#' }
#'
#' @details
#' This function computes correlation between the outcome variable and each independent variable in the specified
#' group. Each correlation's statistical significance is also computed.
#'
#' @seealso \link{print.ryx},\link{summary.ryx},\link{plot.ryx}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- ryx(MASS::Boston, y = "medv")
#' x
#' print(x)
#' summary(x)
#' plot(x)
#' }

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
