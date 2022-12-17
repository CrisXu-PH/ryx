#' @title Computes correlation of a data frame
#'
#' @description
#' \code{summary.ryx} print a text message summarizing the independent variables, the range of correlation coefficient,
#' and the number of significantly correlated variables.
#'
#' @param x an object of class \code{ryx}
#'
#' @import stringr
#'
#' @return NULL
#' @method summary ryx
#' @export
#'
#' @examples
#' \dontrun{
#' x <- ryx(MASS::Boston, y = "medv")
#' summary(x)
#' }

summary.ryx <- function(x){
  if(!inherits(x, "ryx")) stop("Must be class 'ryx'")

  x_vars <- ""

  for (name in x$x){
    x_vars<- str_trim(paste(x_vars, name, sep = " "))
  }

  cat(
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
