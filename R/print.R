#' @title Computes correlation of a dataframe
#'
#' @description
#' \code{print.ryx} prints a table of indepndent variables, their correlation with outcome variable,
#' and the significance of the correlations.
#'
#' @param x an object of class \code{ryx}
#'
#' @return a data frame with 3 components:
#' \describe{
#'  \item{varNames}{name of the independent variable}
#'  \item{rVal}{coefficient of the correlation between the independent variable and the outcome variable}
#'  \item{pVal}{p-value of the correlation}
#'  \item{sigif}{significance level of the correlation}
#' }
#' @method print ryx
#'
#' @examples
#' \dontrun{
#' x <- ryx(MASS::Boston, y = "medv")
#' print(x)
#' }

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
