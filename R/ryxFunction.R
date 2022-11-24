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

library(MASS)
x <- ryx(Boston, y="medv")

print.ryx <- function(x){
  if(!inherits(x, "ryx")) stop("Must be class 'ryx'")

  library(DT)
  cat("Correlations of ",x$y," with \n")

  varNames <- x$df$variable
  rVal <- format(round(x$df$r,digits = 3), nsmall = 3)
  pVal <- ifelse(x$df$p < 2e-16, "< 2e-16", formatC(x$df$p, format = "e", digits = 2))
  sigif <- x$df$sigif

  df <- as.data.frame(cbind(varNames, rVal, pVal, sigif))
  df <- df[order(abs(as.numeric(df$rVal)), decreasing = TRUE),]
  colnames(df) <- c("variable", "r","p","sigif")

  df
}

# print(x)

summary.ryx <- function(x){
  if(!inherits(x, "ryx")) stop("Must be class 'ryx'")

  library(stringr)
  x_vars <- ""

  for (name in x$x){
    x_vars<- str_trim(paste(x_vars, name, sep = " "))
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

# summary(x)

plot.ryx <- function(x){
  if(!inherits(x, "rys")) stop("Must be class 'ryx'")

  plot(x$df)
}

plot(x)
