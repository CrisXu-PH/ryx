
# ryx

![](man/figures/background.jpg)

This package deals with correlation among variables. 
ryx calculates correlations between one outcome variable and a group 
of independent variables, respectively, as well as significance of 
the correlations.

## Installation

From CRAN

``` r
install.packages("ryx")
```

For the development version

``` r
if (!require("remotes")){
  install.packages("remotes")
}

remotes::install_github("CrisXu-PH/ryx")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ryx)

library(MASS)
x <- ryx(Boston, y="medv")

print(x)
summary(x)
plot(x)
```

