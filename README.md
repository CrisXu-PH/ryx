
# ryx

<!-- badges: start -->
<!-- badges: end -->

The goal of ryx is to calculate correlations between y and each variable in x

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

remotes::install_github(<github link>)
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

