
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Global Multidimensional Poverty Prediction Using World Bank Indicators: A Machine Learning Approach

This repository contains the codes and auxiliary files to reproduce the
results of the paper “Global Multidimensional Poverty Prediction Using
World Bank Indicators: A Machine Learning Approach” (2024) García
Arancibia, R.; Girela, I. & González, D.

## Introduction

To reproduce the results you can install the version of this project
from [GitHub](https://github.com/) with:

``` r
devtools::install_github("girelaignacio/globalMPI_prediction_using_WID")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
devtools::load_all()
#> ℹ Loading globalMPI.prediction.using.WID
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.
