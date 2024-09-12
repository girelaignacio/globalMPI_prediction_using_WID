
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Global Multidimensional Poverty Prediction Using World Bank Indicators

This repository contains the codes and auxiliary files to reproduce the
results of the paper “Global Multidimensional Poverty Prediction Using
World Bank Indicators: A Machine Learning Approach” (2024) Girela, I.;
García Arancibia, R. & González, D.

## Abstract of the paper

Accurate and frequent measurements of multidimensional poverty are
crucial for effective poverty reduction programs. While the
Multidimensional Poverty Index (MPI) is a more comprehensive measure
than income-based ones, its availability is limited for many countries,
due to the cost of conducting multi-topic surveys for such measurement
purposes. This paper proposes and compares various statistical learning
methods to predict MPI values using the comprehensive and open-source
World Development Indicators database from the World Bank, in order to
reconstruct the multidimensional poverty series. We adapt these methods
to account for the bounded nature of MPI in the unit interval. Our
results demonstrate the importance of considering this bounded nature
for obtaining accurate predictions. By reconstructing MPI series from
2000 to 2021, we go beyond simply estimating the poverty trend,
identifying specific determinants of poverty changes, such as economic
crisis or effects of structural changes. Our flexible and reproducible
framework can be applied to any country for MPI estimation when data is
not available.

## Introduction

To reproduce the results you can install the version of this project
from [GitHub](https://github.com/) with:

``` r
devtools::install_github("girelaignacio/globalMPI_prediction_using_WID")
```

## Distribution of the folders

- package/

  - data/

    *Contains raw dataframes and processed data sets*

  - R/

    *Contains the functions used in the package*

  - scripts/

    *Contains the scripts for reproducing the experiments of the paper*

  - results/

    *Contains the saved results from the experiments*

  - figures/

    *Contains code and figures used in the paper*
