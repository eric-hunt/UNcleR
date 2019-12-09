
<!-- README.md is generated from README.Rmd. Please edit that file -->

# UNcleR <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

<!-- badges: end -->

## Overview

`UNcleR` is an R package for parsing data exported from an Unchained
Labs *UNcle* instrument for downstream processing of biologics stability
data.

  - `import_DLSsum()` imports DLS data
  - `import_DLSspec()` imports DLS intensity or mass distribution
    spectra
  - `import_SLSsum()` imports SLS data
  - `import_SLSspec()` imports SLS spectra
  - `import_FLUORspec()` imports fluorescence spectra used for Tm calc
  - `join_SLS_DLS` joins SLS and DLS data together before metadata
    assignment
  - `assign_metadata` assigns metadata to an imported and parsed
    dataframe
  - `import_SLSspec` imports raw SLS spectral data at a specified
    wavelength

## Installation

<!--
You can install the released version of UNcleR from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("UNcleR")
```
-->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("eric-hunt/UNcleR")
```

<!--
## Example

This is a basic example which shows you how to solve a common problem:


```r
## library(UNcleR)
## basic example code
```
-->
