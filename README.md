# scimetr: Analysis of Scientific Publication Data with R

This package implements tools for quantitative research in scientometrics and bibliometrics. 
It provides routines for importing bibliographic data from Clarivate Web of Science (<https://www.webofscience.com/wos/>) and performing bibliometric analysis. 

For more information visit <https://rubenfcasal.github.io/scimetr/articles/scimetr.html>. 

## Installation

`scimetr` is available from CRAN, but you can install the development
version from github with:

``` r
# install.packages("remotes")
remotes::install_github("rubenfcasal/scimetr")
```

Alternatively, Windows users may install the corresponding *scimetr_X.Y.Z.zip* file in the [releases section](https://github.com/rubenfcasal/scimetr/releases/latest) of the github repository.

``` r
install.packages('https://github.com/rubenfcasal/scimetr/releases/download/v1.2.0/scimetr_1.2.0.zip', 
                 repos = NULL)
``` 

<!-- 
devtools::install(build_vignettes = TRUE)
pkgdown::build_site()
pkgdown::build_articles()
devtools::check(cran = TRUE, remote = TRUE, manual = TRUE)
# Avoid problems with "Onedrive - Universidade da Coruña" 
devtools::check(getwd())
pkgdown::build_site(getwd())
-->

