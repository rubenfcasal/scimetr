## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  fig.dim = c(8, 6),
  fig.align = "center", out.width = "80%"
)
rebuid <- FALSE # TRUE
# knitr::spin("scimetr.R", knit = FALSE)
# knitr::purl("scimetr.Rmd", documentation = 2)
options(digits = 5)

## -----------------------------------------------------------------------------
library(scimetr)

## ----eval=FALSE---------------------------------------------------------------
# # install.packages("remotes")
# remotes::install_github("rubenfcasal/scimetr")

## ----eval=FALSE---------------------------------------------------------------
# # Dependencies
# install.packages(c("dplyr", "tidyr", "stringr", "ggplot2", "scales", "rlang"))
# # Last released version
# install.packages("https://github.com/rubenfcasal/scimetr/releases/download/v1.1.0/scimetr_1.1.0.zip", repos = NULL)

## ----eval=FALSE---------------------------------------------------------------
# dir("UDC_2018-2023 (01-02-2024)", pattern = "*.txt")

## ----echo=FALSE---------------------------------------------------------------
# dput(dir("UDC_2014-2023 (01-02-2024)", pattern='*.txt'))
c(
  "savedrecs01.txt", "savedrecs02.txt", "savedrecs03.txt", "savedrecs04.txt",
  "savedrecs05.txt", "savedrecs06.txt", "savedrecs07.txt", "savedrecs08.txt",
  "savedrecs09.txt", "savedrecs10.txt"
)

## ----eval=FALSE---------------------------------------------------------------
# wos.data <- import_wos("UDC_2018-2023 (01-02-2024)")

## -----------------------------------------------------------------------------
wos.labels <- attr(wosdf, "variable.labels")
knitr::kable(head(data.frame(wos.labels)),
  col.names = c("Variable", "Label")
)

## -----------------------------------------------------------------------------
res2 <- summary_year(db)
res2

## ----plotdb, warning=FALSE, message=FALSE-------------------------------------
plot(db)

## -----------------------------------------------------------------------------
plot(res1)

plot(res1, pie = TRUE)

## -----------------------------------------------------------------------------
plot(res2)

plot(res2, boxplot = TRUE)

## -----------------------------------------------------------------------------
ida <- get_id_authors(db, AF == "Cao, Ricardo")
ida

## -----------------------------------------------------------------------------
idas <- get_id_authors(db, grepl("Cao", AF))
idas

## -----------------------------------------------------------------------------
get_id_areas(db, SC == "Mathematics")
get_id_areas(db, SC == "Mathematics" | SC == "Computer Science")

## -----------------------------------------------------------------------------
get_id_categories(db, grepl("Mathematics", WC))

## -----------------------------------------------------------------------------
idtest <- get_id_sources(db, SO == "TEST")
idtest
knitr::kable(t(db$Sources[idtest, ]),
  caption = "Test journal",
  col.names = c("Variable", "Value")
)
# get_id_sources(db, JI == 'Test')

## -----------------------------------------------------------------------------
idocs <- get_id_docs(db, id_authors = ida)
idocs

## -----------------------------------------------------------------------------
summary(db, idocs)

## -----------------------------------------------------------------------------
summary_year(db, idocs)

## -----------------------------------------------------------------------------
author_metrics(db, idas)

## ----eval=FALSE---------------------------------------------------------------
# dir("JCR_download", pattern = "*.xlsx")

## ----echo=FALSE---------------------------------------------------------------
# dput(dir("../../JCR_download", pattern='*.xlsx'))
c(
  "JCR_SCIE_2018.xlsx", "JCR_SCIE_2019.xlsx", "JCR_SCIE_2020.xlsx",
  "JCR_SCIE_2021.xlsx", "JCR_SCIE_2022.xlsx", "JCR_SCIE_2023.xlsx",
  "JCR_SSCI_2018.xlsx", "JCR_SSCI_2019.xlsx", "JCR_SSCI_2020.xlsx",
  "JCR_SSCI_2021.xlsx", "JCR_SSCI_2022.xlsx", "JCR_SSCI_2023.xlsx"
)

## ----db-jcr, eval=FALSE-------------------------------------------------------
# jcr <- db_jcr("JCR_download")

## ----add-jcr, eval=FALSE------------------------------------------------------
# dbjcr <- add_jcr(db, jcr)

## ----dbjcr--------------------------------------------------------------------
names(dbjcr)
class(dbjcr)

## -----------------------------------------------------------------------------
head(get_jcr(dbjcr))

## -----------------------------------------------------------------------------
head(get_jcr_cat(dbjcr, best = TRUE))

## -----------------------------------------------------------------------------
res1 <- summary(dbjcr)
res1
res2 <- summary_year(dbjcr)
res2

## ----plotdbjcr, message=FALSE, warning=FALSE, out.width = '100%'--------------
plot(dbjcr)

## ----plot.summary.jcr, fig.dim = c(10, 6), out.width = '100%'-----------------
plot(res1)

## ----plot.summary.year.jcr, out.width = '100%'--------------------------------
plot(res2)

## ----echo=FALSE---------------------------------------------------------------
all.labels <- data.frame(scimetr:::.all.labels)
DT::datatable(all.labels,
  colnames = c("Variable", "Label"), filter = "top",
  options = list(pageLength = 10)
)

