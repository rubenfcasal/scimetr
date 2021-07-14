## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(fig.height = 4, fig.width =7, 
                      fig.align = 'center', out.width = '100%')
# knitr::spin("scimetr.R", knit = FALSE)
# knitr::purl("scimetr.Rmd", documentation = 2)
# options(digits = 5)

## -----------------------------------------------------------------------------
library(scimetr)

## ----dependencies, eval=FALSE-------------------------------------------------
#  install.packages(c('dplyr', 'dbplyr','RSQLite', 'lazyeval', 'stringr', 'ggplot2', 'tidyr'))

## ----eval=FALSE---------------------------------------------------------------
#  # install.packages("devtools")
#  devtools::install_github("rubenfcasal/scimetr")

## ----eval=FALSE---------------------------------------------------------------
#  dir("UDC_2008-2017 (01-02-2019)", pattern='*.txt')

## ----echo=FALSE---------------------------------------------------------------
# dput(dir("UDC_2008-2017 (01-02-2019)", pattern='*.txt'))
c("savedrecs01.txt", "savedrecs02.txt", "savedrecs03.txt", "savedrecs04.txt", 
"savedrecs05.txt", "savedrecs06.txt", "savedrecs07.txt", "savedrecs08.txt", 
"savedrecs09.txt", "savedrecs10.txt", "savedrecs11.txt", "savedrecs12.txt", 
"savedrecs13.txt", "savedrecs14.txt", "savedrecs15.txt")

## ----eval=FALSE---------------------------------------------------------------
#  wos.txt <- ImportSources.wos("UDC_2008-2017 (01-02-2019)", other = FALSE)

## -----------------------------------------------------------------------------
# View(wosdf2) # En RStudio...
variable.labels <- attr(wosdf, "variable.labels")
knitr::kable(as.data.frame(variable.labels)) # caption = "Variable labels"

## ----wosdf2, cache=TRUE-------------------------------------------------------
db <- CreateDB.wos(wosdf2, label = "Mathematics_UDC_2008-2017 (01-02-2019)")
str(db, 1)

## ----summary, cache=TRUE------------------------------------------------------
res1 <- summary(db)
options(digits = 5)
res1

## -----------------------------------------------------------------------------
res2 <- summary_year(db)
res2

## ----plotdb, warning=FALSE, message=FALSE-------------------------------------
plot(db)

## ---- fig.align = 'left'------------------------------------------------------
plot(res1)

plot(res1, pie = TRUE)

## ---- fig.align = 'left'------------------------------------------------------
plot(res2)

plot(res2, boxplot = TRUE)

## -----------------------------------------------------------------------------
idAuthor <- get.idAuthors(db, AF == "Cao, Ricardo")
idAuthor

## -----------------------------------------------------------------------------
idAuthors <- get.idAuthors(db, grepl('Cao', AF))
idAuthors

## -----------------------------------------------------------------------------
get.idAreas(db, SC == 'Mathematics')
get.idAreas(db, SC == 'Mathematics' | SC == 'Computer Science')

## -----------------------------------------------------------------------------
get.idCategories(db, grepl('Mathematics', WC))

## -----------------------------------------------------------------------------
ijss <- get.idSources(db, SO == 'JOURNAL OF STATISTICAL SOFTWARE')
ijss
knitr::kable(db$Journals[ijss, ], caption = "JSS")
# get.idSources(db, JI == 'J. Stat. Softw.')

## -----------------------------------------------------------------------------
idocs <- get.idDocs(db, idAuthors = idAuthor)
idocs

## -----------------------------------------------------------------------------
summary(db, idocs)

## -----------------------------------------------------------------------------
summary_year(db, idocs)

## -----------------------------------------------------------------------------
TC.authors(db, idAuthors)

