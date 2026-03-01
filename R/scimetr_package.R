# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# scimetr package ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' scimetr: Analysis of Scientific Publication Data with R
#'
#' This package implements tools for quantitative research in scientometrics and bibliometrics.
#' It provides routines for importing bibliographic data from
#' Clarivate Web of Science (<https://www.webofscience.com/>) and performing bibliometric analysis.
#' For more information visit <https://rubenfcasal.github.io/scimetr/articles/scimetr.html>.
#' @aliases scimetr
#' @import graphics
#' @import dplyr
#' @import ggplot2
#' @import stringr
#' @importFrom stringi stri_trans_general
#' @importFrom rlang try_fetch abort
#' @importFrom scales trans_breaks
#' @importFrom stats var reorder median na.omit
#' @importFrom utils str read.delim setTxtProgressBar txtProgressBar
#' @importFrom grDevices dev.interactive devAskNewPage nclass.Sturges
#' @importFrom openxlsx readWorkbook
# @importFrom DBI dbConnect dbDisconnect dbListTables
# @importFrom RSQLite SQLite
# @name scimetr-package
# @docType package
"_PACKAGE"


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# wosdf data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# usethis::use_data(wosdf, overwrite = TRUE)


#' Mathematics UDC WoS Core Collection 2018-2023 data
#'
#' The data set consists of 293 publications corresponding to a WoS search by
#' the Affiliation field of *Universidade da Coruña (UDC)* (Affiliation:
#' OG = Universidade da Coruna) in the research area `"Mathematics"` (`SC = Mathematics`),
#' indexed in the Web of Science Core Collection:
#' \itemize{
#'   \item{Science Citation Index Expanded (SCI-EXPANDED).}
#'   \item{Social Sciences Citation Index (SSCI).}
#'   \item{Arts & Humanities Citation Index (A&HCI).}
#' }
#' in the years 2018-2023 (generated using the [import_wos] function).
#' @format
#' A data frame with 293 rows and 48 columns:
#' \describe{
#'   \item{PT}{Publication Type}
#'   \item{AU}{Author}
#'   \item{AF}{Author Full Name}
#'   \item{TI}{Article Title}
#'   \item{SO}{Source Title}
#'   \item{SE}{Book Series Title}
#'   \item{BS}{Book Series Subtitle}
#'   \item{LA}{Language}
#'   \item{DT}{Document Type}
#'   \item{C1}{Address}
#'   \item{C3}{Affiliation}
#'   \item{RI}{Researcher Ids}
#'   \item{OI}{ORCID}
#'   \item{NR}{Cited Reference Count}
#'   \item{TC}{Times Cited, WoS Core}
#'   \item{Z9}{Times Cited, All Databases}
#'   \item{U1}{180 Day Usage Count}
#'   \item{U2}{Since 2013 Usage Count}
#'   \item{PU}{Publisher}
#'   \item{PI}{Publisher City}
#'   \item{PA}{Publisher Address}
#'   \item{SN}{ISSN}
#'   \item{EI}{eISSN}
#'   \item{BN}{ISBN}
#'   \item{J9}{Journal Abbreviation}
#'   \item{JI}{Journal ISO Abbreviation}
#'   \item{PD}{Publication Date}
#'   \item{PY}{Publication Year}
#'   \item{VL}{Volume}
#'   \item{IS}{Issue}
#'   \item{PN}{Part Number}
#'   \item{SU}{Supplement}
#'   \item{SI}{Special Issue}
#'   \item{MA}{Meeting Abstract}
#'   \item{BP}{Start Page}
#'   \item{EP}{End Page}
#'   \item{AR}{Article Number}
#'   \item{DI}{DOI}
#'   \item{D2}{Book DOI}
#'   \item{EA}{Early Access Date}
#'   \item{PG}{Number of Pages}
#'   \item{WC}{WoS Category}
#'   \item{WE}{Web of Science Index}
#'   \item{SC}{Research Area}
#'   \item{HC}{Highly Cited Status}
#'   \item{HP}{Hot Paper Status}
#'   \item{DA}{Date of Export}
#'   \item{UT}{Unique WOS ID}
#' }
#' @source Clarivate Web of Science:
#' \url{https://www.webofscience.com/}.
#' @seealso [import_wos], [db_bib]
# @name wosdf
# @docType data
# @keywords datasets
# @examples
# str(wosdf)
"wosdf"


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# dbjcr data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# usethis::use_data(dbjcr, overwrite = TRUE)

#' Mathematics UDC 2018-2023 bibliographic database
#'
#' Bibliographic database with JCR metrics (a [wos.jcr-class] S3 object)
#' corresponding to a WoS search by the Affiliation field of *Universidade da Coruña (UDC)*
#' in the research area `"Mathematics"` during the years 2018–2023
#' (generated fom [wosdf] data set, using the functions [db_bib], [db_jcr]
#' and [add_jcr]).
# @name dbjcr
# @docType data
# @keywords datasets
# @examples
# print(dbjcr)
"dbjcr"


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# globalVariables ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## quiets concerns of R CMD check re: the .'s that appear in pipelines
# utils::globalVariables(c(".", "Sources", "Authors", "Docs",
#                          "Addresses", "Affiliations", "RI", "OI", "Categories",
#                          "WSIndex", "Areas", .wos.labels$name))
utils::globalVariables(c(
  "AF", "AFOI", "AFRI", "AU", "Areas", "Authors", "BS",
  "Categories", "Countries", "Country", "DT", "Documents", "EI", "Journals",
  "NR", "PG", "PT", "PY", "SC", "SN", "TC", "Types", "U2", "UT", "WC", "WE",
  "Years", "an", "ida", "idd", "ids", "ioi", "iri", "label", "name",
  ".", "HC", "HP", "J9", "JAI", "JIF", "JIFP", "JIFQ", "PU", "WCC", "WCP",
  "idc", "nbest3", "ndocjcr", "y", "id"
))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.onAttach <- function(libname, pkgname) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # pkg.info <- utils::packageDescription(pkgname, libname, fields = c("Title", "Version", "Date"))
  pkg.info <- drop(read.dcf(
    file = system.file("DESCRIPTION", package = "scimetr"),
    fields = c("Title", "Version", "Date")
  ))
  packageStartupMessage(
    paste0(" scimetr: ", pkg.info["Title"], ",\n"),
    paste0(" version ", pkg.info["Version"], " (built on ", pkg.info["Date"], ").\n"),
    paste0(" Copyright (C) UDC Rankings Group 2017-", format(as.Date(pkg.info["Date"]), "%Y"), ".\n"),
    " Type `help(scimetr)` for an overview of the package or\n",
    " visit https://rubenfcasal.github.io/scimetr.\n"
  )
}
