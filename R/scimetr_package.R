# PENDIENTE:
#  - Categorías dentro de misma área de investigación?
#  - Categorías y áreas de investigación son por Sources?
# Otros:
# Número de ResearcherID e Identificador ORCID por autor
# Cargar etiquetas de campo (english...)
# Direcciones vacias which(!nzchar(wosdf$C1))
# Testear si hay fuentes duplicadas
# Definir clases de objetos: wos.bd


#' scimetr: Analysis of Scientific Publication Data with R
#'
#' This package implements tools for quantitative research in scientometrics and bibliometrics.
#' It provides routines for importing bibliographic data from
#' Thomson Reuters' Web of Knowledge (<https://www.webofknowledge.com>) and performing bibliometric analysis.
#' For more information visit <https://rubenfcasal.github.io/scimetr/articles/scimetr.html>.
#' @name scimetr-package
#' @aliases scimetr
#' @docType package
#' @import graphics
#' @import dplyr
#' @import ggplot2
#' @import stringr
#' @importFrom scales trans_breaks
#' @importFrom stats reorder median
#' @importFrom utils str read.delim setTxtProgressBar txtProgressBar
#' @importFrom grDevices dev.interactive devAskNewPage
#' @importFrom DBI dbConnect dbDisconnect dbListTables
#' @importFrom RSQLite SQLite
NULL



#' UDC WoS Core Collection 2015 data
#'
#' The data set consists of 856 registros vinculados a la UDC por el campo Organización-Nombre preferido
#' (Organization-Enhaced: OG = Universidade da Coruna)
#' de los siguientes índices de la Web of Science Core Collection:
#' \itemize{
#'   \item{Science Citation Index Expanded (SCI-EXPANDED).}
#'   \item{Social Sciences Citation Index (SSCI).}
#'   \item{Arts & Humanities Citation Index (A&HCI).}
#' }
#' correspondientes al año 2015.

#' @name wosdf
#' @docType data
#' @format A data frame with 856 observations on the following 62 variables:
#' \describe{
#'   \item{PT}{Publication type.}
#'   \item{AU}{Author.}
#'   \item{BA}{Book authors.}
#'   \item{BE}{Editor.}
#'   \item{GP}{Group author.}
#'   \item{AF}{Author full.}
#'   \item{BF}{Book authors fullname.}
#'   \item{CA}{Corporate author.}
#'   \item{TI}{Title.}
#'   \item{SO}{Publication name.}
#'   \item{SE}{Series title.}
#'   \item{BS}{Book series.}
#'   \item{LA}{Language.}
#'   \item{DT}{Document type.}
#'   \item{CT}{Conference title.}
#'   \item{CY}{Conference year.}
#'   \item{CL}{Conference place.}
#'   \item{SP}{Conference sponsors.}
#'   \item{HO}{Conference host.}
#'   \item{DE}{Keywords.}
#'   \item{ID}{Keywords Plus.}
#'   \item{AB}{Abstract.}
#'   \item{C1}{Addresses.}
#'   \item{RP}{Reprint author.}
#'   \item{EM}{Author email.}
#'   \item{RI}{Researcher id numbers.}
#'   \item{OI}{Orcid numbers.}
#'   \item{FU}{Funding agency and grant number.}
#'   \item{FX}{Funding text.}
#'   \item{CR}{Cited references.}
#'   \item{NR}{Number of cited references.}
#'   \item{TC}{Times cited.}
#'   \item{Z9}{Total times cited count .}
#'   \item{U1}{Usage Count (Last 180 Days).}
#'   \item{U2}{Usage Count (Since 2013).}
#'   \item{PU}{Publisher.}
#'   \item{PI}{Publisher city.}
#'   \item{PA}{Publisher address.}
#'   \item{SN}{ISSN.}
#'   \item{EI}{eISSN.}
#'   \item{BN}{ISBN.}
#'   \item{J9}{Journal.ISI.}
#'   \item{JI}{Journal.ISO.}
#'   \item{PD}{Publication date.}
#'   \item{PY}{Year published.}
#'   \item{VL}{Volume.}
#'   \item{IS}{Issue.}
#'   \item{PN}{Part number.}
#'   \item{SU}{Supplement.}
#'   \item{SI}{Special issue.}
#'   \item{MA}{Meeting abstract.}
#'   \item{BP}{Beginning page.}
#'   \item{EP}{Ending page.}
#'   \item{AR}{Article number.}
#'   \item{DI}{DOI.}
#'   \item{D2}{Book DOI.}
#'   \item{PG}{Page count.}
#'   \item{WC}{WOS category.}
#'   \item{SC}{Research areas.}
#'   \item{GA}{Document delivery number.}
#'   \item{UT}{Access number.}
#'   \item{PM}{Pub Med ID.}
#' }
#' @source Thomson Reuters' Web of Science: \cr
#' \url{http://www.webofknowledge.com}.
#' @keywords datasets
# @examples
# str(wosdf)
NULL


#' Mathematics UDC WoS Core Collection 2008-2017 data
#'
#' The data set consists of 389 registros vinculados a la UDC por el campo Organización-Nombre preferido
#' (Organization-Enhaced: OG = Universidade da Coruna)
#' del área de investigación 'Mathematics' (Research areas: SC = \*Mathematics\*)
#' de los siguientes índices de la Web of Science Core Collection:
#' \itemize{
#'   \item{Science Citation Index Expanded (SCI-EXPANDED).}
#'   \item{Social Sciences Citation Index (SSCI).}
#'   \item{Arts & Humanities Citation Index (A&HCI).}
#' }
#' correspondientes a los años 2008-2017.
#' @name wosdf2
#' @docType data
#' @format A data frame with 389 observations on the following 62 variables:
#' \describe{
#'   \item{PT}{Publication type.}
#'   \item{AU}{Author.}
#'   \item{BA}{Book authors.}
#'   \item{BE}{Editor.}
#'   \item{GP}{Group author.}
#'   \item{AF}{Author full.}
#'   \item{BF}{Book authors fullname.}
#'   \item{CA}{Corporate author.}
#'   \item{TI}{Title.}
#'   \item{SO}{Publication name.}
#'   \item{SE}{Series title.}
#'   \item{BS}{Book series.}
#'   \item{LA}{Language.}
#'   \item{DT}{Document type.}
#'   \item{CT}{Conference title.}
#'   \item{CY}{Conference year.}
#'   \item{CL}{Conference place.}
#'   \item{SP}{Conference sponsors.}
#'   \item{HO}{Conference host.}
#'   \item{DE}{Keywords.}
#'   \item{ID}{Keywords Plus.}
#'   \item{AB}{Abstract.}
#'   \item{C1}{Addresses.}
#'   \item{RP}{Reprint author.}
#'   \item{EM}{Author email.}
#'   \item{RI}{Researcher id numbers.}
#'   \item{OI}{Orcid numbers.}
#'   \item{FU}{Funding agency and grant number.}
#'   \item{FX}{Funding text.}
#'   \item{CR}{Cited references.}
#'   \item{NR}{Number of cited references.}
#'   \item{TC}{Times cited.}
#'   \item{Z9}{Total times cited count .}
#'   \item{U1}{Usage Count (Last 180 Days).}
#'   \item{U2}{Usage Count (Since 2013).}
#'   \item{PU}{Publisher.}
#'   \item{PI}{Publisher city.}
#'   \item{PA}{Publisher address.}
#'   \item{SN}{ISSN.}
#'   \item{EI}{eISSN.}
#'   \item{BN}{ISBN.}
#'   \item{J9}{Journal.ISI.}
#'   \item{JI}{Journal.ISO.}
#'   \item{PD}{Publication date.}
#'   \item{PY}{Year published.}
#'   \item{VL}{Volume.}
#'   \item{IS}{Issue.}
#'   \item{PN}{Part number.}
#'   \item{SU}{Supplement.}
#'   \item{SI}{Special issue.}
#'   \item{MA}{Meeting abstract.}
#'   \item{BP}{Beginning page.}
#'   \item{EP}{Ending page.}
#'   \item{AR}{Article number.}
#'   \item{DI}{DOI.}
#'   \item{D2}{Book DOI.}
#'   \item{PG}{Page count.}
#'   \item{WC}{WOS category.}
#'   \item{SC}{Research areas.}
#'   \item{GA}{Document delivery number.}
#'   \item{UT}{Access number.}
#'   \item{PM}{Pub Med ID.}
#' }
#' @source Thomson Reuters' Web of Science: \cr
#' \url{http://www.webofknowledge.com}.
#' @keywords datasets
# @examples
# str(wosdf)
NULL


## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")
    utils::globalVariables(c(".", "idd", "ida", "ids", "an", "Country",
    "PT", "AU", "BA", "BE", "GP", "AF", "BF", "CA", "TI", "SO",
    "SE", "BS", "LA", "DT", "CT", "CY", "CL", "SP", "HO", "DE", "ID",
    "AB", "C1", "RP", "EM", "RI", "OI", "FU", "FX", "CR", "NR", "TC",
    "Z9", "U1", "U2", "PU", "PI", "PA", "SN", "EI", "BN", "J9", "JI",
    "PD", "PY", "VL", "IS", "PN", "SU", "SI", "MA", "BP", "EP", "AR",
    "DI", "D2", "PG", "WC", "SC", "GA", "UT", "PM",
    "ISS", "MC", "MP", "PC", "RD", "ST", "TD", "year",
    "Types", "Documents", "Categories", "Areas", "Sources", "Journals",
    "Countries", "Authors", "Years", ".wos.variable.labels"))


#--------------------------------------------------------------------
.onAttach <- function(libname, pkgname){
  #--------------------------------------------------------------------
  #   pkg.info <- utils::packageDescription(pkgname, libname, fields = c("Title", "Version", "Date"))
  pkg.info <- drop( read.dcf( file = system.file("DESCRIPTION", package = "scimetr"),
                              fields = c("Title", "Version", "Date") ))
  packageStartupMessage(
    paste0(" scimetr: ", pkg.info["Title"], ",\n"),
    paste0(" version ", pkg.info["Version"], " (built on ", pkg.info["Date"], ").\n"),
    paste0(" Copyright (C) UDC Rankings Group 2017-", format(as.Date(pkg.info["Date"]), "%Y"), ".\n"),
    " Type `help(scimetr)` for an overview of the package or\n",
    " visit https://rubenfcasal.github.io/scimetr.\n")
}
