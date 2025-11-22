# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get table identifiers ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pendiente: ===========================
# idCategories, idAreas se filtran por fuentes
#   get_idDocs(db, idSources = get_idSources(idCategories = idCategories, idAreas = idAreas, idWSI = idWSI))
#   incluir internamente en get.idDocs?
# ... <data-masking> Expressions that return a logical value, and are defined in
# terms of the variables in the corresponding table (XXXX for function \code{get.idXXXX}). If multiple expressions are included, they are combined
# with the & operator. Only rows for which all conditions evaluate to TRUE are kept.


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get table identifiers
#'
#' \code{get.idDocs} returns the document identifiers (values of the \code{db$Docs$idd} variable)
#' corresponding to identifiers of authors (values of \code{db$Authors$ids}), categories (...), areas (...),
#' addresses (...) and/or sources (...). Multiple conditions are combined with &.
#' @param db Object of \code{\link{class}} \code{wos.db}, as returned by [CreateDB].
#' @param idAuthors optional; author identifiers
#' @param idAddresses optional; addresses identifiers
#' @param idSources optional; sources identifiers
#' @param idOI optional; ORCID identifiers or codes (values of \code{db$OI$OI})
#' @param idRI optional; RI identifiers or codes (values of \code{db$RI$RI})
#' @param idAffiliations optional; affiliations identifiers or names (values of \code{db$Affiliations$C3})
#' @seealso [CreateDB], \code{\link[dplyr]{filter}}.
#' @export
get.idDocs <- function(db, ..., idSources, idAuthors, idAddresses,
                       idOI, idRI, idAffiliations) {
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Docs <- dplyr::filter(db$Docs, ...)
  idDocs <- Docs$idd
  if(!missing(idSources)) {
    # if (!missing(idCategories) | !missing(idAreas)) ...
    idDocs <- idDocs[Docs$ids %in% idSources]
  }
  if(!missing(idAuthors)) {
    idDocs <- idDocs[idDocs %in% with(db$AutDoc, idd[ida %in% idAuthors])]
  }
  if(!missing(idAddresses)) {
    idDocs <- idDocs[idDocs %in% with(db$Addresses, idd[idad %in% idAddresses])]
    # idDocs <- idDocs[idDocs %in% with(db$AddAutDoc, idd[idad %in% idAddresses])]
  }
  if(!missing(idOI)) {
    if (is.character(idOI)) idOI <- with(db$OI, ioi[OI %in% idOI])
    idDocs <- idDocs[idDocs %in% with(db$OIDoc, idd[ioi %in% idOI])]
  }
  if(!missing(idRI)) {
    if (is.character(idRI)) idRI <- with(db$RI, iri[RI %in% idRI])
    idDocs <- idDocs[idDocs %in% with(db$RIDoc, idd[iri %in% idRI])]
  }
  if(!missing(idAffiliations)) {
    if (is.character(idAffiliations))
      idAffiliations <- with(db$Affiliations, idaf[C3 %in% idAffiliations])
    idDocs <- idDocs[idDocs %in% with(db$AffDoc, idd[idaf %in% idAffiliations])]
  }
  return(idDocs)
}


#' @rdname get.idDocs
#' @param ... Logical predicates. Multiple conditions are combined with & (see
#' \code{\link[dplyr]{filter}})
#' @export
get.idAuthors <- function(db, ...) {
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Authors <- dplyr::filter(db$Authors, ...) # "ida", "AU", "AF", "ioi"
  result <- Authors$ida
  names(result) <- Authors$AF
  return(result)
}


#' @rdname get.idDocs
#' @export
get.idAddresses <- function(db, ...) {
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Addresses <- dplyr::filter(db$Addresses , ...) # "idad", "idd", "C1", "Univ", "Country"
  return(Addresses$idad)
}


#' @rdname get.idDocs
#' @export
get.idAreas <- function(db, ...) {
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Areas <- dplyr::filter(db$Areas, ...) # "idra", "SC"
  result <- Areas$idra
  names(result) <- Areas$SC
  return(result)
}


#' @rdname get.idDocs
#' @export
get.idCategories <- function(db, ...) {
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Categories <- dplyr::filter(db$Categories, ...) # "idc", "WC"
  result <- Categories$idc
  names(result) <- Categories$WC
  return(result)
}


#' @rdname get.idDocs
#' @export
get.idWSI <- function(db, ...) {
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  WSI <- dplyr::filter(db$WSI, ...) # "idwe", "WE"
  result <- WSI$idwe
  names(result) <- WSI$WE
  return(result)
}


#' @rdname get.idDocs
#' @param idCategories optional; categories identifiers or names (values of \code{db$Categories$WC})
#' @param idAreas optional; research area identifiers or names (values of \code{db$Areas$SC})
#' @param idWSI optional; WoS Index identifiers or names (values of \code{db$WSI$WE})
#' @export
get.idSources <- function(db, ..., idCategories, idAreas, idWSI) {
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Sources <- dplyr::filter(db$Sources, ...)
  idSources <- Sources$ids
  names(idSources) <- Sources$SO # No es único any(duplicated(Sources$SO)) = TRUE
  if(!missing(idCategories)) {
    if (is.character(idCategories))
      idCategories <- with(db$Categories, idc[WC %in% idCategories])
    idSources <- idSources[idSources %in% with(db$CatSour, ids[idc %in% idCategories])]
  }
  if(!missing(idAreas)) {
    if (is.character(idAreas)) idAreas <- with(db$Areas, idra[SC %in% idAreas])
    idSources <- idSources[idSources %in% with(db$AreaSour, ids[idra %in% idAreas])]
  }
  if(!missing(idWSI)) {
    if (is.character(idWSI)) idWSI <- with(db$WSI, idwe[WE %in% idWSI])
    idSources <- idSources[idSources %in% with(db$SourWSI, ids[idwe %in% idWSI])]
  }
  return(idSources)
}


#' @rdname get.idDocs
#' @export
get.idOI <- function(db, ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  OI <- dplyr::filter(db$OI, ...) # "ioi", "OI", "AFOI"
  result <- OI$ioi
  names(result) <- OI$OI
  return(result)
}


#' @rdname get.idDocs
#' @export
get.idRI <- function(db, ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  RI <- dplyr::filter(db$RI, ...) # "iri", "RI", "AFRI"
  result <- RI$iri
  names(result) <- RI$RI
  return(result)
}


#' @rdname get.idDocs
#' @export
get.idAffiliations <- function(db, ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Affiliations <- dplyr::filter(db$Affiliations, ...) # "idaf", "C3"
  result <- Affiliations$idaf
  names(result) <- Affiliations$C3
  return(result)
}


