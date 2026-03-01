# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get table identifiers ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pendiente: ===========================
# id_categories, id_areas se filtran por fuentes
#   get_id_docs(db, id_sources = get_id_sources(id_categories = id_categories, id_areas = id_areas, id_wsi = id_wsi))
#   incluir internamente en get_id_docs?
# ... <data-masking> Expressions that return a logical value, and are defined in
# terms of the variables in the corresponding table (XXXX for function \code{get_idXXXX}). If multiple expressions are included, they are combined
# with the & operator. Only rows for which all conditions evaluate to TRUE are kept.


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get table identifiers
#'
#' Retrieve internal identifiers (entity keys) for the corresponding relational
#' table (authors, sources, categories, areas, ...). Any field in the target
#' table can be used as a condition, and multiple conditions are combined with `&`
#' (only IDs for which all conditions evaluate to TRUE are returned).
#' *Tidy evaluation* can be used to construct the logical expressions,
#' see [dplyr::filter()].
#' @param db Object of \code{\link{class}} \code{wos.db}, as returned by [db_bib].
#' @param id_authors optional; author identifiers (values of \code{db$Authors$ids}).
#' @param id_addresses optional; addresses identifiers.
#' @param id_sources optional; sources identifiers.
#' @param id_oi optional; ORCID identifiers or codes (values of \code{db$OI$OI}).
#' @param id_ri optional; RI identifiers or codes (values of \code{db$RI$RI}).
#' @param id_affiliations optional; affiliations identifiers or names (values of \code{db$Affiliations$C3}).
#' @param ... Logical predicates. Multiple conditions are combined with `&`
#' (see \code{\link[dplyr]{filter}}).
#' @returns
#' An integer vector of identifiers.
#' For instance, \code{get_id_docs} returns the document identifiers (values of
#' the \code{db$Docs$idd} variable) corresponding to identifiers of authors,
#' categories, areas, addresses and/or sources.
#' Logical expressions defined in terms of the variables in \code{db$Docs} can
#' also be used as arguments.
#' @seealso [db_bib], \code{\link[dplyr]{filter}}.
#' @export
get_id_docs <- function(db, ..., id_sources, id_authors, id_addresses,
                       id_oi, id_ri, id_affiliations) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Docs <- dplyr::filter(db$Docs, ...)
  id_docs <- Docs$idd
  if (!missing(id_sources)) {
    # if (!missing(id_categories) | !missing(id_areas)) ...
    id_docs <- id_docs[Docs$ids %in% id_sources]
  }
  if (!missing(id_authors)) {
    id_docs <- id_docs[id_docs %in% with(db$AutDoc, idd[ida %in% id_authors])]
  }
  if (!missing(id_addresses)) {
    id_docs <- id_docs[id_docs %in% with(db$Addresses, idd[idad %in% id_addresses])]
    # id_docs <- id_docs[id_docs %in% with(db$AddAutDoc, idd[idad %in% id_addresses])]
  }
  if (!missing(id_oi)) {
    if (is.character(id_oi)) id_oi <- with(db$OI, ioi[OI %in% id_oi])
    id_docs <- id_docs[id_docs %in% with(db$OIDoc, idd[ioi %in% id_oi])]
  }
  if (!missing(id_ri)) {
    if (is.character(id_ri)) id_ri <- with(db$RI, iri[RI %in% id_ri])
    id_docs <- id_docs[id_docs %in% with(db$RIDoc, idd[iri %in% id_ri])]
  }
  if (!missing(id_affiliations)) {
    if (is.character(id_affiliations)) {
      id_affiliations <- with(db$Affiliations, idaf[C3 %in% id_affiliations])
    }
    id_docs <- id_docs[id_docs %in% with(db$AffDoc, idd[idaf %in% id_affiliations])]
  }
  return(id_docs)
}


#' @rdname get_id_docs
#' @export
get_id_authors <- function(db, ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Authors <- dplyr::filter(db$Authors, ...) # "ida", "AU", "AF", "ioi"
  result <- Authors$ida
  names(result) <- Authors$AF
  return(result)
}


#' @rdname get_id_docs
#' @export
get_id_addresses <- function(db, ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Addresses <- dplyr::filter(db$Addresses, ...) # "idad", "idd", "C1", "Univ", "Country"
  return(Addresses$idad)
}


#' @rdname get_id_docs
#' @export
get_id_areas <- function(db, ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Areas <- dplyr::filter(db$Areas, ...) # "idra", "SC"
  result <- Areas$idra
  names(result) <- Areas$SC
  return(result)
}


#' @rdname get_id_docs
#' @export
get_id_categories <- function(db, ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Categories <- dplyr::filter(db$Categories, ...) # "idc", "WC"
  result <- Categories$idc
  names(result) <- Categories$WC
  return(result)
}


#' @rdname get_id_docs
#' @export
get_id_wsi <- function(db, ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  WSI <- dplyr::filter(db$WSI, ...) # "idwe", "WE"
  result <- WSI$idwe
  names(result) <- WSI$WE
  return(result)
}


#' @rdname get_id_docs
#' @param id_categories optional; categories identifiers or names (values of \code{db$Categories$WC})
#' @param id_areas optional; research area identifiers or names (values of \code{db$Areas$SC})
#' @param id_wsi optional; WoS Index identifiers or names (values of \code{db$WSI$WE})
#' @export
get_id_sources <- function(db, ..., id_categories, id_areas, id_wsi) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Sources <- dplyr::filter(db$Sources, ...)
  id_sources <- Sources$ids
  names(id_sources) <- Sources$SO # No es único any(duplicated(Sources$SO)) = TRUE
  if (!missing(id_categories)) {
    if (is.character(id_categories)) {
      id_categories <- with(db$Categories, idc[WC %in% id_categories])
    }
    id_sources <- id_sources[id_sources %in% with(db$CatSour, ids[idc %in% id_categories])]
  }
  if (!missing(id_areas)) {
    if (is.character(id_areas)) id_areas <- with(db$Areas, idra[SC %in% id_areas])
    id_sources <- id_sources[id_sources %in% with(db$AreaSour, ids[idra %in% id_areas])]
  }
  if (!missing(id_wsi)) {
    if (is.character(id_wsi)) id_wsi <- with(db$WSI, idwe[WE %in% id_wsi])
    id_sources <- id_sources[id_sources %in% with(db$SourWSI, ids[idwe %in% id_wsi])]
  }
  return(id_sources)
}


#' @rdname get_id_docs
#' @export
get_id_oi <- function(db, ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  OI <- dplyr::filter(db$OI, ...) # "ioi", "OI", "AFOI"
  result <- OI$ioi
  names(result) <- OI$OI
  return(result)
}


#' @rdname get_id_docs
#' @export
get_id_ri <- function(db, ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  RI <- dplyr::filter(db$RI, ...) # "iri", "RI", "AFRI"
  result <- RI$iri
  names(result) <- RI$RI
  return(result)
}


#' @rdname get_id_docs
#' @export
get_id_affiliations <- function(db, ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Affiliations <- dplyr::filter(db$Affiliations, ...) # "idaf", "C3"
  result <- Affiliations$idaf
  names(result) <- Affiliations$C3
  return(result)
}
