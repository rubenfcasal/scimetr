# ---------------------------------------
# Operaciones Tablas
# ---------------------------------------
# NOTAS:
# - Se podría emplear semi_join(x, y) keeps all observations in x that have a match in y.
#   Utiliza duplicated() internamente (no óptimo)

#' Get table identifiers
#'
#' \code{get.idDocs} returns the document identifiers (values of the \code{db$Docs$idd} variable)
#' corresponding to identifiers of authors (values of \code{db$Authors$ids}), categories (...), areas (...),
#' addresses (...) and/or sources (...). Multiple conditions are combined with &.
#' @param db Object of \code{\link{class}} \code{wos.db}, as returned by \code{\link{CreateDB.wos}}.
#' @param idAuthors optional; author identifiers
#' @param idCategories optional; categories identifiers
#' @param idAreas optional; areas identifiers
#' @param idAddresses optional; addresses identifiers
#' @param idSources optional; sources identifiers
#' @seealso \code{\link{CreateDB.wos}}, \code{\link[dplyr]{filter}}.
#' @export
get.idDocs <- function(db, idAuthors, idCategories, idAreas, idAddresses, idSources) {
# ---------------------------------------
  idDocs <- db$Docs$idd
  if(!missing(idSources)) {
    idDocs <- idDocs[db$Docs$ids %in% idSources]
  }
  if(!missing(idAuthors)) {
    idDocs <- idDocs[idDocs %in% with(db$AutDoc, idd[ida %in% idAuthors])]
  }
  if(!missing(idCategories)) {
    idDocs <- idDocs[idDocs %in% with(db$CatDoc, idd[idc %in% idCategories])]
  }
  if(!missing(idAreas)) {
    idDocs <- idDocs[idDocs %in% with(db$AreaDoc, idd[idra %in% idAreas])]
  }
  if(!missing(idAddresses)) {
    idDocs <- idDocs[idDocs %in% with(db$Addresses, idd[idad %in% idAddresses])]
    # idDocs <- idDocs[idDocs %in% with(db$AddAutDoc, idd[idad %in% idAddresses])]
  }
  return(idDocs)
}

#' @rdname get.idDocs
#' @param ... Logical predicates. Multiple conditions are combined with & (see
#' \code{\link[dplyr]{filter}})
#' @export
get.idAuthors <- function(db, ...) {
# ---------------------------------------
  Authors <- dplyr::filter(db$Authors, ...)
  result <- Authors$ida
  names(result) <- Authors$AF
  return(result)
}

#' @rdname get.idDocs
#' @export
get.idAddresses <- function(db, ...) {
# ---------------------------------------
  Addresses <- dplyr::filter(db$Addresses , ...)
  return(Addresses$idad)
}


#' @rdname get.idDocs
#' @export
get.idAreas <- function(db, ...) {
# ---------------------------------------
  Areas <- dplyr::filter(db$Areas, ...)
  result <- Areas$idra
  names(result) <- Areas$SC
  return(result)
}

#' @rdname get.idDocs
#' @export
get.idCategories <- function(db, ...) {
# ---------------------------------------
  Categories <- dplyr::filter(db$Categories, ...)
  result <- Categories$idc
  names(result) <- Categories$WC
  return(result)
}


#' @rdname get.idDocs
#' @export
get.idSources <- function(db, ...) {
# ---------------------------------------
  Sources <- dplyr::filter(db$Sources, ...)
  result <- Sources$ids
  # PENDIENTE: Establecer nombre fuente dependiendo de ST?
  # names(result) <- Sources$SO
  return(result)
}


# ---------------------------------------

#' @name scimetr-internals
#' @aliases .get.idDocs2
#' @title scimetr internal and secondary functions
#' @description Listed below are supporting functions for the major methods in scimetr.
#' @keywords internal
.get.idDocs2 <- function(db, idAuthors, idCategories, idAreas, idAddresses, idSources,
                        indices = TRUE) {
  ifalse <- logical(nrow(db$Docs))
  idDocs <- if(missing(idSources)) !ifalse else db$Docs$ids %in% idSources
  if(!missing(idAuthors)) {
    index <- ifalse
    index[with(db$AutDoc, idd[ida %in% idAuthors])] <- TRUE
    idDocs <- idDocs & index
  }
  if(!missing(idCategories)) {
    index <- ifalse
    index[with(db$CatDoc, idd[idc %in% idCategories])] <- TRUE
    idDocs <- idDocs & index
  }
  if(!missing(idAreas)) {
    index <- ifalse
    index[with(db$AreaDoc, idd[idra %in% idAreas])] <- TRUE
    idDocs <- idDocs & index
  }
  if(!missing(idAddresses)) {
    index <- ifalse
    index[with(db$AddAutDoc, idd[idad %in% idAddresses])] <- TRUE
    idDocs <- idDocs & index
  }
  if(indices) idDocs <- which(idDocs)
  return(idDocs)
}
