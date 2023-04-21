#' @keywords internal
outer_list <- function(X, Y, FUN, ...) {
  # https://rdrr.io/cran/rmngb/src/R/listOperations.R
  nX <- length(X)
  nY <- length(Y)
  x <- rep(X, nY)
  y <- rep(Y, each = nX)
  res <- mapply(FUN, x, y, ...)
  matrix(res, nrow = nX, dimnames = list(names(X), names(Y)))
}

# Proporción de veces que el primero no está en el segundo
#' @keywords internal
dist_names <- function(X, Y) {
  nX <- length(X)
  nY <- length(Y)
  x <- rep(X, nY)
  y <- rep(Y, each = nX)
  res <- mapply(function(a, t) mean(a %in% t), x, y)
  res <- matrix(1 - res, nrow = nX, dimnames = list(X, Y))
  return(res)
}

# No tiene en cuenta family name, first name
#' @keywords internal
adist_names <- function(names, table) {
  snames <- lapply(str_split(str_remove(names, ','), ' |-'), toupper)
  stable <- lapply(str_split(str_remove(table, ','), ' |-'), toupper)
  res <- dist_names(snames, stable)
  dimnames(res) <- list(names, table)
  return(res)
}


#' @keywords internal
authors.short <- function(authors) {
  sauthors <- str_split(authors, ', ', simplify = TRUE)
  family <- lapply(str_split(sauthors[, 1], ' |-'), function(x) toupper(x))
  name <- if (ncol(sauthors) == 1) as.list(rep("", nrow(sauthors))) else
            lapply(str_split(sauthors[, 2], ' |-'), function(x) substr(x, 1, 1))
  return(list(family = family, name = name))
}


#' Approximate Authors' Names Distances
#'
#' Compute an approximate string distance between character vectors containing authors' names
#' (of the form \code{"family name, first name"}).
#' The distance is a weighted proportion of the differences between (capitalized)
#' family names and between first name initial letters.
#' @param authors author names to be approximately matched.
#' @param table lookup table for matching.
#' @param weight weights associated with differences in family names and in first names.
#' @seealso \code{\link{match_authors}}.
#' @export
adist_authors <- function(authors, table, weight = c(family = 0.9, name = 0.1)) {
  sauthors <- authors.short(authors)
  stable <- authors.short(table)
  family.match <- dist_names(sauthors$family, stable$family)
  name.match <- dist_names(sauthors$name, stable$name)
  res <- weight[1]*family.match + weight[2]*name.match
  dimnames(res) <- list(authors, table)
  return(res)
}


#' Approximate Authors' Names Matching
#'
#' Approximate string matching between character vectors containing authors' names
#' (equivalents of R's native \code{\link{match}}).
#' @inheritParams adist_authors
#' @param max.dist maximum distance between strings to be matched.
#' @param attr.dist logical; indicating whether to optionally return the distances
#' (weighted proporpion of names coincidences) as the \code{"dist"} attribute of the return value.
#' @return Returns a vector of the positions of the closest matches of its first argument in its second.
#' When multiple matches with the same smallest distance exist, the first one is returned.
#' When no match is found \code{NA_integer_} is returned.
#' @seealso \code{\link{adist_authors}}.
#' @export
match_authors <- function(authors, table, weight = c(family = 0.9, name = 0.1),
                          max.dist = 0.2, attr.dist = FALSE) {
  adist <- adist_authors(authors, table, weight = weight)
  match <- drop(apply(adist, 1, which.min))
  dist <- adist[cbind(seq_len(nrow(adist)), match)]
  is.na(match) <- dist > max.dist
  if (attr.dist) attr(match, 'dist') <- dist
  return(match)
}

