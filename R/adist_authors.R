# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# utils ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' @keywords internal
authors.short <- function(authors) {
  # authors <- stringi::stri_trans_general(authors, "upper; latin-ascii")
  sauthors <- str_split(authors, ', ', simplify = TRUE)
  family <- lapply(str_split(sauthors[, 1], ' |-'), function(x) toupper(x))
  name <- if (ncol(sauthors) == 1) as.list(rep("", nrow(sauthors))) else
    lapply(str_split(sauthors[, 2], ' |-'), function(x) substr(x, 1, 1))
  return(list(family = family, name = name))
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# match_authors ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' @keywords internal
dist_names0 <- function(x, Y)
  1 - sapply(Y, function(y) (mean(y %in% x) + mean(x %in% y))/2)

#' @keywords internal
adist_author0 <- function(sautname, sautfamily, stable,
                          weight = c(family = 0.9, name = 0.1)) {
  # sauthor <- authors.short(author)
  # stable <- authors.short(table)
  family.match <- dist_names0(sautfamily, stable$family)
  name.match <- dist_names0(sautname, stable$name)
  res <- weight[1]*family.match + weight[2]*name.match
  # names(res) <- list(author, table)
  return(res)
}


#' @keywords internal
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
match_authors <- function(authors, table, weight = c(family = 0.9, name = 0.1),
                          max.dist = 0.25, attr.dist = FALSE) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # authors <- res[, 1]; table <- autores0
  # weight = c(family = 0.9, name = 0.1); max.dist = 0.2; attr.dist = FALSE
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sauthors <- authors.short(authors)
  stable <- authors.short(table)
  itable <- seq_along(stable[[1]])
  naut <- length(sauthors[[1]])
  match <- rep(NA, naut)
  dist <- numeric(naut)
  for (i in seq_len(naut)) { # i <- 0; i <- i + 1
    sautname <- sauthors$name[[i]]
    sautfamily <- sauthors$family[[i]]
    adist <- adist_author0(sautname, sautfamily, stable, weight = weight)
    imatch <- which.min(adist)
    dist[i] <- adist[imatch]
    if (adist[imatch] <= max.dist) {
      match[i] <- itable[imatch]
      # Si lo encuentra lo elimina de la lista de búsqueda
      stable$family <- stable$family[-imatch]
      stable$name <- stable$name[-imatch]
      itable <- itable[-imatch]
      if (!length(itable)) break
    }
  }
  # Buscar si los no asignados están a menor distancia de los asignados
  nomatch <- which(is.na(match))
  nnomatch <- length(nomatch)
  if (nnomatch & sum(!is.na(match))) {
    stable <- authors.short(table[match[-nomatch]])
    # Pendiente poner en función (con reducción de tabla o no)
    dist2 <- numeric(nnomatch)
    for (i in seq_along(nomatch)) {
      sautname <- sauthors$name[[nomatch[i]]]
      sautfamily <- sauthors$family[[nomatch[i]]]
      adist <- adist_author0(sautname, sautfamily, stable, weight = weight)
      imatch <- which.min(adist)
      dist2[i] <- adist[imatch]
      if (dist2[i] < dist[imatch]) {
        match[nomatch[i]] <- match[imatch]
        match[imatch] <- NA
      }
    }
  }
  if (attr.dist) attr(match, 'dist') <- dist
  names(match) <- authors
  return(match)
}








