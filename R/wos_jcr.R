# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# wos.jcr ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add JCR data to a bibliographic database
#'
#' Extends the bibliographic database by adding JCR metrics to sources, per year
#' and WoS category.
#' @param db a bibliographic database (a [wos.db-class] object; typically the
#' output of the function [CreateDB]).
#' @param jcrdb a JCR database (a [jcr.db-class] object; typically the
#' output of the function [CreateDBJCR]).
#' @return An S3 object of [class] `wos.jcr`.
#' A [wos.db-class] object with additional components `JCRSour` and `JCRCatSour`.
# \describe{
#   item{Docs}{}
# }
#' @seealso [CreateDB], [CreateDBJCR].
#' @aliases wos.jcr-class
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
AddDBJCR <- function(db, jcrdb) {
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # jcrdb <- jcr
  # Adapta a Sources y Categorías en la BBDD
  # Añade dos tablas nuevas "JCRSour", "JCRCatSour",
  # adaptando los índices ids de Sources e idc de Categorías a las de la BBDD,
  # y considerando solo las combinaciones de ids y PY de la BBDD
  # PENDIENTE: argumento warning y avisar si no se encuentran todas las
  # fuentes, categorías, o combinación con años
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(inherits(db, "wos.db"), inherits(jcrdb, "jcr.db"))

  # Filtrar y sustituir ids en jcrdb$JCRSour
  # ~~~~~~~~~~~~~~~~~~~~~~~~~
  # NOTA: Suponemos que jcrdb$Sources$ids es consecutivo
  is2 <- match(jcrdb$Sources$J9, db$Sources$J9)
  JCRSour <- jcrdb$JCRSour %>% mutate(ids = is2[ids]) %>% filter(!is.na(ids))

  # Filtrar y sustituir idc en JCRCatSour
  # ~~~~~~~~~~~~~~~~~~~~~~~~~
  # Solo disponemos de índices (edition): SSCI y SCIE
  # Pendiente AHCI (artes y humanidades) y ESCI (emergentes)
  # TENER EN CUENTA: Puede que alguna categoría en db$Categories no esté en db$JCRCatSour

  # NOTA: Suponemos que jcrdb$Categories$idc es consecutivo
  # JCRCatSour <- jcrdb$JCRCatSour %>% mutate(ids = is2[ids]) %>% filter(!is.na(ids))
  ic2 <- match(jcrdb$Categories$WC, str_to_upper(db$Categories$WC))
  JCRCatSour <- jcrdb$JCRCatSour %>% mutate(ids = is2[ids], idc = ic2[idc]) %>%
    filter(!is.na(idc), !is.na(ids))

  # Filtrar años, solo incluir combinaciones de años y revistas de db$Docs
  # ~~~~~~~~~~~~~~~~~~~~~~~~~
  PYselect <- db$Docs %>% select(ids, PY) %>% unique() # 193 únicos
  JCRSour <- JCRSour %>% semi_join(PYselect, by = join_by(ids, PY)) # 192
  # PYselect %>% anti_join(JCRSour, by = join_by(ids, PY))

  PYselect <- db$CatSour %>% select(ids, idc) %>%
    left_join(PYselect, by = join_by(ids), relationship = "many-to-many")
  JCRCatSour <- JCRCatSour %>% semi_join(PYselect, by = join_by(ids, idc, PY))

  # Resultado
  attr(JCRSour, "variable.labels") <- .all.labels[names(JCRSour)]
  attr(JCRCatSour, "variable.labels") <- .all.labels[names(JCRCatSour)]
  db$JCRSour <- JCRSour
  db$JCRCatSour <- JCRCatSour
  oldClass(db) <- c("wos.jcr", oldClass(db))
  return(db)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Generic methods wos.jcr ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname AddDBJCR
#' @method plot wos.jcr
#' @param filter vector of document identifiers (usually a result of [get.idDocs]).
#' @param plot logical; if `TRUE` (default), the plots are drawn, otherwise only
#' the ggplot2 object is (invisibly) returned.
#' @param all logical; if `TRUE`, function [plot.wos.db] is called (additional
#' parameters `...` are passed to this function), otherwise only a JCR metrics
#' plot is generated.
#' @param ask	logical; if `TRUE`, the user is asked before each plot
#' (see \code{\link{par}(ask=.)}).
# @seealso [plot.summary.wos.jcr], [plot.summary.year.jcr]
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot.wos.jcr <- function(x, filter, plot = TRUE, all = FALSE,
                         ask = plot && all && interactive(), ...) {
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ggplot(docjcr, aes(JIF, fill=I("blue"))) + geom_histogram(bins = 2*nclass.Sturges(docjcr$JIF)) +
  #         labs(x = labels["JIF"])
  # ggplot(docjcr, aes(JIF5, fill=I("blue"))) + geom_histogram() +
  #         labs(x = labels["JIF5"])
  # ggplot(docjcr, aes(JEFN, fill=I("blue"))) + geom_histogram() +
  #         labs(x = labels["JEFN"])
  # ggplot(docjcr, aes(JAI, fill=I("blue"))) + geom_histogram() +
  #         labs(x = labels["JAI"])
  # ~~~~~~~~~~~~~~~~~~~~
  docjcr <- get_jcr(x, filter) %>%
    tidyr::pivot_longer(JIF:JAI, names_to = "var", values_to = "x")
  pobj <- ggplot(docjcr, aes(x, fill=I("blue"))) +
    geom_histogram(bins = 2*nclass.Sturges(docjcr$x)) +
    labs(x = "") + facet_wrap(~ var, scales = "free")
  if (!all) {
    if (plot) print(pobj)
    result <- pobj
  } else {
    result <- plot.wos.db(x, filter = filter, plot = FALSE, ...)
    result <- c(result, list(pobj))
    if (plot) {
      if (ask) {
        oask <- devAskNewPage(TRUE)
        on.exit(devAskNewPage(oask))
      }
      lapply(result, print)
    }
  }
  return(invisible(result))
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# get_jcr ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get JCR metrics
#'
#' `get_jcr` combines document indexes with their source JCR metrics per year.
#' @param db a bibliographic database with JCR information (a [wos.jcr-class] object;
#' typically the output of the function [AddDBJCR]).
#' @param filter vector of document identifiers (usually a result of [get.idDocs]).
#' @seealso [AddDBJCR], [CreateDBJCR], [CreateDB].
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_jcr <- function(db, filter) {
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  docs <- if(!missing(filter)) db$Docs[filter, ] else db$Docs
  result <- docs %>% select(idd, ids, PY) %>%
              left_join(db$JCRSou, by = join_by(ids, PY))
  attr(result, "variable.labels") <- .all.labels[names(result)]
  return(result)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname get_jcr
#' @description
#' `get_jcr_cat` combines document indexes with their source JCR metrics per year
#' and WoS category.
#' @param best logical; if `TRUE` (default), only the results for the WoS category
#' with the best ranking for each document are returned.
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_jcr_cat <- function(db, filter, best = TRUE) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Pendiente: opción de filtrar categorías y WoS index
  # ~~~~~~~~~~~~~~~~~~~~
  docs <- if(!missing(filter)) db$Docs[filter, ] else db$Docs
  if (best) {
    bestjcrcat <- db$JCRCatSour %>%
      slice_min(JIFP, by = c(ids, PY), with_ties = FALSE)
    result <- docs %>% select(idd, ids, PY) %>%
      left_join(bestjcrcat, relationship = "many-to-one",
                by = join_by(ids, PY))
  } else
    result <- docs %>% select(idd, ids, PY) %>%
      left_join(db$JCRCatSour, relationship = "many-to-many",
                by = join_by(ids, PY))
  attr(result, "variable.labels") <- .all.labels[names(result)]
  return(result)
}


