# scimetr: Analysis of Scientific Publication Data with R

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# WoS DB Summaries ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PENDIENTE:
#   Hacer version simplificada de summary para author_metrics
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Summaries of a bibliographic database
#'
#' [summary()] methods for bibliometric databases.
#' @method summary wos.db
#' @param object an object for which a summary is desired.
#' @param filter vector of document identifiers
#' (optional, usually a result of [get_id_docs()]).
#' @param index citation indexes.
#' @param nmax number of top levels.
#' @param ...	further arguments passed to or from other methods.
#' @returns A list of summary statistics (an object of class `summary.wos.db`
#' and/or `summary.wos.jcr`), which has specialized [print()] and [plot()] methods.
#' @seealso [db_bib()], [add_jcr()].
#' @examples
#' # Bibliographic database
#' db <- db_bib(wosdf)
#' summary(db)
#' # Bibliographic database with JCR metrics
#' summary(dbjcr)
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary.wos.db <- function(object, filter, index = c("H", "G"), nmax = 10, ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # PENDIENTE: Documentar resultados
  # object=db; filtered=FALSE; index = c("H", "G"); nmax = 10; docs <- object$Docs
  # OJO: supone que Docs esta ordenado por idd
  # El filtro puede ser numérico o lógico
  # stopifnot(is.numeric(filter))
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  filtered <- !missing(filter)
  docs <- if (filtered) object$Docs[filter, ] else object$Docs

  # Tipo de documentos
  doctypes <- as.matrix(table(droplevels(docs$DT)))
  dimnames(doctypes)[[2]] <- "Documents"

  # Citas
  TC <- docs$TC
  index <- match.arg(index, several.ok = TRUE)
  ind.tc <- NULL
  if (length(index)) {
    TC <- sort(TC, decreasing = TRUE)
    ind.tc <- list()
    if (!sum(TC)) {
      if ("H" %in% index) ind.tc$H <- 0
      if ("G" %in% index) ind.tc$G <- 0
    } else {
      pos <- seq_along(TC) # 1:length(TC)
      if ("H" %in% index) ind.tc$H <- max(which(TC >= pos))
      if ("G" %in% index) ind.tc$G <- max(which(pos^2 <= cumsum(TC)))
    }
    # oldClass(ind.tc) <- c("summaryDefault", "table")
  }

  # Documentos por autor
  ida <- with(
    object$AutDoc,
    if (filtered) ida[idd %in% filter] else ida
  )
  autdoc <- as.numeric(table(ida))

  # HC	Highly Cited Status, HP	Hot Paper Status
  status <- with(docs, c(
    "Highly Cited" = sum(HC),
    "Hot Papers" = sum(HP)
  ))
  # TOP
  ftable <- function(data, names, nmax) {
    top.table <- as.matrix(.stable(data, nmax = nmax))
    nres <- min(nmax, nrow(top.table))
    dnames <- dimnames(top.table)[[1]][1:nres]
    dnames <- names[as.numeric(dnames)]
    dimnames(top.table)[[2]] <- "Documents"
    dimnames(top.table)[[1]][1:nres] <- dnames
    oldClass(top.table) <- c("table")
    return(top.table)
  }

  # Top 10 Categories
  idc <- docs %>%
    select(idd, ids) %>%
    inner_join(object$CatSour, by = "ids", relationship = "many-to-many") %>%
    pull(idc)
  # idc <- with(object$CatSour, idc[ids %in% docs$ids]) # Indice de categorías
  top.cat <- ftable(idc, object$Categories$WC, nmax = nmax)

  # Top 10 Areas
  idra <- docs %>%
    select(idd, ids) %>%
    inner_join(object$AreaSour, by = "ids", relationship = "many-to-many") %>%
    pull(idra)
  # idra <- with(object$AreaSour, idra[ids %in% docs$ids]) # Indice de áreas
  top.area <- ftable(idra, object$Areas$SC, nmax = nmax)

  # Top 10 Journals
  ind.jour <- match(docs$ids, object$Sources$ids)
  top.jour <- with(
    object$Sources,
    ftable(docs$ids[PT[ind.jour] == "Journal"], JI, nmax = nmax)
  )
  # Top 10 Countries
  aux <- object$Addresses %>% dplyr::select(idd, Country)
  if (filtered) aux <- aux %>% dplyr::filter(idd %in% filter)
  top.coun <- as.matrix(.stable(dplyr::distinct(aux)$Country, nmax = nmax))
  dimnames(top.coun)[[2]] <- "Documents"
  oldClass(top.coun) <- "table"

  # International colaboration (with top country)
  # Pendiente: filtrar documentos que no tengan algún autor del top country?
  int.col <- aux %>%
    dplyr::filter(Country != dimnames(top.coun)[[1]][1]) %>%
    pull(idd) %>%
    unique() %>%
    length()

  # Top 10 autores
  ida <- with(object$AutDoc, ida[idd %in% docs$idd]) # Indice de autores
  top.aut <- ftable(ida, object$Authors$AF, nmax = nmax)

  # Result
  result <- list(
    ndocs = nrow(docs), naut = length(autdoc),
    yrange = range(docs$PY, na.rm = TRUE),
    nmax = nmax,
    # doctypes = with(docs, table(DT, PT)),
    doctypes = doctypes,
    summary.an = with(docs, summary(an)),
    summary.autdoc = summary(autdoc),
    summary.TC = summary(TC),
    index = ind.tc,
    status = status,
    int.col = int.col,
    top = list(cat = top.cat, area = top.area, jour = top.jour, coun = top.coun)
  )
  oldClass(result) <- "summary.wos.db"
  return(result)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname summary.wos.db
#' @method print summary.wos.db
#' @param x	an object used to select a method.
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.summary.wos.db <- function(x, ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("Number of documents:", x$ndocs, "\n")
  cat("Authors:", x$naut, "\n")
  if (diff(x$yrange)) {
    cat("Period:", paste(x$yrange, collapse = " - "), "\n")
  } else {
    cat("Period:", x$yrange[1], "\n")
  }
  cat("\nDocument types:\n")
  print(x$doctypes)
  cat("\nNumber of authors per document:\n")
  print(x$summary.an)
  cat("\nNumber of documents per author:\n")
  print(x$summary.autdoc)
  cat("\nNumber of times cited:\n")
  print(x$summary.TC)
  cat("\nIndexes:\n")
  print.table(x$index)
  cat("\nStatus:\n")
  print(x$status)
  # print(c('values', unlist(x$index)))
  # with(object$Docs, print(as.data.frame(table(TC)), row.names = FALSE))
  with(x$top, {
    cat("\nTop Categories:\n")
    print(cat)
    cat("\nTop Areas:\n")
    print(area)
    cat("\nTop Journals:\n")
    print(jour)
    cat("\nTop Countries:\n")
    print(coun)
  })
  cat("\nInternational colaboration documents (",
    dimnames(x$top$coun)[[1]][1], "): ", x$int.col, "\n",
    sep = ""
  )
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname summary.wos.db
#' @param db Object of \code{\link{class}} \code{wos.db}, as returned by [db_bib].
#' @param id_authors optional; author identifiers
#' @export
# PENDIENTE: Hacer version simplificada de summary
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
author_metrics <- function(db, id_authors) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  faut <- function(x) summary(db, get_id_docs(db, id_authors = x))$index
  t(sapply(id_authors, faut))
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname summary.wos.db
#' @method plot summary.wos.db
#' @param which	 if a subset of the plots is required,
#' specify a subset of the numbers \code{1:5}.
#' @param pie logical; if \code{TRUE}, pie charts are drawn.
#' @param others logical; if \code{FALSE}, only \code{nmax} top levels categories are shown.
#' @param accuracy numerical scalar; see \code{\link[scales]{label_number}(accuracy=.)}.
#' @param plot logical; if `TRUE` (default), the plots are drawn, otherwise only
#' the list of ggplot2 objects is (invisibly) returned.
#' @param ask	logical; if \code{TRUE}, the user is asked before each plot,
#' see \code{\link{par}(ask=.)}.
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot.summary.wos.db <- function(x, which = 1:5, pie = FALSE, others = !pie,
                                accuracy = 0.1, plot = TRUE,
                                ask = length(which) > 1 && interactive(), ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # x <- summary(wos.db); which = 1:5; pie = FALSE; others = !pie; accuracy = 0.1
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  nmax <- x$nmax
  ndf <- if (others) nmax + 1 else nmax
  result <- list()
  show <- rep(FALSE, 5)
  force(which)
  show[which] <- TRUE
  if (show[1]) {
    doc <- x$doctypes
    df.doc <- data.frame(Types = factor(rownames(doc)), Documents = doc, row.names = NULL)
    pobj <- if (!pie) {
      ggplot(data = df.doc, aes(x = reorder(Types, Documents), y = Documents)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(x = "Types") +
        scale_y_continuous(labels = scales::number_format(accuracy = accuracy))
    } else {
      ggplot(df.doc, aes(x = "", y = Documents, fill = Types)) +
        labs(x = "") +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start = 0)
    }
    result <- c(result, list(pobj))
  }
  if (show[2]) {
    df <- as.data.frame(x$top$cat)
    df <- df[1:min(ndf, nrow(df)), c(1, 3)]
    names(df) <- c("Categories", "Documents")
    pobj <- if (!pie) {
      ggplot(data = df, aes(x = Categories, y = Documents)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(x = "Categories") +
        scale_x_discrete(limits = rev(df$Categories)) +
        scale_y_log10(
          breaks = scales::trans_breaks("log10", function(x) 10^x),
          labels = scales::number_format(accuracy = accuracy)
        )
    } # + annotation_logticks(sides = "l")
    else {
      ggplot(df, aes(x = "", y = Documents, fill = Categories)) +
        labs(x = "") +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start = 0)
    }
    result <- c(result, list(pobj))
  }
  if (show[3]) {
    df <- as.data.frame(x$top$area)
    df <- df[1:min(ndf, nrow(df)), c(1, 3)]
    names(df) <- c("Areas", "Documents")
    pobj <- if (!pie) {
      ggplot(data = df, aes(x = Areas, y = Documents)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(x = "Areas") +
        scale_x_discrete(limits = rev(df$Areas)) +
        scale_y_log10(
          breaks = scales::trans_breaks("log10", function(x) 10^x),
          labels = scales::number_format(accuracy = accuracy)
        )
    } # + annotation_logticks(sides = "l")
    else {
      ggplot(df, aes(x = "", y = Documents, fill = Areas)) +
        labs(x = "") +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start = 0)
    }
    result <- c(result, list(pobj))
  }
  if (show[4]) {
    df <- as.data.frame(x$top$jour)
    df <- df[1:min(ndf, nrow(df)), c(1, 3)]
    names(df) <- c("Journals", "Documents")
    pobj <- if (!pie) {
      ggplot(data = df, aes(x = Journals, y = Documents)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(x = "Journals") +
        scale_x_discrete(limits = rev(df$Journals)) +
        scale_y_log10(
          breaks = scales::trans_breaks("log10", function(x) 10^x),
          labels = scales::number_format(accuracy = accuracy)
        )
    } # + annotation_logticks(sides = "l")
    else {
      ggplot(df, aes(x = "", y = Documents, fill = Journals)) +
        labs(x = "") +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start = 0)
    }
    result <- c(result, list(pobj))
  }
  if (show[5]) {
    df <- as.data.frame(x$top$coun)
    df <- df[1:min(ndf, nrow(df)), c(1, 3)]
    names(df) <- c("Countries", "Documents")
    pobj <- if (!pie) {
      ggplot(data = df, aes(x = Countries, y = Documents)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(x = "Countries") +
        scale_x_discrete(limits = rev(df$Countries)) +
        scale_y_log10(
          breaks = scales::trans_breaks("log10", function(x) 10^x),
          labels = scales::number_format(accuracy = accuracy)
        )
    } # + annotation_logticks(sides = "l")
    else {
      ggplot(df, aes(x = "", y = Documents, fill = Countries)) +
        labs(x = "") +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start = 0)
    }
    result <- c(result, list(pobj))
  }
  if (plot) {
    # if (!warning) {
    #   oldwarn <- options("warn" = -1) # Disable warnings
    #   on.exit(options(warn = oldwarn$warn))
    # }
    if (ask) {
      oask <- devAskNewPage(TRUE)
      on.exit(devAskNewPage(oask))
    }
    lapply(result, print)
  }
  return(invisible(result))
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# JCR bibliographic DB Summaries ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PENDIENTE:
#   plot.summary.wos.jcr
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Summaries of JCR bibliographic DB
# [summary] methods for [wos.jcr]-class...
#' 	further arguments (`...`) are passed to method `summary.wos.db()`.
#' @rdname summary.wos.db
#' @method summary wos.jcr
# @param object an object for which a summary is desired.
# @param filter vector of document identifiers (usually a result of [get_id_docs]).
#' @param all logical; if `TRUE`, the corresponding [wos.db-class] method
#' is called (additional parameters `...` are passed to it), otherwise only results
#' of JCR metrics are returned.
# @param ...	further arguments passed to or from other methods.
# @seealso [summary.wos.db].
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary.wos.jcr <- function(object, filter, all = FALSE, ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # PENDIENTE: Documentar resultados
  # object=dbjcr; all = TRUE
  # OJO: supone que Docs esta ordenado por idd
  # El filtro puede ser numérico o lógico
  # stopifnot(is.numeric(filter))
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  docjcr <- get_jcr(object, filter = filter)
  docjcrcat <- get_jcr_cat(object, filter = filter, best = TRUE)
  docjcrcat <- docjcr %>% left_join(docjcrcat, by = join_by(idd, ids, PY))
  attr(docjcrcat, "variable.labels") <- .all.labels[names(docjcrcat)]

  # Summary JCR metrics
  # summary.jcr <- summary(docjcr %>% select(-idd, -ids, -PY))
  summary.jcr <- docjcrcat %>%
    select(JIF:JAI, JIFP, WCP) %>%
    sapply(summary)
  # Artículos en JCR
  ndocjcr <- sum(!is.na(docjcrcat$JIFQ)) # sum(!is.na(docjcr$JIF))
  # Documents per quartile
  # tQ <- table(docjcrcat$JIFQ)
  # Artículos en Q1 (en la mejor de las categorías seleccionadas)
  nQ1 <- sum(docjcrcat$JIFP > 75, na.rm = TRUE)
  # Artículos en D1 (en la mejor de las categorías seleccionadas)
  nD1 <- sum(docjcrcat$JIFP > 90, na.rm = TRUE)
  # Artículos en 3 primeras posiciones (en la mejor de las categorías seleccionadas)
  nbest3 <- sum(docjcrcat$WCP <= 3, na.rm = TRUE)
  # Result
  result <- list(
    docjcrcat = docjcrcat,
    summary.jcr = summary.jcr, ndocjcr = ndocjcr,
    nQ1 = nQ1, nD1 = nD1, nbest3 = nbest3
  )
  oldClass(result) <- "summary.wos.jcr"
  if (all) {
    result <- c(summary.wos.db(object, filter, ...), result)
    oldClass(result) <- c("summary.wos.jcr", "summary.wos.db")
  }
  return(result)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname summary.wos.db
#' @method print summary.wos.jcr
#' @param digits minimal number of significant digits, see [print.default].
# @param x	an object used to select a method.
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.summary.wos.jcr <- function(x, digits = 2, all = TRUE, ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (all && inherits(x, "summary.wos.db")) {
    print.summary.wos.db(x, ...)
    cat("\n")
  }
  cat("JCR metrics:\n")
  print(x$summary.jcr, digits = digits)
  ndocjcr <- x$ndocjcr
  cat("\nDocuments in JCR: ", ndocjcr, " (",
    round(100 * ndocjcr / nrow(x$docjcrcat), digits), "%)\n",
    sep = ""
  )
  # Categories JCR
  cat("Documents in Q1: ", x$nQ1, " (",
    round(100 * x$nQ1 / ndocjcr, digits), "% of JCR)\n",
    sep = ""
  )
  cat("Documents in D1: ", x$nD1, " (",
    round(100 * x$nD1 / ndocjcr, digits), "% of JCR)\n",
    sep = ""
  )
  cat("Documents in top 3 journals: ", x$nbest3, " (",
    round(100 * x$nbest3 / ndocjcr, digits), "% of JCR)\n",
    sep = ""
  )
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname summary.wos.db
#' @method plot summary.wos.jcr
# @param which	 if a subset of the plots is required,
# specify a subset of the numbers \code{1:5}.
# @param plot logical; if `TRUE` (default), the plots are drawn, otherwise only
# the list of ggplot2 objects is (invisibly) returned.
# @param ask	logical; if \code{TRUE}, the user is asked before each plot,
# see \code{\link{par}(ask=.)}.
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot.summary.wos.jcr <- function(x, plot = TRUE, all = FALSE,
                                 ask = plot && all && interactive(), ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  docjcrcat <- x$docjcrcat %>% filter(!is.na(JIFP))
  pobj <- docjcrcat %>%
    tidyr::pivot_longer(c(JIFP, WCP), names_to = "var", values_to = "x") %>%
    ggplot(aes(x, fill = I("blue"))) +
    geom_histogram(bins = 2 * nclass.Sturges(docjcrcat$JIFP)) +
    scale_y_continuous(sec.axis = sec_axis(~ . / nrow(docjcrcat), labels = scales::label_percent())) +
    labs(x = "") +
    facet_wrap(~var, scales = "free")
  # Result
  if (all && inherits(x, "summary.wos.db")) {
    result <- plot.summary.wos.db(x, plot = FALSE, ...)
    result <- c(result, list(pobj))
    if (plot) {
      if (ask) {
        oask <- devAskNewPage(TRUE)
        on.exit(devAskNewPage(oask))
      }
      lapply(result, print)
    }
  } else {
    if (plot) print(pobj)
    result <- pobj
  }
  return(invisible(result))
}
