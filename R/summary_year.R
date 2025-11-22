# scimetr: Analysis of Scientific Publication Data with R
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Summaries per year ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Summaries per year
#'
#' The `summary_year` methods return summaries by year of a bibliometric database.
#' @param object an object for which a summary is desired.
#' @param ...	further arguments passed to or from other methods.
#' @seealso [CreateDB], [AddDBJCR].
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary_year <- function(object, ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  UseMethod("summary_year")
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# WoS DB summaries per year ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname summary_year
#' @method summary_year wos.db
#' @param filter vector of document identifiers
#' (optional, usually a result of [get.idDocs]).
#' @export
# PENDIENTE: RESULTADOS COMO TABLAS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary_year.wos.db <- function(object, filter, ...) {
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # object=db; docs <- object$Docs; filter=idocs
  # stopifnot(is.numeric(filter))
  docs <- if(!missing(filter)) object$Docs[filter, ] else object$Docs

  # Convertir años a factor para que aparezcan consecutivos
  docs$PY <- with(docs, factor(PY, levels = seq(min(PY), max(PY))))

  # Years
  Doc.PY <- as.matrix(table(docs$PY))
  dimnames(Doc.PY)[[2]] <- "Documents"

  # Authors per document per year
  AuDoc.PY <- docs %>% group_by(PY, .drop = FALSE)  %>%
      summarise(avg = mean(an), med = median(an)) %>%
      replace(is.na(.), 0)
      # mutate(across(everything(), ~replace_na(.x, 0)))
  names(AuDoc.PY) <- c("PY", "Mean", "Median")

  # Times cited
  TC.PY <- docs %>% group_by(PY, .drop = FALSE)  %>%
      summarise(n = sum(TC), avg = mean(TC), med = median(TC)) %>%
      replace(is.na(.), 0)
  names(TC.PY) <- c("PY", "Cites", "Mean", "Median")

  # HC	Highly Cited Status, HP	Hot Paper Status
  status.PY <- docs %>% group_by(PY, .drop = FALSE)  %>%
      summarise(nhc = sum(HC), nhp = mean(HP)) %>%
      replace(is.na(.), 0)
  names(status.PY) <- c("PY", "Highly Cited", "Hot Paper")

  result <- list(Doc.PY = Doc.PY,
                 AuDoc.PY = as.data.frame(AuDoc.PY),
                 # DocAu.PY = as.data.frame(DocAu.PY),
                 TC.PY = as.data.frame(TC.PY),
                 status.PY = as.data.frame(status.PY),
                 docs = docs[c("PY", "an", "TC")]
                 # , nDocAu = nDocAu
            )
  oldClass(result) <- "summary.year.wos"
  return(result)

}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname summary_year
#' @method print summary.year.wos
#' @param x	an object used to select a method.
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.summary.year.wos <- function(x, ...)  {
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("Annual Scientific Production:\n")
  print(x$Doc.PY, row.names=FALSE)
  cat("\nAnnual Authors per Document:\n")
  print(x$AuDoc.PY, row.names=FALSE)
  cat("\nAnnual Times Cited:\n")
  print(x$TC.PY, row.names=FALSE)
  cat("\nStatus:\n")
  print(x$status.PY, row.names=FALSE)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname summary_year
#' @method plot summary.year.wos
#' @param which	 if a subset of the plots is required,
#' specify a subset of the numbers \code{1:3}.
#' @param boxplot logical; if \code{TRUE}, boxplots are drawn (for plots from 2 to 3).
#' @param plot logical; if `TRUE` (default), the plots are drawn, otherwise only
#' the list of ggplot2 objects is (invisibly) returned.
#' @param ask	logical; if \code{TRUE}, the user is asked before each plot,
#' see \code{\link{par}(ask=.)}.
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot.summary.year.wos <- function(x, which = 1:3, boxplot = FALSE, plot = TRUE,
                        ask = plot && length(which) > 1 && interactive(), ...) {
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  result <- list()
  show <- rep(FALSE, 3)
  show[which] <- TRUE
  if(show[1] & !boxplot) {
    Doc.PY <- data.frame(Types = factor(rownames(x$Doc.PY)), Documents = x$Doc.PY, row.names = NULL)
    names(Doc.PY) <- c("Years","Documents")
    pobj <- ggplot(data=Doc.PY, aes(x=Years, y=Documents, group=1)) +
      geom_line(color = "blue")+
      geom_point(color = "blue") + ggtitle("Scientific Production")
    result <- c(result, list(pobj))
  }
  if(show[2]) {
    names(x$AuDoc.PY) <- c("Years","Authors","Median")
    pobj <- if (!boxplot)
      ggplot(data=x$AuDoc.PY, aes(x=factor(Years), y=Authors, group=1)) +
      geom_line(color = "blue")+
      geom_point(color = "blue") + ggtitle("Authors per Document") +
      xlab("Years")
    else
    ggplot(x$docs, aes(x = factor(x$docs$PY), y = x$docs$an)) +
      geom_boxplot(fill = "#4271AE", colour = "#1F3552") +
      xlab("Years") + ylab("Authors per document") +
      scale_y_log10() +
      annotation_logticks(sides='l')
    result <- c(result, list(pobj))
  }
  if(show[3]) {
    names(x$TC.PY) <- c("Years","TC","Median")
    pobj <- if (!boxplot)
      ggplot(data=x$TC.PY, aes(x=factor(Years), y=TC, group=1)) +
      geom_line(color = "blue")+
      geom_point(color = "blue") + ggtitle("Times Cited") +
      xlab("Years")
    else
    ggplot(x$docs, aes(x = factor(x$docs$PY), y = x$docs$TC)) +
      geom_boxplot(fill = "#4271AE", colour = "#1F3552") +
      xlab("Years") + ylab("Times Cited") +
      scale_y_sqrt(breaks = scales::trans_breaks("sqrt", function(x) x^2))
      # scale_y_log10() +
      # annotation_logticks(sides='l')
      # scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x))
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
# JCR WoS DB Summaries per year ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname summary_year
#' @method summary_year wos.jcr
# @param filter vector of document identifiers (usually a result of [get.idDocs]).
#' @param all logical; if `TRUE`, the corresponding [wos.db-class] method
#' is called (additional parameters `...` are passed to it), otherwise only results
#' of JCR metrics are returned.
#' @export
# PENDIENTE: RESULTADOS COMO TABLAS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary_year.wos.jcr <- function(object, filter, all = FALSE, ...) {
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  docjcr <- get_jcr(object, filter = filter)
  docjcrcat <- get_jcr_cat(object, filter = filter, best = TRUE)
  docjcrcat <- docjcr %>% left_join(docjcrcat, by = join_by(idd, ids, PY))
  # Mean of JCR metrics
  mean.jcr <- docjcrcat %>%
    summarise(across(c(JIF:JAI, JIFP, WCP), \(x) mean(x, na.rm = TRUE)), .by = PY) %>%
    arrange(PY)
  # Documents in JCR categories
  n.jcr <- docjcrcat %>%  summarise(
    ndocjcr = sum(!is.na(JIFQ)),          # Documents in JCR
    nQ1 = sum(JIFP > 75, na.rm = TRUE),   # Artículos en Q1
    nD1 = sum(JIFP > 90, na.rm = TRUE),   # Artículos en D1
    nbest3 = sum(WCP <= 3, na.rm = TRUE), # Artículos en 3 primeras posiciones
    .by = PY) %>%
    arrange(PY)
  # Result
  result <- list(mean.jcr = mean.jcr, n.jcr = n.jcr)
  oldClass(result) <- "summary.year.jcr"
  if (all) {
    result <- c(summary_year.wos.db(object, filter, ...), result)
    oldClass(result) <- c("summary.year.jcr", "summary.year.wos")
  }
  return(result)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname summary_year
#' @method print summary.year.jcr
# @param x	an object used to select a method.
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.summary.year.jcr <- function(x, digits = 3, all = TRUE, ...)  {
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (all && inherits(x, "summary.year.wos")) {
    print.summary.year.wos(x, ...)
    cat("\n")
  }
  cat('Mean of JCR metrics:\n')
  print(x$mean.jcr, digits = digits, row.names = FALSE)

  nms <- x$n.jcr$PY
  cat("\nDocuments in JCR:\n")
  ndocjcr <- x$n.jcr$ndocjcr
  names(ndocjcr) <- nms
  print(ndocjcr)

  # Categories JCR
  cat("\nDocuments in Q1:\n")
  nQ1 <- x$n.jcr$nQ1
  names(nQ1) <- nms
  print(nQ1)
  cat("\nDocuments in D1:\n")
  nD1 <- x$n.jcr$nD1
  names(nD1) <- nms
  print(nD1)
  cat("\nDocuments in top 3 journals:\n")
  nbest3 <- x$n.jcr$nbest3
  names(nbest3) <- nms
  print(nbest3)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname summary_year
#' @method plot summary.year.jcr
#' @param plot logical; if `TRUE` (default), the plots are drawn, otherwise only
#' the list of ggplot2 objects is (invisibly) returned.
#' @param ask	logical; if \code{TRUE}, the user is asked before each plot,
#' see \code{\link{par}(ask=.)}.
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot.summary.year.jcr <- function(x, plot = TRUE, all = FALSE,
                                  ask = plot && interactive(), ...) {
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # x <- res2; plot = TRUE; all = TRUE
  if (all && inherits(x, "summary.year.wos")) {
    result <- plot.summary.year.wos(x, plot = FALSE, ...)
  } else
    result <- list()
  pobj <- x$mean.jcr %>% tidyr::pivot_longer(JIF:WCP, names_to = "var", values_to = "y") %>%
    mutate(var = factor(var, levels = names(x$mean.jcr)[-1])) %>%
    ggplot(aes(x = factor(PY), y = y, group = 1)) +
      facet_grid(var ~ ., scales = "free") +
      geom_line(color = "blue")+
      geom_point(color = "blue") +
      xlab("Years") + ylab("Mean")
  result <- c(result, list(pobj))
  pobj <- x$n.jcr %>% tidyr::pivot_longer(ndocjcr:nbest3, names_to = "var", values_to = "y") %>%
    mutate(var = factor(var, levels = names(x$n.jcr)[-1])) %>%
    ggplot(aes(x = factor(PY), y = y, group = 1)) +
      facet_grid(var ~ ., scales = "free") +
      geom_line(color = "blue")+
      geom_point(color = "blue") +
      xlab("Years") + ylab("Mean")
  result <- c(result, list(pobj))
  if (plot) {
    if (ask) {
      oask <- devAskNewPage(TRUE)
      on.exit(devAskNewPage(oask))
    }
    lapply(result, print)
  }
  return(invisible(result))
}
