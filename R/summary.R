
#' WoS DB Summaries
#'
#' \code{summary} methods for \code{wos.db}-class...
#' @method summary wos.db
#' @param object an object for which a summary is desired.
#' @param filter vector of document identifiers (usually a result of \code{\link{get.idDocs}}).
#' @param TC.index citation indexes.
#' @param nmax number of top levels.
#' @param ...	further arguments passed to or from other methods.
#' @seealso \code{\link{CreateDB.wos}}.
#' @export
summary.wos.db <- function(object, filter, TC.index = c("H", "G"), nmax = 10, ...) {
  # object=db; filter=idocs; TC.index = c("H", "G"); nmax = 10; docs <- object$Docs
  # OJO: supone que Docs esta ordenado por idd
  # OJO CON EL FILTRO...
  # stopifnot(is.numeric(filter))
  filtered <- !missing(filter)
  # object=wos.dbi; filtered=FALSE; TC.index = c("H", "G"); nmax = 10
  docs <- if(filtered) object$Docs[filter, ] else object$Docs
  # Tipo de documentos
  doctypes <- as.matrix(table(droplevels(docs$DT)))
  dimnames(doctypes)[[2]] <- "Documents"
  # Citas
  TC <- docs$TC
  TC.index <- match.arg(TC.index, several.ok = TRUE)
  ind.tc <- NULL
  if(length(TC.index)) {
    TC <- sort(TC, decreasing = TRUE)
    ind.tc <- list()
    if(!sum(TC)) {
      if ("H" %in% TC.index) ind.tc$H <- 0
      if ("G" %in% TC.index) ind.tc$G <- 0
    } else {
      pos <- seq_along(TC) # 1:length(TC)
      if ("H" %in% TC.index) ind.tc$H <- max(which(TC >= pos))
      if ("G" %in% TC.index) ind.tc$G <- max(which(pos^2 <= cumsum(TC)))
    }
    # oldClass(ind.tc) <- c("summaryDefault", "table")

  }
  # Documentos por autor
  ida <- with(object$AutDoc,
              if(filtered) ida[idd %in% filter] else ida)
  autdoc <- as.numeric(table(ida))
  # SELECCIONAR LAS FILAS DE LAS TABLAS CON idd EN filter
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
  #top.cat <- ftable(object$CatDoc$idc, object$Categories$WC, nmax = nmax)
  idc <- with(object$CatDoc,
              if(filtered) idc[idd %in% filter] else idc)
  top.cat <- ftable(idc, object$Categories$WC, nmax = nmax)
  # Top 10 Areas
  idra <- with(object$AreaDoc,
               if(filtered) idra[idd %in% filter] else idra)
  top.area <- ftable(idra, object$Areas$SC, nmax = nmax)

  # Top 10 Journals
  ind.jour <- match(docs$ids,  object$Sources$ids)
  top.jour <- with(object$Sources,
                   ftable(docs$ids[PT[ind.jour] == "Journal"], JI, nmax = nmax))
  # Top 10 Countries
  aux <- object$Addresses %>% dplyr::select(idd, Country)
  if(filtered) aux <- aux %>% dplyr::filter(idd %in% filter)
  aux <- aux %>% dplyr::distinct()
  top.coun <- as.matrix(.stable(aux$Country, nmax = nmax))
  dimnames(top.coun)[[2]] <- "Documents"
  oldClass(top.coun) <- "table"
  # Result
  result <- list(ndocs = nrow(docs), naut = length(autdoc), yrange = range(docs$PY, na.rm = TRUE), nmax = nmax,
                 # doctypes = with(docs, table(DT, PT)),
                 doctypes = doctypes,
                 summary.an = with(docs, summary(an)),
                 summary.autdoc= summary(autdoc),
                 summary.TC = summary(TC), TC.index = ind.tc,
                 top = list(cat = top.cat, area = top.area, jour = top.jour, coun = top.coun)
  )
  oldClass(result) <- "summary.wos.db"
  return(result)
}

#' @rdname summary.wos.db
#' @method print summary.wos.db
#' @param x	an object used to select a method.
#' @export
print.summary.wos.db <- function(x, ...)  {
  cat('Number of documents:', x$ndocs,'\n')
  cat('Authors:', x$naut,'\n')
  if (diff(x$yrange))
    cat('Period:', paste(x$yrange, collapse = ' - '),'\n')
  else
    cat('Period:', x$yrange[1],'\n')
  cat('\nDocument types:\n')
  print(x$doctypes)
  cat('\nNumber of authors per document:\n')
  print(x$summary.an)
  cat('\nNumber of documents per author:\n')
  print(x$summary.autdoc)
  cat('\nNumber of times cited:\n')
  print(x$summary.TC)
  cat('\nIndexes:\n')
  print.table(x$TC.index)
  # print(c('values', unlist(x$TC.index)))
  # with(object$Docs, print(as.data.frame(table(TC)), row.names = FALSE))
  with(x$top,{
    cat('\nTop Categories:\n')
    print(cat)
    cat('\nTop Areas:\n')
    print(area)
    cat('\nTop Journals:\n')
    print(jour)
    cat('\nTop Countries:\n')
    print(coun)

  })
}

#' @rdname summary.wos.db
#' @param db Object of \code{\link{class}} \code{wos.db}, as returned by \code{\link{CreateDB.wos}}.
#' @param idAuthors optional; author identifiers
#' @export
# PENDIENTE: Hacer version simplificada de summary
TC.authors <- function(db, idAuthors) {
  faut <- function(x) summary(db, get.idDocs(db, idAuthors = x))$TC.index
  t(sapply(idAuthors, faut))
}


#' @rdname summary.wos.db
#' @method plot summary.wos.db
#' @param which	 if a subset of the plots is required,
#' specify a subset of the numbers \code{1:5}.
#' @param pie logical; if \code{TRUE}, pie charts are drawn.
#' @param others logical; if \code{FALSE}, only \code{nmax} top levels categories are shown.
#' @param accuracy numerical scalar; see \code{\link[scales]{label_number}(accuracy=.)}.
#' @param ask	logical; if \code{TRUE}, the user is asked before each plot,
#' see \code{\link{par}(ask=.)}.
#' @export
plot.summary.wos.db <- function(x, which = 1:5,
                        pie = FALSE,
                        others = !pie,
                        accuracy = 0.1,
                        # ask = prod(par("mfcol")) < length(which) && dev.interactive(),
                        ask = length(which) > 1 && dev.interactive(),
                        ...) {
  # x <- summary(wos.db); which = 1:5; pie = FALSE; others = !pie; accuracy = 0.1
  nmax <- x$nmax
  ndf <- if(others) nmax + 1 else nmax
  show <- rep(FALSE, 5)
  show[which] <- TRUE
  if (ask) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }
  if(show[1]) {
    doc <- x$doctypes
    df.doc <- data.frame(Types = factor(rownames(doc)), Documents = doc, row.names = NULL)
    pobj <- if (!pie)
      ggplot(data = df.doc, aes(x = reorder(Types, Documents), y = Documents)) +
      geom_bar(stat = "identity") + coord_flip() + labs(x = "Types") +
      scale_y_continuous(labels = scales::number_format(accuracy = accuracy))
    else
      ggplot(df.doc, aes(x = "", y = Documents, fill = Types)) + labs(x = "") +
      geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0)
    print(pobj)
  }
  if(show[2]) {
    df <- as.data.frame(x$top$cat)
    df <- df[1:min(ndf, nrow(df)), c(1,3)]
    names(df) <- c("Categories", "Documents")
    pobj <- if (!pie)
      ggplot(data = df, aes(x = Categories, y = Documents)) +
      geom_bar(stat = "identity") + coord_flip() + labs(x = "Categories") +
      scale_x_discrete(limits = rev(df$Categories)) +
      scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                    labels = scales::number_format(accuracy = accuracy))
      # + annotation_logticks(sides = "l")
    else
      ggplot(df, aes(x = "", y = Documents, fill = Categories)) + labs(x = "") +
      geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0)
   print(pobj)
  }
  if(show[3]) {
    df <- as.data.frame(x$top$area)
    df <- df[1:min(ndf, nrow(df)), c(1,3)]
    names(df) <- c("Areas", "Documents")
    pobj <- if (!pie)
      ggplot(data = df, aes(x = Areas, y = Documents)) +
      geom_bar(stat = "identity") + coord_flip() + labs(x = "Areas") +
      scale_x_discrete(limits = rev(df$Areas)) +
      scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                    labels = scales::number_format(accuracy = accuracy))
      # + annotation_logticks(sides = "l")
    else
      ggplot(df, aes(x = "", y = Documents, fill = Areas)) + labs(x = "") +
      geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0)
    print(pobj)
  }
  if(show[4]) {
    df <- as.data.frame(x$top$jour)
    df <- df[1:min(ndf, nrow(df)), c(1,3)]
    names(df) <- c("Journals", "Documents")
    pobj <- if (!pie)
      ggplot(data = df, aes(x = Journals, y = Documents)) +
      geom_bar(stat = "identity") + coord_flip() + labs(x = "Journals") +
      scale_x_discrete(limits = rev(df$Journals)) +
      scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                    labels = scales::number_format(accuracy = accuracy))
      # + annotation_logticks(sides = "l")
    else
      ggplot(df, aes(x = "", y = Documents, fill = Journals)) + labs(x = "") +
      geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0)
    print(pobj)
  }
  if(show[5]) {
    df <- as.data.frame(x$top$coun)
    df <- df[1:min(ndf, nrow(df)), c(1,3)]
    names(df) <- c("Countries", "Documents")
    pobj <- if (!pie)
      ggplot(data = df, aes(x = Countries, y = Documents)) +
      geom_bar(stat = "identity") + coord_flip() + labs(x = "Countries") +
      scale_x_discrete(limits = rev(df$Countries)) +
      scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                    labels = scales::number_format(accuracy = accuracy))
      # + annotation_logticks(sides = "l")
    else
      ggplot(df, aes(x = "", y = Documents, fill = Countries)) + labs(x = "") +
      geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0)
    print(pobj)
  }

}
