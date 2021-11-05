#' Summary year
#'
#' \code{summary_year} returns a summary per year...
#' @param object an object for which a summary is desired.
#' @param filter vector of document identifiers (usually a result of \code{\link{get.idDocs}}).
#' @param ...	further arguments passed to or from other methods.
#' @seealso \code{\link{CreateDB.wos}}.
#' @export
# PENDIENTE: RESULTADOS COMO TABLAS
summary_year <- function(object, filter, ...) {
  # object=db; filter=idocs
  # stopifnot(is.numeric(filter))
  docs <- if(!missing(filter)) object$Docs[filter, ] else object$Docs

  # Years
  Doc.PY <- as.matrix(table(docs$PY))
  dimnames(Doc.PY)[[2]] <- "Documents"


  # Authors per document per year

  AuDoc.PY <- docs %>% group_by(PY)  %>% summarise(avg = mean(an), med = median(an)) %>% na.omit()
  names(AuDoc.PY) <- c("", "Avg", "Median")

  # # Documents per author per year
  # ida <- object$AutDoc$ida # pendiente filtar AutDoc
  # year <- docs$PY[object$AutDoc$idd]
  # as.data.frame(table(ida, year))
  #
  # idd.filt <- object$AutDoc$idd[object$AutDoc$idd %in% docs$idd]
  # ida.filt <- object$AutDoc$ida[object$AutDoc$idd %in% docs$idd]
  # nDocAu.Year <- data.frame(ida=ida.filt,PY=docs$PY[match(idd.filt,docs$idd)])
  # DocAu.PY <- data.frame(sort(unique(nDocAu.Year$PY)),round(colMeans(table(nDocAu.Year)),2),apply(table(nDocAu.Year),2,median))
  # aux <- table(nDocAu.Year)
  # nDocAu<- data.frame(Docs=as.vector(aux), PY=rep(colnames(aux), each = dim(aux)[1]))
  # rownames(DocAu.PY)=NULL
  # names(DocAu.PY) <- c("", "Avg", "Median")
  # Times cited
  TC.PY <- docs %>% group_by(PY)  %>% summarise(n = sum(TC), avg = mean(TC), med = median(TC)) %>% na.omit()
  names(TC.PY) <- c("", "Cites", "Avg", "Median")


  result <- list(Doc.PY = Doc.PY,
                 AuDoc.PY = as.data.frame(AuDoc.PY),
                 # DocAu.PY = as.data.frame(DocAu.PY),
                 TC.PY = as.data.frame(TC.PY),
                 docs = docs[c("PY", "an", "TC")]
                 # , nDocAu = nDocAu
            )
  oldClass(result) <- "summary.year"
  return(result)

}


#' @rdname summary_year
#' @method print summary.year
#' @param x	an object used to select a method.
#' @export
print.summary.year <- function(x, ...)  {
  cat("\nAnnual Scientific Production:\n\n")
  print(x$Doc.PY, row.names=FALSE)
  cat("\nAnnual Authors per Document:\n\n")
  print(x$AuDoc.PY, row.names=FALSE)
  # cat("\nAnnual Documents per Author\n\n")
  # print(x$DocAu.PY)
  cat("\nAnnual Times Cited:\n\n")
  print(x$TC.PY, row.names=FALSE)
}

#' @rdname summary_year
#' @method plot summary.year
#' @param which	 if a subset of the plots is required,
#' specify a subset of the numbers \code{1:3}.
#' @param boxplot logical; if \code{TRUE}, boxplots are drawn (for plots from 2 to 4).
#' @param ask	logical; if \code{TRUE}, the user is asked before each plot,
#' see \code{\link{par}(ask=.)}.
#' @export
plot.summary.year <- function(x, which = 1:3, boxplot = FALSE,
                              # ask = prod(par("mfcol")) < length(which) && dev.interactive(),
                              ask = length(which) > 1 && dev.interactive(), ...) {
  show <- rep(FALSE, 3)
  show[which] <- TRUE
  if (ask) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }
  if(show[1] & !boxplot) {
    Doc.PY <- data.frame(Types = factor(rownames(x$Doc.PY)), Documents = x$Doc.PY, row.names = NULL)
    names(Doc.PY) <- c("Years","Documents")
    pobj <- ggplot(data=Doc.PY, aes(x=Years, y=Documents, group=1)) +
      geom_line(color="blue")+
      geom_point(color="blue") + ggtitle("Scientific Production")
    print(pobj)
  }
  if(show[2]) {
    names(x$AuDoc.PY) <- c("Years","Authors","Median")
    pobj <- if (!boxplot)
      ggplot(data=x$AuDoc.PY, aes(x=factor(Years), y=Authors, group=1)) +
      geom_line(color="blue")+
      geom_point(color="blue") + ggtitle("Authors per Document") +
      xlab("Years")
    else
    ggplot(x$docs, aes(x = factor(x$docs$PY), y = x$docs$an)) +
      geom_boxplot(fill = "#4271AE", colour = "#1F3552") +
      xlab("Years") + ylab("Authors per document") +
      scale_y_log10() +
      annotation_logticks(sides='l')
      print(pobj)
  }
  if(show[3]) {
  #   names(x$DocAu.PY) <- c("Years","Authors","Median")
  #   pobj <- if (!boxplot)
  #     ggplot(data=x$DocAu.PY, aes(x=Years, y=Authors, group=1)) +
  #     geom_line(color="blue")+
  #     geom_point(color="blue") + ggtitle("Documents per Author")
  #   else
  #   ggplot(x$nDocAu, aes(x = factor(x$nDocAu$PY), y = x$nDocAu$Docs)) +
  #     geom_boxplot(fill = "#4271AE", colour = "#1F3552") +
  #     xlab("Years") + ylab("Documents per Author")
  #   print(pobj)
  # }
  # if(show[4]) {
    names(x$TC.PY) <- c("Years","TC","Median")
    pobj <- if (!boxplot)
      ggplot(data=x$TC.PY, aes(x=factor(Years), y=TC, group=1)) +
      geom_line(color="blue")+
      geom_point(color="blue") + ggtitle("Times Cited") +
      xlab("Years")
    else
    ggplot(x$docs, aes(x = factor(x$docs$PY), y = x$docs$TC)) +
      geom_boxplot(fill = "#4271AE", colour = "#1F3552") +
      xlab("Years") + ylab("Times Cited") +
      scale_y_sqrt(breaks = scales::trans_breaks("sqrt", function(x) x^2))
      # scale_y_log10() +
      # annotation_logticks(sides='l')
      # scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x))
    print(pobj)
  }

}


