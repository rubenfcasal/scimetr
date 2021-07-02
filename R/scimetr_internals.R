
#' @keywords internal
.stable <- function(x, nmax = 10, others = TRUE) {
  res <- sort(table(x), decreasing = TRUE)
  n <- max(which(res > 0))
  res <- res[seq_len(n)]
  if ( n > nmax) {
    if (others) {
      res[nmax + 1] <- sum(res[(nmax+1):n])
      names(res)[nmax + 1] <- "Others"
    }
    res <- res[seq_len(nmax + others)]
  }
  return(res)
}


#' @keywords internal
.get.ida.AutDoc<- function(db, idocs)
  return(with(db$AutDoc, ida[idd %in% idocs]))


#' @keywords internal
scale.2f <- function(x) sprintf("%.2f", x)

#' @keywords internal
binwidth.scott <- function(x) {
  # Ver nclass.scott()
  h <- 3.5 * sqrt(stats::var(x)) * length(x)^(-1/3)
  if (h==0) h <- diff(range(x))
  return(h)
}

#' @keywords internal
binwidth.sturges <- function(x) {
  # Ver nclass.Sturges()
  h <- range(x)/ceiling(log2(length(x)) + 1)
  if (h==0) h <- diff(range(x))
  return(h)
}

#' @keywords internal
binwidth.fd <- function(x) {
  # Ver nclass.FD()
  h <- 2 * stats::IQR(signif(x, digits = 5))
  if (h==0) h <- binwidth.sturges(x)
  return(h)
}
