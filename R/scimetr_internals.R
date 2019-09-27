
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
