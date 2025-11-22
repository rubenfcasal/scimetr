# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# jcr.db ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Import JCR data from Web of Science (WoS)
#'
#' Reads JCR data from excel files downloaded from WoS and generates a
#' relational database (a list of data.frames).
#' It is assumed that the file name format is *JCR_[WE]_[PY]*, where *[WE]* is
#' the WoS index (SCIE, SSCI, ...) and *[PY]* the JCR year.
#' @param path character; path to the directory containing the files.
#' Defaults to the working directory.
#' @param files character vector with the file names. Defaults to filenames in
#' directory `path` with extension *.xlsx"*.
#' @param verbose logical; indicating whether the name of the file being processed
#' is printed. Defaults to `TRUE`.
#' @return An S3 object of [class] `jcr.db`. A `list` with components:
#' `Sources`, `Categories`, `JCRSour` and `JCRCatSour`.
# \describe{
#   item{Docs}{}
# }
#' @seealso [CreateDB], [AddDBJCR].
#' @aliases jcr.db-class
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CreateDBJCR <- function(path = '.', files = dir(path, pattern = "*.xlsx"),
                        verbose = TRUE) {
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # path = '.'; files = dir(path, pattern = "*.xlsx")
  # Pendiente: formato como parámetro
  # La misma revista puede estar en dos índices WE
  # La misma categoría puede estar en dos índices WE
  # Aparentemente asteriscos en SN = "ISSN" o EI = "eISSN" equivalen a NA
  cols <- c(1:5, 28:34, 37:42)
  names(cols) <- c("Title20", "ISO_ABBREV", "TITLE", "ISSN", "EISSN", "IMPACT_FACTOR",
    "IMMEDIACY_INDEX", "CITED_HALF_LIFE",
    "5YR_IMPACT_FACTOR", "EIGENFACTOR", "NORM_EIGENFACTOR", "ARTL_INFLUENCE",
    "PUBLISHER_NAME", "CATEGORY_CODE", "CATEGORY_DESCRIPTION", "CATEGORY_RANKING",
    "QUARTILE_RANK", "JIF_PERCENTILE")
  # as.data.frame(cols)
  # Obtener índices y años
  WE <- str_sub(files, 5, 8)
  PY <- as.integer(str_sub(files, 10, 13))
  # Preparar resultados
  sourtitle <- catcode <- c() # Se van incrementando
  nfiles <- length(files)
  Sources <- JCRSour <- Categories <- JCRCatSour <- vector(nfiles, mode = "list")
  # Recorrer ficheros
  for (i in seq_len(nfiles)) { # i <- 1
      if (verbose) cat(files[i], "\n")
      jcr <- openxlsx::readWorkbook(file.path(path, files[i]), cols = cols)
      if (any(str_to_upper(names(jcr)) != str_to_upper(names(cols)))) {
        # res <- data.frame(file = names(jcr), jcr = names(cols))
        stop("The .xlsx file does not contain all the necessary variables in the correct order.")
      }
      # Cambiar etiquetas
      names(jcr) <- names(.jcr.labels)
      # Pendiente: asegurarse que todas las variables nec. son numéricas
      jcr$JIF <- as.numeric(jcr$JIF)
      jcr$IMM <- as.numeric(jcr$IMM)
      jcr$CHL <- as.numeric(jcr$CHL)
      jcr$JIF5 <- as.numeric(jcr$JIF5)
      jcr$JEF <- as.numeric(jcr$JEF)
      jcr$JAI <- as.numeric(jcr$JAI)

      # ~~~~~~~~~~~~~~~~~~
      ## Tablas Sources ----
      # ~~~~~~~~~~~~~~~~~~
      # Emplear Title20 como clave de fuente
      jcr$ids <- match(jcr$J9, sourtitle)
      new.ids <- is.na(jcr$ids) # nuevos
      new.J9 <- jcr$J9[new.ids]
      sourtitle <- c(sourtitle, unique(new.J9)) # Se va incrementando
      jcr$ids[new.ids] <- match(new.J9, sourtitle)
      Sources[[i]] <- jcr[new.ids, ] %>%  select(ids, J9:EI, PU, JIF:JAI) %>%
        filter(!duplicated(ids)) # Nuevas fuentes
      # NOTA: Supondremos que ids es consecutivo/incremental

      # ~~~~~~~~~~~~~~~~~~
      # Tabla JCRSour
      # ~~~~~~~~~~~~~~~~~~
      JCRSour[[i]] <- jcr %>% select(ids, JIF:JAI) %>% filter(!duplicated(ids))
      # Depende del año, no depende de WE
      # JCRSour[[i]]$WE <- WE[i] # WE Web of Science Index
      JCRSour[[i]]$PY <- PY[i] # PY JCR Year

      # Eliminar JIF:JAI de Sources
      Sources[[i]] <- Sources[[i]] %>%  select(ids:PU)

      # Tabla Categories
      jcr$idc <- match(jcr$WCC, catcode)
      new.idc <- is.na(jcr$idc) # nuevos
      new.WCC <- jcr$WCC[new.idc]
      catcode <- c(catcode, unique(new.WCC)) # Se va incrementando
      jcr$idc[new.idc] <- match(new.WCC, catcode)

      Categories[[i]] <- jcr[new.idc, ] %>%  select(idc, WCC, WC) %>%
        filter(!duplicated(idc)) # Se va incrementando/ lista nuevos

      # Tabla JCRCatSour
      # JCRCatSour depende de WE
      JCRCatSour[[i]] <- jcr %>%  select(ids, idc, WCR:JIFP)
      JCRCatSour[[i]]$WE <- WE[i] # WE Web of Science Index
      JCRCatSour[[i]]$PY <- PY[i] # PY JCR Year
  }

  # Tabla Sources
  Sources <- do.call(rbind, Sources)
  # Asteriscos en SN = "ISSN" o EI = "eISSN" equivalen a NA
  is.na(Sources$SN) <- Sources$SN == "****-****"
  is.na(Sources$EI) <- Sources$EI == "****-****"
  attr(Sources, "variable.labels") <- .all.labels[names(Sources)]

  # Tabla Categories
  Categories <- do.call(rbind, Categories)
  attr(Categories, "variable.labels") <- .all.labels[names(Categories)]

  # Tabla JCRSour
  JCRSour <- do.call(rbind, JCRSour) %>%
    distinct(ids, PY, .keep_all = TRUE) # JCRSour no depende de WE
  attr(JCRSour, "variable.labels") <- .all.labels[names(JCRSour)]

  # Tabla JCRCatSour
  JCRCatSour <- do.call(rbind, JCRCatSour)
  JCRCatSour$JIFQ <- as.factor(JCRCatSour$JIFQ)
  WCR <- stringr::str_split_fixed(JCRCatSour$WCR, "/", n = 2)
  # WCP = "Category Position"
  JCRCatSour$WCP <- as.numeric(WCR[, 1])
  # WCT = "Total Category"
  JCRCatSour$WCT <- as.numeric(WCR[, 2])
  # WoS utiliza WCP/WCT para cuartiles
  # https://support.clarivate.com/ScientificandAcademicResearch/s/article/Journal-Citation-Reports-Quartile-rankings-and-other-metrics
  # WCR = "Category Ranking" se sustituye por 1 - (WCP - 1)/(WCT - 1))
  JCRCatSour$WCR <- with(JCRCatSour, 1 - (WCP - 1)/(WCT - 1))
  attr(JCRCatSour, "variable.labels") <- .all.labels[names(JCRCatSour)]

  # Resultado
  res <- list(Sources = Sources, Categories = Categories,
              JCRSour = JCRSour, JCRCatSour = JCRCatSour)
  # attr(res, "variable.labels") <- .wos.variable.labels
  oldClass(res) <- "jcr.db"
  return(res)
}

