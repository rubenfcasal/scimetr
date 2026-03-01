# scimetr: Analysis of Scientific Publication Data with R

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import bibliographic data from WoS ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Import bibliographic data downloaded from Web of Science (WoS).
#'
#' Reads bibliography entries from UTF-8 encoded Tab-delimited files containing
#' "Full Record and Cited References" (see [wosdf] and vignette [Downloading data from the Web of Science](https://rubenfcasal.github.io/scimetr/articles/WoS_export.html)).
#' @param path character; path to the directory containing the files.
#' @param pattern regular expression; only matching files will be loaded.
#' Defaults to `"*.txt"`.
#' @param all logical; indicating whether sources without ISSN are included
#' (`TRUE`) or not (`FALSE`) in the result. Defaults to `TRUE`.
#' @param progress logical; indicating whether a progress bar should be displayed.
#' Defaults to `TRUE` when R is used interactively and there is more than one file,
#' and `FALSE` otherwise.
#' @return A `data.frame` with rows corresponding to sources and columns to
#' WoS variables.
#' @seealso [wosdf], [db_bib()].
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
import_wos <- function(path = ".", pattern = "*.txt",
                              all = TRUE, progress = NULL) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # path <- "USC (29-03-2023)";  pattern = '*.txt'; all = TRUE
  # NOTAS:
  #   Por defecto se consideran fuentes aunque no tengan ISSN (argumento all)
  #   OJO: No convierte las variables de texto a factor...
  files <- dir(path, pattern = pattern, full.names = TRUE)
  nfiles <- length(files)
  if (!nfiles) stop("No files matching 'pattern' in 'path'")
  if (is.null(progress)) progress <- interactive() & (nfiles > 1)
  if (progress) {
    cat("\nProcessing files...\n")
    progressbar <- txtProgressBar(min = 0, max = nfiles, width = 40, style = 3)
  }
  # Nombres y etiquetas de variables
  tmp <- .wos.labels %>%
    filter(!is.na(id)) %>%
    select(name, label)
  labels <- tmp$label
  names(labels) <- tmp$name
  # Bucle
  data.list <- list(nfiles)
  for (i in seq_len(nfiles)) {
    # Pendiente: probar carga de archivos con readr
    data <- read.delim(files[i],
      row.names = NULL, na.strings = "",
      colClasses = "character", stringsAsFactors = FALSE, quote = "", encoding = "UTF-8"
    )
    # Guardar fichero de origen por si surgen problemas
    # data$origin <- x # Poner como opción?
    # Comprobar variables y generar error si no están todas (all_of()?)
    data <- rlang::try_fetch(
      data %>% select(all_of(names(labels))),
      error = function(cnd) {
        rlang::abort(paste("Problem with file", files[i]), parent = cnd)
      }
    )
    # update progress bar
    if (progress) setTxtProgressBar(progressbar, i)
    data.list[[i]] <- data
  }
  wosdf <- do.call("rbind", data.list)
  # names(wosdf) <- names(wosdf)[-1] # Versión anterior
  ind <- !is.na(wosdf$SN)
  if (!all && sum(ind) < nrow(wosdf)) {
    warning("Sources with no ISSN found; dropped...")
    wosdf <- wosdf[ind, ]
  }
  attr(wosdf, "variable.labels") <- labels
  oldClass(wosdf) <- c("wos.data", "data.frame")
  return(wosdf)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create bibliographic data base ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Bibliographic data base creation
#'
#' @param data a `data.frame` with bibliographic data.
#' @param ... further arguments passed to or from other methods.
#' @aliases wos.db-class
#' @export
# S3 generic function db_bib
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db_bib <- function(data, ...) {
  UseMethod("db_bib")
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname db_bib
#' @method db_bib wos.data
#' @description
#' \code{db_bib.wos.data()} converts a \code{data.frame} with WoS variables
#' (as returned by \code{\link{import_wos}}; see \code{\link{wosdf}}) into
#' a relational database (a list of data.frames).
#' @param label character string describing the data.
#' @param progress logical; if `TRUE` the progress is printed.
#' @param verbose logical; if `TRUE` additional information is printed.
#' @return An S3 object of [class] `wos.db`.
#' A `list` with the following components:
#'
#' - `Docs`: document-level records (year, type, citations, identifiers).
#'
#' - `Authors` and `AutDoc`: author dictionary and document-author links.
#'
#' - `OI` and `OIDoc`: ORCID identifiers and their linkage to documents.
#'
#' - `RI` and `RIDoc`: ResearcherID identifiers and their linkage to documents.
#'
#' - `Affiliations` and `AffDoc`: affiliation dictionary and document links.
#'
#' - `Addresses` and `AddAutDoc`: address and document-author-address links.
#'
#' - `Sources`: document sources (journals, books, proceedings, ...).
#'
#' - `Categories`, `Areas`, `CatSour` and `AreaSour`: thematic classifications
#'   and their linkage to sources.
#'
#' - `WSIndex` and `SourWSI`: Web of Science indexes and their linkage to
#'   sources.
#'
#' - `label` and `date`: metadata that helps tracking the dataset
#'   identity over time.
#'
#' This object has specialized [print] and [plot] methods.
#' @seealso \code{\link{wosdf}}, \code{\link{import_wos}}.
#' @examples
#' db <- db_bib(wosdf)
#' str(db, 1)
#' print(db)
#' summary(db)
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db_bib.wos.data <- function(data, label = "", progress = interactive(),
                              verbose = FALSE, ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # data = wosdf; label = ""; verbose = FALSE; progress = FALSE
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ndocs <- nrow(data)
  data$idd <- seq_len(ndocs) # OJO: Se añade una variable a los datos

  if (progress) {
    cat("Processing Documents...\n")
    progressbar <- txtProgressBar(min = 0, max = ndocs, width = 40, style = 3)
  }

  # Bucle documentos  ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  lautores <- lorcid <- lri <- ldirs <- autdir <- vector("list", ndocs)
  # an = Número de autores
  an <- oin <- rin <- dirsn <- numeric(ndocs)
  san <- soin <- srin <- 0
  # ~~~~~~~~~~~~~~~~~~
  for (i in 1:ndocs) {
    # i <- 0; i <- i +1
    # i <- 838 # ", ALO/0000-0003-1456-7342"

    # update progress bar
    if (progress) {
      setTxtProgressBar(progressbar, i)
      if (verbose) cat(" r", i, sep = "")
    }
    # ~~~~~~~~~~~~~~~~~~
    # Authors: Tabla de autores
    # ida: author index
    # ~~~~~~~~~~~~~~~~~~
    # AF	Nombre completo de autor
    # PROBLEMA: Puede aparecer el segundo nombre como apellido
    #   Antonio Gomez-Fraguela, Jose
    autores0 <- str_split(data$AF[i], "; ")[[1]]
    an[i] <- length(autores0) # nº de autores [Docs]
    ida0 <- san + seq_len(an[i]) # Indice secuencial de author (fila en autores)
    san <- san + an[i]

    ## Identificadores autores  ----
    # ~~~~~~~~~~~~~~~~~~
    # Data frame para lautores[[i]]
    #   Asignará NAs cuando no hay OI
    autores1 <- data.frame(AF = autores0, ioi0 = NA_integer_, iri0 = NA_integer_)
    autores0 <- stringi::stri_trans_general(autores0, "upper; latin-ascii")

    ## OI	Identificador ORCID ----
    # ~~~~~~~~~~~~~~~~~~
    # https://support.orcid.org/hc/en-us/articles/360006897674-Structure-of-the-ORCID-Identifier
    # PROBLEMA: puede haber orcids asignados incorrectamente en wos
    OI0 <- data$OI[i]
    if (is.na(OI0)) {
      oin[i] <- 0
      # Lista de orcids con NAs
      lorcid[[i]] <- data.frame(ioi0 = NA, OI = NA_character_) # No lo tengo claro
    } else {
      OI0 <- str_split(OI0, "; ")[[1]]
      # Trocear en nombre y orcid
      res <- str_split(OI0, "/", simplify = TRUE)
      # PROBLEMA: Pueden aparecer orcids duplicados
      res <- res[!duplicated(res[, 2]), , drop = FALSE]
      # PROBLEMA: Pueden aparecer orcids vacios
      res <- res[nzchar(res[, 2]), , drop = FALSE]
      # Añadir a la lista de orcids
      oin[i] <- nrow(res) # nº de orcids [Docs]
      ioi0 <- soin + seq_len(oin[i]) # Indice secuencial de orcid (fila)
      soin <- soin + oin[i]
      lorcid[[i]] <- data.frame(ioi0 = ioi0, OI = res[, 2], AFOI = res[, 1])
      # Trabajamos con ioi0 para tabla OIAut, OIDoc
      res <- data.frame(AFOI = res[, 1], ioi0 = ioi0)
      # PROBLEMA: Algún orcid puede tener / en el nombre
      # Hocking / Mennie, Lynne J/0000-0002-2414-2826 (https://orcid.org/0000-0002-2414-2826)
      # PROBLEMA: Puede haber orcid sin nombre
      # "/0000-0003-2170-2733"
      res <- res[nzchar(res[, 1]), ]
      if (nrow(res)) {
        # PENDIENTE: Escribir mensajes en fichero .log
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # PENDIENTE: Mejorar eficiencia (ir buscando cada autor y eliminando)
        # Modificar asignación, AF por AF: prueba_adist_author
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # PENDIENTE: Paralelizar balanceo de carga?
        # PENDIENTE: Implementar evitar acentos en match_authors
        res[, 1] <- stringi::stri_trans_general(res[, 1], "upper; latin-ascii")
        # authors <- res[, 1]; table <- autores0; weight = c(family = 0.9, name = 0.1); max.dist = 0.2; attr.dist = FALSE
        amatch <- match_authors(res[, 1], autores0) # , attr.dist = TRUE)
        # View(cbind(res[, 1], autores0[amatch]))
        index <- !is.na(amatch)
        autores1[amatch[index], 2] <- res[index, 2]
      }
      # Avisar si no encuentra alguno (o si hay mas)
      if (verbose && oin[i] > nrow(autores1)) {
        cat(
          "\nWarning: number of authors (", nrow(autores1),
          ") less than the number of ORCIDs (", nrow(res), "), in source", i, "\n"
        )
      } else if (verbose && (tmp <- oin[i] - sum(!is.na(autores1$ioi0))) > 0) {
        cat(
          "\nWarning: ", tmp,
          " ORCIDs where not assigned, in source", i, "\n"
        )
      }
    }


    ## RI	Identificador Researcher Id ----
    # ~~~~~~~~~~~~~~~~~~
    # PROBLEMA: puede haber ris asignados incorrectamente en wos
    RI0 <- data$RI[i]
    if (is.na(RI0)) {
      rin[i] <- 0
      # Lista de ris con NAs
      lri[[i]] <- data.frame(iri0 = NA, RI = NA_character_) # No lo tengo claro
    } else {
      RI0 <- str_split(RI0, "; ")[[1]]
      # Trocear en nombre y ri
      res <- str_split(RI0, "/", simplify = TRUE)
      # PROBLEMA: Pueden aparecer ris duplicados?
      res <- res[!duplicated(res[, 2]), , drop = FALSE]
      # PROBLEMA: Pueden aparecer ris vacios?
      res <- res[nzchar(res[, 2]), , drop = FALSE]
      # Añadir a la lista de ris
      rin[i] <- nrow(res) # nº de ris [Docs]
      iri0 <- srin + seq_len(rin[i]) # Indice secuencial de ri (fila)
      srin <- srin + rin[i]
      lri[[i]] <- data.frame(iri0 = iri0, RI = res[, 2], AFRI = res[, 1])
      # Trabajamos con iri0 para tabla RIAut, RIDoc
      res <- data.frame(AFRI = res[, 1], iri0 = iri0)
      # PROBLEMA: Algún ri puede tener / en el nombre?
      # PROBLEMA: Puede haber ri sin nombre?
      res <- res[nzchar(res[, 1]), ]
      if (nrow(res)) {
        # PENDIENTE: Escribir mensajes en fichero .log
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # PENDIENTE: Mejorar eficiencia (ir buscando cada autor y eliminando)
        # Modificar asignación, AF por AF: prueba_adist_author
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # PENDIENTE: Paralelizar balanceo de carga?
        # PENDIENTE: Implementar evitar acentos en match_authors
        res[, 1] <- stringi::stri_trans_general(res[, 1], "upper; latin-ascii")
        # authors <- res[, 1]; table <- autores0; weight = c(family = 0.9, name = 0.1); max.dist = 0.2; attr.dist = FALSE
        amatch <- match_authors(res[, 1], autores0) # , attr.dist = TRUE)
        # View(cbind(res[, 1], autores0[amatch]))
        index <- !is.na(amatch)
        autores1[amatch[index], 3] <- res[index, 2]
      }
      # Avisar si no encuentra alguno (o si hay mas)
      if (verbose && rin[i] > nrow(autores1)) {
        cat(
          "\nWarning: number of authors (", nrow(autores1),
          ") less than the number of RIs (", nrow(res), "), in source", i, "\n"
        )
      } else if (verbose && (tmp <- rin[i] - sum(!is.na(autores1$iri0))) > 0) {
        cat(
          "\nWarning: ", tmp,
          " RIs where not assigned, in source", i, "\n"
        )
      }
    }


    # Lista de autores ----
    # ~~~~~~~~~~~~~~~~~~
    # AU	Autores
    # El nombre corto se establecerá como el primero al correspondiente AF
    autores1 <- data.frame(AU = str_split(data$AU[i], "; ")[[1]], autores1)
    lautores[[i]] <- autores1
    # Pendiente: EM	Dirección de correo electrónico
    #   Aparecen sin asociar al autor y sin criterio aparente
    # Pendiente: Emplear valores anteriores para identificar autores con distintos AF

    # ~~~~~~~~~~~~~~~~~~
    # Addresses: Tabla direcciones ----
    # idad, C1, Univ, Country
    # ~~~~~~~~~~~~~~~~~~
    # C1	Dirección de autor
    # PENDIENTE: direcciones vacias which(is.na(data$C1)) Warning?
    ldirs0 <- str_split(data$C1[i], "(; )?\\[")[[1]] # Elimina primer [
    if (length(ldirs0) == 1) {
      ldirs[i] <- str_split(ldirs0, "; ")
      dirsn[i] <- length(ldirs[[i]])
      autdir[[i]] <- rep(list(ida0), length(ldirs[[i]]))
    } else {
      # Hay direcciones identificadas con [] (la primera "")
      # Dividir entre autores y dirección
      d <- str_split(ldirs0[-1], "\\] ")
      # Puede haber múltiples direcciones para un grupo de autores
      ldirs[[i]] <- lapply(d, function(x) unlist(str_split(x[2], "; ")))
      # Número de direcciones en documento
      res <- sapply(ldirs[[i]], length)
      dirsn[i] <- sum(res)
      # Lista con índices secuenciales de autor para cada dirección
      autdir[[i]] <- rep(lapply(d, function(x) ida0[charmatch(str_split(x[1], "; ")[[1]], autores0)]), res)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  } # for(i in 1:ndocs)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (progress) cat('\nProcessing "Authors"...\n')

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Preparar etiquetas para tablas
  # ~~~~~~~~~~~~~~~~~~
  labels <- .wos.labels$label
  names(labels) <- .wos.labels$name

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Tablas ORCID ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  orcids <- dplyr::bind_rows(lorcid, .id = "idd")
  orcids$idd <- as.numeric(orcids$idd)
  # Documentos sin ningún ORCID
  # table(is.na(orcids$ioi))
  orcids <- na.omit(orcids)
  attr(orcids, "na.action") <- NULL
  # str(orcids)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calcular ioi
  # Se combinan ioi mismo ORCID
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  orcids <- orcids %>% mutate(ioi = cur_group_id(), .by = OI)

  # ~~~~~~~~~~~~~~~~~~
  # OI: Tabla de orcids
  # ioi OI, AFOI
  # ~~~~~~~~~~~~~~~~~~
  OI <- orcids %>%
    select(ioi, OI, AFOI) %>%
    filter(!duplicated(ioi))
  # Convertir cadenas vacias en NAs
  is.na(OI$AFOI) <- !nzchar(OI$AFOI)
  # Añadir etiquetas
  attr(OI, "variable.labels") <- labels[names(OI)]

  # ~~~~~~~~~~~~~~~~~~
  # OIDoc: Tabla de orcids por documentos
  # idd, ioi
  # ~~~~~~~~~~~~~~~~~~
  OIDoc <- orcids %>% select(idd, ioi)
  # Añadir etiquetas
  attr(OIDoc, "variable.labels") <- labels[names(OIDoc)]


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Tablas RI ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ris <- dplyr::bind_rows(lri, .id = "idd")
  ris$idd <- as.numeric(ris$idd)
  # Documentos sin ningún RI
  # table(is.na(ris$iri))
  ris <- na.omit(ris)
  attr(ris, "na.action") <- NULL
  # str(ris)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calcular iri
  # Se combinan iri mismo RI
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ris <- ris %>% mutate(iri = cur_group_id(), .by = RI)

  # ~~~~~~~~~~~~~~~~~~
  # RI: Tabla de ris
  # iri RI, AFRI
  # ~~~~~~~~~~~~~~~~~~
  RI <- ris %>%
    select(iri, RI, AFRI) %>%
    filter(!duplicated(iri))
  # Convertir cadenas vacias en NAs
  is.na(RI$AFRI) <- !nzchar(RI$AFRI)
  # Añadir etiquetas
  attr(RI, "variable.labels") <- labels[names(RI)]

  # ~~~~~~~~~~~~~~~~~~
  # RIDoc: Tabla de ris por documentos
  # idd, iri
  # ~~~~~~~~~~~~~~~~~~
  RIDoc <- ris %>% select(idd, iri)
  # Añadir etiquetas
  attr(RIDoc, "variable.labels") <- labels[names(RIDoc)]

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Tablas autores ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Combinar autores
  # ~~~~~~~~~~~~~~~~~~
  autores <- dplyr::bind_rows(lautores, .id = "idd")
  autores$idd <- as.integer(autores$idd)
  # Obtener ioi
  index <- match(autores$ioi0, orcids$ioi0)
  autores$ioi <- orcids$ioi[index]
  autores$ioi0 <- NULL
  # Obtener iri
  index <- match(autores$iri0, ris$iri0)
  autores$iri <- ris$iri[index]
  autores$iri0 <- NULL


  # ~~~~~~~~~~~~~~~~~~
  # Calcular ida
  # ~~~~~~~~~~~~~~~~~~
  # Agrupar autores con mismo ORCID
  # Problema: Pueda haber errores en RI
  # No agrupamos autores con mismo RI
  # Puede haber mismo AF con distinto OI
  # Calcular ida agrupado (por OI o por AF)
  # Cuidado: Versión actualizada de R
  # Mantenemos el orden inicial para direcciones

  # Separamos autores según si disponen de ORCID
  res <- split(autores, is.na(autores$ioi))
  # names(res) "FALSE" "TRUE"
  # Autores con ORCID ida consecutivos agrupando por ORCID
  if (!is.null(res[["FALSE"]])) {
    res[["FALSE"]] <- res[["FALSE"]] %>% mutate(ida = cur_group_id(), .by = ioi)
    maxida <- max(res[["FALSE"]]$ida)
  } else {
    maxida <- 0
  }
  # Autores sin ORCID ida consecutivos a partir de maxida agrupando por AF
  if (!is.null(res[["TRUE"]])) {
    res[["TRUE"]] <- res[["TRUE"]] %>% mutate(ida = cur_group_id() + maxida, .by = AF)
  }

  # Volver a combinar
  autores <- bind_rows(res)

  # ~~~~~~~~~~~~~~~~~~
  # Authors: Tabla de autores
  # ida, AU, AF, ioi
  # ~~~~~~~~~~~~~~~~~~
  Authors <- autores %>%
    select(ida, AU, AF, ioi, iri) %>%
    filter(!duplicated(ida)) %>%
    arrange(ida)
  # Añadir etiquetas
  attr(Authors, "variable.labels") <- labels[names(Authors)]

  # ~~~~~~~~~~~~~~~~~~
  # AutDoc: Tabla de autores por documento
  # idd, ida, idad
  # ~~~~~~~~~~~~~~~~~~
  AutDoc <- autores[c("idd", "ida")]
  # Añadir etiquetas
  attr(AutDoc, "variable.labels") <- labels[names(AutDoc)]


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Addresses: Tabla direcciones ----
  # (contiene todas las direcciones: AddDoc)
  # idad, idd, C1, Univ, Country
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # C1	Dirección de autor
  # PENDIENTE: direcciones vacias which(is.na(data$C1)) Warning?
  if (progress) cat('Processing "Addresses"...\n')
  dirs <- unlist(ldirs)
  # ~~~~
  # Extraer Universidad y Pais
  # NOTA: La universidad se toma como la primera subcadena conteniendo "Univ"
  # NOTA: El pais se toma a partir de la última subcadena
  res <- t(sapply(str_split(dirs, ", "), function(x) c(x[grepl("Univ", x)][1], x[length(x)])))
  Country <- res[, 2]
  # USA aparece con estado e incluso zip: "AL 35294 USA"
  Country[grepl("(.+ )?USA$", Country)] <- "USA"
  # United Kingdom (UK) aparece por territorios: England, Scotland, Wales (y probablemente Northern Ireland)
  Country[Country %in% c("England", "Scotland", "Wales", "Northern Ireland")] <- "UK"
  # Hay otros valores "raros": "Peoples R China"
  Country[grepl("Peoples R China", Country)] <- "China"
  Country <- as.factor(Country)
  idad <- seq_along(dirs)
  idd <- rep(data$idd, dirsn)
  Addresses <- data.frame(
    idad = idad, idd = idd, C1 = dirs, Univ = res[, 1], Country = Country,
    stringsAsFactors = FALSE
  )
  # PENDIENTE:
  # Countries <- levels(Country)
  # Country en Addresses como entero
  # Crear tabla CouDoc? sin repetidos
  # Opción para no tener en cuenta autor en direcciones?
  # Añadir etiquetas
  attr(Addresses, "variable.labels") <- labels[names(Addresses)]

  # ~~~~~~~~~~~~~~~~~~
  # AddAutDoc: Tabla de direcciones por autor y documento
  # ida, idd, idad
  # ~~~~~~~~~~~~~~~~~~
  res <- unlist(lapply(autdir, function(x) lapply(x, length)))
  idad <- rep(idad, res)
  idd <- rep(data$idd, sapply(autdir, function(x) sum(sapply(x, length)))) # optimizar...
  AddAutDoc <- data.frame(ida = autores$ida[unlist(autdir)], idd = idd, idad = idad)
  # Comprobación
  # AddAutDoc %>% filter(idd == 207)  %>% left_join(Addresses) %>% left_join(Authors)
  # data$C1[207]
  # Añadir etiquetas
  attr(AddAutDoc, "variable.labels") <- labels[names(AddAutDoc)]

  # ~~~~~~~~~~~~~~~~~~
  ## Tablas Sources ----
  # PT: Tipo de publicación
  # Corregir SN/EI
  # Crear ids
  # ~~~~~~~~~~~~~~~~~~
  if (progress) cat('Processing "Sources"...\n')
  # ~~~~~~~~~~~~~~~~~~
  # PENDIENTE: Artículos en series:
  # ~~~~~~~~~~~~~~~~~~
  # CI título artículo, SO título del libro, SE título de la serie
  # En Scopus/Sources solo tenemos SE (y no tenemos BN ni título del libro)
  # Las series PT="S" tienen SE Book Series Title (de momento ninguna con BS)
  # with(data, table(PT, !is.na(SE)))
  # y "todas" tienen BN
  # with(data, table(PT, !is.na(BN)))


  # PT: Tipo de publicación -> Sources$ST
  # ~~~~~~~~~~~~~~~~~~
  # Clasificar las revisiones de libro como artículos
  data <- data %>% mutate(PT = ifelse(PT == "B" & DT == "Book Review", "J", PT))
  # Clasificar Conference con BS (Book series) como Proceeding Series
  # en caso contrario como Proceedings
  # PENDIENTE: Comprobar con datos de BS (Book series)
  data <- data %>% mutate(PT = ifelse(PT == "C",
    ifelse(is.null(BS), "Proceedings", "Proceeding Series"), PT
  ))
  # Convertir en factor
  data$PT <- factor(data$PT, levels = c(
    "J", "S", "Proceeding Series",
    "Proceedings", "B", "P"
  ))
  # Niveles PT (reorganizados)
  levels(data$PT) <- c(
    "Journal", "Book Series", # WoS y Scopus Sources ("Trade Journal" -> "Journal")
    "Proceeding Series", # Scopus Serial Conf. Proc. with profile
    "Proceedings", # nivel scimagojr y "C" WoS
    "Book", "Patent"
  ) # WoS


  # Corregir SN/EI
  # ~~~~~~~~~~~~~~~~~~
  # En principio no podría haber revistas con el mismo EI y distinto SN
  # salvo cambios en revistas ("nuevas ediciones")
  # Por si acaso evitamos reemplazar SN!=NA (sel)
  # CUIDADO: puede haber revistas con el mismo SN y distinto EI
  # (varían las publicaciones electrónicas)

  # Problema WOS: Puede aparecer revistas duplicadas con SN=EI y EI=NA
  # Mirar si algún SN está en EI
  index <- with(data, match(SN, EI, incomparables = NA_character_))
  res <- which(!is.na(index))
  # View(data[as.vector(t(cbind(res, index[res]))), ])
  data[res, c("SN", "EI")] <- data[index[res], c("SN", "EI")]
  # Problema WOS: Puede quedar revistas no duplicadas con SN=EI y EI=NA

  # Problema WOS: Puede aparecer revistas duplicadas con el mismo EI y distinto SN (SN, NA)
  # Revistas con SN y EI completos
  res <- data %>%
    filter(!is.na(SN) & !is.na(EI)) %>%
    distinct(SN, EI)
  # Completar SN a partir de EI solo si SN=NA
  index <- match(data$EI, res$EI, incomparables = NA_character_)
  sel <- !is.na(index) & is.na(data$SN)
  data$SN[sel] <- res$SN[index[sel]]


  # Crear ids
  # ~~~~~~~~~~~~~~~~~~
  ids <- with(
    data,
    ifelse(!is.na(BN), BN,
      ifelse(!is.na(SN), SN, EI)
    )
  )
  # Tener en cuenta la posibilidad de que any(!nzchar(ids)) sea verdadero
  ids <- match(ids, unique(ids), incomparables = NA_character_) # as.integer(as.factor(ids))
  index <- which(is.na(ids))
  # PENDIENTE:
  if (length(index)) {
    warning("Sources without BN, SN or EI were found.")
    ids[index] <- max(ids, na.rm = TRUE) + seq_along(index)
  }
  data$ids <- ids

  # ~~~~~~~~~~~~~~~~~~
  # Sources: Tabla de fuentes
  # ids: Journal ID (según BN o SN o EI)
  # ids, SO:LA, PU:EI, J9, JI
  # ~~~~~~~~~~~~~~~~~~
  wos.var <- .wos.labels %>%
    filter(table == "Sources", !is.na(id)) %>%
    pull(name)
  Sources <- data %>%
    select(ids, WC, SC, WE, all_of(wos.var)) %>%
    filter(!duplicated(ids))
  oldClass(Sources) <- "data.frame"
  # Añadir etiquetas
  attr(Sources, "variable.labels") <- labels[names(Sources)]

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Tablas categorías ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # WosCat: Tabla de categorías WoS
  # idc, WC
  # ~~~~~~~~~~~~~~~~~~
  if (progress) cat('Processing "Categories"...\n')
  categ <- strsplit(Sources$WC, "; ")
  nn <- sapply(categ, length) # nº de categ
  categ <- unlist(categ)

  # Categorías WOS en "R/sysdata.rda"
  # PENDIENTE: Crear Categories solo con las categorías en BD?
  idc <- match(toupper(categ), toupper(WosCat$WC))
  # Hay categorías que no están en el listado ...
  if (any(is.na(idc))) warning("No matches were found for some categories.")
  Sources$WC <- NULL

  # ~~~~~~~~~~~~~~~~~~
  # CatSour: Tabla de categorías por fuente
  # ids, idc
  # ~~~~~~~~~~~~~~~~~~
  CatSour <- data.frame(ids = rep(Sources$ids, nn), idc = idc)
  # Añadir etiquetas
  attr(CatSour, "variable.labels") <- labels[names(CatSour)]

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Tablas áreas ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Areas: Tabla de áreas de investigación
  # idra: research area index
  # idra, SC
  # ~~~~~~~~~~~~~~~~~~
  if (progress) cat('Processing "Areas"...\n')
  area <- strsplit(Sources$SC, "; ")
  nn <- sapply(area, length) # nº de areas
  area <- as.factor(unlist(area))
  # No utilizamos listado áreas WOS "WosArea.RData"
  # Puede haber nuevas áreas
  Areas <- data.frame(idra = seq_along(levels(area)), SC = levels(area), stringsAsFactors = FALSE)
  Sources$SC <- NULL
  # Añadir etiquetas
  attr(Areas, "variable.labels") <- labels[names(Areas)]

  # ~~~~~~~~~~~~~~~~~~
  # AreaSour: Tabla de áreas por fuentes
  # idd, idra
  # ~~~~~~~~~~~~~~~~~~
  AreaSour <- data.frame(ids = rep(Sources$ids, nn), idra = as.integer(area))
  # head(AreaSour)
  # Añadir etiquetas
  attr(AreaSour, "variable.labels") <- labels[names(AreaSour)]

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Tabla Docs ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Pendiente: Insertar fuente ¿?
  # Pendiente: Convertir variables carácter a numéricas
  # ~~~
  if (progress) cat('Processing "Docs"...\n')

  # table(data$PT)
  # ~~~
  # DT	Tipo de documento (respuesta múltiple?)
  # Se convierte a factor sin fijar niveles
  # levels <- c("Article", "Article; Proceedings Paper", "Book Review", "Correction",
  #             "Editorial Material", "Letter", "Meeting Abstract", "News Item",
  #             "Proceedings Paper", "Review", "Retraction")
  # data$DT <- factor(data$DT, levels = levels)
  # # NOTA: Some records in Web of Science may have two document types: Article and Proceedings Paper.
  # data$DT[data$DT == "Article; Proceedings Paper"] <- "Proceedings Paper"
  # data$DT <- droplevels(data$DT)
  # table(data$DT)
  # table(data$DT, data$PT)
  data$DT <- factor(data$DT)

  # HC Highly Cited Status
  data$HC <- data$HC == "Y"
  data$HC[is.na(data$HC)] <- FALSE
  # HP Hot Paper Status
  data$HP <- data$HP == "Y"
  data$HP[is.na(data$HP)] <- FALSE
  # Determinar variables WoS
  wos.var <- .wos.labels %>%
    filter(table == "Docs", !is.na(id)) %>%
    pull(name)
  Docs <- data %>%
    select(idd, ids, all_of(wos.var)) %>%
    mutate(across(c(NR:U2, PY, PG), ~ suppressWarnings(as.integer(.x)))) %>% # Convierte a entero
    mutate(UT = as.numeric(substr(UT, 5, 19)), an = an)
  # UT	Número de acceso
  # str(Docs)
  # Añadir etiquetas
  oldClass(Docs) <- "data.frame"
  attr(Docs, "variable.labels") <- labels[names(Docs)]


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Tablas afiliaciones ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Affiliations
  # idaf, C3
  # ~~~~~~~~~~~~~~~~~~
  # if (progress) cat('Processing "Affiliations"...\n')
  aff <- strsplit(data$C3, "; ")
  # Eliminar duplicados
  aff <- lapply(aff, unique)
  nn <- sapply(aff, length) # nº de afiliaciones
  aff <- as.factor(unlist(aff))
  Affiliations <- data.frame(
    idaf = seq_along(levels(aff)), C3 = levels(aff),
    stringsAsFactors = FALSE
  )
  # Añadir etiquetas
  attr(Affiliations, "variable.labels") <- labels[names(Affiliations)]

  # ~~~~~~~~~~~~~~~~~~
  # AffDoc: Tabla afiliaciones por documentos
  # idd, idaf
  # ~~~~~~~~~~~~~~~~~~
  AffDoc <- data.frame(idd = rep(data$idd, nn), idaf = as.integer(aff))
  # Añadir etiquetas
  attr(AffDoc, "variable.labels") <- labels[names(AffDoc)]

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Tablas Web of Science Index ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # WSIndex: Tabla Web of Science Index
  # idwe, WE
  # ~~~~~~~~~~~~~~~~~~
  # Depende de la revista
  # PENDIENTE: Comprobar que no cambia a lo largo del tiempo
  wsi <- str_extract_all(Sources$WE, "(?<=\\().+?(?=\\))")
  nn <- sapply(wsi, length) # nº de afiliaciones
  wsi <- as.factor(unlist(wsi))
  # Eliminar &amp; en levels(wsi)
  levels(wsi) <- str_replace(levels(wsi), "&amp;", "")
  WSIndex <- data.frame(
    idwe = seq_along(levels(wsi)), WE = levels(wsi),
    stringsAsFactors = FALSE
  )
  Sources$WE <- NULL
  # Añadir etiquetas
  attr(WSIndex, "variable.labels") <- labels[names(WSIndex)]

  # ~~~~~~~~~~~~~~~~~~
  # SourWSI: Tabla WSI por fuente
  # ids, idwe
  # ~~~~~~~~~~~~~~~~~~
  SourWSI <- data.frame(ids = rep(Sources$ids, nn), idwe = as.integer(wsi))
  # Añadir etiquetas
  attr(SourWSI, "variable.labels") <- labels[names(SourWSI)]

  # ~~~~~~~~~~~~~~~~~~
  ## Resultados ----
  # ~~~~~~~~~~~~~~~~~~
  res <- list(
    Docs = Docs, Authors = Authors, AutDoc = AutDoc,
    OI = OI, OIDoc = OIDoc, RI = RI, RIDoc = RIDoc,
    Categories = WosCat, CatSour = CatSour, Areas = Areas, AreaSour = AreaSour,
    Addresses = Addresses, AddAutDoc = AddAutDoc,
    Affiliations = Affiliations, AffDoc = AffDoc,
    Sources = Sources, WSIndex = WSIndex, SourWSI = SourWSI,
    label = label, date = data$DA[1]
  )
  oldClass(res) <- "wos.db"
  return(res)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Generic methods wos.db ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname db_bib
#' @method print wos.db
#' @param x	an object used to select a method.
#' @param ...	further arguments passed to or from other methods.
#' @seealso \code{\link{summary.wos.db}}.
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.wos.db <- function(x, ...) {
  str(x, 1, strict.width = "cut")
}


#' @rdname db_bib
#' @method plot wos.db
#' @param filter vector of document identifiers (usually a result of \code{\link{get_id_docs}}).
#' @param which	 if a subset of the plots is required,
#' specify a subset of the numbers \code{1:3}.
#' @param plot logical; if `TRUE` (default), the plots are drawn, otherwise only
#' the list of ggplot2 objects is (invisibly) returned.
#' @param warning logical; if `FALSE` (default), warnings are ignored.
#' @param ask	logical; if `TRUE`, the user is asked before each plot
#' (see \code{\link{par}(ask=.)}).
#' @seealso \code{\link{plot.summary.wos.db}}, \code{\link{plot.summary.year.wos}}
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot.wos.db <- function(x, filter, which = 1:3, plot = TRUE, warning = FALSE,
                        ask = plot && (length(which) > 1) && interactive(), ...) {
  filtered <- !missing(filter)
  result <- list()
  show <- rep(FALSE, 3)
  show[which] <- TRUE
  if (any(show[c(1, 3)])) {
    docs <- if (filtered) x$Docs[filter, ] else x$Docs
  }
  if (show[1]) { # Authors per document
    pobj <- ggplot(docs, aes(an, fill = I("blue"))) +
      geom_histogram(binwidth = max(binwidth.scott(docs$an), 1)) +
      labs(x = "Authors per document") +
      scale_y_log10() + # scale_y_continuous(trans = scale_y_log_2(base=10, from=0.9)) +
      annotation_logticks(sides = "l")+
      geom_rug()
    result <- c(result, list(pobj))
  }

  if (show[2]) { # Documents per author
    ida <- with(
      x$AutDoc,
      if (filtered) ida[idd %in% filter] else ida
    )
    autdoc <- data.frame(ida = as.numeric(table(ida)))
    pobj <- ggplot(autdoc, aes(ida, fill = I("blue"))) +
      geom_histogram(binwidth = max(binwidth.scott(autdoc$ida), 1)) +
      labs(x = "Documents per author") +
      scale_y_log10() + # scale_y_continuous(trans = scale_y_log_2(base=10, from=0.9)) +
      annotation_logticks(sides = "l") +
      geom_rug()
    result <- c(result, list(pobj))
  }
  if (show[3]) { # Times cited
    pobj <- ggplot(docs, aes(TC, fill = I("blue"))) +
      geom_histogram(binwidth = max(binwidth.scott(docs$TC), 1)) +
      labs(x = "Times cited") +
      scale_y_log10() + # scale_y_continuous(trans = scale_y_log_2(base=10, from=0.9)) +
      annotation_logticks(sides = "l") +
      geom_rug()
    result <- c(result, list(pobj))
  }
  if (plot) {
    if (!warning) {
      oldwarn <- options("warn" = -1) # Disable warnings
      on.exit(options(warn = oldwarn$warn))
    }
    if (ask) {
      oask <- devAskNewPage(TRUE)
      on.exit(devAskNewPage(oask))
    }
    lapply(result, print)
  }
  return(invisible(result))
}
