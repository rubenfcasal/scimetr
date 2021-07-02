#' Import bibliographic data downloaded from Web of Science (WoS).
#'
#' Reads bibliography entries from UTF-8 encoded Tab-delimited files containing "Full Record and Cited References"
#' (see \code{\link{wosdf}}.
#'
#' @param  path to a subdir...
#' @param  pattern file pattern (= '*.txt' by default)
#' @param  other Guardar otros documentos "include", "ignore", "split", pendiente: cambiar split por path
#' @return A \code{data.frame} with columns corresponding to WoS variables.
# and including a document index variable \code{idd}.
#' @seealso \code{\link{wosdf}}, \code{\link{CreateDB.wos}}.
#' @export
ImportSources.wos <- function(path = '.', pattern = '*.txt', other = c("include", "ignore", "split")){
  # path <- 'doinowos';  pattern = '*.txt'; other = FALSE
  # NOTAS:
  #   Se consideran fuentes todo lo que tenga ISSN
  #   OJO: No convierte las variables de texto a factor...
  files <- dir(path, pattern = pattern, full.names = TRUE)
  if(!length(files)) stop("No files matching 'pattern' in 'path'")
  data.list <- lapply(files, function(x) read.delim(x, row.names=NULL, colClasses = 'character', stringsAsFactors=FALSE, quote = "", encoding="UTF-8"))
  # PENDIENTE: Establecer na.strings = ""
  # considerar eliminar row.names = NULL: Error duplicate 'row.names' are not allowed
  # considerar incluir colClasses = A character vector of classes to be assumed for the columns: logical, integer, numeric, complex, character
  # wosdf <- bind_rows(data.list)
  wosdf <- do.call('rbind', data.list)
  #  write.csv2(wosdf, "wosdf.csv")
  names(wosdf) <- names(wosdf)[-1] # Linux?
  names(wosdf)[1] <- "PT"
  wosdf <- wosdf[,-ncol(wosdf)]
  # nwos <- nrow(wosdf)
  # wosdf %>% group_by
  ind <- nzchar(wosdf$SN) #  wosdf$SN != ""
  other <- match.arg(other)
  if (other != "include" && sum(ind) < nrow(wosdf)) {
    if (other == "split") {
      OtherDocs <- wosdf[!ind,]
      # PENDIENTE: Devolver como componente o atributo?
      save(OtherDocs, file = "OtherDocs.RData")
      warning(paste("Sources with no ISSN found; saved as ", file.path(getwd(), "OtherDocs.RData")))
    } else
      warning("Sources with no ISSN found; dropped...")
    wosdf <- wosdf[ind, ]
    # nwos <- nrow(wosdf)
  }
  attr(wosdf, "variable.labels") <- .wos.variable.labels
  return(wosdf)
  # PENDENTE: añadir clase wos.data a data.frame
}

.wos.variable.labels <- c(PT = "Publication type", AU = "Author", BA = "Book authors",
  BE = "Editor", GP = "Group author", AF = "Author full", BF = "Book authors fullname",
  CA = "Corporate author", TI = "Title", SO = "Publication name",
  SE = "Series title", BS = "Book series", LA = "Language", DT = "Document type",
  CT = "Conference title", CY = "Conference year", CL = "Conference place",
  SP = "Conference sponsors", HO = "Conference host", DE = "Keywords",
  ID = "Keywords Plus", AB = "Abstract", C1 = "Addresses", RP = "Reprint author",
  EM = "Author email", RI = "Researcher id numbers", OI = "Orcid numbers",
  FU = "Funding agency and grant number", FX = "Funding text",
  CR = "Cited references", NR = "Number of cited references", TC = "Times cited",
  Z9 = "Total times cited count ", U1 = "Usage Count (Last 180 Days)",
  U2 = "Usage Count (Since 2013)", PU = "Publisher", PI = "Publisher city",
  PA = "Publisher address", SN = "ISSN", EI = "eISSN", BN = "ISBN",
  J9 = "Journal.ISI", JI = "Journal.ISO", PD = "Publication date",
  PY = "Year published", VL = "Volume", IS = "Issue", PN = "Part number",
  SU = "Supplement", SI = "Special issue", MA = "Meeting abstract",
  BP = "Beginning page", EP = "Ending page", AR = "Article number",
  DI = "DOI", D2 = "Book DOI", PG = "Page count", WC = "WOS category",
  SC = "Research areas", GA = "Document delivery number", UT = "Access number",
  PM = "Pub Med ID")

.wos.names <- c("Docs", "Authors", "AutDoc", "Categories", "CatDoc", "Areas",
  "AreaDoc", "Addresses", "AddAutDoc", "JCR", "Sources") # NOTA: JCR opcional

.jcr.variable.labels <- c(PY = "JCR year", issn = "JCR ISSN",
  ISI = "JCR ISI", MC = "Best quartile", MP = "Best position",
  ide = "WoS index", MD = "Best WoS category", FI = "Impact factor",
  TD = "Total category", RD = "Category rank, 1 - (MP-1)/(TD-1)")

.db.variable.labels <- c(.wos.variable.labels, .jcr.variable.labels,
  ida = "id author", idd = "id document", idad = "id address", idra = "id area",
  idc = "id category", ids = "id Source", an = "Author number", Country = "Country",
  Univ = "University", OG = "Organization enhanced",
  Sub_org = "Suborganization", DA = "WoS author ID") # OJO: Diferencias en API/Texto


#' WoS bibliographic data base
#'
#' \code{CreateDB.wos} converts a \code{data.frame} with WoS variables (see \code{\link{wosdf}}) into...
#'
#' @aliases wos.db-class
#' @param wosdf data.frame with WoS data... (as returned by \code{\link{ImportSources.wos}}).
#' @param label character string describing the data.
#' @param print logical; if \code{TRUE} the progress is printed.
#' @return An S3 object of \code{\link{class}} \code{wos.db}.
#' A \code{list} with the folowing \code{data.frame}s:
#' \describe{
#'   \item{Docs}{}
#' }
#' Authors AutDoc Categories CatDoc Areas AreaDoc Addresses AddAutDoc Sources
#' @seealso \code{\link{wosdf}}, \code{\link{ImportSources.wos}}.
#' @examples
#' db <- CreateDB.wos(wosdf)
#' str(db, 1)
#' print(db)
#' summary(db)
#' @export
CreateDB.wos <- function(wosdf, label = "", print = interactive()) {
# library(scimetr); data(wosdf); library(dplyr); library(stringr); label = ""; print = TRUE
  ndocs <- nrow(wosdf)
  wosdf$idd = seq_len(ndocs) # OJO: Se añade una variable a los datos

  if (print) {
    progress <- txtProgressBar(min = 0, max = ndocs, style = 3)
    if (print) cat('\nProcessing Documents...\n')
  }
  lautores <- ldirs <- autdir <- vector("list", ndocs)
  # an = Número de autores
  an <- dirsn <- numeric(ndocs)
  san <- 0

  for(i in 1:ndocs){
    # i <- 0; i <- i +1
    # i <- 5
    # update progress bar
    if (print) setTxtProgressBar(progress, i)
    # ~~~~~~~~~~~~~~~~~~
    # Authors: Tabla de autores
    # ida: author index
    # ~~~~~~~~~~~~~~~~~~
    # AF	Nombre completo de autor
    autores0 <- str_split(wosdf$AF[i], '; ')[[1]]
    an[i] <- length(autores0)                       # nº de autores [Docs]
    ida0 <- san + seq_len(an[i]) # Indice provisional de author
    san <- san + an[i]
    # OI	Identificador ORCID
    # https://support.orcid.org/hc/en-us/articles/360006897674-Structure-of-the-ORCID-Identifier
    OI0 <- str_split(wosdf$OI[i], '; ')[[1]]
    res <- str_split(OI0, '/', simplify = TRUE)
    # PROBLEMA: Algún orcid puede tener / en el nombre
    # Hocking / Mennie, Lynne J/0000-0002-2414-2826 (https://orcid.org/0000-0002-2414-2826)
    res <- res[nzchar(res[, 1]), , drop = FALSE]
    autores1 <- cbind(AF = autores0, OI = NA_character_)
    if(nrow(res)) {
      amatch <- match_authors(res[, 1], autores0) # , attr.dist = TRUE)
      # View(cbind(res[, 1], autores0[amatch]))
      index <- !is.na(amatch)
      autores1[amatch[index], ] <- res[index, 1:2]
    }
    # AU	Autores
    # El nombre corto se establecerá como el primero al correspondiente AF
    autores1 <- cbind(AU = str_split(wosdf$AU[i], '; ')[[1]], autores1)
    # Lista de autores
    lautores[[i]] <- autores1
    # do.call(rbind, lautores)
    # Pendiente: RI	Número de ResearcherID
    #   Aparentemente es preferible usar el ORCID
    # Pendiente: EM	Dirección de correo electrónico
    #   Aparecen sin asociar al autor y sin criterio aparente
    # Pendiente: Emplear valores anteriores para identificar autores con distintos AF
    # ~~~~~~~~~~~~~~~~~~
    # Addresses: Tabla direcciones
    # idad, C1, Univ, Country
    # ~~~~~~~~~~~~~~~~~~
    # C1	Dirección de autor
    # PENDIENTE: direcciones vacias which(!nzchar(wosdf$C1)) Warning Unknown?

    ldirs0 <- str_split(wosdf$C1[i], "(; )?\\[")[[1]]
    if (length(ldirs0) == 1) {
      ldirs[i] <- str_split(ldirs0, '; ')
      dirsn[i] <- length(ldirs[[i]])
      autdir[[i]] <- rep(list(ida0), length(ldirs[[i]]))
    } else {
      d <- str_split(ldirs0[-1], '\\] ')
      # Puede haber múltiples direcciones para un grupo de autores
      ldirs[[i]] <- lapply(d, function(x) unlist(str_split(x[2], '; ')))
      res <- sapply(ldirs[[i]], length)
      dirsn[i] <- sum(res)
      autdir[[i]] <- rep(lapply(d, function(x) ida0[charmatch(str_split(x[1], '; ')[[1]], autores0)] ), res)
    }
  } # for(i in 1:ndocs)

  if (print) cat('\nProcessing "Authors"...\n')
  autores <- do.call(rbind, lautores)
  # Autores con mismo identificador ORCID
  OI <- unique(c(NA_character_, autores[, "OI"]))
  ida1 <- match(autores[, "OI"], OI) # Indice de autores
  ida1.first <- match(OI, autores[, "OI"]) # Primer índice
  # Autores sin ORCID por nombre completo
  index <- ida1 == 1  # Autores sin ORCID
  # ida.na <- which(is.na(OI)) # es 1
  # index <- ida1 == ida.na
  if(any(index)) {
    AF0 <- autores[index, "AF"]
    AF <- unique(AF0)
    ida2 <- match(AF0, AF) # Indice de autores por AF
    ida2.first <- match(AF, AF0) # Primer índice
    ida1.first <- c(ida1.first[-1], which(index)[ida2.first])
    ida1[!index] <- ida1[!index] - 1
    ida1[index] <- ida2 + length(OI) - 1
  } else {
    ida1.first <- ida1.first[-1]
    ida1 <- ida1 - 1
  }
  Authors <- data.frame(ida = seq_along(ida1.first), autores[ida1.first, ], stringsAsFactors = FALSE)
  # cbind(autores[1:10, 1], Authors[ida1[1:10], 1])
  # PENDIENTE: En el último paso ordenar por orden alfabético

  # ~~~~~~~~~~~~~~~~~~
  # AutDoc: Tabla de autores por documento
  # idd, ida, idad
  # ~~~~~~~~~~~~~~~~~~
  AutDoc <- data.frame(idd = rep(wosdf$idd, an), ida = ida1)
  # head(AutDoc)
  # tail(AutDoc)

  # ~~~~~~~~~~~~~~~~~~
  # Addresses: Tabla direcciones (contiene todas las direcciones: AddDoc)
  # idad, idd, C1, Univ, Country
  # ~~~~~~~~~~~~~~~~~~
  # C1	Dirección de autor
  # PENDIENTE: direcciones vacias which(!nzchar(wosdf$C1)) Warning Unknown?
  if (print) cat('Processing "Addresses"...\n')
  dirs <- unlist(ldirs)
  # ~~~~
  # Extraer Universidad y Pais
  # NOTA: La universidad se toma como la primera subcadena conteniendo "Univ"
  # NOTA: El pais se toma a partir de la última subcadena
  res <- t(sapply(str_split(dirs, ', '), function(x) c(x[grepl('Univ', x)][1], x[length(x)])))
  Country <- res[, 2]
  # USA aparece con estado e incluso zip: "AL 35294 USA"
  Country[grepl('(.+ )?USA$', Country)] <- 'USA'
  # United Kingdom (UK) aparece por territorios: England, Scotland, Wales (y probablemente Northern Ireland)
  Country[Country %in% c('England', 'Scotland', 'Wales', 'Northern Ireland')] <- 'UK'
  # Hay otros valores "raros": "Peoples R China"
  Country[grepl('Peoples R China', Country)] <- 'China'
  Country <- as.factor(Country)
  idad <- seq_along(dirs)
  idd <- rep(wosdf$idd, dirsn)
  Addresses <- data.frame(idad = idad, idd = idd, C1 = dirs, Univ = res[,1], Country = Country, stringsAsFactors = FALSE)
  # head(Addresses)
  # tail(Addresses)
  # PENDIENTE:
  # Countries <- levels(Country)
  # Country en Addresses como entero
  # Crear tabla CouDoc? sin repetidos
  # Opción para no tener en cuenta autor en direcciones?

  # ~~~~~~~~~~~~~~~~~~
  # AddAutDoc: Tabla de direcciones por autor y documento
  # ida, idd, idad
  # ~~~~~~~~~~~~~~~~~~
  res <- unlist(lapply(autdir, function (x) lapply(x,length)))
  idad <- rep(idad, res)
  idd <- rep( wosdf$idd, sapply(autdir, function(x) sum(sapply(x, length)))) # optimizar...
  AddAutDoc <- data.frame(ida = ida1[unlist(autdir)], idd = idd, idad = idad )
  # head(AddAutDoc)
  # tail(AddAutDoc)

  # ~~~~~~~~~~~~~~~~~~
  # Categories: Tabla de categorías WoS
  # idc, WC
  # ~~~~~~~~~~~~~~~~~~
  if (print) cat('Processing "Categories"...\n')
  categ <- strsplit(wosdf$WC, '; ')
  nn <- sapply(categ, length)   # nº de categ
  categ <- as.factor(unlist(categ))
  Categories <- data.frame(idc = seq_along(levels(categ)), WC = levels(categ), stringsAsFactors=FALSE )
  # head(Categories)

  # ~~~~~~~~~~~~~~~~~~
  # CatDoc: Tabla de categorías por documento/fuentes
  # idd, idc
  # ~~~~~~~~~~~~~~~~~~
  # No todas las fuentes tienen categorías...
  CatDoc <- data.frame(idd = rep(wosdf$idd, nn), idc = as.integer(categ))
  # head(CatDoc)

  # ~~~~~~~~~~~~~~~~~~
  # Areas: Tabla de áreas de investigación
  # idra: research area index
  # idra, SC
  # ~~~~~~~~~~~~~~~~~~
  if (print) cat('Processing "Areas"...\n')
  area <- strsplit(wosdf$SC, '; ')
  nn <- sapply(area, length)   # nº de area
  area <- as.factor(unlist(area))
  Areas <- data.frame(idra = seq_along(levels(area)), SC = levels(area), stringsAsFactors=FALSE )
  # str(Areas)

  # ~~~~~~~~~~~~~~~~~~
  # AreaDoc: Tabla de áreas por documento/fuentes
  # idd, idra
  # ~~~~~~~~~~~~~~~~~~
  # No todas las fuentes tienen categorías...
  AreaDoc <- data.frame(idd = rep(wosdf$idd, nn), idra = as.integer(area))
  # head(AreaDoc)

  # ~~~~~~~~~~~~~~~~~~
  # Sources: Tabla de fuentes
  # ids: Journal ID (ordenado de ISSN)
  # ids, SO:LA, PU:EI, J9, JI
  # ~~~~~~~~~~~~~~~~~~
  if (print) cat('Processing "Sources"...\n')

  # De momento PT -> Sources$ST
  # PT	Tipo de publicación (J=Journal; B=Book; S=Book in Series; P=Patent)
  # wosdf$PT <- factor(wosdf$PT, levels = c("J", "S"))   # Pondrá NA si no es "J" ó "S"
  # levels(wosdf$PT) <- c("Journal", "Series")
  wosdf$PT <- factor(wosdf$PT, levels = c("B", "S", "J", "P"))
  levels(wosdf$PT) <- c("Book", "Book in series", "Journal", "Patent")

  # ids <- with(wosdf,
  #             ifelse(!is.na(BN), BN,
  #               ifelse(!is.na(SN), SN, EI)))
  ids <- with(wosdf,
              ifelse(nzchar(BN), BN,
                ifelse(nzchar(SN), SN, EI)))
  # Tener en cuenta la posibilidad de que any(!nzchar(ids)) sea verdadero
  ids <- match(ids, unique(ids), incomparables = "") # as.integer(as.factor(ids))
  index <- which(is.na(ids))
  # PENDIENTE:
  if(length(index)) {
      warning("Sources without BN, SN or EI were found.")
      ids[index] <- max(ids, na.rm = TRUE) + seq_along(index)
  }
  wosdf$ids <- ids
  Sources <- wosdf %>% select(ids, PT, SO:LA, PU:JI) %>%
    filter(!duplicated(ids))
  attr(Sources, "variable.labels") <- NULL

  # ~~~~~~~~~~~~~~~~~~
  # Docs: Tabla de documentos
  # ~~~~~~~~~~~~~~~~~~
  # Pendiente: Insertar fuente
  # Pendiente: Convertir variables carácter a numéricas
  # ~~~
  if (print) cat('Processing "Docs"...\n')

  # table(wosdf$PT)
  # ~~~
  # DT	Tipo de documento (respuesta múltiple?)
  # Se convierte a factor sin fijar niveles
  # levels <- c("Article", "Article; Proceedings Paper", "Book Review", "Correction",
  #             "Editorial Material", "Letter", "Meeting Abstract", "News Item",
  #             "Proceedings Paper", "Review", "Retraction")
  # wosdf$DT <- factor(wosdf$DT, levels = levels)
  # # NOTA: Some records in Web of Science may have two document types: Article and Proceedings Paper.
  # wosdf$DT[wosdf$DT == "Article; Proceedings Paper"] <- "Proceedings Paper"
  # wosdf$DT <- droplevels(wosdf$DT)
  # table(wosdf$DT)
  # table(wosdf$DT, wosdf$PT)
  wosdf$DT <- factor(wosdf$DT)

  Docs <- wosdf %>%
    select(idd, ids, TI, DT, NR:U2, PD:PG, UT) %>%
    mutate(across(c(NR:U2, PY, PG),  ~suppressWarnings(as.integer(.x)))) %>%           # Convierte a entero
    mutate (UT = as.numeric(substr(UT, 5, 19)),                                        # No se puede almacenar como entero...
            an = an)
  # UT	Número de acceso
  # str(Docs)

  # Resultados
  res <- list(Docs = Docs, Authors = Authors, AutDoc = AutDoc,
              Categories = Categories, CatDoc = CatDoc, Areas = Areas, AreaDoc = AreaDoc,
              Addresses = Addresses, AddAutDoc = AddAutDoc, Sources = Sources,
              label = label)
  attr(res, "variable.labels") <- .wos.variable.labels
  oldClass(res) <- "wos.db"
  return(res)
}


# Funciones genéricas BD
# ~~~~~~~~~~~~~~~~~~

#' @rdname CreateDB.wos
#' @method print wos.db
#' @param x	an object used to select a method.
#' @param ...	further arguments passed to or from other methods.
#' @seealso \code{\link{summary.wos.db}}.
#' @export
print.wos.db <- function(x, ...) {
  str(x, 1)
}

# Pendiente: escala logarítmica comenzando debajo de 1
#' @keywords internal
trans_y_log_2 <- function (base = exp(1), from = 0.1) {
  trans <- function(x) pmax(log(x, base), log(from, base)) # OJO: pmax(log(0), -2) = -2
  inv <- function(x) base^x
  scales::trans_new("scale_y_log_2", trans, inv, scales::log_breaks(base = base),
                    domain =  c(1e-100, Inf))
}
# De momento + geom_rug() siguiendo sugerencia de Edward Joseph Velo Fuentes


#' @rdname CreateDB.wos
#' @method plot wos.db
#' @param filter vector of document identifiers (usually a result of \code{\link{get.idDocs}}).
#' @param which	 if a subset of the plots is required,
#' specify a subset of the numbers \code{1:3}.
#' @param plot.summary logical; if \code{TRUE}, \code{\link{plot.summary.wos.db}} is called.
#' @param which.summary	 indices of the required plots in \code{\link{plot.summary.wos.db}}.
#' @param nmax number of top levels (see \code{\link{summary.wos.db}}).
#' @param plot.summary.year logical; if \code{TRUE}, \code{\link{plot.summary.year}} is called.
#' @param which.summary.year	 indices of the required plots in \code{\link{plot.summary.year}}.
#' @param accuracy numerical scalar; see \code{\link[scales]{label_number}(accuracy=.)}.
#' @param ask	logical; if \code{TRUE}, the user is asked before each plot,
#' see \code{\link{par}(ask=.)}.
#' @seealso \code{\link{plot.summary.wos.db}}, \code{\link{plot.summary.year}}
#' @export
plot.wos.db <- function(x, filter, which = 1:3,
                        plot.summary = FALSE, which.summary = 1:5, nmax = 10,
                        plot.summary.year = FALSE, which.summary.year = 1:3,
                        accuracy = 0.1,
                        ask = (length(which) + plot.summary * length(which.summary) +
                                 plot.summary.year * length(which.summary.year) > 1 )
                        && dev.interactive(),
                        ...) {
  filtered <- !missing(filter)
  if (plot.summary) {
    res <- summary(x, filter, nmax = nmax)
    plot(res, which = which.summary, ask = ask, ...)
  }

  if (plot.summary.year) {
    res <- summary_year(x, filter)
    plot(res, which = which.summary.year, ask = ask, ...)
  }
  show <- rep(FALSE, 3)
  show[which] <- TRUE
  if (ask) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }
  if(any(show[c(1,3)])) docs <- if(filtered) x$Docs[filter, ] else x$Docs
  if(show[1])   # Authors per document
    print(ggplot(x$Docs, aes(an, fill=I("blue"))) + geom_histogram() +
      labs(x = "Authors per document") +
      scale_y_log10() + # scale_y_continuous(trans = scale_y_log_2(base=10, from=0.9)) +
      annotation_logticks(sides = "l")
    )
    # print(qplot(x$Docs$an, geom="histogram", xlab = "Authors per document", fill=I("blue"),log="y"))

  if(show[2]) {
    # Documents per author
    ida <- with(x$AutDoc,
                if(filtered) ida[idd %in% filter] else ida)
    autdoc <- data.frame(ida = as.numeric(table(ida)))
    print(ggplot(autdoc, aes(ida, fill=I("blue"))) + geom_histogram() +
      labs(x = "Documents per author") +
      scale_y_log10() + # scale_y_continuous(trans = scale_y_log_2(base=10, from=0.9)) +
      annotation_logticks(sides = "l") +
      geom_rug()
    )
    # print(qplot(autdoc, geom="histogram", xlab = "Documents per author", fill=I("blue"),log="y"))
    }
  if(show[3])   # Times cited
    print(ggplot(docs, aes(TC, fill=I("blue"))) + geom_histogram() +
      labs(x = "Times cited") +
      scale_y_log10() + # scale_y_continuous(trans = scale_y_log_2(base=10, from=0.9)) +
      annotation_logticks(sides = "l") +
      geom_rug()
    )
    # qplot(docs$TC, geom="histogram", xlab = "Times cited", fill=I("blue"),log="y"))
}



