#' Import DB
#'
#' `ImportDB.wos` imports tables from remote DataBase Management System (DBMS)...
#'
#' @inherit CreateDB.wos
#' @param dbname 	The path to the database file.
#' @param drv 	DBMS driver (e.g., `RSQLite()`, `MySQL()`, `PostgreSQL()`...)
#' @param ... authentication arguments needed by the DBMS instance;
#' these typically include `user`, `password`, `host`, `port`, etc.
#' For details see the appropriate DBIDriver.
#' @seealso \code{\link{wosdf}}, , \code{\link{ImportSources.wos}}, \code{\link{CreateDB.wos}}.
#' @export
# @rdname CreateDB.wos
ImportDB.wos <- function(dbname, label = dbname, drv = RSQLite::SQLite(), ...) {
  wos_sql <- DBI::dbConnect(RSQLite::SQLite(), dbname, ...)
# dbname <- "bibliometrics_v3.db"; label <- dbname; wos_sql <- DBI::dbConnect(RSQLite::SQLite(), dbname)
  tables <- DBI::dbListTables(wos_sql)
  # stopifnot(tables %in% .wos.names)
  result <- lapply(tables, function(t) wos_sql %>% tbl(t) %>% collect)
  names(result) <- tables
  # str(result, strict.width = "cut")
  # Convertir $Docs$DT a factor (no se fijan los niveles)
  result$Docs$DT <- factor(result$Docs$DT)
  # result$Docs$PT <- factor(result$Docs$PT)
  result$Docs$PT <- NULL # Eliminamos $Docs$PT
  # Renombrar Sources$ST como PT y asignar niveles
  result$Sources <- result$Sources %>% rename(PT = ST)
  levels(result$Sources$PT) <- c("Book", "Book in series", "Journal", "Patent")
  # PENDIENTE: Asegurarse de que no hay NAs
  # JCR
  # -------------
  # PENDIENTE: ELIMINAR rename y variable PC
  # NOTA: Renombramos PD porque coincide con "Publication Date"
  # -------------
  result$JCR <- result$JCR %>% rename(MP = PD, PY = year) %>% select(-PC) %>%
    mutate(RD = 1 - (MP - 1)/(TD - 1), MC = factor(MC, levels = c("Q1", "Q2", "Q3", "Q4")))
  # PENDIENTE: Editions$ide
  # Añadir variables $Docs
  # -------------
  # MC = "Mejor cuartil", MP = "Posición disciplina", TD = "Total disciplina", RD = 1 - (MP - 1)/(TD - 1)
  result$Docs <- result$Docs %>% rename(IS = ISS) %>%
    left_join(select(result$JCR, PY, ids, MC, MP, TD, RD), by = c("ids", "PY"))
  # Se renombra ISS en Docs, IS es una palabra reservada en Sqlite y no se pudo emplear
  # AutDoc no sería necesario al disponer de AddAutDoc
    # Sin embargo nosotros lo utilizamos mucho...
  result$AutDoc <- result$AddAutDoc %>% select(ida, idd) %>% distinct()
  # Devolver objeto S3 de clase `wos.dbi` extendiendo `wos.db`
  result$label <- label
  attr(result, "variable.labels") <- .db.variable.labels # .wos.variable.labels
  # Pendiente: añadir etiquetas por separado a cada tabla
  attr(result, "src_dbi") <- dbplyr::src_dbi(wos_sql)
  DBI::dbDisconnect(wos_sql)
  oldClass(result) <- c("wos.dbi", "wos.jcr", "wos.db")
  return(result)
}

