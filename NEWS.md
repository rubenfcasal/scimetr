# scimetr 1.1 (2025-11-22)

* Added methods for `wos.jcr`-class objects: `plot.wos.jcr()`, `summary.wos.jcr()`,
  `print.summary.wos.jcr()`, `plot.summary.wos.jcr()`, `summary.year.jcr()`,
  `print.summary.year.jcr()`, `plot.summary.year.jcr()`

* Added dataset `dbjcr`.

* Added functions `CreateDBJCR()`, `AddDBJCR()`, `get_jcr()`, `get_jcr_cat()`.

* Added (internal) global variables `.jcr.labels` and `.all.labels`.

* `summary.year()` is now a generic function 
  (with methods for `wos.db` and `wos.jcr` classes).

* Added a return value in plot methods `plot.wos.db()`, `plot.summary.wos.db()`
  and `plot.summary.year.wos()`.
  

# scimetr 1.0 (2025-03-21)

* Major internal changes:

  - Changes to the database structure: new variables, tables, and relationships 
    have been added.
    
  - `NA_character_` is used instead of `""` as missing value in string variables.
  
  - New global objects `.wos.labels` and `WosCat` (stored in *R/sysdata.rda*).
  
* New generic function `CreateDB()` (the previous function `CreateDB.wos()` is 
  now method `CreateDB.wos.data()`).
  
* Added arguments `idOI`, `idRI`, `idAffiliations` to `get.idDocs()`.  
  
* Added functions `get.idOI()`, `get.idRI()`, `get.idAffiliations()`.

* New results were added to `summary()` and `summary_year()`.

* A new dataset `wosdf` is included instead of the previous ones.

* Changes in `match_authors()` to speed up computations.


# scimetr 0.3.6 (2023-04-21)

* Changes in `CreateDB.wos()` to correct inconsistencies in ISSN and eISSN 
  of WoS downloaded data.


# scimetr 0.3.5 (2022-09-16)

* Added *NEWS.md* (Changelog)
  
* Small changes in `ImportSources.wos()` 
  (format changes in WoS tab-delimited files).
  
* Fixed bug in `authors.short()`
  (when author's first name is not included after a comma).
  
* Added a check in `CreateDB.wos()` for duplicated ORCIDs.  


# scimetr 0.3.4 (2021-11-05)

* First stable version.


# scimetr 0.2.3 (2019-09-27)

* Initial version in package form.
  
