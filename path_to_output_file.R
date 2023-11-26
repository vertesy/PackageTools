# _________________________________________________________________________________________________
# MarkdownHelpers.R ----
# _________________________________________________________________________________________________
# devtools::load_all("~/GitHub/Packages/MarkdownHelpers")
# source('~/GitHub/Packages/MarkdownHelpers/R/MarkdownHelpers.R')
# rm(list = ls(all.names = TRUE)); try(dev.off(), silent = TRUE)



# Functions
# require(Stringendo); require(ReadWriter); require(CodeAndRoll2); require(ggExpress); require(MarkdownReports); require(Seurat.utils)

# Table of Contents ------------------------------------
# - Generic auxiliary functions
# - Helper functions to parse markdown syntax
# - Printing to the markdown file and to the screen
# - Writing out tabular data / importing markdown
# - Writing markdown tables
# - Internal functions (for Markdown parsing)
# - Graphics and colors
# - Less used functions


# ______________________________________________________________________________________________----
# Generic auxiliary functions ----
# _________________________________________________________________________________________________


# ______________________________________________________________________________________________________________________________
#' @title stopif
#'
#' @description Stop script if the condition is met, and print a message
#' @param condition any condition check that gives TRUE or FALSE
#' @param message print a message
#' @export
#' @examples a <- 1
#' stopif(a != 1, message = "A is 1")
stopif <- function(condition, message = "") {
  if (condition) {
    iprint(message)
    stop()
  }
}

# ______________________________________________________________________________________________________________________________
#' @title irequire
#'
#' @description Load a package. If it does not exist, try to install it from CRAN.
#' @param package Packagename to load
#' @examples irequire(gtools)
#' @export

irequire <- function(package) {
  package_ <- as.character(substitute(package))
  print(package_) # Load a package. If it does not exist, try to install it from CRAN.
  if (!require(package = package_, character.only = TRUE)) {
    print("Not Installed yet.")
    install.packages(pkgs = package_)
    Sys.sleep(1)
    print("Loading package:")
    require(package = package_, character.only = TRUE)
  }
} # install package if cannot be loaded


# ______________________________________________________________________________________________________________________________
#' @title unless.specified
#'
#' @description Return value X (TRUE by default) unless the variable is defined.
#' If defined, it returns the variable.
#' @param NameOfaVariable Name of a possibly defined variable to be tested.
#' @param def Default return value
#' @export
#' @examples unless.specified("xsadasf32", 2)
#' Num <- 22
#' unless.specified("Num", 1)
#' unless.specified("c", 333)
unless.specified <- function(NameOfaVariable, def = TRUE) {
  if (exists(NameOfaVariable)) {
    get(NameOfaVariable)
  } else {
    def
  }
}

# ______________________________________________________________________________________________________________________________
#' @title TRUE.unless
#'
#' @description Return TRUE unless the variable is defined. If defined, it returns the value of the variable.
#' @param NameOfaVariable Name of a possibly defined variable to be tested.
#' @export
#' @examples TRUE.unless("xsadasf32")
#' Num <- 22
#' TRUE.unless("Num")
#' TRUE.unless("c")
TRUE.unless <- function(NameOfaVariable = "VarName") {
  if (exists(substitute(NameOfaVariable))) {
    get(substitute(NameOfaVariable))
  } else {
    iprint(NameOfaVariable, "is not defined, returning TRUE")
    TRUE
  }
}


# ______________________________________________________________________________________________________________________________
#' @title FALSE.unless
#'
#' @description Return FALSE unless the variable is defined. If defined, it returns the value of the variable.
#' @param NameOfaVariable Name of a possibly defined variable to be tested.
#' @export
#' @examples FALSE.unless("xsadasf32")
#' Num <- 22
#' FALSE.unless("Num")
#' FALSE.unless("c")
FALSE.unless <- function(NameOfaVariable = "VarName") {
  if (exists(substitute(NameOfaVariable))) {
    get(substitute(NameOfaVariable))
  } else {
    iprint(NameOfaVariable, "is not defined, returning FALSE")
    FALSE
  }
}



# ______________________________________________________________________________________________________________________________
#' @title lookup
#'
#' @description Awesome pattern matching for a set of values in another set of values. Returns a list with all kinds of results.
#' @param needle A vector of values to look for.
#' @param haystack A vector of values to search in.
#' @param exact Logical. Whether to do an exact match or a partial match.
#' @param report Logical. Whether to print a report of the results.
#' @importFrom Stringendo percentage_formatter
#' @return A list with the results of the lookup.
#'
#' @export
lookup <- function(needle, haystack, exact = TRUE, report = FALSE) { # Awesome pattern matching for a set of values in another set of values. Returns a list with all kinds of results.
  ls_out <- as.list(c(ln_needle = length(needle), ln_haystack = length(haystack), ln_hits = "", hit_poz = "", hits = ""))
  Findings <- numeric(0)
  ln_needle <- length(needle)
  if (exact) {
    for (i in 1:ln_needle) {
      Findings <- c(Findings, which(haystack == needle[i]))
    } # for
  } else {
    for (i in 1:ln_needle) {
      Findings <- c(Findings, grep(needle[i], haystack, ignore.case = TRUE, perl = FALSE))
    } # for
  } # exact or partial match
  ls_out$"hit_poz" <- Findings
  ls_out$"ln_hits" <- length(Findings)
  ls_out$"hits" <- haystack[Findings]
  if (length(Findings)) {
    ls_out$"nonhits" <- haystack[-Findings]
  } else {
    ls_out$"nonhits" <- haystack
  }
  if (report) {
    llprint(
      length(Findings), "/", ln_needle, "(", Stringendo::percentage_formatter(length(Findings) / ln_needle),
      ") of", substitute(needle), "were found among", length(haystack), substitute(haystack), "."
    )
    if (length(Findings)) {
      llprint(substitute(needle), "findings: ", paste(haystack[Findings], sep = " "))
    }
  } else {
    iprint(length(Findings), "Hits:", haystack[Findings])
  } # if (report)
  return(ls_out)
}

# _________________________________________________________________________________________________
#' @title combine.matrices.by.rowname.intersect
#'
#' @description Combine two matrices by their rownames intersect.
#' @param matrix1 A matrix.
#' @param matrix2 A matrix.
#' @param k The number of rows to print from the matrices with the most missing values.
#' @return A matrix with the rows of `matrix1` and `matrix2` that intersect.
#' @importFrom CodeAndRoll2 symdiff
#' @importFrom Stringendo percentage_formatter
#'
#' @export

combine.matrices.by.rowname.intersect <- function(matrix1, matrix2, k = 2) { # combine matrices by rownames intersect
  rn1 <- rownames(matrix1)
  rn2 <- rownames(matrix2)
  idx <- intersect(rn1, rn2)
  llprint(length(idx), "out of", substitute(matrix1), length(rn1), "and", length(rn2), substitute(matrix2), "rownames are merged")
  merged <- cbind(matrix1[idx, ], matrix2[idx, ])
  diffz <- CodeAndRoll2::symdiff(rn1, rn2)
  print("Missing Rows 1, 2")
  x1 <- rowSums(matrix1[diffz[[1]], ])
  x2 <- rowSums(matrix2[diffz[[2]], ])
  print("")
  iprint("Values lost 1: ", round(sum(x1)), "or", Stringendo::percentage_formatter(sum(x1) / sum(merged)))
  print(tail(sort(x1), n = 10))
  print("")
  iprint("Values lost 2: ", round(sum(x2)), "or", Stringendo::percentage_formatter(sum(x2) / sum(merged)))
  print(tail(sort(x2), n = 10))
  iprint("dim:", dim(merged))
  return(merged)
}



# ______________________________________________________________________________________________----
# Helper functions to parse markdown syntax ----
# _________________________________________________________________________________________________


# ______________________________________________________________________________________________________________________________
#' @title ww.variable.and.path.exists
#'
#' @description Check if a variable name is defined, and if so, does the path (to a file) stored in that
#'  variable points to an existing directory?
#' @param path A variable name that might not exist and might point to a non-existent direcotry.
#' @param alt.message Alternative message if the variable + path does not exist. FALSE or string.
#' @export
#' @examples ww.variable.and.path.exists(path = B, alt.message = "Hello, your path/var does not exist.")
ww.variable.and.path.exists <- function(path = path_of_report, alt.message = NULL) {
  Variable.Name <- substitute(path)
  if (exists(as.character(Variable.Name))) {
    dn <- dirname(path)
    ExisingDir <- (dn != "." & dir.exists(dn))
    if (ExisingDir) {
      TRUE
    } else {
      cat("Variable", Variable.Name, " points to a non-existent directory: ", path)
      FALSE
    }
  } else {
    if (is.null(alt.message)) {
      iprint("Variable", Variable.Name, "does not exist.")
    } else {
      cat(alt.message)
    }
    FALSE
  }
}



# ______________________________________________________________________________________________----
# Printing to the markdown file and to the screen ----
# _________________________________________________________________________________________________


# ______________________________________________________________________________________________________________________________
#' @title llprint
#'
#' @description Collapse by white spaces a sentence from any variable passed on to the function.
#' Print the sentence to the screen and write it to your markdown report file,
#' if the "path_of_report" variable is defined.
#' @param ... Variables (strings, vectors) to be collapsed in consecutively.
#' @export
#' @examples MyFriends <- c("Peter", "Bence")
#' llprint("My friends are: ", MyFriends)
llprint <- function(...) {
  argument_list <- c(...)
  LogEntry <- print(paste(argument_list, collapse = " "))
  if (ww.variable.and.path.exists(
    path = path_of_report,
    alt.message = "NOT LOGGED: Log path and filename is not defined in path_of_report."
  )) {
    write(kollapse("\n", LogEntry, print = FALSE),
      path_of_report,
      append = TRUE
    )
  }
}

# ______________________________________________________________________________________________________________________________
#' @title llogit
#'
#' @description Collapse by white spaces a sentence from any variable passed on to the function.
#' llogit() writes it to your markdown report file, if the "path_of_report" variable is defined.
#' It does not print the sentence to the screen.
#' @param ... Variables (strings, vectors) to be collapsed in consecutively.
#' @export
#' @examples MyFriends <- c("Peter", "Bence")
#' llogit("My friends are: ", MyFriends)
llogit <- function(...) {
  argument_list <- c(...)
  LogEntry <- paste(argument_list, collapse = " ")
  LogEntry <- gsub("^ +| +$", "", LogEntry)
  if (ww.variable.and.path.exists(
    path = path_of_report,
    alt.message = "NOT LOGGED: Log path and filename is not defined in path_of_report."
  )) {
    write(kollapse("\n", LogEntry, print = FALSE),
      path_of_report,
      append = TRUE
    )
  }
}

# ______________________________________________________________________________________________________________________________
#' @title md.write.as.list
#'
#' @description Writes a vector as a (numbered) list into the report file.
#' @param vector Vecot to be wirtten as a list
#' @param h Level of header above tl list.
#' @param numbered TRUE = Numbered list, FALSE = unordered list is written
#' @param ... Additional parameters
#' @export
#' @examples md.write.as.list()
md.write.as.list <- function(vector = 1:3,
                             h = 4,
                             numbered = FALSE,
                             path_of_report = ww.set.path_of_report(),
                             ...) {
  LogEntry <- kollapse(rep("#", h), " ", substitute(vector), print = FALSE)
  write(kollapse("\n", LogEntry, print = FALSE),
    path_of_report,
    ...,
    append = TRUE
  )
  LV <- length(vector)
  LN <- if (numbered) {
    paste0(" ", 1:LV, ". ", vector)
  } else {
    paste0(" - ", vector)
  }
  for (i in 1:LV) {
    write(LN[i], path_of_report, append = TRUE)
  }
}

# ______________________________________________________________________________________________________________________________
#' @title md.image.linker
#'
#' @description Format a markdown image reference (link) to a .pdf and .png versions of graph,
#' and insert both links to the markdown report, set by "path_of_report".
#' If the "b.png4Github" variable is set, the .png-link is set up such,
#' that you can upload the whole report with the .png image into your GitHub repo's wiki,
#' under "Reports"/OutDir/ (Reports is a literal string, OutDir is the last/deepest
#' directory name in the "OutDir" variable. See create_set_OutDir() function.).
#' This function is called by the ~wplot functions.
#' @param fname_wo_ext Name of the image file where markdown links going to point to.
#' @param OutDir_ The output directory (absolute / full path).
#' @export
#' @examples md.image.linker(fname_wo_ext = "MyPlot")
md.image.linker <- function(fname_wo_ext, OutDir_ = ww.set.OutDir()) {
  splt <- strsplit(fname_wo_ext, "/")
  fn <- splt[[1]][length(splt[[1]])]
  if (unless.specified("b.usepng")) {
    if (unless.specified("b.png4Github")) {
      dirnm <- strsplit(x = OutDir_, split = "/")[[1]]
      dirnm <- dirnm[length(dirnm)]
      llogit(kollapse("![]", "(Reports/", dirnm, "/", fname_wo_ext, ".png)", print = FALSE))
    } else {
      if (exists("b.Subdirname") && !b.Subdirname == FALSE) {
        fname_wo_ext <- paste0(b.Subdirname, "/", fname_wo_ext)
      } # set only if b.Subdirname is defined, it is not FALSE.
      llogit(kollapse("![", fn, "]", "(", fname_wo_ext, ".png)", print = FALSE))
    }
  } else {
    llogit(kollapse("![", fn, "]", "(", fname_wo_ext, ".pdf)", print = FALSE))
  } # if b.usepng
}

# ______________________________________________________________________________________________________________________________
#' @title llwrite_list
#'
#' @description Print a list object from R, one element per line, into your markdown report
#' @param yourlist your list
#' @param printName print header level 4: the name of the list or a custom string
#' @export
#' @examples your_list <- list(letters[1:4], 5:9)
#' llwrite_list(your_list)
llwrite_list <- function(yourlist, printName = "self") {
  if (printName == "self") {
    llprint("####", substitute(yourlist))
  } else if (printName == FALSE) {
    ""
  } else {
    llprint("####", printName)
  }
  for (e in 1:length(yourlist)) {
    if (is.null(names(yourlist))) {
      llprint("#####", names(yourlist)[e])
    } else {
      llprint("#####", e)
    }
    print(yourlist[e])
    llogit("`", yourlist[e], "`")
  }
}


# ______________________________________________________________________________________________----
# Writing out tabular data / importing markdown ----
# _________________________________________________________________________________________________


# ______________________________________________________________________________________________________________________________
#' @title md.import
#'
#' @description Import and concatenated an external markdown or text file to the report
#' @param from.file File to be appended at the (current) last line of the report
#' @param to.file The report file. Defined as "path_of_report" by default,
#' which is set by the "setup_MarkdownReports" function.
#' @examples path_of_report <- ww.set.path_of_report()
#' llprint("Hello")
#' # md.import(path_of_report)
#' @importFrom Stringendo iprint
#'
#' @export
md.import <- function(from.file, to.file = ww.set.path_of_report()) {
  linez <- readLines(from.file)
  if (ww.variable.and.path.exists(to.file,
    alt.message = "Log path and filename is not defined in path_of_report"
  )) {
    Stringendo::iprint(
      length(linez), "lines from", basename(from.file),
      "are concatenated to:", basename(to.file)
    )
  }
  for (LogEntry in linez) {
    write(LogEntry, to.file, append = TRUE)
  }
}


# Writing markdown tables --------------------------------------------------------------------------

# ______________________________________________________________________________________________________________________________
#' @title md.LogSettingsFromList
#'
#' @description Log the parameters & settings used in the script and stored in a list, in a table format
#'  in the report.
#' @param parameterlist List of Paramters
#' @param maxlen Maximum length of entries in a parameter list element
#' @export
#' @examples md.LogSettingsFromList(parameterlist = list("min" = 4, "method" = "pearson", "max" = 10))
md.LogSettingsFromList <- function(parameterlist,
                                   maxlen = 20) {
  LZ <- unlist(lapply(parameterlist, length)) # collapse paramters with multiple entires
  LNG <- names(which(LZ > 1))
  for (i in LNG) {
    if (length(parameterlist[[i]]) > maxlen) {
      parameterlist[[i]] <- parameterlist[[i]][1:maxlen]
    }
    parameterlist[[i]] <- paste(parameterlist[[i]], collapse = ", ")
  } # for
  DF <- t(as.data.frame(parameterlist))
  colnames(DF) <- "Value"
  md.tableWriter.DF.w.dimnames(DF, title_of_table = "Script Parameters and Settings")
}


# Writing markdown tables --------------------------------------------------------------------------

#' @title md.List2Table
#'
#' @description Broader variant of md.LogSettingsFromList(). Log the values (col2) from a
#' named (col1) list, in a table format in the report.
#' @param title Title of the table.
#' @param colname2 Name of the 2nd column.
#' @param parameterlist List of Paramters.
#' @param maxlen Maximum length of entries in a parameter list element,.
#' @export
#' @examples md.LogSettingsFromList(parameterlist = list("min" = 4, "method" = "pearson", "max" = 10))
md.List2Table <- function(parameterlist,
                          title = "List elements",
                          colname2 = "Value",
                          maxlen = 20) {
  LZ <- unlist(lapply(parameterlist, length)) # collapse paramters with multiple entires
  LNG <- names(which(LZ > 1))
  for (i in LNG) {
    if (length(parameterlist[[i]]) > maxlen) {
      parameterlist[[i]] <- parameterlist[[i]][1:maxlen]
    }
    parameterlist[[i]] <- paste(parameterlist[[i]], collapse = ", ")
  } # for
  DF <- t(as.data.frame(parameterlist))
  colnames(DF) <- colname2
  md.tableWriter.DF.w.dimnames(DF, title_of_table = title)
}


# ______________________________________________________________________________________________________________________________
#' @title md.tableWriter.DF.w.dimnames
#'
#' @description Take an R data frame with row- and column- names, parse a markdown table from it,
#' and write it to the markdown report, set by "path_of_report".
#' @param df Input data frame to be plotted
#' @param FullPath Full path to the file.
#' @param percentify Format numbers between 0-1 to percentages 0-100.
#' @param title_of_table Title above the table (in the markdown report).
#' @param print2screen Print the markdown formatted table to the sceen.
#' @param WriteOut Write the table into a TSV file.
#' @examples df <- matrix(1:9, 3)
#' rownames(df) <- 6:8
#' rownames(df) <- 9:11
#' md.tableWriter.DF.w.dimnames(df, percentify = FALSE, title_of_table = NA)
#' @importFrom ReadWriter write.simple.tsv
#' @importFrom CodeAndRoll2 iround
#' @importFrom Stringendo percentage_formatter
#'
#' @export


md.tableWriter.DF.w.dimnames <- function(df,
                                         FullPath = ww.set.path_of_report(),
                                         percentify = FALSE,
                                         title_of_table = NA,
                                         print2screen = FALSE,
                                         WriteOut = FALSE) {
  if (is.na(title_of_table)) {
    t <- paste0(substitute(df), collapse = " ")
  } else {
    t <- title_of_table
  }

  title_of_table <- paste("\n#### ", t)
  if (file.exists(FullPath)) {
    write(title_of_table, FullPath, append = TRUE)

    h <- paste(colnames(df), collapse = " \t| ")
    h <- paste("\n| |", h, " |", collapse = "")
    ncolz <- dim(df)[2] + 1
    nrows <- dim(df)[1]
    rn <- rownames(df)
    sep <- kollapse(rep("| ---", ncolz), " |", print = FALSE)

    write(h, FullPath, append = TRUE)
    if (print2screen) {
      cat(h, "\n")
    }
    write(sep, FullPath, append = TRUE)
    if (print2screen) {
      cat(sep, "\n")
    }
    for (r in 1:nrows) {
      if (is.numeric(unlist(df[r, ]))) {
        b <- CodeAndRoll2::iround(df[r, ])
        if (percentify) {
          b <- Stringendo::percentage_formatter(b)
        }
      } else {
        b <- df[r, ]
      }
      b <- paste(b, collapse = " \t| ")
      b <- paste("|", rn[r], "\t|", b, " |", collapse = "")
      write(b, FullPath, append = TRUE)
      if (print2screen) {
        cat(b, "\n")
      }
    }
  } else {
    print("NOT LOGGED: Log path and filename is not defined in FullPath")
  }
  if (WriteOut) {
    ReadWriter::write.simple.tsv(df, ManualName = paste0(substitute(df), ".tsv"))
  }
}
# md.tableWriter.DF.w.dimnames(GeneCounts.per.sex, print2screen = TRUE)
# ALIAS
# MarkDown_Table_writer_DF_RowColNames = md.tableWriter.DF.w.dimnames


# ______________________________________________________________________________________________________________________________
#' @title md.tableWriter.VEC.w.names
#'
#' @description Take an R vector with names, parse a markdown table from it, and write it to the markdown report,
#'  set by "path_of_report".
#' @param NamedVector A vector for the table body, with names as table header.
#' @param FullPath Full path to the file.
#' @param percentify Format numbers (0, 1) to percentages 0-100.
#' @param title_of_table Title above the table (in the markdown report).
#' @param print2screen Print the markdown formatted table to the sceen.
#' @param WriteOut Write the table into a TSV file.
#' @examples x <- -1:2
#' names(x) <- LETTERS[1:4]
#' md.tableWriter.VEC.w.names(NamedVector = x, percentify = FALSE, title_of_table = NA)
#' @importFrom ReadWriter write.simple.tsv
#' @importFrom CodeAndRoll2 iround
#' @importFrom Stringendo percentage_formatter
#'
#' @export
md.tableWriter.VEC.w.names <- function(NamedVector,
                                       FullPath = ww.set.path_of_report(),
                                       percentify = FALSE,
                                       title_of_table = NA,
                                       print2screen = FALSE,
                                       WriteOut = FALSE) {
  if (is.na(title_of_table)) {
    t <- paste0(substitute(NamedVector), collapse = " ")
  } else {
    t <- title_of_table
  }
  title_of_table <- paste("\n#### ", t)
  if (file.exists(FullPath)) {
    write(title_of_table, FullPath, append = TRUE)
    if (!is.table(NamedVector)) {
      if (is.list(NamedVector) & any(lapply(NamedVector, length) > 1)) {
        print("This complex list cannot be parsed to a table.")
      }
      if (is.numeric(NamedVector)) {
        NamedVector <- CodeAndRoll2::iround(NamedVector)
      }
    }
    h <- paste(names(NamedVector), collapse = " \t| ")
    h <- paste("\n| ", h, " |", collapse = "")
    ncolz <- length(NamedVector)
    sep <- kollapse(rep("| ---", ncolz), " |", print = FALSE)
    write(h, FullPath, append = TRUE)
    if (print2screen) {
      cat(h, "\n")
    }
    write(sep, FullPath, append = TRUE)
    if (print2screen) {
      cat(sep, "\n")
    }

    if (percentify & is.numeric(NamedVector)) {
      NamedVector <- Stringendo::percentage_formatter(NamedVector)
    }
    b <- paste(NamedVector, collapse = " \t| ")
    b <- paste("|", b, " |", collapse = "")
    write(b, FullPath, append = TRUE)
  } else {
    print("NOT LOGGED: Log path and filename is not defined in FullPath")
  }
  if (WriteOut) {
    ReadWriter::write.simple.tsv(NamedVector, ManualName = paste0(substitute(NamedVector), ".tsv"))
  }
  if (print2screen) {
    cat(b, "\n")
  }
}



# ______________________________________________________________________________________________________________________________
#' @title md.LinkTable
#'
#' @description Take a dataframe where every entry is a string containing an html link, parse and write out.
#'  a properly formatted markdown table.
#' @param tableOfLinkswRownames A dataframe where every entry is a string containing an html link.
#' @export
#'
#' @examples tableOfLinkswRownames(tableOfLinkswRownames = df_of_LinksParsedByDatabaseLinkeR)
md.LinkTable <- function(tableOfLinkswRownames) {
  TBL <- tableOfLinkswRownames
  RN <- rownames(tableOfLinkswRownames)
  for (i in 1:ncol(tableOfLinkswRownames)) {
    x <- tableOfLinkswRownames[, i]
    TBL[, i] <- paste0("[", RN, "]", "(", x, ")")
  } # for
  md.tableWriter.DF.w.dimnames(TBL,
    FullPath = paste0(OutDir, substitute(tableOfLinkswRownames), ".tsv.md")
  )
}



# ______________________________________________________________________________________________________________________________
#' @title md.import.table
#'
#' @description Import a table (.csv, or tab seprated values, .tsv file) and write it
#' in markdown format to the report.
#' @param from.file.table  The *.tsv file to be appended  as table at
#' the (current) last line of the report.
#' @param title_of_table Title above the table (as header 4, in the markdown report).
#' @param has.rownames If the first column contains (unique!) rownames.
#' @param has.colnames If the first line of the file contains the header, or the column names.
#' @param field.sep Field separator in table file. Tab's by default.
#' @param to.file The report file. Defined as "path_of_report" by default,
#'  which is set by the "setup_MarkdownReports" function.
#' @importFrom Stringendo iprint
#' @examples x <- matrix(1:9, 3)
#' write.table(x, sep = "\t", file = "~/x.tsv")
#' md.import.table("~/x.tsv")
#'
#' @export
md.import.table <- function(from.file.table,
                            title_of_table,
                            has.rownames = TRUE,
                            has.colnames = TRUE,
                            field.sep = "\t",
                            to.file = ww.set.path_of_report()) {
  TTL <- if (missing(title_of_table)) {
    basename(from.file.table)
  } else {
    title_of_table
  }
  importedtable <- if (has.rownames) {
    read.table(
      from.file.table,
      stringsAsFactors = FALSE,
      sep = "\t",
      header = has.colnames,
      row.names = 1
    )
  } else if (!has.rownames) {
    read.table(
      from.file.table,
      stringsAsFactors = FALSE,
      sep = "\t",
      header = has.colnames
    )
  }
  md.tableWriter.DF.w.dimnames(importedtable, title_of_table = TTL)
  Stringendo::iprint("The follwoing table is included in the markdown report:")
  return(importedtable)
}

# ______________________________________________________________________________________________----
# Filtering Data ----
# _________________________________________________________________________________________________


#' @title filter_HP
#'
#' @description Filter values that fall between above high-pass-threshold (X >).
#'
#' @param numeric_vector Values to be filtered.
#' @param threshold A numeric value above which "numeric_vector" passes.
#' @param passequal Pass if a value is larger, or equal than the threshold. FALSE by default.
#' @param prepend Text prepended to the results.
#' @param return_conclusion Return conclusion sentence that (also printed). return_survival_ratio must be FALSE
#' @param return_survival_ratio Return a number with the survival ratio (TRUE).
#' or a logical index vector of the survivors (FALSE). return_conclusion must be FALSE
#' @param plot.hist Plot the histogram of the input data
#' @param saveplot Save the histogram as PDF, FALSE by defeault
#' @param na.rm Remove NA-s? Default: TRUE
#' @param verbose print output to console? Default: yes.
#' @param ... Additional arguments for the histogram
#' @examples filter_HP(
#'   numeric_vector = rnorm(1000, 6), threshold = 5,
#'   prepend = "From all values ", return_survival_ratio = FALSE
#' )
#' @importFrom CodeAndRoll2 iround
#' @importFrom Stringendo percentage_formatter
#'
#' @export
filter_HP <- function(numeric_vector,
                      threshold,
                      passequal = FALSE,
                      prepend = "",
                      return_survival_ratio = FALSE,
                      return_conclusion = FALSE,
                      na.rm = TRUE,
                      plot.hist = TRUE,
                      saveplot = FALSE,
                      verbose = TRUE,
                      ...) {
  survivors <-
    if (passequal) {
      numeric_vector >= threshold
    } else {
      numeric_vector > threshold
    }
  pc <- Stringendo::percentage_formatter(sum(survivors, na.rm = na.rm) / length(survivors))
  conclusion <- kollapse(
    prepend, pc, " or ", sum(survivors, na.rm = na.rm), " of ", length(numeric_vector),
    " entries in ", substitute(numeric_vector), " fall above a threshold value of: ",
    CodeAndRoll2::iround(threshold),
    print = verbose
  )

  if (ww.variable.and.path.exists(path_of_report, alt.message = "NOT LOGGED")) {
    llogit(conclusion)
  }

  if (plot.hist & require("MarkdownReports")) {
    plotname <- substitute(numeric_vector)
    MarkdownReports::whist(
      variable = numeric_vector,
      main = plotname,
      vline = threshold,
      filtercol = 1,
      savefile = saveplot,
      ...
    )
  }
  if (return_survival_ratio) {
    return(sum(survivors, na.rm = na.rm) / length(survivors))
  } else if (return_conclusion) {
    return(conclusion)
  } else if (!return_survival_ratio) {
    return(survivors)
  }
}


# ______________________________________________________________________________________________________________________________
#' @title filter_LP
#'
#' @description Filter values that fall below the low-pass threshold (X <).
#' @param numeric_vector Values to be filtered.
#' @param threshold A numeric value below which "numeric_vector" passes.
#' @param passequal Pass if a value is smaller, or equal than the threshold. FALSE by default.
#' @param prepend Text prepended to the results.
#' @param return_conclusion Return conclusion sentence that (also printed). return_survival_ratio must be FALSE
#' @param return_survival_ratio Return a number with the survival ratio (TRUE).
#' or a logical index vector of the survivors (FALSE). return_conclusion must be FALSE
#' @param plot.hist Plot the histogram of the input data
#' @param saveplot Save the histogram as PDF, FALSE by defeault
#' @param na.rm Remove NA-s? Default: TRUE
#' @param verbose print output to console? Default: yes.
#' @param ... Additional arguments for the histogram
#' @examples filter_LP(
#'   numeric_vector = rnorm(1000, 6), threshold = 5,
#'   prepend = "From all values ", return_survival_ratio = FALSE
#' )
#' @importFrom CodeAndRoll2 iround
#' @importFrom Stringendo percentage_formatter
#'
#' @export
filter_LP <- function(numeric_vector,
                      threshold,
                      passequal = FALSE,
                      prepend = "",
                      return_survival_ratio = FALSE,
                      return_conclusion = FALSE,
                      na.rm = TRUE,
                      plot.hist = TRUE,
                      saveplot = FALSE,
                      verbose = TRUE,
                      ...) {
  survivors <-
    if (passequal) {
      numeric_vector <= threshold
    } else {
      numeric_vector < threshold
    }
  pc <- Stringendo::percentage_formatter(sum(survivors, na.rm = na.rm) / length(survivors))
  conclusion <- kollapse(
    prepend, pc, " or ", sum(survivors, na.rm = na.rm), " of ",
    length(numeric_vector), " entries in ", substitute(numeric_vector),
    " fall below a threshold value of: ", CodeAndRoll2::iround(threshold),
    print = verbose
  )
  if (ww.variable.and.path.exists(path_of_report, alt.message = "NOT LOGGED")) {
    llogit(conclusion)
  }

  if (plot.hist & require("MarkdownReports")) {
    plotname <- substitute(numeric_vector)
    MarkdownReports::whist(
      variable = numeric_vector,
      main = plotname,
      vline = threshold,
      filtercol = -1,
      savefile = saveplot,
      ...
    )
  }
  if (return_survival_ratio) {
    return(sum(survivors, na.rm = na.rm) / length(survivors))
  } else if (return_conclusion) {
    return(conclusion)
  } else if (!return_survival_ratio) {
    return(survivors)
  }
}



# ______________________________________________________________________________________________________________________________
#' @title filter_MidPass
#'
#' @description Filter values that fall above high-pass-threshold !(X >= )! and below
#' the low-pass threshold (X <).
#' @param numeric_vector Values to be filtered.
#' @param HP_threshold Lower threshold value. (>= )
#' @param LP_threshold Upper threshold value. (<)
#' @param prepend Text prepended to the results.
#' @param return_conclusion Return conclusion sentence that (also printed). return_survival_ratio must be FALSE
#' @param return_survival_ratio Return a number with the survival ratio (TRUE).
#' or a logical index vector of the survivors (FALSE). return_conclusion must be FALSE
#' @param EdgePass If TRUE, it reverses the filter:
#' everything passes except between the two thresholds.
#' @param plot.hist Plot the histogram of the input data
#' @param saveplot Save the histogram as PDF, FALSE by defeault
#' @param na.rm Remove NA-s? Default: TRUE
#' @param verbose print output to console? Default: yes.
#' @param ... Additional arguments for the histogram
#' @examples filter_MidPass(
#'   numeric_vector = rnorm(1000, 6), HP_threshold = 4,
#'   LP_threshold = 8, prepend = "From all values ", return_survival_ratio = FALSE, EdgePass = TRUE
#' )
#' @importFrom Stringendo percentage_formatter
#' @importFrom CodeAndRoll2 iround
#'
#' @export
filter_MidPass <- function(numeric_vector,
                           HP_threshold,
                           LP_threshold,
                           prepend = "",
                           return_survival_ratio = FALSE,
                           return_conclusion = FALSE,
                           EdgePass = FALSE,
                           na.rm = TRUE,
                           plot.hist = TRUE,
                           saveplot = FALSE,
                           verbose = TRUE,
                           ...) {
  survivors <- (numeric_vector >= HP_threshold & numeric_vector < LP_threshold)
  keyword <- "between"
  relation <- " <= x < "

  if (EdgePass) {
    survivors <- (numeric_vector < HP_threshold |
      numeric_vector >= LP_threshold)
    keyword <- "outside"
    relation <- " >= x OR x > "
  }
  pc <- Stringendo::percentage_formatter(sum(survivors, na.rm = na.rm) / length(survivors))
  conclusion <- kollapse(prepend, pc, " or ", sum(survivors, na.rm = na.rm), " of ",
    length(numeric_vector), " entries in ", substitute(numeric_vector),
    " fall ", keyword, " the thresholds: ", CodeAndRoll2::iround(HP_threshold),
    relation, CodeAndRoll2::iround(LP_threshold),
    print = verbose
  )
  if (ww.variable.and.path.exists(path_of_report, alt.message = "NOT LOGGED")) {
    llogit(conclusion)
  }

  if (plot.hist & require("MarkdownReports")) {
    plotname <- substitute(numeric_vector)
    MarkdownReports::whist(
      variable = numeric_vector,
      main = plotname,
      vline = c(HP_threshold, LP_threshold),
      filtercol = if (EdgePass) -1 else 1,
      savefile = saveplot,
      ...
    )
  }
  if (return_survival_ratio) {
    return(sum(survivors, na.rm = na.rm) / length(survivors))
  } else if (return_conclusion) {
    return(conclusion)
  } else if (!return_survival_ratio) {
    return(survivors)
  }
}




# ______________________________________________________________________________________________----
# Internal functions (for Markdown parsing) ----
# _________________________________________________________________________________________________



#' @title ww.FnP_parser
#'
#' @description Internal Function. Parses the full path from the filename & location of the file.
#' @param fname Name of the file
#' @param ext_wo_dot File extension without separating dot.
#' @examples ww.FnP_parser(fname = "myplot", ext_wo_dot = "jpg")
#'
#' @export
ww.FnP_parser <- function(fname, ext_wo_dot) {
  path <- if (exists("ww.set.OutDir")) {
    ww.set.OutDir()
  } else {
    (getwd())
    "install or load vertesy/MarkdownHelpers for saving into OutDir!"
  }

  FnP <- if (hasArg(ext_wo_dot)) {
    kollapse(path, fname, ".", ext_wo_dot)
  } else {
    FnP <- kollapse(path, fname)
  }
}




#' @title ww.variable.and.path.exists
#'
#' @description Check if a variable name is defined, and if so, does the path (to a file) stored in that
#'  variable points to an existing directory?
#' @param path A variable name that might not exist and might point to a non-existent direcotry.
#' @param alt.message Alternative message if the variable + path does not exist. FALSE or string.
#' @importFrom Stringendo iprint
#' @export
#' @examples ww.variable.and.path.exists(path = B, alt.message = "Hello, your path/var does not exist.")
ww.variable.and.path.exists <- function(path = path_of_report, alt.message = NULL) {
  Variable.Name <- substitute(path)
  if (exists(as.character(Variable.Name))) {
    dn <- dirname(path)
    ExisingDir <- (dn != "." & dir.exists(dn))
    if (ExisingDir) {
      TRUE
    } else {
      cat("Variable", Variable.Name, " points to a non-existent directory: ", path)
      FALSE
    }
  } else {
    if (is.null(alt.message)) {
      Stringendo::iprint("Variable", Variable.Name, "does not exist.")
    } else {
      cat(alt.message)
    }
    FALSE
  }
}


#' @title ww.variable.exists.and.true
#'
#' @description Check if a variable name is defined, and if so, is it TRUE
#' @param var A variable
#' @param alt.message Alternative message if the variable + path does not exist. FALSE or string.
#' @importFrom Stringendo iprint
#' @export
#' @examples ww.variable.and.path.exists(path = B, alt.message = "Hello, your path/var does not exist.")
ww.variable.exists.and.true <- function(var, alt.message = NULL) {
  Variable.Name <- substitute(var)
  if (exists(as.character(Variable.Name))) {
    if (isTRUE(var)) {
      TRUE
    } else {
      cat("Variable", Variable.Name, " is not true: ", var)
      FALSE
    }
  } else {
    if (is.null(alt.message)) {
      Stringendo::iprint("Variable", Variable.Name, "does not exist.")
    } else {
      cat(alt.message)
    }
    FALSE
  }
}

# al1=TRUE; al3=FALSE; al4=3232; # al2 not defined
# ww.variable.exists.and.true(al1)
# ww.variable.exists.and.true(al2)
# ww.variable.exists.and.true(al3)
# ww.variable.exists.and.true(al4)



#' @title ww.set.OutDir
#'
#' @description Checks if global variable OutDir is defined. If not,
#' it returns the current working directory
#' @param dir OutDir to check and set.
#' @importFrom Stringendo iprint
#' @examples ww.set.OutDir()
#'
#' @export

ww.set.OutDir <- function(dir = OutDir) {
  if (!exists("OutDir")) Stringendo::iprint("OutDir not defined !!! Saving in working directory.")
  dir <- getwd()
  if (!dir.exists(dir)) Stringendo::iprint("OutDir defined, but folder does not exist!!! Saving in working directory.")
  NewOutDir <-
    if (exists("OutDir") & dir.exists(dir)) {
      dir
    } else {
      paste0(getwd(), "/", collapse = "")
    }
  return(FixPath(NewOutDir))
}


#' @title ww.set.path_of_report
#'
#' @description Checks if global variable path_of_report is defined. If not,
#' it defines it as Analysis.md in the current working directory
#' @importFrom Stringendo iprint
#' @examples ww.set.path_of_report()
#'
#' @export

ww.set.path_of_report <- function() {
  new.path_of_report <-
    if (ww.variable.and.path.exists(path = path_of_report)) {
      path_of_report
    } else {
      Stringendo::iprint(
        "path_of_report is not defined! Setting it to Analysis.md in the working directory:",
        getwd(), "/Analysis.md"
      )
      paste0(getwd(), "/Analysis.md", collapse = "")
    }
}


#' @title ww.set.PlotName
#'
#' @description Generates a plotname (use if none is specified)
#' @importFrom Stringendo iprint
#' @examples ww.set.PlotName()
#'
#' @export

ww.set.PlotName <- function() {
  NewPlotname <-
    if (exists("plotnameLastPlot")) {
      plotnameLastPlot
    } else {
      Stringendo::iprint("plotnameLastPlot not defined! Naming file after the date and time.")
      paste0(ww.autoPlotName(), ".pdf", collapse = "")
    }
  print(NewPlotname)
}


#' @title ww.set.mdlink
#'
#' @description Internal function. Sets inserting a markdown link to the image
#' (created by the wplot* function that calls this function) only if 'path_of_report' is defined
#'  and 'b.mdlink' is defined as TRUE.
#' @param NameOfaVariable Name of a possibly defined variable to be tested.
#' @param def Default return value
#' @export
#' @examples ww.set.mdlink() # It is an internal function, not intended for manual use.
ww.set.mdlink <- function(NameOfaVariable = "b.mdlink",
                          def = FALSE) {
  if (ww.variable.and.path.exists(path_of_report) && exists(NameOfaVariable)) {
    get(NameOfaVariable)
  } else {
    def
  }
}


#' @title ww.md.image.link.parser
#'
#' @description Format a markdown image reference (link) from the file path to the file.
#' It can parse the file path, if you pass it in separate variables and strings.
#' E.g. ww.md.image.link.parser(Directory, "MyImage.png").
#' @param ... Variables (strings, vectors) to be collapsed in consecutively.
#' @export
#' @examples ww.md.image.link.parser("/MyPlot.jpg")
#' ww.md.image.link.parser(getwd(), "/MyPlot.jpg")
ww.md.image.link.parser <- function(...) {
  FullPath <- kollapse(..., print = FALSE)
  splt <- strsplit(FullPath, "/")
  fn <- splt[[1]][length(splt[[1]])]
  kollapse("![", fn, "]", "(", FullPath, ")", print = FALSE)
}

#' @title ww.ttl_field
#'
#' @description Internal function. Creates the string written into the PDF files "Title' (metadata) field.
#' @param plotname Name of the plot
#' @param creator String X in: "plotblabla by X". Defaults: "MarkdownReports".
#' @export
#' @examples ww.ttl_field("/Users/myplot.jpg")
ww.ttl_field <- function(plotname, creator = "MarkdownReports") {
  paste0(
    basename(plotname), " by ",
    unless.specified("b.scriptname", def = creator)
  )
}


#' @title ww.autoPlotName
#'
#' @description Internal function. Creates automatic plot and file-names.
#' @param name Manually name your plot
#' @export
#' @examples ww.autoPlotName()
ww.autoPlotName <- function(name = NULL) {
  if (is.null(name)) {
    filename <- if (exists("plotnameLastPlot")) {
      plotnameLastPlot
    } else {
      make.names(date())
    }
  } else {
    filename <- name
  }
  return(filename)
}


#' @title ww.assign_to_global
#'
#' @description A function loading results to the global environment.
#' Source: https://stackoverflow.com/questions/28180989/
#' @param name Name of the global variabe to be assigned
#' @param value Value of the global variabe to be assigned
#' @param verbose Print directory to screen? Default: TRUE
#' @param max_print Print max this many elements, Default: 10
#' @param pos defaults to 1 which equals an assingment to global environment
#' @importFrom Stringendo iprint
#'
#' @export

ww.assign_to_global <- function(name, value, pos = 1, max_print = 5, verbose = TRUE) {
  if (verbose) Stringendo::iprint(name, "defined as:", head(value, max_print)) # , "is a new global environment variable"
  assign(name, value, envir = as.environment(pos))
}



# ______________________________________________________________________________________________----
# Graphics and colors ----
# _________________________________________________________________________________________________

#' @title try.dev.off
#'
#' @description Tries to close R graphical devices without raising an error.
#' @export
#' @examples try.dev.off()
try.dev.off <- function() {
  try(dev.off(), silent = TRUE)
}


#' @title jjpegA4
#'
#' @description Setup an A4 size jpeg.
#' @param filename The filename of the jpeg file to create.
#' @param r The resolution of the jpeg file.
#' @param q The quality of the jpeg file.
#' @param h Height
#' @param w Width
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   jjpegA4(filename = "my_plot.jpeg", r = 225, q = 90)
#' }
#' }
#'
#' @export
jjpegA4 <- function(filename, r = 225, q = 90, w = 8.27, h = 11.69) { # Setup an A4 size jpeg
  jpeg(file = filename, width = w, height = h, units = "in", quality = q, res = r)
}


#' @title color_check
#'
#' @description Display the colors encoded by the numbers / color-ID-s you pass on to this function
#' @param ... Additional parameters.
#' @param incrBottMarginBy Increase the blank space at the bottom of the plot.
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @examples color_check(1:3)
#'
#' @export
color_check <- function(..., incrBottMarginBy = 0, savefile = FALSE) {
  if (incrBottMarginBy) {
    .ParMarDefault <- par("mar")
    par(mar = c(par("mar")[1] + incrBottMarginBy, par("mar")[2:4]))
  } # Tune the margin
  Numbers <- c(...)
  if (length(names(Numbers)) == length(Numbers)) {
    labelz <- names(Numbers)
  } else {
    labelz <- Numbers
  }
  barplot(rep(10, length(Numbers)), col = Numbers, names.arg = labelz, las = 2)
  if (incrBottMarginBy) {
    par("mar" = .ParMarDefault)
  }

  fname <- substitute(...)
  if (savefile) {
    dev.copy2pdf(file = ww.FnP_parser(fname, "ColorCheck.pdf"))
  }
}




#' @title wcolorize
#'
#' @description Generate color palettes. Input: a vector with categories, can be numbers or strings.
#' Handles repeating values. Output: color vector of equal length as input.
#' Optionally it can ouput a list where an extra element lists the
#' categories (simply using unique would remove the names). See example.
#' Some color scale depend on packages "colorRamps", or "gplots".
#'
#' @param vector A vector with categories, can be numbers or strings
#' @param ReturnCategoriesToo Return unique Categories. See example.
#' @param show Show generated color palette
#' @param set Color palette for base
#' ("heat.colors", "terrain.colors", "topo.colors", "rainbow"),
#' or "rich" for gplots::rich.colors, or  "matlab" for colorRamps::matlab.like.
#' @param RColorBrewerSet Use one of the RColorBrewer color sets? Provide that name
#' @param randomize Randomize colors
#' @examples wcolorize(vector = c(1, 1, 1:6), ReturnCategoriesToo = TRUE, show = TRUE)
#' @importFrom RColorBrewer brewer.pal
#' @importFrom colorRamps matlab.like
#' @importFrom gplots rich.colors
#' @importFrom CodeAndRoll2 as.numeric.wNames.factor
#'
#' @export
wcolorize <- function(vector = c(1, 1, 1:6),
                      RColorBrewerSet = FALSE,
                      ReturnCategoriesToo = FALSE,
                      show = FALSE,
                      randomize = FALSE,
                      set = c(
                        FALSE,
                        "rich",
                        "heat.colors",
                        "terrain.colors",
                        "topo.colors",
                        "matlab",
                        "rainbow"
                      )[1]) {
  NrCol <- length(unique(vector))
  COLZ <- CodeAndRoll2::as.numeric.wNames.factor(vector) # if basic numbers
  if (randomize) {
    COLZ <- sample(COLZ)
  } # if randomise
  if (RColorBrewerSet != FALSE) {
    COLZ <- RColorBrewer::brewer.pal(NrCol, name = RColorBrewerSet)[CodeAndRoll2::as.numeric.wNames.factor(vector)]
  } else {
    COLZ <- if (set == "rainbow") {
      rainbow(NrCol)[COLZ]
    } else if (set == "heat.colors") {
      heat.colors(NrCol)[COLZ]
    } else if (set == "terrain.colors") {
      terrain.colors(NrCol)[COLZ]
    } else if (set == "topo.colors") {
      topo.colors(NrCol)[COLZ]
    } else if (set == "matlab") {
      colorRamps::matlab.like(NrCol)[COLZ]
    } else if (set == "rich") {
      gplots::rich.colors(NrCol)[COLZ]
    } else {
      CodeAndRoll2::as.numeric.wNames.factor(vector)
    } # if basic numbers
  } # if
  COLZ <- as.vector(COLZ)
  names(COLZ) <- vector
  CATEG <- COLZ[!duplicated(COLZ)]
  if (show) {
    color_check(CATEG)
  }
  if (ReturnCategoriesToo) {
    COLZ <- list("vec" = COLZ, "categ" = CATEG)
  }
  return(COLZ)
}




# ______________________________________________________________________________________________----
# Less used functions ----
# _________________________________________________________________________________________________

#' @title filter_survival_length
#'
#' @description Parse a sentence reporting the % of filter survival.
#' @param length_new The number of elements that survived the filter.
#' @param length_old The total number of elements.
#' @param prepend A string to prepend to the sentence.
#' @importFrom Stringendo percentage_formatter
#' @return A string.
#'
#' @export
filter_survival_length <- function(length_new, length_old, prepend = "") { # Parse a sentence reporting the % of filter survival.
  pc <- Stringendo::percentage_formatter(length_new / length_old)
  llprint(prepend, pc, " of ", length_old, " entries make through the filter")
}






#' @title ww.set.file.extension
#'
#' @description Internal function. Sets file extension for ggExpress plotting functions.
#' @param global_setting global file extension setting stored in a global variable. Detault: 'b.def.ext'
#' @param default Default file extension: png
#' @param also_pdf Save plot in both png and pdf formats.
#' @export

ww.set.file.extension <- function(global_setting = "b.def.ext", default = "png", also_pdf) {
  ext <- unless.specified(NameOfaVariable = global_setting, def = default)
  if (isTRUE(also_pdf)) {
    ext <- "png"
  }
  return(ext)
}
