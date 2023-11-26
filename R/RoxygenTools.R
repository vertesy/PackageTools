######################################################################
# RoxygenTools.R
#####################################################################
# source('~/GitHub/Packages/PackageTools/R/RoxygenTools.R')
# stop(); rm(list = ls(all.names = TRUE)); try(dev.off(), silent = T); gc()





# _____________________________________________________________________________________________ ----
#' @title Main Function to Process R Script for Package Calls
#'
#' @description Reads an R script file, processes its content to find and add `@importFrom` statements
#' for package function calls in function bodies. The statements are added to the Roxygen documentation
#' blocks of the functions. Default: Excludes "MarkdownReports" from processing.
#' @param file_path The path to the R script file.
#' @param suffix A string suffix to add to the `@importFrom` lines.
#' Default: "ADDED_BY_add_importFrom_statements".
#' @param exclude_packages Packages to exclude from adding `@importFrom` statements.
#' Default: c("MarkdownReports").
#' @export
add_importFrom_statements <- function(file_path, suffix = "ADDED_BY_add_importFrom_statements", exclude_packages = c("MarkdownReports")) {
  stopifnot(file.exists(file_path), is.character(suffix), is.character(exclude_packages))

  file_content <- readLines(file_path, warn = FALSE)
  function_bodies <- get_function_bodies(file_content)
  modified_content <- add_import_from(file_content, function_bodies, suffix = suffix, exclude_packages = exclude_packages)

  stopifnot(is.character(modified_content))
  writeLines(modified_content, file_path)
}


# # Example Usage
# pff <- "~/GitHub/Packages/MarkdownHelpers/R/MarkdownHelpers.R"
# exclude_packages <- c("MarkdownReports")
# add_importFrom_statements(pff, exclude_packages = exclude_packages)



# _______________________________________________________________________________________________
#' @title Extract Function Bodies from R Script
#'
#' @description This function identifies the start and end lines of each function body in an R script.
#' Each function body is extracted for further processing. Default: Extracts function bodies from
#' a given R script content.
#' @param file_content A character vector representing the lines of an R script.
#' @return A list where each element is a vector of line numbers representing a function body.
#' @export
get_function_bodies <- function(file_content) {
  stopifnot(is.character(file_content), length(file_content) > 0)

  func_lines <- grep("<-\\s*function", file_content)
  function_bodies <- lapply(func_lines, function(x) {
    next_line <- x + 1
    while (next_line <= length(file_content) && !grepl("<-\\s*function", file_content[next_line])) {
      next_line <- next_line + 1
    }
    c(x:next_line - 1)
  })

  stopifnot(is.list(function_bodies))
  return(function_bodies)
}


# _______________________________________________________________________________________________
#' @title Finding `::` Usage within Function Bodies
#'
#' @description Searches for package function calls using the `::` operator within the given content of
#' function bodies, excluding specified packages. Default: Searches for `::` usage, excluding packages
#' listed in `exclude_packages`.
#' @param body_content A character vector of lines in a function body.
#' @param pattern A regular expression pattern to identify package function calls.
#' Default: "\\b\\w+::\\w+\\b".
#' @param exclude_packages A character vector of package names to exclude from the search.
#' Default: empty character vector.
#' @return A unique list of package calls found in the body content.
#' @export
find_package_calls <- function(body_content, pattern = "\\b\\w+::\\w+\\b", exclude_packages = c()) {
  stopifnot(is.character(body_content), is.character(exclude_packages))

  calls <- list()
  for (line in body_content) {
    match <- regexpr(pattern, line, perl = TRUE)
    if (match != -1) {
      match_text <- regmatches(line, match)
      if (length(match_text) > 0) {
        pkg_function <- match_text[[1]]
        pkg_function_split <- strsplit(pkg_function, "::")[[1]]
        package_name <- pkg_function_split[1]
        func_name <- pkg_function_split[2]
        if (!package_name %in% exclude_packages) {
          calls <- append(calls, list(list(package = package_name, func_name = func_name)))
        }
      }
    }
  }

  stopifnot(is.list(calls))
  return(unique(calls))
}


# _______________________________________________________________________________________________
#' @title Add 'importFrom' Statements to Roxygen Blocks
#'
#' @description Iterates over function bodies in an R script to identify package function calls and
#' adds corresponding `@importFrom` statements to the Roxygen blocks of these functions.
#' Default: Suffix for added lines is "ADDED_BY_add_importFrom_statements".
#' @param file_content A character vector representing the lines of an R script.
#' @param bodies A list of function body line numbers.
#' @param suffix A string suffix to add to the` @importFrom` lines.
#' Default: "ADDED_BY_add_importFrom_statements".
#' @param exclude_packages Packages to exclude from adding `@importFrom` statements.
#' Default: empty character vector.
#' @return Modified file content with added `@importFrom` statements.
#' @export
add_import_from <- function(file_content, bodies, suffix = "ADDED_BY_add_importFrom_statements", exclude_packages = c()) {
  stopifnot(is.character(file_content), is.list(bodies), is.character(exclude_packages))

  for (body in bodies) {
    body_content <- file_content[body]
    package_calls <- find_package_calls(body_content, exclude_packages = exclude_packages)

    for (call in package_calls) {
      package_name <- call$package
      func_name <- call$func_name
      import_from_line <- paste0("#' @importFrom ", package_name, " ", func_name, " ", suffix)

      roxygen_start <- max(grep("^#'\\s*$", file_content[1:body[1]]))
      roxygen_end <- roxygen_start
      while (roxygen_end < body[1] && grepl("^#'", file_content[roxygen_end + 1])) {
        roxygen_end <- roxygen_end + 1
      }

      existing_imports <- grep(paste0("@importFrom ", package_name, " ", func_name), file_content[roxygen_start:roxygen_end], value = TRUE)

      if (length(existing_imports) == 0) {
        file_content[roxygen_end] <- paste(file_content[roxygen_end], import_from_line, sep = "\n")
      }
    }
  }

  stopifnot(is.character(file_content))
  return(file_content)
}
