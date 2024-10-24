######################################################################
# Miscellaneous.R
#####################################################################
# source('~/GitHub/Packages/PackageTools/R/Miscellaneous.R')
# stop(); rm(list = ls(all.names = TRUE)); try(dev.off(), silent = TRUE); gc()


# ____________________________________________________________________________________________ ----
#' @title Check Script Environment
#' @description Checks if all functions and variables called in a script are found in a specified environment.
#'              Optionally replaces missing function calls in the script with their fully qualified names.
#'
#' @param path Path to the R script file.
#' @param input.variables A character vector of variable names to be copied to the new environment.
#' @param exclude_var A character vector of variable names to be excluded from the report.
#' @param env Optional; an environment to use instead of creating a new one.
#' @param packages_load A character vector of package names to load functions from.
#' @param packages_load_default A character vector of default package names to load functions from.
#' @param replace_missing_calls Logical; if TRUE, replaces missing function calls in the script with their fully qualified names.
#' @export
checkScriptEnv_v1 <- function(path, input.variables, exclude_var = c("i", "path"),
                              env = NULL, packages_load = c("Seurat"),
                              packages_load_default = c(
                                "utils", "grDevices", "graphics", "stats", "methods",
                                "ggplot2",
                                "Stringendo", "ReadWriter", "CodeAndRoll2", "MarkdownHelpers",
                                "MarkdownReports", "ggExpress", "Seurat.utils", "isoENV", "UVI.tools",
                                "Connectome.tools", "NestedMultiplexer"
                              ),
                              replace_missing_calls = FALSE) {
  stopifnot(
    is.character(path), length(path) == 1, file.exists(path),
    is.character(input.variables), is.character(exclude_var)
  )
  warning("Experimental Function", immediate. = TRUE)

  all_packages_load <- union(packages_load_default, packages_load)

  # Use provided environment or create new one
  if (is.null(env)) {
    env <- new.env(parent = baseenv())

    # Copy specified global variables to env
    vars <- mget(input.variables, .GlobalEnv, ifnotfound = NA)
    vars <- Filter(Negate(is.na), vars)
    list2env(vars, envir = env)
  }

  # Load the package into the specified environment
  if (length(all_packages_load)) {
    for (pkg in all_packages_load) {
      .importPackageFunctions(pkg, env)
    }
  }

  # Parse script without running
  script_content <- tryCatch(parse(file = path), error = function(e) stop("Error parsing script: ", e$message))
  original_script <- readLines(path)

  # Extract all symbols from the script
  symbols <- all.names(script_content, functions = TRUE, unique = TRUE)

  # Classify symbols into functions and variables based on their type in standard environments
  symbol_types <- sapply(symbols, function(sym) {
    if (exists(sym, envir = baseenv())) {
      return(typeof(get(sym, envir = baseenv())))
    }
    if (exists(sym, envir = asNamespace("base"))) {
      return(typeof(get(sym, envir = asNamespace("base"))))
    }
    return(NA)
  })

  # Identify missing functions and variables
  not_found_funs <- names(symbol_types)[symbol_types == "closure" &
    !sapply(names(symbol_types), function(x) exists(x, envir = env))] |>
    na.omit() |>
    as.character()
  not_found_vars <- names(symbol_types)[!symbol_types %in% c("closure", NA) & !sapply(names(symbol_types), function(x) exists(x, envir = env))]

  # Find packages for missing functions and replace in script
  not_found_funs_packages <- sapply(not_found_funs, function(x) {
    pkg <- gsub(pattern = "package:", replacement = "", x = find(x))
    pkg_name <- pkg[length(pkg)]
    if (replace_missing_calls && !is.na(pkg_name)) {
      pattern <- paste0("\\b", x, "\\(")
      replacement <- paste0(pkg_name, "::", x, "(")
      original_script <- gsub(pattern, replacement, original_script, fixed = FALSE)
    }
    return(paste(pkg_name, x, sep = "::"))
  })

  # Overwrite the file with changes if replace_missing_calls is TRUE
  if (replace_missing_calls) {
    message("THIS DOES NOT WORK AND IT IS IGNORED.")
    # writeLines(original_script, path)
  }

  # Exclude specified variables from report
  not_found_vars <- setdiff(not_found_vars, exclude_var)

  # Report
  list(Functions_Not_Found = not_found_funs_packages, Variables_Not_Found = not_found_vars)
}




# _____________________________________________________________________________________________ ----
## Package Section ---------------------------------------------------------------------------

#' @title Copy GitHub Badge Markdown Image Link to Clipboard
#'
#' @description
#' This function copies the Markdown code for a GitHub badge to the clipboard based on the
#' specified status. It supports four statuses: 'experimental', 'active', 'archive', and
#' 'hibernate'.
#'
#' @param status The status of the project, determining which badge is copied.
#'               Valid options are 'experimental', 'active', 'archive', and 'hibernate'.
#'               Default: 'experimental'.
#' @param prefix The URL prefix for the badge images.
#'               Default: "https:,//raw.githubusercontent.com/vertesy/TheCorvinas/master/GitHub/Badges/".
#' @param copy_to_clipboard Logical. If TRUE, the badge Markdown code is copied to the clipboard.
#'
#' @return The function does not return a value but copies the relevant badge Markdown code to the clipboard.
#' @examples
#' copy_github_badge("experimental")
#' copy_github_badge("active", "https://example.com/badges/")
#' @importFrom clipr write_clip
#' @export
copy_github_badge <- function(status = "experimental",
                              prefix = "https://raw.githubusercontent.com/vertesy/TheCorvinas/master/GitHub/Badges/",
                              copy_to_clipboard = FALSE
                              ) {
  valid_statuses <- c("experimental", "active", "archive", "hibernate")
  if (!status %in% valid_statuses) {
    stop("Invalid status. Choose from 'experimental', 'active', 'archive', 'hibernate'.")
  }

  badge_link <- paste0(prefix, status, ".svg")
  markdown_text <- paste0("![status: ", status, "](", badge_link, ")")

  # Copy to clipboard, or return the Markdown text
  if (copy_to_clipboard & require(clipr)) clipr::write_clip(markdown_text) else return(markdown_text)

}




# _____________________________________________________________________________________________
#' @title Open README.md of the Current Active RStudio Project
#'
#' @description Opens the README.md file located in the root of the currently active RStudio project.
#'              This function automatically detects the operating system to use the appropriate
#'              system command for opening the file.
#'
#' @return Invisible NULL. The function is used for its side effect of opening a file.
#' @importFrom rstudioapi getActiveProject
#' @examples openReadme() # Opens README.md in the active RStudio project
#' @export

openReadme <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    readme_path <- file.path(rstudioapi::getActiveProject(), "README.md")
  } else {
    stop("rstudioapi package is required.")
  }

  if (Sys.info()["sysname"] == "Darwin") {
    system(paste("open", shQuote(readme_path)))
  } else if (Sys.info()["sysname"] == "Windows") {
    system(paste("start", shQuote(readme_path)), wait = FALSE)
  } else if (Sys.info()["sysname"] == "Linux") {
    system(paste("xdg-open", shQuote(readme_path)))
  } else {
    stop("Unsupported operating system.")
  }
}




# # _____________________________________________________________________________________________ ----
# #' @title Find which Package a given Function belongs to
# #' @description Determines the package that a given function is defined in and optionally copies
# #' the result to the clipboard. Warns if the function is found in multiple packages.
# #'
# #' @param functionName The name of the function (character or function object).
# #' @param searchInstalled If TRUE, searches all installed packages (default FALSE).
# #' @param toClipboard If TRUE, copies the result to the clipboard (default TRUE).
# #' @return The name of the package containing the function, or NULL if not found.
# #' @importFrom clipr write_clip
# #' @examples
# #'  find_package_of_this_function('sppp')
# #'  find_package_of_this_function(sppp)
# #'  find_package_of_this_function(filter)
# #'  find_package_of_this_function("filterzzz")
# #'
# #' @export

# from <- function(functionName, searchInstalled = FALSE, toClipboard = TRUE) {
#   # Validate input
#   if (is.function(functionName)) {
#     functionName <- deparse(substitute(functionName))
#   } else if (!is.character(functionName) || length(functionName) != 1) {
#     stop("functionName must be a single character string or a function object.")
#   }

#   packagesFound <- character(0)

#   # Search in loaded namespaces
#   searchSpaces <- search()
#   for (space in searchSpaces) {
#     if (startsWith(space, "package:")) {
#       packageName <- substring(space, 9) # Remove "package:" prefix
#       if (exists(functionName, envir = asNamespace(packageName), inherits = FALSE)) {
#         packagesFound <- c(packagesFound, packageName)
#       }
#     }
#   }

#   # Optionally search in installed packages
#   if (searchInstalled && length(packagesFound) == 0) {
#     installedPkgs <- installed.packages()[, "Package"]
#     for (pkg in installedPkgs) {
#       if (exists(functionName, envir = asNamespace(pkg), inherits = FALSE)) {
#         packagesFound <- c(packagesFound, pkg)
#       }
#     }
#   }

#   # Check for multiple packages
#   if (length(packagesFound) > 1) {
#     warning(functionName, " found in multiple packages: ", paste(packagesFound, collapse = ", "), immediate. = TRUE)
#   } else if (length(packagesFound) == 0) {
#     warning(functionName , " not found!", immediate. = TRUE)
#     return(NULL)
#   }

#   # Handle clipboard and return
#   foundPackage <- packagesFound[1]
#   if (toClipboard) {
#     try(clipr::write_clip(foundPackage), silent = TRUE)
#   }
#   return(foundPackage)
# }
