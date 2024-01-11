######################################################################
# Miscellaneous.R
#####################################################################
# source('~/GitHub/Packages/PackageTools/R/Miscellaneous.R')
# stop(); rm(list = ls(all.names = TRUE)); try(dev.off(), silent = TRUE); gc()




# _____________________________________________________________________________________________ ----
#' @title Find which Package a given Function belongs to
#' @description Determines the package that a given function is defined in and optionally copies
#' the result to the clipboard. Warns if the function is found in multiple packages.
#'
#' @param functionName The name of the function (character or function object).
#' @param searchInstalled If TRUE, searches all installed packages (default FALSE).
#' @param toClipboard If TRUE, copies the result to the clipboard (default TRUE).
#' @return The name of the package containing the function, or NULL if not found.
#' @importFrom clipr write_clip
#' @examples
#'  find_package_of_this_function('sppp')
#'  find_package_of_this_function(sppp)
#'  find_package_of_this_function(filter)
#'  find_package_of_this_function("filterzzz")
#'
#' @export

find_package_of_this_function <- function(functionName, searchInstalled = FALSE, toClipboard = TRUE) {
  # Validate input
  if (is.function(functionName)) {
    functionName <- deparse(substitute(functionName))
  } else if (!is.character(functionName) || length(functionName) != 1) {
    stop("functionName must be a single character string or a function object.")
  }

  packagesFound <- character(0)

  # Search in loaded namespaces
  searchSpaces <- search()
  for (space in searchSpaces) {
    if (startsWith(space, "package:")) {
      packageName <- substring(space, 9) # Remove "package:" prefix
      if (exists(functionName, envir = asNamespace(packageName), inherits = FALSE)) {
        packagesFound <- c(packagesFound, packageName)
      }
    }
  }

  # Optionally search in installed packages
  if (searchInstalled && length(packagesFound) == 0) {
    installedPkgs <- installed.packages()[, "Package"]
    for (pkg in installedPkgs) {
      if (exists(functionName, envir = asNamespace(pkg), inherits = FALSE)) {
        packagesFound <- c(packagesFound, pkg)
      }
    }
  }

  # Check for multiple packages
  if (length(packagesFound) > 1) {
    warning(functionName, " found in multiple packages: ", paste(packagesFound, collapse = ", "), immediate. = TRUE)
  } else if (length(packagesFound) == 0) {
    warning(functionName , " not found!", immediate. = TRUE)
    return(NULL)
  }

  # Handle clipboard and return
  foundPackage <- packagesFound[1]
  if (toClipboard) {
    try(clipr::write_clip(foundPackage), silent = TRUE)
  }
  return(foundPackage)
}



# _____________________________________________________________________________________________ ----
## Package Section ---------------------------------------------------------------------------

#' Copy GitHub Badge Markdown Image Link to Clipboard
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
#' @return The function does not return a value but copies the relevant badge Markdown#'         code to the clipboard.
#' @examples
#' copy_github_badge("experimental")
#' copy_github_badge("active", "https://example.com/badges/")
#' @importFrom clipr write_clip
#' @export
copy_github_badge <- function(status = "experimental",
                              prefix = "https://raw.githubusercontent.com/vertesy/TheCorvinas/master/GitHub/Badges/") {
  valid_statuses <- c("experimental", "active", "archive", "hibernate")
  if (!status %in% valid_statuses) {
    stop("Invalid status. Choose from 'experimental', 'active', 'archive', 'hibernate'.")
  }

  badge_link <- paste0(prefix, status, ".svg")
  markdown_text <- paste0("![status: ", status, "](", badge_link, ")")

  if (require(clipr)) {
    clipr::write_clip(markdown_text)
  } else {
    return(markdown_text)
  }
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
#' @examples openReadme()  # Opens README.md in the active RStudio project
#' @export

openReadme <- function() {
  if(requireNamespace("rstudioapi", quietly = TRUE)) {
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
