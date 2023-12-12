######################################################################
# Miscellaneous.R
#####################################################################
# source('~/GitHub/Packages/PackageTools/R/Miscellaneous.R')
# stop(); rm(list = ls(all.names = TRUE)); try(dev.off(), silent = TRUE); gc()


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
