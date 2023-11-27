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
