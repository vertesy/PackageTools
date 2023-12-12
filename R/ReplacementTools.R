######################################################################
# ReplacementTools.R
#####################################################################
# source('~/GitHub/Packages/PackageTools/R/ReplacementTools.R')
# stop(); rm(list = ls(all.names = TRUE)); try(dev.off(), silent = TRUE); gc()



# ______________________________________________________________________________________________----
# 1. Replacement Functions  ----
# ____________________________________________________________________

#' @title Replace T and F with TRUE and FALSE in R Scripts
#'
#' @description This function reads an R script, safely replaces all instances of `T` with `TRUE`
#' and `F` with `FALSE`, under specific conditions, and writes the modified script back to a file.
#'
#' @param file_path The path to the R script file. Default: file_path.
#' @param output_path The path where the modified script will be saved. Default: file_path.
#' @param strict_mode Logical; if TRUE, only replace `T` and `F` that are surrounded by specified characters.
#'                    Default: TRUE.
#' @param preceding_chars Characters that can precede `T` or `F`. Default: "\\s".
#' @param following_chars Characters that can follow `T` or `F`. Default: c(",", "\\)").
#' @return Invisible NULL.
#' @examples
#' replace_tf_with_true_false("my_script.R", "my_script_modified.R")
#' @export
replace_tf_with_true_false <- function(file_path, output_path = file_path,
                                       strict_mode = TRUE,
                                       preceding_chars = "\\s",
                                       following_chars = c(",", "\\)", "\\]")) {
  warning("Much safer results are obtained if you ran styler::style_file(file_path). Did you do it?")
  # Input argument assertion
  stopifnot(is.character(file_path), file.exists(file_path))
  stopifnot(is.character(output_path))

  # Read the file
  script_lines <- readLines(file_path, warn = FALSE)

  # Process each line
  processed_lines <- sapply(script_lines, .safely_replace_tf,
    USE.NAMES = FALSE,
    strict_mode, preceding_chars, following_chars
  )

  # Write the modified script
  writeLines(processed_lines, output_path)

  # Output assertion
  stopifnot(length(processed_lines) == length(script_lines), file.exists(output_path))

  invisible(NULL)
}


# _____________________________________________________________________________________________
#' @title Replace l() with length() in an R Script
#'
#' @description This function reads an R script file and replaces instances of `l(` with `length(`.
#' It supports a strict mode to ensure accurate replacement.
#'
#' @param file_path A string representing the path to the R script file.
#' @param output_path A string representing the path to save the modified R script.
#' Default is the same as `file_path`.
#' @param strict_mode A boolean flag to determine the strictness of the match.
#' If `TRUE`, matches `l(` only when it's not part of a larger alphanumeric string.
#' If `FALSE`, all instances of `l(` are replaced.
#'
#' @return None
#' @importFrom stringr str_replace_all
#' @export
replace_l_with_length <- function(file_path, output_path = file_path, strict_mode = TRUE) {
  warning("Much safer results are obtained if you ran styler::style_file(file_path). Did you do it?")

  stopifnot(is.character(file_path), file.exists(file_path))
  stopifnot(is.character(output_path))

  script_lines <- readLines(file_path, warn = FALSE)

  processed_lines <- sapply(script_lines, .safely_replace_l, strict_mode, USE.NAMES = FALSE)

  writeLines(processed_lines, output_path)

  stopifnot(length(processed_lines) == length(script_lines), file.exists(output_path))

  invisible(NULL)
}
# replace_l_with_length('~/GitHub/Projects/CON/_sc6_19/Get.Annotation.from.Objectnames.sc16_19.R', strict_mode = T)


# _____________________________________________________________________________________________


# _____________________________________________________________________________________________



# ______________________________________________________________________________________________----
# 2. Private Helper Functions  ----
# ____________________________________________________________________


# _____________________________________________________________________________________________
#' @title Safely Replace T and F in a Line of R Script
#'
#' @description This helper function replaces instances of `T` and `F` in a single line of R
#' script based on the specified mode and character constraints.
#'
#' @param line A single line of R script.
#' @param strict_mode Logical; specifies the mode of replacement. Default: TRUE.
#' @param preceding_chars Characters that can precede `T` or `F` for replacement.
#' @param following_chars Characters that can follow `T` or `F` for replacement.
#' @return The modified line of R script.
.safely_replace_tf <- function(line, strict_mode, preceding_chars, following_chars) {
  if (strict_mode) {
    # Create regular expressions based on preceding and following characters
    preceding_pattern <- paste0("(", paste0(preceding_chars, collapse = "|"), ")")
    following_pattern <- paste0("(", paste0(following_chars, collapse = "|"), ")")

    # Replace 'T' and 'F'
    modified_line <- gsub(paste0(preceding_pattern, "T", following_pattern),
      "\\1TRUE\\2", line,
      perl = TRUE
    )
    modified_line <- gsub(paste0(preceding_pattern, "F", following_pattern),
      "\\1FALSE\\2", modified_line,
      perl = TRUE
    )
  } else {
    # Replace standalone 'T' and 'F'
    modified_line <- gsub("\\bT\\b", "TRUE", line, perl = TRUE)
    modified_line <- gsub("\\bF\\b", "FALSE", modified_line, perl = TRUE)
  }

  return(modified_line)
}


# _____________________________________________________________________________________________
#' @title Safely Replace l() with length() in a Line of R Script
#'
#' @description This function safely replaces instances of `l(` with `length(` in a given line of R script.
#' It can operate in a strict mode, which ensures that `l(` is replaced only when it is not part of a larger word
#' or variable name.
#'
#' @param line A single line from an R script.
#' @param strict_mode A boolean flag to determine the strictness of the match.
#' If `TRUE`, matches `l(` only when it's not part of a larger alphanumeric string.
#' If `FALSE`, all instances of `l(` are replaced.
#'
#' @return A string representing the modified line.
#' @importFrom stringr str_detect
#' @export
.safely_replace_l <- function(line, strict_mode) {
  if (strict_mode) {
    # Replace 'l(' when it is likely a function call
    modified_line <- gsub("(^|[^a-zA-Z0-9_])l\\(", "\\1length(", line)
  } else {
    # Replace all instances of 'l('
    modified_line <- gsub("\\bl\\(", "length(", line, perl = TRUE)
  }

  return(modified_line)
}


# _____________________________________________________________________________________________



# _____________________________________________________________________________________________



# scriptPath <- '~/GitHub/Packages/Seurat.utils/R/Seurat.Utils.R'
#
# # Now use the function with the expanded path
# replace_tf_with_true_false(file_path = scriptPath, strict_mode = TRUE,
#                            preceding_chars =  "\\s",
#                            following_chars = c(",", "\\)", "\\]", "$"))
#
# replace_tf_with_true_false(file_path = scriptPath, strict_mode = FALSE)
