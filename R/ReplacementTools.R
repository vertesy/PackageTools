######################################################################
# ReplacementTools.R
#####################################################################
# source('~/GitHub/Packages/PackageTools/R/ReplacementTools.R')
# stop(); rm(list = ls(all.names = TRUE)); try(dev.off(), silent = TRUE); gc()





# _____________________________________________________________________________________________ ----
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


# _____________________________________________________________________________________________ ----
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



# scriptPath <- '~/GitHub/Packages/Seurat.utils/R/Seurat.Utils.R'
#
# # Now use the function with the expanded path
# replace_tf_with_true_false(file_path = scriptPath, strict_mode = TRUE,
#                            preceding_chars =  "\\s",
#                            following_chars = c(",", "\\)", "\\]", "$"))
#
# replace_tf_with_true_false(file_path = scriptPath, strict_mode = FALSE)
