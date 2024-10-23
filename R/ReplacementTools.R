######################################################################
# ReplacementTools.R
#####################################################################
# source('~/GitHub/Packages/PackageTools/R/ReplacementTools.R')
# stop(); rm(list = ls(all.names = TRUE)); try(dev.off(), silent = TRUE); gc()



# ______________________________________________________________________________________________----
# 1. Replacement Functions  ----
# ____________________________________________________________________


#' @title Replace a string in a file with options for whole word and case matching
#'
#' @description
#' The `replace_a_string_in_a_file()` function replaces all instances of a string in a file, with
#' options to replace only whole word matches and to replace with case sensitivity or ignore case.
#' It also allows backing up the file before making the replacements.
#'
#' @param file_path The path to the file where replacements will be made. Default: none.
#' @param from The string to be replaced. Default: none.
#' @param to The string that will replace `from`. Default: none.
#' @param whole_word Logical. If `TRUE`, replaces only whole word matches. Default: `TRUE`.
#' @param ignore_case Logical. If `TRUE`, the replacement is case insensitive. Default: `FALSE`.
#' @param perl Logical. If `TRUE`, use Perl-compatible regular expressions. Default: `TRUE`.
#' @param backup Logical. If `TRUE`, a backup of the original file is saved with a `.bac` extension.
#'   Default: `FALSE`.
#' @param ... Additional arguments to be passed to `gsub()` and `grepl()`.
#'
#' @return The function performs replacements in the file, returning the number of instances replaced
#'   and outputting a message indicating the completion of the replacement process.
#'
#' @examples
#' # Replace "from" with "to" in "my_file.txt", with case matching and whole word match.
#' replace_a_string_in_a_file("my_file.txt", "from", "to")
#'
#' # Replace "from" with "to" without matching the case and ignoring whole word match.
#' replace_a_string_in_a_file("my_file.txt", "from", "to", whole_word = FALSE, ignore_case = FALSE)
#'
#' @export
replace_a_string_in_a_file <- function(
    file_path, from, to, whole_word = TRUE, ignore_case = FALSE,
    perl = TRUE, backup = FALSE,
    ...) {
  # Input argument assertions
  stopifnot(
    is.character(file_path) && length(file_path) == 1,
    file.exists(file_path),
    is.character(from) && length(from) == 1,
    is.character(to) && length(to) == 1,
    is.logical(whole_word) && length(whole_word) == 1,
    is.logical(ignore_case) && length(ignore_case) == 1,
    is.logical(perl) && length(perl) == 1,
    is.logical(backup) && length(backup) == 1
  )

  # Read the file content
  file_content <- readLines(file_path)

  # Create the pattern for replacement, with or without whole word match
  pattern <- if (whole_word) paste0("\\b", from, "\\b") else from

  # Backup the file if requested
  if (backup) {
    backup_file_path <- paste0(file_path, ".bac")
    file.copy(file_path, backup_file_path, overwrite = TRUE)
    message("Backup created: ", backup_file_path)
  }

  # Count the number of matches before replacement
  match_count <- sum(grepl(pattern, file_content, ignore.case = ignore_case, perl = perl, ...))

  # Perform replacement based on case sensitivity
  updated_content <- gsub(pattern, to, file_content, ignore.case = ignore_case, perl = perl, ...)

  # Write the updated content back to the file
  writeLines(updated_content, file_path)

  # Message the number of instances replaced
  message(
    "Replacement complete: '", from, "' -> '", to, "'\nin ", file_path,
    ".\n", match_count, " instances replaced."
  )

  return(invisible(match_count)) # Return the count of replacements made
}



# _____________________________________________________________________________________________
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

#' @title Replace Short Function Calls with Full Names in an R Script
#'
#' @description Reads an R script file and replaces instances of `length(` with `length(` and `p0` with `paste0(`.
#' It supports a strict mode to ensure accurate replacements.
#'
#' @param file_path A string representing the path to the R script file.
#' @param output_path A string representing the path to save the modified R script.
#' Default is the same as `file_path`.
#' @param strict_mode A boolean flag to determine the strictness of the matches.
#' If `TRUE`, matches `length(` and `p0` only when they're not part of larger alphanumeric strings.
#' If `FALSE`, all instances of `length(` and `p0` are replaced.
#'
#' @return None
#' @importFrom stringr str_detect
#'
#' @export
replace_short_calls <- function(file_path, output_path = file_path, strict_mode = TRUE) {
  warning("It's safer to run styler::style_file(file_path) first. Did you do it?")

  stopifnot(
    is.character(file_path),
    file.exists(file_path),
    is.character(output_path)
  )

  script_lines <- readLines(file_path, warn = FALSE)

  processed_lines <- sapply(script_lines, .safely_replace_calls, strict_mode, USE.NAMES = FALSE)

  writeLines(processed_lines, output_path)

  stopifnot(length(processed_lines) == length(script_lines), file.exists(output_path))

  invisible(NULL)
}


# _____________________________________________________________________________________________
#' @title Replace length() with length() in an R Script
#'
#' @description This function reads an R script file and replaces instances of `length(` with `length(`.
#' It supports a strict mode to ensure accurate replacement.
#'
#' @param file_path A string representing the path to the R script file.
#' @param output_path A string representing the path to save the modified R script.
#' Default is the same as `file_path`.
#' @param strict_mode A boolean flag to determine the strictness of the match.
#' If `TRUE`, matches `length(` only when it's not part of a larger alphanumeric string.
#' If `FALSE`, all instances of `length(` are replaced.
#'
#' @return None
#' @importFrom stringr str_replace_all
#'
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
# replace_l_with_length('~/GitHub/Projects/CON/_sc6_19/Get.Annotation.from.Objectnames.sc16_19.R', strict_mode = TRUE)



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
#' @title Safely Replace Short Function Calls in a Line of R Script
#'
#' @description Safely replaces instances of `length(` with `length(` and `p0` with `paste0(` in a given line of R script.
#' Operates in strict mode to ensure that replacements are made only when not part of a larger word or variable name.
#'
#' @param line A single line from an R script.
#' @param strict_mode A boolean flag to determine the strictness of the match.
#' If `TRUE`, matches are made only when not part of larger alphanumeric strings.
#' If `FALSE`, all instances are replaced.
#'
#' @return A string representing the modified line.
#' @importFrom stringr str_detect
#' @export
.safely_replace_calls <- function(line, strict_mode) {
  if (strict_mode) {
    # Replace 'length(' and 'p0' when they are likely function calls
    modified_line <- gsub("(^|[^a-zA-Z0-9_])l\\(", "\\1length(", line)
    modified_line <- gsub("(^|[^a-zA-Z0-9_])p0\\(", "\\1paste0(", modified_line)
  } else {
    # Replace all instances of 'length(' and 'p0'
    modified_line <- gsub("\\bl\\(", "length(", line, perl = TRUE)
    modified_line <- gsub("\\bp0\\(", "paste0(", modified_line, perl = TRUE)
  }

  return(modified_line)
}

# _____________________________________________________________________________________________
#' @title Safely Replace length() with length() in a Line of R Script
#'
#' @description This function safely replaces instances of `length(` with `length(` in a given line of R script.
#' It can operate in a strict mode, which ensures that `length(` is replaced only when it is not part of a larger word
#' or variable name.
#'
#' @param line A single line from an R script.
#' @param strict_mode A boolean flag to determine the strictness of the match.
#' If `TRUE`, matches `length(` only when it's not part of a larger alphanumeric string.
#' If `FALSE`, all instances of `length(` are replaced.
#'
#' @return A string representing the modified line.
#' @importFrom stringr str_detect
#' @export
.safely_replace_l <- function(line, strict_mode) {
  if (strict_mode) {
    # Replace 'length(' when it is likely a function call
    modified_line <- gsub("(^|[^a-zA-Z0-9_])l\\(", "\\1length(", line)
  } else {
    # Replace all instances of 'length('
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
