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
  ...
) {
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

  # Process each line, carrying open string/raw-string state across line boundaries
  in_quote <- NULL
  processed_lines <- character(length(script_lines))
  for (i in seq_along(script_lines)) {
    result <- .safely_replace_tf(
      script_lines[i], strict_mode, preceding_chars, following_chars,
      initial_quote = in_quote
    )
    processed_lines[i] <- result$text
    in_quote <- result$end_quote
  }

  # Write the modified script
  writeLines(processed_lines, output_path)

  # Output assertion
  stopifnot(length(processed_lines) == length(script_lines), file.exists(output_path))

  invisible(NULL)
}

#' @title Replace Short Function Calls with Full Names in an R Script
#'
#' @description Reads an R script file and replaces short developer shorthands for common
#' functions and package calls with their full names, e.g. `l(` with `length(`, `p0(` with
#' `paste0(`, `u(` with `unique(`, `dfilter(` with `dplyr::filter(`, `dselect(` with
#' `dplyr::select(`, and `sort.natural(` with `gtools::mixedsort(`.
#' It supports a strict mode to ensure accurate replacements.
#'
#' @param file_path A string representing the path to the R script file.
#' @param output_path A string representing the path to save the modified R script.
#' Default is the same as `file_path`.
#' @param strict_mode A boolean flag to determine the strictness of the matches.
#' If `TRUE`, matches shorthand calls only when they're not part of larger alphanumeric strings.
#' If `FALSE`, all instances of the shorthand calls are replaced.
#' @param call_map A named character vector mapping each shorthand (the name) to the fully
#' qualified function call it stands for (the value). Default: `.default_call_shorthands`,
#' covering the shorthands listed in the description. Pass your own map to add or override
#' shorthands.
#'
#' @return None
#' @importFrom stringr str_detect
#'
#' @export
replace_short_calls <- function(file_path, output_path = file_path, strict_mode = TRUE,
                                call_map = .default_call_shorthands) {
  warning("It's safer to run styler::style_file(file_path) first. Did you do it?")

  stopifnot(
    is.character(file_path),
    file.exists(file_path),
    is.character(output_path)
  )

  script_lines <- readLines(file_path, warn = FALSE)

  escaped_shorthands <- gsub(".", "\\.", names(call_map), fixed = TRUE)
  alternation <- paste(escaped_shorthands, collapse = "|")
  boundary <- if (strict_mode) "(?<![a-zA-Z0-9_.])" else "\\b"
  pattern <- paste0(boundary, "(?:", alternation, ")\\(")

  in_quote <- NULL
  processed_lines <- character(length(script_lines))
  for (i in seq_along(script_lines)) {
    parsed <- .tokenize_r_line(script_lines[i], initial_quote = in_quote)
    in_quote <- parsed$end_quote
    tokens <- parsed$tokens

    replaced_tokens <- sapply(tokens, function(tok) {
      if (!tok$is_code || nchar(tok$text) == 0) {
        return(tok$text)
      }
      t_text <- tok$text

      matches <- gregexpr(pattern, t_text, perl = TRUE)
      match_text <- regmatches(t_text, matches)[[1]]
      if (length(match_text) == 0) {
        return(t_text)
      }

      shorthand <- sub("\\($", "", match_text)
      regmatches(t_text, matches) <- list(paste0(call_map[shorthand], "("))
      return(t_text)
    }, USE.NAMES = FALSE)

    processed_lines[i] <- paste(replaced_tokens, collapse = "")
  }

  writeLines(processed_lines, output_path)

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
#'
#' @export
replace_l_with_length <- function(file_path, output_path = file_path, strict_mode = TRUE) {
  warning("Much safer results are obtained if you ran styler::style_file(file_path). Did you do it?")

  stopifnot(is.character(file_path), file.exists(file_path))
  stopifnot(is.character(output_path))

  script_lines <- readLines(file_path, warn = FALSE)

  # Process each line, carrying open string/raw-string state across line boundaries
  in_quote <- NULL
  processed_lines <- character(length(script_lines))
  for (i in seq_along(script_lines)) {
    result <- .safely_replace_l(
      script_lines[i], strict_mode,
      initial_quote = in_quote, return_state = TRUE
    )
    processed_lines[i] <- result$text
    in_quote <- result$end_quote
  }

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
#' @title Tokenize a Line of R Script into Code and Non-Code Tokens
#'
#' @description Splits a line of R script into tokens representing R code vs. non-code (string
#' literals and comments) to prevent accidental replacements inside strings and comments. Handles
#' both ordinary quoted strings (with backslash escaping) and raw strings (e.g. `r"(...)"`,
#' `R'---[...]---'`), whose closing delimiter is a bracket/dashes/quote sequence in which
#' backslashes are literal, not escape characters.
#'
#' @param line A single line of R script.
#' @param initial_quote A list describing an open string literal that parsing resumes inside of
#' (as returned in `end_quote` by a previous call), or `NULL` if parsing starts in code.
#' @return A list containing `tokens` (a list of token objects with `text` and `is_code`) and
#' `end_quote` (the open string state at the end of the line, or `NULL`).
.tokenize_r_line <- function(line, initial_quote = NULL) {
  chars <- strsplit(line, "")[[1]]
  n <- length(chars)
  if (n == 0) {
    return(list(tokens = list(list(text = "", is_code = is.null(initial_quote))), end_quote = initial_quote))
  }

  tokens <- list()
  current_text <- ""
  current_is_code <- is.null(initial_quote)
  in_quote <- initial_quote
  escaped <- FALSE

  matching_bracket <- c("(" = ")", "[" = "]", "{" = "}")

  i <- 1
  while (i <= n) {
    ch <- chars[i]
    if (!is.null(in_quote)) {
      term <- in_quote$terminator
      term_len <- nchar(term)
      if (!in_quote$raw && !escaped && ch == "\\") {
        current_text <- paste0(current_text, ch)
        escaped <- TRUE
      } else if (!in_quote$raw && escaped) {
        current_text <- paste0(current_text, ch)
        escaped <- FALSE
      } else if (i + term_len - 1 <= n && paste(chars[i:(i + term_len - 1)], collapse = "") == term) {
        current_text <- paste0(current_text, term)
        tokens[[length(tokens) + 1]] <- list(text = current_text, is_code = FALSE)
        current_text <- ""
        in_quote <- NULL
        current_is_code <- TRUE
        i <- i + term_len
        next
      } else {
        current_text <- paste0(current_text, ch)
      }
    } else {
      if (ch == "#") {
        if (nchar(current_text) > 0) {
          tokens[[length(tokens) + 1]] <- list(text = current_text, is_code = TRUE)
          current_text <- ""
        }
        comment_text <- paste(chars[i:n], collapse = "")
        tokens[[length(tokens) + 1]] <- list(text = comment_text, is_code = FALSE)
        i <- n + 1
        break
      } else if (ch == '"' || ch == "'") {
        # Detect a raw-string prefix: an `r`/`R` immediately before the quote that is not
        # itself part of a larger identifier, followed by optional dashes and an opening bracket.
        is_raw_prefix <- FALSE
        if (nchar(current_text) > 0) {
          last_char <- substr(current_text, nchar(current_text), nchar(current_text))
          if (last_char %in% c("r", "R")) {
            before_r <- if (nchar(current_text) > 1) {
              substr(current_text, nchar(current_text) - 1, nchar(current_text) - 1)
            } else {
              ""
            }
            is_raw_prefix <- !grepl("[a-zA-Z0-9_.]", before_r, perl = TRUE)
          }
        }

        raw_open <- NULL
        if (is_raw_prefix) {
          rest <- if (i < n) paste(chars[(i + 1):n], collapse = "") else ""
          m <- regmatches(rest, regexpr("^-*[(\\[{]", rest, perl = TRUE))
          if (length(m) == 1 && nchar(m) > 0) {
            open_bracket <- substr(m, nchar(m), nchar(m))
            ndash <- nchar(m) - 1
            close_bracket <- matching_bracket[[open_bracket]]
            raw_open <- list(
              opener = m,
              terminator = paste0(close_bracket, strrep("-", ndash), ch)
            )
          }
        }

        if (!is.null(raw_open)) {
          code_before <- substr(current_text, 1, nchar(current_text) - 1)
          if (nchar(code_before) > 0) {
            tokens[[length(tokens) + 1]] <- list(text = code_before, is_code = TRUE)
          }
          current_text <- paste0(last_char, ch, raw_open$opener)
          in_quote <- list(terminator = raw_open$terminator, raw = TRUE)
          current_is_code <- FALSE
          i <- i + nchar(raw_open$opener)
        } else {
          if (nchar(current_text) > 0) {
            tokens[[length(tokens) + 1]] <- list(text = current_text, is_code = TRUE)
            current_text <- ""
          }
          in_quote <- list(terminator = ch, raw = FALSE)
          current_text <- ch
          current_is_code <- FALSE
        }
      } else {
        current_text <- paste0(current_text, ch)
      }
    }
    i <- i + 1
  }

  if (nchar(current_text) > 0) {
    tokens[[length(tokens) + 1]] <- list(text = current_text, is_code = current_is_code)
  }

  return(list(tokens = tokens, end_quote = in_quote))
}


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
#' @param initial_quote An open string/raw-string state to resume parsing inside of (as returned
#' in `end_quote` by a previous call), or `NULL` if parsing starts in code. Default: `NULL`.
#' @return A list with `text` (the modified line) and `end_quote` (the open string state at the
#' end of the line, to be passed as `initial_quote` for the next line).
.safely_replace_tf <- function(line, strict_mode, preceding_chars, following_chars, initial_quote = NULL) {
  parsed <- .tokenize_r_line(line, initial_quote = initial_quote)
  tokens <- parsed$tokens

  replaced_tokens <- sapply(tokens, function(tok) {
    if (!tok$is_code || nchar(tok$text) == 0) {
      return(tok$text)
    }
    t_text <- tok$text
    if (strict_mode) {
      preceding_pattern <- paste0("(", paste0(preceding_chars, collapse = "|"), ")")
      following_pattern <- paste0("(", paste0(following_chars, collapse = "|"), ")")

      modified <- gsub(paste0(preceding_pattern, "T", following_pattern),
        "\\1TRUE\\2", t_text,
        perl = TRUE
      )
      modified <- gsub(paste0(preceding_pattern, "F", following_pattern),
        "\\1FALSE\\2", modified,
        perl = TRUE
      )
    } else {
      modified <- gsub("\\bT\\b", "TRUE", t_text, perl = TRUE)
      modified <- gsub("\\bF\\b", "FALSE", modified, perl = TRUE)
    }
    return(modified)
  }, USE.NAMES = FALSE)

  list(text = paste(replaced_tokens, collapse = ""), end_quote = parsed$end_quote)
}


# _____________________________________________________________________________________________
# Default shorthand -> full call map shared by `replace_short_calls()` and `.safely_replace_calls()`.
.default_call_shorthands <- c(
  "l" = "length",
  "p0" = "paste0",
  "u" = "unique",
  "dfilter" = "dplyr::filter",
  "dselect" = "dplyr::select",
  "sort.natural" = "gtools::mixedsort"
)

# _____________________________________________________________________________________________
#' @title Safely Replace Short Function Calls in a Line of R Script
#'
#' @description Safely replaces short developer shorthands for common functions and package
#' calls (e.g. `l(` with `length(`, `p0(` with `paste0(`, `u(` with `unique(`, `dfilter(` with
#' `dplyr::filter(`, `dselect(` with `dplyr::select(`, and `sort.natural(` with
#' `gtools::mixedsort(`) in a given line of R script.
#' Operates in strict mode to ensure that replacements are made only when not part of a larger word or variable name.
#'
#' @param line A single line from an R script.
#' @param strict_mode A boolean flag to determine the strictness of the match.
#' If `TRUE`, matches are made only when not part of larger alphanumeric strings.
#' If `FALSE`, all instances are replaced.
#' @param call_map A named character vector mapping each shorthand (the name) to the fully
#' qualified function call it stands for (the value). Default: `.default_call_shorthands`.
#'
#' @return A string representing the modified line.
#' @importFrom stringr str_detect
#' @export
.safely_replace_calls <- function(line, strict_mode, call_map = .default_call_shorthands) {
  parsed <- .tokenize_r_line(line)
  tokens <- parsed$tokens

  escaped_shorthands <- gsub(".", "\\.", names(call_map), fixed = TRUE)
  alternation <- paste(escaped_shorthands, collapse = "|")
  boundary <- if (strict_mode) "(?<![a-zA-Z0-9_.])" else "\\b"
  pattern <- paste0(boundary, "(?:", alternation, ")\\(")

  replaced_tokens <- sapply(tokens, function(tok) {
    if (!tok$is_code || nchar(tok$text) == 0) {
      return(tok$text)
    }
    t_text <- tok$text

    matches <- gregexpr(pattern, t_text, perl = TRUE)
    match_text <- regmatches(t_text, matches)[[1]]
    if (length(match_text) == 0) {
      return(t_text)
    }

    shorthand <- sub("\\($", "", match_text)
    regmatches(t_text, matches) <- list(paste0(call_map[shorthand], "("))
    return(t_text)
  }, USE.NAMES = FALSE)

  paste(replaced_tokens, collapse = "")
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
#' @param initial_quote An open string/raw-string state to resume parsing inside of (as returned
#' in `end_quote` by a previous call), or `NULL` if parsing starts in code. Default: `NULL`.
#' @param return_state Whether to return both the modified text and the ending quote state.
#' Default: `FALSE`.
#'
#' @return A string representing the modified line. If `return_state = TRUE`, a list with
#' `text` and `end_quote` is returned instead.
#' @importFrom stringr str_detect
#' @export
.safely_replace_l <- function(line, strict_mode, initial_quote = NULL, return_state = FALSE) {
  stopifnot(is.character(line), length(line) == 1, is.logical(strict_mode), length(strict_mode) == 1,
            is.logical(return_state), length(return_state) == 1)
  parsed <- .tokenize_r_line(line, initial_quote = initial_quote)
  tokens <- parsed$tokens

  replaced_tokens <- sapply(tokens, function(tok) {
    if (!tok$is_code || nchar(tok$text) == 0) {
      return(tok$text)
    }
    t_text <- tok$text
    if (strict_mode) {
      modified <- gsub("(^|[^a-zA-Z0-9_])l\\(", "\\1length(", t_text)
    } else {
      modified <- gsub("\\bl\\(", "length(", t_text, perl = TRUE)
    }
    return(modified)
  }, USE.NAMES = FALSE)

  text <- paste(replaced_tokens, collapse = "")
  if (return_state) {
    return(list(text = text, end_quote = parsed$end_quote))
  }
  text
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
