######################################################################
# PackageTools.R
#####################################################################
# source('~/GitHub/Packages/PackageTools/R/PackageTools.R')
# stop(); rm(list = ls(all.names = TRUE)); try(dev.off(), silent = T); gc()



# _____________________________________________________________________________________________ ----
## Function List from Roxygen ---------------------------------------------------------------------------

#' @title Parse Roxygen Comments
#'
#' @description Extracts and summarizes Roxygen documentation comments from a specified R script file.
#' This function reads an R script, identifies Roxygen comments for function titles and descriptions,
#' and writes a summary to an output file.
#'
#' @param file Path to the R script file to be parsed.
#'             Default: None, a valid file path must be provided.
#' @param output_file Path to the output file where the summary will be written.
#'                    Default: "1list.of.functions.in.YOURFILE.md"
#' @param fun_header_level header level for functions. Default: "####"
#' @return This function does not return a value; it writes output to the specified file.
#' @examples
#' \dontrun{
#'   parse_roxygen_simple("path/to/your_script.R", "path/to/output_file.txt")
#' }
#' @export

parse_roxygen_simple <- function(file, output_file = .convertFilePathToOutput(file)
                                 , fun_header_level = "####") {
  # Input argument assertions
  stopifnot(is.character(file), length(file) == 1, file.exists(file))
  stopifnot(is.character(output_file), length(output_file) == 1)

  # Read the file as a vector of lines
  lines <- readLines(file)

  # Find the lines containing function names
  function_lines <- grep("<- function", lines, value = TRUE)
  cat(length(function_lines)-1, "functions are found. \n")

  # Find the lines containing @title
  title_lines <- grep("@title", lines, value = TRUE)

  # Find the lines containing @description
  description_lines <- grep("@description", lines, value = TRUE)

  # Extract the function names
  function_names <- gsub("\\s*<-.*$", "", function_lines)

  # Extract the titles
  titles <- gsub("^.*@title\\s*", "", title_lines)

  # Extract the descriptions
  descriptions <- gsub("^.*@description\\s*", "", description_lines)

  # Open a connection to the output file
  file_conn <- file(output_file, open = "w")

  cat(paste0("## List of Functions (", length(function_names)-1, ") \n"), file = file_conn)
  cat(paste0("Updated: ", format(Sys.time(), "%Y/%m/%d %H:%M"),"\n"), file = file_conn)

  # Write each function name, title, and description to the output file
  for (i in seq_along(function_names)) {
    cat(paste0("- ", fun_header_level, " ", i, " `", function_names[i], "()`\n"), file = file_conn)
    cat(paste0(titles[i], ". ", descriptions[i], "\n\n"), file = file_conn)
  }

  # Close the connection
  close(file_conn)

  # Output assertion
  stopifnot(file.exists(output_file))

  print(paste("Output written to", output_file))
}

# Use the function
# parse_roxygen_simple("~/GitHub/Packages/CodeAndRoll2/R/CodeAndRoll2.R")


# __________________________________________________________________________________________
#' @title Parse Roxygen Comments from R Script
#'
#' @description This function parses a given R script for Roxygen comments, extracts function titles and descriptions,
#'              and writes a summary to an output markdown file. The output file can have a custom name, and the
#'              function allows specifying the header level for functions in the markdown file.
#'
#' @param file The path to the R script file to be parsed.
#'             Default: None, a valid file path must be provided.
#' @param output_file The path to the output markdown file where the summary will be written.
#'                    Default: Generated using .convertFilePathToOutput() with ".det.md" extension.
#' @param write_title_field Logical; whether to include the title field in the output.
#'                          Default: TRUE.
#' @param fun_header_level The markdown header level to be used for function names.
#'                         Default: "####".
#' @return This function does not return a value; it writes output to the specified markdown file.
#' @examples
#' \dontrun{
#'   parse_roxygen("path/to/your_script.R", "path/to/output_file.md")
#' }
#' @export

parse_roxygen <- function(file, output_file = .convertFilePathToOutput(file, ext = ".det.md"),
                          write_title_field = TRUE, fun_header_level = "####") {

  warning("Does not find all functions sometimes!!!")

  # Input argument assertions
  stopifnot(is.character(file), length(file) == 1, file.exists(file))
  stopifnot(is.character(output_file), length(output_file) == 1)
  stopifnot(is.logical(write_title_field), length(write_title_field) == 1)
  stopifnot(is.character(fun_header_level), length(fun_header_level) == 1)

  # Read the file as a vector of lines
  lines <- readLines(file)

  # Initialize empty vectors to store function names, titles and descriptions
  function_names <- character(0)
  titles <- character(0)
  descriptions <- character(0)

  # Initialize temporary variables
  current_function_name <- ""
  current_title <- ""
  current_description <- ""
  in_description <- FALSE

  for (line in lines) {

    # Detect the start of a function Roxygen skeleton
    if (grepl("^#' @title", line)) {
      # Extract the title
      current_title <- sub("^#' @title\\s*", "", line)
      next
    }

    # Detect the start of a description
    if (grepl("^#' @description", line)) {
      # Extract the start of the description
      current_description <- sub("^#' @description\\s*", "", line)
      in_description <- TRUE
      next
    }

    # Handle lines within the description
    if (in_description && grepl("^#'", line) && !grepl("^#' @param|^#' @export|^#' @returns", line)) {
      # Continue the description
      current_description <- paste0(current_description, " ", sub("^#'", "", line))
      next
    }

    # Detect the end of a description or function Roxygen skeleton
    if (in_description && (grepl("^#' @param|^#' @export|^#' @returns", line) || grepl("<- function", line))) {
      # End the description
      in_description <- FALSE
      function_names <- c(function_names, current_function_name)
      titles <- c(titles, current_title)
      descriptions <- c(descriptions, current_description)
      current_function_name <- ""
      current_title <- ""
      current_description <- ""
      next
    }

    # Detect a function definition
    if (grepl("<- function|<-function", line)) {
      # Extract the function name
      current_function_name <- gsub("\\s*<-.*$", "", line)
    }

  }

  cat(length(function_names)-1, "functions are found. \n")
  { "check"
    function_lines <- grep("<- function", lines, value = TRUE)
    if (length(function_lines) != length(function_names)) {
      msg <- paste(length(function_names), "found here but", length(function_lines)
                   , "functions are defined (as `<- function`)" )
      warning(msg)

    }
  }


  # Open a connection to the output file
  file_conn <- file(output_file, open = "w")

  # cat("## List of Functions\n", file = file_conn)
  cat(paste0("## List of Functions (", length(function_names)-1, ") \n"), file = file_conn)
  cat(paste0("Updated: ", format(Sys.time(), "%Y/%m/%d %H:%M"),"\n"), file = file_conn)

  # Write each function name, title, and description to the output file
  for (i in seq_along(function_names)) {
    if (i==1) next
    cat(paste0("- ", fun_header_level, " ", i-1,' `', function_names[i], "()`\n"), file = file_conn)

    if (write_title_field) {
      cat(paste0(titles[i-1], ". ", descriptions[i-1], "\n\n"), file = file_conn)
    } else {
      cat(paste0(descriptions[i-1], "\n\n"), file = file_conn)
    }

  }

  # Close the connection
  close(file_conn)

  # Output assertion (Check if output file exists after writing)
  stopifnot(file.exists(output_file))

  print(paste("Output written to", output_file))
}




# Use the function
# parse_roxygen("~/GitHub/Packages/CodeAndRoll2/R/CodeAndRoll2.R")



# _____________________________________________________________________________________________ ----
## Package Documentation ---------------------------------------------------------------------------




# _____________________________________________________________________________________________




# _____________________________________________________________________________________________ ----
## Package Dependencies ---------------------------------------------------------------------------




# _____________________________________________________________________________________________



# _____________________________________________________________________________________________ ----
## Code Refactoring ---------------------------------------------------------------------------



# _____________________________________________________________________________________________



# _____________________________________________________________________________________________

# _____________________________________________________________________________________________ ----
## Aux ---------------------------------------------------------------------------

#' @title Convert File Path for Documentation
#'
#' @description Converts a file path from an R script format to a markdown file format,
#'              indicating that it contains a documentation list, such as a list of functions.
#'              The function allows specifying a prefix for the markdown filename and the file extension.
#'
#' @param inputPath The path to the R script file.
#'                  Default: None, a valid file path must be provided.
#' @param fn_prefix A prefix added before the script's base name in the output markdown filename.
#'                  Default: "list.of.functions.in.".
#' @param ext The file extension for the output file.
#'            Default: ".md".
#' @return A string representing the converted file path in the specified markdown format.
#' @examples
#' \dontrun{
  # .convertFilePathToOutput("path/to/your_script.R")
  # .convertFilePathToOutput("path/to/your_script.R", fn_prefix = "custom.prefix.", ext = ".txt")
#' }
#' @export

.convertFilePathToOutput <- function(inputPath, fn_prefix = "list.of.functions.in"
                                     , ext = '.md') {
  stopifnot(is.character(inputPath), length(inputPath) == 1)

  # Replace the file extension and modify the filename
  outputFileName <- sub("\\.R$", ext, basename(inputPath))
  # outputPath <- gsub("(.*/)?(.*)\\.md$", "\\1list.of.functions.in.\\2", outputPath)
  outputPath <- paste0(dirname(inputPath), "/", fn_prefix, ".", outputFileName)

  stopifnot(is.character(outputPath), nchar(outputPath) > 0)

  return(outputPath)
}
# .convertFilePathToOutput("path/to/your_script.R")
