######################################################################
# DocumentationTools.R
#####################################################################
# source('~/GitHub/Packages/PackageTools/R/DocumentationTools.R')
# stop(); rm(list = ls(all.names = TRUE)); try(dev.off(), silent = TRUE); gc()





# _____________________________________________________________________________________________ ----


# _____________________________________________________________________________________________ ----


# _____________________________________________________________________________________________ ----
# 1. Package Section ---------------------------------------------------------------------------


#' @title Create R Package from Configuration
#'
#' @description Automates the creation of an R package from a configuration file.
#' This function automates the creation of an R package by sourcing a configuration file
#' from the specified package directory. It assumes the presence of a `config.R` file in
#' the `Development` subdirectory of the package.
#'
#' @param package_dir The path to the package directory. Example: '~/GitHub/Packages/PackageX'.
#' @param config_file The configuration file name within the package's Development directory.
#'                    Default: 'config.R'.
#' @param update_citation Whether to update the CITATION file. Default: FALSE.
#' @param backup_r_script Whether to back up the previous R script into another file. Default: FALSE.
#' @param dev_folder The name of the development folder. Default: 'Development'.
#'
#' @return None
#' @importFrom usethis create_package
#' @importFrom devtools create document
#' @importFrom rstudioapi isAvailable
#'
#' @examples
#' document_and_create_package("~/GitHub/Packages/PackageX", "config.R", TRUE)
#'
#' @export
document_and_create_package <- function(package_dir,
                                        config_file = "config.R",
                                        backup_r_script = FALSE,
                                        update_citation = TRUE,
                                        dev_folder = "Development") {
  # Source configuration file
  config_path <- file.path(package_dir, dev_folder, config_file)
  if (!file.exists(config_path)) {
    stop("Configuration file not found: ", config_path)
  }

  # Parse DESCRIPTION
  DESCRIPTION <- .parse_description(config_path)
  # print(DESCRIPTION)
  stopifnot(!is.null(DESCRIPTION$"Version"))
  stopifnot(!is.null(DESCRIPTION$"Package"))

  # Set up directories and file paths
  RepositoryDir <- package_dir
  BackupDir <- file.path(RepositoryDir, dev_folder)
  package.FnP <- file.path(RepositoryDir, "R", paste0(DESCRIPTION$"Package", ".R"))

  # Create Backup Directory and Perform Backup
  if (backup_r_script) {
    dir.create(BackupDir, recursive = TRUE, showWarnings = FALSE)
    BackupOldFile <- file.path(BackupDir, "Development.bac")
    file.copy(from = package.FnP, to = BackupOldFile, overwrite = TRUE)
  }


  # Define the list
  # DESCRIPTION <- list(Element1 = "Element1", Element2 = "Element2", Element3 = "", Element4 = "Element4")

  # Get the indices of the empty elements
  empty_indices <- names(which(sapply(DESCRIPTION, nchar) == 0))

  # Loop over the empty indices and raise warnings
  for (i in empty_indices) {
    warning(paste(i, "in DESCRIPTION is empty, and now removed!"), immediate. = TRUE)
  }
  # Replace the empty elements with NULL
  DESCRIPTION[empty_indices] <- NULL


  # Create or Update Package
  if (!dir.exists(RepositoryDir)) {
    devtools::create(path = RepositoryDir, DESCRIPTION, rstudio = rstudioapi::isAvailable())
  } else {
    setwd(RepositoryDir)
    file.remove(c("DESCRIPTION", "NAMESPACE"))
    message("> usethis::create_package...")
    usethis::create_package(path = RepositoryDir, fields = DESCRIPTION, open = FALSE)
  }

  # Compile Package
  setwd(RepositoryDir)
  message("> devtools::document...")
  devtools::document(pkg = RepositoryDir)
  warnings()

  # Update CITATION.cff if requested
  if (update_citation) {
    .update_citation_file(RepositoryDir, DESCRIPTION$"Version")
  }
}
# document_and_create_package(repository.dir, config_file = 'config.R')

# _____________________________________________________________________________________________
#' @title Parse DESCRIPTION File
#'
#' @description Helper function to parse the DESCRIPTION file from a configuration file.
#'
#' @param config_path The file path of the configuration file.
#'
#' @return A list representing the DESCRIPTION file.
.parse_description <- function(config_path) {
  source(config_path) # Should create a DESCRIPTION list
  stopifnot(exists("DESCRIPTION"))
  list(
    Package = DESCRIPTION$"package.name",
    # Type = "Package",
    Title = DESCRIPTION$"title",
    Version = DESCRIPTION$"version",
    Author = person(
      given = DESCRIPTION$"author.given", family = DESCRIPTION$"author.family",
      email = DESCRIPTION$"author.email", role = c("aut", "cre")
    ),
    "Authors@R" = paste0(
      'person(given = "', DESCRIPTION$"author.given",
      '", family = "', DESCRIPTION$"author.family",
      '", email = "', DESCRIPTION$"author.email", '", role = c("aut", "cre"))'
    ),
    # Maintainer = person(
    #   given = DESCRIPTION$"maintainer.given", family = DESCRIPTION$"maintainer.family",
    #   email = DESCRIPTION$"maintainer.email", role = "ctb"
    # ),
    Description = DESCRIPTION$"description",
    License = DESCRIPTION$"license",
    Depends = DESCRIPTION$"depends",
    Imports = DESCRIPTION$"imports",
    Suggests = DESCRIPTION$"suggests",
    BugReports = if (!is.null(DESCRIPTION$"bug.reports")) DESCRIPTION$"bug.reports" else file.path("https://github.com", DESCRIPTION$"github.user", DESCRIPTION$"package.name", "issues"),
    Packaged = Sys.time()
  )
}


# _____________________________________________________________________________________________
#' @title Helper function to update the CITATION file of a package.
#' @description Update the CITATION.cff file of a package based on its version.
#'
#' @param RepositoryDir The directory of the repository.
#' @param version The version of the package.
#'
#' @importFrom xfun gsub_file
#' @return None
.update_citation_file <- function(RepositoryDir, version) {
  citpath <- file.path(RepositoryDir, "CITATION.cff")
  xfun::gsub_file(
    file = citpath, perl = TRUE,
    pattern = "^version: v.+",
    replacement = paste0("version: v", version)
  )
}



# _____________________________________________________________________________________________ ----
# 2. Documenting Dependencies ---------------------------------------------------------------------------

#' @title Extract Package Dependencies
#'
#' @description This function checks the package dependencies by listing functions used in all R scripts
#' found in a specified package directory and writes them to a dependencies file, separated
#' by script.
#'
#' @param package_dir The path to the package directory. Default: '~/GitHub/Packages/PackageX'.
#' @param output_file The relative path from the package directory to the dependencies file.
#'                      Default: 'Development/Dependencies.R'.
#' @param copy_to_clipboard Logical. If TRUE, the dependencies are copied to the clipboard.
#'                          Default: FALSE.
#'
#' @return None
#' @examples
#' extract_package_dependencies(
#'   "~/GitHub/Packages/PackageX",
#'   "Development/Dependencies.R"
#' )
#'
#' @importFrom NCmisc list.functions.in.file
#' @importFrom clipr write_clip
#'
#' @export
extract_package_dependencies <- function(package_dir, output_file = "Development/Dependencies.R"
                                         , copy_to_clipboard = FALSE) {
  # Assertions
  stopifnot(
    is.character(package_dir),
    is.character(output_file),
    dir.exists(file.path(package_dir, "R"))
  )

  depFile <- file.path(package_dir, output_file)
  r_files <- list.files(file.path(package_dir, "R"), full.names = TRUE, pattern = "\\.R$")


  # Overwrite with timestamp at the beginning
  cat(paste("Dependency file generated on", date(), "\n\n"), append = FALSE, file = depFile)

  # Iterating over R files
  for (file in r_files) {
    print(file)
    f.deps <- NCmisc::list.functions.in.file(filename = file)

    # Copying to clipboard or printing to console
    if (copy_to_clipboard & require(clipr)) clipr::write_clip(f.deps) else print(f.deps)

    # Writing to dependencies file
    sink(file = depFile, append = TRUE)
    cat("", file = depFile, append = TRUE)
    cat(paste0(rep("#", 100), collapse = ""), "\n", file = depFile, append = TRUE) # Separator
    cat(basename(file), file = depFile, append = TRUE, fill = TRUE)
    cat(paste0(rep("#", 100), collapse = ""), "\n", file = depFile, append = TRUE) # Separator
    print(f.deps)
    sink()
    p.deps <- gsub(x = names(f.deps), pattern = "package:", replacement = "")
    write(x = p.deps, file = depFile, append = TRUE)
  }

  # Output assertion
  stopifnot(file.exists(depFile))

}
