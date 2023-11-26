######################################################################
# DocumentationTools.R
#####################################################################
# source('~/GitHub/Packages/PackageTools/R/DocumentationTools.R')
# stop(); rm(list = ls(all.names = TRUE)); try(dev.off(), silent = T); gc()





# _____________________________________________________________________________________________ ----


# _____________________________________________________________________________________________ ----


# ____________________________________________________________________________________________ ----
#' @title Create R Package from Configuration
#'
#' @description Automate the creation of an R package from a configuration file.
#' This function automates the creation of an R package by sourcing a configuration file
#' from the specified package directory. It assumes the presence of a `config.R` file in
#' the `Development` subdirectory of the package.
#'
#' @param package_dir The path to the package directory. Example: '~/GitHub/Packages/PackageX'.
#' @param config_file The configuration file name within the package's Development directory.
#'                    Default: 'config.R'.
#' @param update_citation Whether to update the CITATION file. Default: FALSE.
#' @param backup_r_script Whether to backup the previous r script into another file. Default: FALSE.
#' @return None
#' @importFrom usethis create_package
#' @importFrom devtools create document
#'
#' @examples
#' create_package("~/GitHub/Packages/PackageX", "config.R", TRUE)
#'
#' @export
create_package <- function(package_dir,
                           config_file = 'config.R',
                           backup_r_script = FALSE,
                           update_citation = FALSE) {
  # Source configuration file
  config_path <- file.path(package_dir, "Development", config_file)
  if (!file.exists(config_path)) {
    stop("Configuration file not found: ", config_path)
  }

  # Parse DESCRIPTION
  DESCRIPTION <- parse_description(config_path)

  # Set up directories and file paths
  RepositoryDir <- package_dir
  BackupDir <- file.path(RepositoryDir, "Development")
  package.FnP <- file.path(RepositoryDir, "R", paste0(DESCRIPTION$Package, ".R"))

  # Create Backup Directory and Perform Backup
  if (backup_r_script) {
    dir.create(BackupDir, recursive = TRUE, showWarnings = FALSE)
    BackupOldFile <- file.path(BackupDir, "Development.bac")
    file.copy(from = package.FnP, to = BackupOldFile, overwrite = TRUE)
  }


  # Create or Update Package
  if (!dir.exists(RepositoryDir)) {
    devtools::create(path = RepositoryDir, DESCRIPTION, rstudio = rstudioapi::isAvailable())
  } else {
    setwd(RepositoryDir)
    file.remove(c("DESCRIPTION", "NAMESPACE"))
    usethis::create_package(path = RepositoryDir, fields = DESCRIPTION, open = FALSE)
  }

  # Compile Package
  setwd(RepositoryDir)
  devtools::document(pkg = RepositoryDir)
  warnings()

  # Update CITATION.cff if requested
  if (update_citation) {
    update_citation_file(RepositoryDir, DESCRIPTION$Version)
  }
}

# _____________________________________________________________________________________________ ----
#' @title Parse DESCRIPTION File
#'
#' @description Helper function to parse the DESCRIPTION file from a configuration file.
#'
#' @param config_path The file path of the configuration file.
#'
#' @importFrom utils person
#' @importFrom base source
#' @return A list representing the DESCRIPTION file.
parse_description <- function(config_path) {
  source(config_path)
  list(
    Package = DESCRIPTION$"package.name",
    # Type = "Package",
    Title = DESCRIPTION$"title",
    Version = DESCRIPTION$"version",
    Author = person(given = DESCRIPTION$"author.given", family = DESCRIPTION$"author.family",
                    email = DESCRIPTION$"author.email", role = c("aut", "cre")),
    "Authors@R" = paste0('person(given = "', DESCRIPTION$"author.given",
                      '", family = "', DESCRIPTION$"author.family",
                      '", email = "', DESCRIPTION$"author.email", '", role = c("aut", "cre"))'),
    Description = DESCRIPTION$"description",
    License = DESCRIPTION$"license",
    Depends = DESCRIPTION$"depends",
    Imports = DESCRIPTION$"imports",
    BugReports = file.path("https://github.com", DESCRIPTION$"github.user", DESCRIPTION$"package.name", "issues"),
    Packaged = Sys.time()
  )
}

# _____________________________________________________________________________________________ ----
#' @title Helper function to update the CITATION file of a package.
#' @description Update the CITATION.cff file of a package based on its version.
#'
#' @param RepositoryDir The directory of the repository.
#' @param version The version of the package.
#'
#' @importFrom xfun gsub_file
#' @return None
update_citation_file <- function(RepositoryDir, version) {
  citpath <- file.path(RepositoryDir, 'CITATION.cff')
  xfun::gsub_file(file = citpath, perl = TRUE,
                  pattern = "^version: v.+",
                  replacement = paste0("version: v", version))
}







