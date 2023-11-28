######################################################################
# PackageSetupTools.R
#####################################################################
# source('~/GitHub/Packages/PackageTools/R/PackageSetupTools.R')
# stop(); rm(list = ls(all.names = TRUE)); try(dev.off(), silent = TRUE); gc()





# _____________________________________________________________________________________________ ----

#' Setup New Package from Template
#'
#' This function sets up a new R package by copying contents from a template directory,
#' replacing template placeholders in file names and file contents with the specified package name.
#'
#' @title Setup New Package from Template
#' @description Setup a new R package by copying template contents and replacing placeholders.
#'
#' @param package_name Name of the new package.
#'        Default: 'NewPackage'.
#' @param packages_path Path to the packages directory.
#'        Default: '~/GitHub/Packages/'.
#' @param template_pkg Name of the template package.
#'        Default: 'PackageTools'.
#'
#' @return None
#' @export
#'
#' @examples
#' setup_new_package_from_template("MyNewPackage", "~/GitHub/Packages/", "PackageTools")

setup_new_package_from_template <- function(package_name = 'NewPackage',
                                        packages_path = '~/GitHub/Packages/',
                                        template_pkg = 'PackageTools') {
  stopifnot(is.character(package_name), is.character(packages_path), is.character(template_pkg))

  # Paths
  template_path <- file.path(packages_path, template_pkg, 'Templates')
  new_package_path <- file.path(packages_path, package_name)
    print(paste("From template_path", template_path))
    print(paste("To new_package_path", new_package_path))

  # Ensure the template directory exists
  stopifnot(dir.exists(template_path))

  if (dir.exists(new_package_path)) {
    warning("new_package_path already exists.")
  } else {
    warning("new_package_path does not exists. Use usethis create_package().")
    dir.create(new_package_path, recursive = TRUE)
  }

  # Copying contents from template directory
  file.copy(list.files(template_path, full.names = TRUE), new_package_path, recursive = TRUE)

  # Get all files in the new package directory
  package_files <- list.files(new_package_path, full.names = TRUE, recursive = TRUE)

  # Renaming files and replacing contents
  for (file in package_files) {
    # Replace in file names
    file_new_name <- gsub(template_pkg, package_name, file)
    if (file != file_new_name) {
      file.rename(file, file_new_name)
    }

    # Replace contents if the file is a text file
    if (grepl("\\.(R|Rmd|md|txt)$", file_new_name)) {
      file_contents <- readLines(file_new_name, warn = FALSE)
      file_contents <- gsub(template_pkg, package_name, file_contents)
      writeLines(file_contents, file_new_name)
    }
  }
}


# setup_new_package_from_template(package_name = "GenotypeUnmixerTest")

