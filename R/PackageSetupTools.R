######################################################################
# PackageSetupTools.R
#####################################################################
# source('~/GitHub/Packages/PackageTools/R/PackageSetupTools.R')
# stop(); rm(list = ls(all.names = TRUE)); try(dev.off(), silent = TRUE); gc()





# _____________________________________________________________________________________________ ----

#' @title Set Up a New R Package
#' @description This function sets up a new R package by copying src_pkg content into a specified directory
#' and replacing src_pkg placeholders in file names and file contents.
#'
#' @param package_name The name of the new package.
#'                     Default: None, a valid package name must be provided.
#' @param packages_path The root path where packages are located.
#'                      Default: "~/GitHub/Packages/".
#' @param src_pkg The src_pkg package name.
#'                 Default: "PackageTools".
#'
#' @return None
#' @export
#'
#' @examples
#' setupNewRPackage("MyNewPackage", packages_path = "~/GitHub/Packages/", src_pkg = "PackageTools")
#'
#' @importFrom base dir.exists file.copy
#' @importFrom stringr str_replace_all
setupNewRPackage <- function(package_name, packages_path = "~/GitHub/Packages", src_pkg = "PackageTools") {
  stopifnot(is.character(package_name), nchar(package_name) > 0,
            is.character(packages_path), dir.exists(packages_path),
            is.character(src_pkg), nchar(src_pkg) > 0)

  # Paths
  template_path <- file.path(packages_path, src_pkg, "Templates")
  print(paste("template_path", template_path))
  new_package_path <- file.path(packages_path, package_name)
  print(paste("new_package_path", new_package_path))

  # Copy src_pkg contents
  stopifnot(dir.exists(template_path))
  if (dir.exists(new_package_path)) warning("new_package_path exists.")

  file.copy(template_path, new_package_path, recursive = TRUE)

  # Rename files and replace content
  files <- list.files(new_package_path, recursive = TRUE, full.names = TRUE)
  for (file in files) {
    new_file_name <- stringr::str_replace_all(file, src_pkg, package_name)
    file.rename(file, new_file_name)
    file_content <- readLines(new_file_name)
    file_content <- stringr::str_replace_all(file_content, src_pkg, package_name)
    writeLines(file_content, new_file_name)
  }

  stopifnot(dir.exists(new_package_path))
}




#' Set Up New R Package from Template
#'
#' This function sets up a new R package by copying contents from a template directory,
#' replacing placeholder text in filenames and file contents.
#'
#' @title Set Up New R Package from Template
#' @description Automate the setup of a new R package using a predefined template.
#'
#' @param package_name The name of the new package.
#'                     Default: None, a valid package name must be provided.
#' @param packages_path The path to the packages directory.
#'                      Default: "~/GitHub/Packages/".
#' @param template The name of the template to use.
#'                 Default: "PackageTools".
#'
#' @return None
#' @export
#' @importFrom base dir file.exists
#' @importFrom stringr str_replace
setupNewPackageFromTemplate <- function(package_name,
                                        packages_path = "~/GitHub/Packages",
                                        template = "PackageTools") {
  # Assertions
  stopifnot(is.character(package_name),
            is.character(packages_path),
            is.character(template),
            nchar(package_name) > 0,
            dir.exists(packages_path),
            dir.exists(file.path(packages_path, template, "Templates")))

  # Paths
  template_path <- file.path(packages_path, template, "Templates")
  print(paste("template_path", template_path))
  new_package_path <- file.path(packages_path, package_name)
  print(paste("new_package_path", new_package_path))

  # Copy Templates contents
  stopifnot(dir.exists(template_path))
  if (dir.exists(new_package_path)) {
    warning("new_package_path exists.")
  } else {
    dir.create(new_package_path, recursive = TRUE)
  }

  # Copying files from the template directory
  template_files <- list.files(template_path, full.names = TRUE)
  sapply(template_files, function(file) {
    new_file_name <- gsub(template, package_name, basename(file))
    file.copy(from = file, to = file.path(new_package_path, new_file_name))
  })

  # Replacing template name in all files
  new_package_files <- list.files(new_package_path, full.names = TRUE, recursive = TRUE)
  lapply(new_package_files, function(file) {
    file_content <- readLines(file, warn = FALSE)
    file_content <- gsub(template, package_name, file_content)
    writeLines(file_content, file)
  })

  # Output assertion
  stopifnot(dir.exists(new_package_path))
}



setupNewRPackage(package_name = "GenotypeUnmixer")

