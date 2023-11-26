######################################################################################################
# Create_the_PackageTools_Package.R
######################################################################################################
# source("~/GitHub/Packages/PackageTools/Development/Create_the_PackageTools_Package.R")
# rm(list = ls(all.names = TRUE));
try(dev.off(), silent = TRUE)

# Functions ------------------------
# install_version("devtools", version = "2.0.2", repos = "http://cran.at.r-project.org") # install.packages("devtools")
# require("devtools")
# require("RcppArmadillo")
# require("roxygen2")
# require("stringr")

# # devtools::install_github(repo = "vertesy/CodeAndRoll2")
# require('Stringendo')
# require('CodeAndRoll2')



# Setup ------------------------
package.name <- 	"PackageTools"
package.version <- "0.2.1"
setwd("~/GitHub/Packages/")

RepositoryDir <- paste0("~/GitHub/Packages/", package.name, "/")
fname <-	paste0(package.name, ".R")
package.FnP <-		paste0(RepositoryDir, "R/", fname)

BackupDir <- "~/GitHub/Packages/PackageTools/Development/"
dir.create(BackupDir)

DESCRIPTION <- list("Title" = "PackageTools - Help for R package development."
                    , "Author" = person(given = "Abel", family = "Vertesy", email = "av@imba.oeaw.ac.at", role =  c("aut", "cre") )
                    , "Authors@R" = 'person(given = "Abel", family = "Vertesy", email = "av@imba.oeaw.ac.at", role =  c("aut", "cre") )'
                    , "Description" = "PackageTools is a collection of utility functions for code base statistics & and dependencies."
                    , "License" = "GPL-3 + file LICENSE"
                    , "Version" = package.version
                    , "Packaged" =  Sys.time()
                    # , "Repository" =  "CRAN"
                    , "Depends" =  "base"
                    , "Imports" = "checkmate"
                    # , "Suggests" = ""
                    , "BugReports"= "https://github.com/vertesy/PackageTools/issues"
)


setwd(RepositoryDir)
if ( !dir.exists(RepositoryDir) ) { devtools::create(path = RepositoryDir, DESCRIPTION, rstudio = rstudioapi::isAvailable())
} else {
  getwd()
  try(file.remove(c("DESCRIPTION","NAMESPACE"))) # , "PackageTools.Rproj"
  usethis::create_package(path = RepositoryDir,fields = DESCRIPTION, open = FALSE)
}



# go and write fun's ------------------------------------------------------------------------
# file.edit(package.FnP)

# replace output files ------------------------------------------------
BackupOldFile <-	paste0(BackupDir, "Development", ".bac")
AnnotatedFile <-	paste0(BackupDir, "Development", ".annot.R")
file.copy(from = package.FnP, to = BackupOldFile, overwrite = TRUE)
# file.copy(from = AnnotatedFile, to = package.FnP, overwrite = TRUE)

# Manual editing of descriptors ------------------------------------------------
# file.edit(package.FnP)

# Compile a package ------------------------------------------------
setwd(RepositoryDir)
getwd()
devtools::document()
warnings()

{
  "update cff version"
  citpath <- paste0(RepositoryDir, 'CITATION.cff')
  xfun::gsub_file(file = citpath, perl = T
                  , "^version: v.+", paste0("version: v", package.version))
}


# Install your package ------------------------------------------------
setwd(RepositoryDir)
devtools::install_local(RepositoryDir, upgrade = F)
# unload(PackageTools)
# require("PackageTools")
# # remove.packages("PackageTools")
# # Test your package ------------------------------------------------
# help("wplot")
# cat("\014")
# devtools::run_examples()


# Test if you can install from github ------------------------------------------------
pak::pkg_install("vertesy/PackageTools")

# require("PackageTools")

# Clean up if not needed anymore ------------------------------------------------
# View(installed.packages())
# remove.packages("PackageTools")
# "check(RepositoryDir, cran = TRUE)"
# system("cd ~/GitHub/PackageTools/; ls -a; open .Rbuildignore")

# Check package dependencies ------------------------------------------------
depFile = paste0(RepositoryDir, 'Development/Dependencies.R')

(f.deps <- NCmisc::list.functions.in.file(filename = package.FnP))
clipr::write_clip( f.deps)

sink(file = depFile); print(f.deps); sink()
p.deps <- gsub(x = names(f.deps), pattern = 'package:', replacement = '')
write(x = p.deps, file = depFile, append = T)


# Package styling, and visualization ------------------------------------------------
{
  styler::style_pkg(RepositoryDir)
}




# Else ------------------------------------------------
if (F) {

  findFunctionOrigin <- function(function_names) {
    # Get a list of all installed packages
    installed_packages <- rownames(installed.packages())


    # Initialize an empty list to store the results
    function_origins <- list()

    # Iterate over each function name
    for (func_name in function_names) {
      print(func_name)
      # Initialize a vector to store the packages where the function is found
      found_in_packages <- c()

      # Check each package
      for (pkg in installed_packages) {
        cat('.', append = T)
        # Check if the package contains the function
        if (func_name %in% ls(getNamespace(pkg), all.names = TRUE)) {
          found_in_packages <- c(found_in_packages, pkg)
        }
      }

      # Add the vector of packages to the result list, named by the function
      function_origins[[func_name]] <- found_in_packages
    }

    # Return the list
    return(function_origins)
  }

  FunctionOrigins <- findFunctionOrigin( f.deps$`character(0)`)

}

# NCmisc::list.functions.in.file(package.FnP)


