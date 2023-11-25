######################################################################
# Usage.of.DependencyTools.R
#####################################################################
# source('~/GitHub/Packages/PackageTools/Examples/Usage.of.DependencyTools.R')
# stop(); rm(list = ls(all.names = TRUE)); try(dev.off(), silent = T); gc()

# Functions ------------------------------------------------
# source('~/GitHub/Packages/Rocinante/R/Rocinante.R')


# _____________________________________________________________________________________________ ----

source('~/GitHub/Packages/PackageTools/R/DependencyTools.R')

# List of packages to analyze
packages <- c("Stringendo", "ReadWriter")

# Analyzing dependencies with extended search
dependencies <- analyze_package_dependencies(packages, extended_search = T)

# Printing dependencies
print(dependencies)

dependencies_present <- filter_dependencies(dependencies, include_only_with_deps = TRUE)
dependencies_none <- filter_dependencies(dependencies, include_only_with_deps = FALSE)
(dependencies_conflicts <- filter_conflicts(dependencies_present))


# _____________________________________________________________________________________________ ----
