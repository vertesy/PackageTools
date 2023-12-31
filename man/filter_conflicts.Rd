% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DependencyTools.R
\name{filter_conflicts}
\alias{filter_conflicts}
\title{Filter Out Conflict Dependencies}
\usage{
filter_conflicts(dependencies)
}
\arguments{
\item{dependencies}{A list of dependencies for each function.
Default: None, a valid list of dependencies must be provided.}
}
\value{
A list of dependencies that are marked as conflicts.
}
\description{
Filters the dependencies to extract only those with conflicts.
}
\examples{
deps <- analyze_package_dependencies(c("stats", "utils"))
conflicts <- filter_conflicts(deps)
}
