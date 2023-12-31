% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReplacementTools.R
\name{.safely_replace_tf}
\alias{.safely_replace_tf}
\title{Safely Replace T and F in a Line of R Script}
\usage{
.safely_replace_tf(line, strict_mode, preceding_chars, following_chars)
}
\arguments{
\item{line}{A single line of R script.}

\item{strict_mode}{Logical; specifies the mode of replacement. Default: TRUE.}

\item{preceding_chars}{Characters that can precede \code{T} or \code{F} for replacement.}

\item{following_chars}{Characters that can follow \code{T} or \code{F} for replacement.}
}
\value{
The modified line of R script.
}
\description{
This helper function replaces instances of \code{T} and \code{F} in a single line of R
script based on the specified mode and character constraints.
}
