% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DependencyTools.R
\name{convert_igraph_to_mermaid}
\alias{convert_igraph_to_mermaid}
\title{Convert an igraph object to a Mermaid.js flowchart}
\usage{
convert_igraph_to_mermaid(
  graph,
  direction = "LR",
  node_shape = "round",
  copy_to_clipboard = TRUE,
  openMermaid = TRUE,
  pkg_path_for_scripts_as_subgraphs = FALSE,
  add_subgraph_template = TRUE,
  add_embedding_comments = TRUE
)
}
\arguments{
\item{graph}{An igraph object representing a network graph. Default: None (must be provided).}

\item{direction}{The direction of the flowchart. One of 'TB', 'TD', 'BT', 'RL', 'LR'. Default: 'LR'.}

\item{node_shape}{The shape of the nodes in the flowchart. One of 'round', 'default'. Default: 'round'.}

\item{copy_to_clipboard}{Whether to copy the resulting Mermaid.js code to the clipboard. Default: TRUE.}

\item{openMermaid}{open www.mermaid.live website? Default: TRUE.}

\item{pkg_path_for_scripts_as_subgraphs}{Do you want to add it to subgraphs? Provide the package
path for scripts represented as subgraphs."}

\item{add_subgraph_template}{Add subgraph template? Default: TRUE.}

\item{add_embedding_comments}{Add lines for direct markdown embedding of the code,
formatted as mermaid comments. The \code{\%\%} must be removed in the \code{.md} file.}
}
\value{
A string containing the Mermaid.js code for the flowchart.
}
\description{
This function takes an igraph object representing a network graph and
converts it into Mermaid.js code for creating a flowchart. It allows customization of the
flowchart's direction and node shapes and can optionally copy the resulting code to the clipboard.
}
\examples{
result <- pkgnet::CreatePackageReport("YourPackage")
graph <- result$FunctionReporter$pkg_graph$igraph
mermaid_code <- convert_igraph_to_mermaid(graph)
cat(mermaid_code)
}
