######################################################################
# DependencyTools.R
#####################################################################
# source('~/GitHub/Packages/PackageTools/R/DependencyTools.R')
# stop(); rm(list = ls(all.names = TRUE)); try(dev.off(), silent = T); gc()


# _____________________________________________________________________________________________ ----
## Package Section ---------------------------------------------------------------------------

#' @title Get Package Functions
#'
#' @description Retrieves all function names from a specified package.
#'
#' @param package_name The name of the package.
#'                     Default: None, a valid package name must be provided.
#' @return A character vector of function names from the package.
#' @examples
#' get_package_functions("stats")
#' @export
get_package_functions <- function(package_name) {
  stopifnot(is.character(package_name), length(package_name) == 1)

  ns <- asNamespace(package_name)
  fun_names <- ls(ns)
  fun_names[sapply(fun_names, function(x) is.function(get(x, envir = ns)))]
}


# _____________________________________________________________________________________________
#' @title Map Functions to Packages
#'
#' @description Creates a mapping of functions to their source packages.
#'              Optionally performs an extended search across all installed packages.
#'
#' @param packages A vector of package names to include in the mapping.
#'                 Default: None, a valid vector of package names must be provided.
#' @param verbose Print logs? Default: TRUE.
#' @param extended_search Logical; whether to perform an extended search across all installed packages.
#'                        Default: FALSE.
#' @param exclude_from_extended A vector of package names to exclude from the extended search.
#'                              Default: Empty character vector.
#' @return A list mapping function names to their source package.
#' @examples
#' map_functions_to_packages(c("stats", "utils"), extended_search = TRUE)
#' @export
map_functions_to_packages <- function(
    packages, extended_search = FALSE, verbose = TRUE,
    exclude_from_extended = character()) {
  stopifnot(is.character(packages), is.logical(extended_search), is.character(exclude_from_extended))

  func_to_pkg <- list()

  # Mapping within specified packages
  cat("--- listing functions:", fill = T)
  for (pkg in packages) {
    cat(pkg, fill = T) # if(verbose)

    funcs <- get_package_functions(pkg)
    for (fn in funcs) {
      func_to_pkg[[fn]] <- pkg
    }
  }

  if (extended_search) {
    cat("--- extended search among installed packages:", fill = T)
    all_installed_packages <- setdiff(installed.packages()[, "Package"], exclude_from_extended)
    cat(length(all_installed_packages), " installed packages.", fill = T)

    for (pkg in setdiff(all_installed_packages, packages)) {
      if (verbose) cat(pkg, fill = T)

      # Safely load package namespaces
      tryCatch(
        {
          funcs <- get_package_functions(pkg)
          for (fn in funcs) {
            if (!is.null(func_to_pkg[[fn]])) {
              func_to_pkg[[fn]] <- paste("CONFLICT", func_to_pkg[[fn]], pkg, sep = "::")
            } else {
              func_to_pkg[[fn]] <- pkg
            }
          }
        },
        error = function(e) {}
      )
    }
  }
  return(func_to_pkg)
}



# _____________________________________________________________________________________________
#' @title Analyze Function Dependencies with Exclusions
#'
#' @description Analyzes dependencies of functions within a package, excluding specified packages and strings.
#'
#' @param func_name The name of the function to analyze.
#'                  Default: None, a valid function name must be provided.
#' @param package_name The name of the package containing the function.
#'                     Default: None, a valid package name must be provided.
#' @param func_to_pkg_map A list mapping function names to their source package.
#'                        Default: None, a valid mapping must be provided.
#' @param exclude_packages A vector of package names whose functions should be excluded from the analysis.
#'                         Default: c("base", "utils", "methods", "stats").
#' @param exclude_strings A vector of strings to exclude from the dependencies.
#'                        Default: c("HYPERLINK", "Deprecated")
#' @return A vector of function names that the specified function depends on, excluding specified strings.
#' @examples
#' analyze_function_dependencies("lm", "stats", map_functions_to_packages(c("stats")), exclude_strings = c("HYPERLINK", "Deprecated"))
#' @export
analyze_function_dependencies <- function(
    func_name, package_name, func_to_pkg_map,
    exclude_packages = c("base", "utils", "methods", "stats"),
    exclude_strings = c("HYPERLINK", "Deprecated")) {
  # Input assertions
  stopifnot(is.character(func_name), is.character(package_name), is.list(func_to_pkg_map), is.character(exclude_packages), is.character(exclude_strings))

  func <- get(func_name, envir = asNamespace(package_name))
  func_body <- deparse(body(func))

  # Find function calls
  func_calls <- regmatches(func_body, gregexpr("\\b[a-zA-Z0-9_.]+\\(", func_body))
  func_calls <- unique(unlist(func_calls))
  func_calls <- gsub("\\(", "", func_calls)

  # Remove functions from excluded packages
  exclude_funcs <- unlist(lapply(exclude_packages, function(pkg) ls(paste0("package:", pkg))))
  func_calls <- setdiff(func_calls, exclude_funcs)

  # Exclude specific strings from dependencies
  func_calls <- setdiff(func_calls, exclude_strings)

  # Map function calls to their packages
  func_calls_with_pkg <- sapply(func_calls, function(fn) {
    pkg <- func_to_pkg_map[[fn]]
    if (!is.null(pkg)) {
      return(paste0(pkg, "::", fn))
    }
    return(fn)
  })

  return(func_calls_with_pkg)
}




# _____________________________________________________________________________________________
#' @title Analyze Package Dependencies
#'
#' @description Analyzes and maps the dependencies of all functions in a set of packages.
#'
#' @param packages A vector of package names to analyze.
#'                 Default: None, a valid vector of package names must be provided.
#' @param exclude_packages A vector of package names whose functions should be excluded from the analysis.
#'                         Default: c("base", "utils", "methods", "stats").
#' @param extended_search Logical; whether to perform an extended search across all installed packages.
#'                        Default: FALSE.
#' @param verbose Print logs? Default: FALSE.
#' @return A list of dependencies for each function in the specified packages.
#' @examples
#' analyze_package_dependencies(c("stats", "utils"))
#' @export

analyze_package_dependencies <- function(
    packages, exclude_packages = c("base", "utils", "methods", "stats"),
    extended_search = FALSE, verbose = FALSE) {
  # Input assertions
  stopifnot(is.character(packages), is.character(exclude_packages), is.logical(extended_search))
  func_to_pkg_map <- map_functions_to_packages(packages, extended_search, verbose = verbose)
  dependencies <- lapply(packages, function(pkg) {
    funcs <- get_package_functions(pkg)
    deps <- lapply(funcs, function(fn) analyze_function_dependencies(fn, pkg, func_to_pkg_map, exclude_packages))
    setNames(deps, funcs)
  })
  setNames(dependencies, packages)
}



# _____________________________________________________________________________________________
#' @title Filter Function Dependencies
#'
#' @description Filters the function dependencies to either include only functions with dependencies
#'              or only those without dependencies.
#'
#' @param dependencies A list of dependencies for each function.
#'                     Default: None, a valid list of dependencies must be provided.
#' @param include_only_with_deps Logical; if TRUE, only includes functions with dependencies.
#'                               Default: TRUE.
#' @return A filtered list of dependencies.
#' @examples
#' deps <- analyze_package_dependencies(c("stats", "utils"))
#' filter_dependencies(deps)
#' @export
filter_dependencies <- function(dependencies, include_only_with_deps = TRUE) {
  # Input assertions
  stopifnot(is.list(dependencies), is.logical(include_only_with_deps))

  dependencies_filtered <-
    if (include_only_with_deps) {
      lapply(dependencies, function(pkg_deps) Filter(function(d) length(d) > 0, pkg_deps))
    } else {
      lapply(dependencies, function(pkg_deps) Filter(function(d) length(d) == 0, pkg_deps))
    }
  # w_wo <- if(include_only_with_deps) "with" else  "without"
  # cat(">>>", length(dependencies_filtered), "functions found", w_wo, "dependencies.", fill = T)
  .count_and_print_function_summary(dependencies_filtered)

  return(dependencies_filtered)
}

# _____________________________________________________________________________________________
#' @title Filter Out Conflict Dependencies
#'
#' @description Filters the dependencies to extract only those with conflicts.
#'
#' @param dependencies A list of dependencies for each function.
#'                     Default: None, a valid list of dependencies must be provided.
#' @return A list of dependencies that are marked as conflicts.
#' @examples
#' deps <- analyze_package_dependencies(c("stats", "utils"))
#' conflicts <- filter_conflicts(deps)
#' @export
filter_conflicts <- function(dependencies) {
  # Input assertions
  stopifnot(is.list(dependencies))

  conflicts <- lapply(dependencies, function(pkg_deps) {
    lapply(pkg_deps, function(deps) {
      if (length(deps) > 0) {
        return(deps[grep("CONFLICT::", deps)])
      } else {
        return(character(0))
      }
    })
  })

  # Remove empty entries
  conflicts <- lapply(conflicts, function(pkg_deps) {
    pkg_deps[sapply(pkg_deps, length) > 0]
  })
  .count_and_print_function_summary(conflicts)
  return(conflicts)
}

# _____________________________________________________________________________________________
#' @title Count and print the number of functions results
#'
#' @description Private function to count and print the number of functions in results ("dependencies").
#'
#' @param dependencies Result from above functions dependencies
#' @return Nothing.
.count_and_print_function_summary <- function(dependencies) {
  counts <- sapply(dependencies, function(pkg_deps) length(pkg_deps))
  message_string <- paste(sapply(names(counts), function(pkg) paste(length(dependencies[[pkg]]), "functions in", pkg)), collapse = " and ")
  message(message_string, " are returned")
}



# _____________________________________________________________________________________________

#' Convert an igraph object to a Mermaid.js flowchart
#'
#' This function takes an igraph object representing a network graph and converts it into Mermaid.js code for
#' creating a flowchart. It allows customization of the flowchart's direction and node shapes and can optionally
#' copy the resulting code to the clipboard.
#'
#' @param graph An igraph object representing a network graph. Default: None (must be provided).
#' @param direction The direction of the flowchart. One of 'TB', 'TD', 'BT', 'RL', 'LR'. Default: 'LR'.
#' @param node_shape The shape of the nodes in the flowchart. One of 'round', 'default'. Default: 'round'.
#' @param copy_to_clipboard Whether to copy the resulting Mermaid.js code to the clipboard. Default: TRUE.
#' @param openMermaid open www.mermaid.live website? Default: TRUE.
#' @return A string containing the Mermaid.js code for the flowchart.
#' @examples
#' result <- pkgnet::CreatePackageReport("YourPackage")
#' graph <- result$FunctionReporter$pkg_graph$igraph
#' mermaid_code <- convert_igraph_to_mermaid(graph)
#' cat(mermaid_code)
#' @importFrom igraph get.edgelist
#' @importFrom clipr write_clip
#' @export
convert_igraph_to_mermaid <- function(
    graph, direction = "LR", node_shape = "round",
    copy_to_clipboard = TRUE, openMermaid = TRUE) {
  stopifnot(
    "graph must be an igraph object" = inherits(graph, "igraph"),
    "direction must be one of 'TB', 'TD', 'BT', 'RL', 'LR'" = direction %in% c("TB", "TD", "BT", "RL", "LR"),
    # "node_shape must be 'round' or 'default'" = node_shape %in% c("round", "default"), # not true!
  )

  edges <- igraph::get.edgelist(graph)
  mermaid_code <- paste("flowchart", direction, "\n")

  format_node <- function(node) {
    if (node_shape == "round") {
      return(sprintf("%s(%s)", node, node))
    } else {
      return(sprintf("%s[%s]", node, node))
    }
  }

  for (edge in 1:nrow(edges)) {
    from <- format_node(edges[edge, 1])
    to <- format_node(edges[edge, 2])
    mermaid_code <- paste(mermaid_code, sprintf("  %s --> %s", from, to), sep = "\n")
  }

  stopifnot("Mermaid.js code should be a non-empty string" = is.character(mermaid_code) && nchar(mermaid_code) > 0)

  if (copy_to_clipboard) {
    clipr::write_clip(mermaid_code)
  }
  print("Check output on https://mermaid.live")
  if (openMermaid) browseURL("https://mermaid.live")
  return(mermaid_code)
}


# _____________________________________________________________________________________________
