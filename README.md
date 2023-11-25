# PackageTools
Functions and wrappers to manage R packages (creation, documentation, dependencies, checks).

![PackageTools.logo.small](Development/PackageTools.logo.small.png)





# DependencyTools

`DependencyTools`, is the first working part of the R package `PackageTools` and it is designed to analyze and visualize **function-level dependencies** within and **across R packages**. It provides a suite of tools for extracting, filtering, and examining the intricate network of function calls, making it easier for developers and analysts to understand and manage code dependencies.

## Motivation

In complex R projects, understanding the relationships between different functions across various packages can be challenging. Dependencies are not always clear, leading to difficulties in maintenance, debugging, and optimization. DependencyTools addresses this challenge by providing a clear and comprehensive view of function dependencies, aiding in:

- Identifying redundant or conflicting functions.
- Understanding the impact of changes in one function on others.
- Streamlining package development and enhancing code quality.

## Installation

You can install DependencyTools from GitHub using the following command:

```R
# install.packages("devtools")
devtools::install_github("vertesy/PackageTools")
```

## Usage Example

Below is a detailed example of how to use DependencyTools to analyze function dependencies.

### Analyzing Dependencies

First, load the DependencyTools package:

```R
library(PackageTools)
```

#### Analyze dependencies in a set of R packages:

```R
packages <- c("dplyr", "ggplot2")
dependencies <- analyze_package_dependencies(packages)
```

This function will return a list where each element represents the dependencies of functions within the specified packages.

#### Analyze excluding specific packages and strings:

> Note: The `exclude_packages` option allows you to specify packages whose functions should not be considered as dependencies. The `exclude_strings` option lets you exclude any function whose name contains any of the specified strings. This is particularly useful for ignoring irrelevant or non-essential functions in the dependency analysis.

```R
packages <- c("CodeAndRoll2", "stringr")
exclude_pkg <- c("base", "stats") # Packages to exclude
exclude_str <- c("HYPERLINK", "Deprecated") # Strings to exclude

dependencies <- analyze_package_dependencies(packages, exclude_packages = exclude_pkg, exclude_strings = exclude_str)
```

This function will return a list where each element represents the dependencies of functions within the specified packages, excluding dependencies from the specified packages and functions whose names contain the specified strings.

### Filtering Dependencies

Filter out functions with no dependencies / only with dependencies:

```R
funs_with_deps <- filter_dependencies(dependencies, include_only_with_deps = TRUE)
funs_no_deps <- filter_dependencies(dependencies, include_only_with_deps = TRUE)
```

### Identifying Conflicts

Extract dependencies with conflicts:

```R
dep_conflicts <- filter_conflicts(dependencies)
```

### Expected Output

The output from `analyze_package_dependencies` will be a list structured as follows:

```R
$package_name
  $function_name
    [1] "package1::dep_function1" "package2::dep_function2"
```

For `filter_dependencies` and `filter_conflicts`, the output will be similar but filtered based on the criteria you set (presence of dependencies or conflicts).



## List of Functions

### PackageTools.R (3) 

Updated: 2023/11/24 16:45

- #### 1 `parse_roxygen_simple()`

  Parse Roxygen Comments. Extracts and summarizes Roxygen documentation comments from a specified R script file.

- #### 2 `parse_roxygen()`


- #### 3 `.convertFilePathToOutput()`

  Convert File Path for Documentation. Converts a file path from an R script format to a markdown file format,


### DependencyTools.R (5) 

Updated: 2023/11/25 14:00

- #### 1 `get_package_functions()`

  Get Package Functions. Retrieves all function names from a specified package. 

- #### 2 `map_functions_to_packages()`

  Map Functions to Packages. Creates a mapping of functions to their source packages.               Optionally performs an extended search across all installed packages. 

- #### 3 `analyze_function_dependencies()`

  Analyze Function Dependencies with Exclusions. Analyzes dependencies of functions within a package, excluding specified packages and strings. 

- #### 4 `analyze_package_dependencies()`

  Analyze Package Dependencies. Analyzes and maps the dependencies of all functions in a set of packages. 

- #### 5 `filter_dependencies()`

  Filter Function Dependencies. Filters the function dependencies to either include only functions with dependencies               or only those without dependencies. 

- #### 6 `filter_conflicts()`

  Filter Out Conflict Dependencies. Filters the dependencies to extract only those with conflicts. 



## Contributing

Contributions to DependencyTools are welcome, please communicate via issues.

---



