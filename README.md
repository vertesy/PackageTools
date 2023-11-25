# PackageTools
Functions and wrappers to manage R packages (creation, documentation, dependencies, checks).





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

