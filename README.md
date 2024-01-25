# PackageTools ![status: experimental](https://raw.githubusercontent.com/vertesy/TheCorvinas/master/GitHub/Badges/experimental.svg)
Functions and wrappers to manage R packages (creation, documentation, dependencies, checks).

![PackageTools.logo.small](Development/PackageTools.logo.small.png)

[TOC]



# 1. PackageTools

## Overview

`PackageTools` is designed for parsing R function source files, specifically the extraction of function  documentation details from Roxygen comments. It offers two primary functions: `list_of_funs_to_markdown_simple()` and the recommended `list_of_funs_to_markdown()`. Both functions serve to automate the process of generating markdown documentation from Roxygen comments in R source files, streamlining the process of maintaining up-to-date documentation for R packages.

### `list_of_funs_to_markdown_simple()`

- **Purpose**: Extracts basic information (function names, titles, and descriptions) from Roxygen comments in an R script and writes them to a markdown file.
- **Usage**: Intended for simpler R scripts where a quick summary of functions is needed.
- **Functionality**:
  - Reads an R script, identifying functions and their corresponding Roxygen comments.
  - Extracts function names, titles, and descriptions.
  - Handles discrepancies in the number of titles and descriptions.
  - Outputs the information in a structured markdown format.
  - Optionally opens the resulting markdown file automatically.

### `list_of_funs_to_markdown()`

- **Purpose**: A more advanced version of `list_of_funs_to_markdown_simple`, offering enhanced parsing capabilities and additional options for output customization.
- **Usage**: Suitable for more complex R scripts where detailed documentation is required.
- **Functionality**:
  - Parses Roxygen comments with greater depth, accommodating for descriptions spanning multiple lines.
  - Incorporates functionality to handle edge cases and possible parsing issues.
  - Provides options for including titles in the output and customizing the header level of function names in the markdown output.
  - Outputs a detailed markdown document with a list of functions, their titles, and descriptions.
  - Verifies the completeness of function extraction against the actual functions defined in the script.
  - Outputs an assertion to confirm the existence of the generated documentation file.

Both functions emphasize ease of use and automation in documentation generation, making them valuable tools for R developers looking to maintain clear and consistent documentation for their code. The script facilitates a more efficient workflow, especially beneficial in agile development environments where code changes are frequent, and documentation needs to be kept up-to-date with minimal effort.

The functions are designed to be run on R scripts with properly formatted Roxygen comments. Proper use of these tools can significantly reduce the manual effort involved in maintaining accurate and comprehensive documentation for R packages.



------------------------------

# 2. DependencyTools

`DependencyTools`, is the first fully working part of the R package `PackageTools` and it is designed to analyze and visualize **function-level dependencies** within and **across R packages**. It provides a suite of tools for extracting, filtering, and examining the intricate network of function calls, making it easier for developers and analysts to understand and manage code dependencies.

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





------------------------------

# 3. RoxygenTools
`Currenly limited in scope`
## Goal: Adding @importFrom Statements to Roxygen

This R package provides functionality to automatically add `@importFrom` statements to the Roxygen documentation of functions in an R script. It scans for functions called using the `::` operator within function bodies and appends the corresponding `@importFrom` directives to the Roxygen blocks above the function definitions.


## Usage

To use the package, simply call the `add_importFrom_statements` function with the path to your R script. You can also specify a suffix for the `@importFrom` statements and a list of packages to exclude from processing.

`warning: add_importFrom_statements() overwrites your file, with the @importFrom statements added`

```R
# Example script path
script_path <- '~/path/to/your/my_functions.R'

# Exclude specific packages (optional) - not to add them to 
exclude_packages <- c("MarkdownReports")

# Add @importFrom statements
add_importFrom_statements(script_path, suffix = "ADDED_BY_add_importFrom_statements", exclude_packages = exclude_packages)

# Now open the file (ideally it's under git) and check each line containing the suffix.
```

After running this function, your R script will have `@importFrom` statements and `suffix` added to the Roxygen documentation of functions that use other packages' functions with the `::` operator.

They are not at the correct place, but easy to fix.



---



# 4. ReplacementTools.R

`Replace T and F with TRUE and FALSE in R Scripts.` This script contains a utility function for R scripts that **<u>safely</u>** replaces shorthand boolean representations (`T` and `F`) with their full representations (`TRUE` and `FALSE`). 

**Why Use This Function?** R allows `T` and `F` as shorthand for `TRUE` and `FALSE`. However, it's considered best practice to use `TRUE` and `FALSE` for clarity and to avoid accidental errors. The `T` and `F` are just regular objects in R that can be overwritten, which can lead to bugs if they are redefined unintentionally. This function helps in refactoring existing R scripts to replace these shorthands with their full representations.

## Features

- Safely replaces `T` and `F` with `TRUE` and `FALSE` in R scripts. **Still you have to manually check the results**!
- Offers a `strict_mode` to replace `T` and `F` only *when they are surrounded by specific characters or at the end of a line* (recommended, default).
- Customizable for different contexts through user-defined preceding and following characters.
- Useful for cleaning up and standardizing R scripts.

## Function Usage

### `replace_tf_with_true_false`

Replaces all instances of `T` and `F` in an R script with `TRUE` and `FALSE`, respectively.

#### Arguments

- `file_path`: Path to the R script file.
- `output_path`: Path where the modified script will be saved. Defaults to `file_path`.
- `strict_mode`: Logical; if `TRUE`, only replaces `T` and `F` that are surrounded by specified characters. Default is `TRUE`.
- `preceding_chars`: Characters that can precede `T` or `F`. Default is `"\\s"`.
- `following_chars`: Characters that can follow `T` or `F`. Default is `c(",", "\\)", "\\]")`.

#### Example

```R
replace_tf_with_true_false("path/to/original_script.R", "path/to/modified_script.R")
```





---------

# 5. DocumentationTools

This utility streamlines the process of setting up and maintaining R packages. It sources `configuration` details from a specified file and performs tasks like setting up package structure, backing up files, and updating citation information.

## Usage

To use this utility, place a `config.R` file in the `Development` subdirectory of your package's directory. Then call the `create_package` function with the path to your package.

### Creating a Package

```R
create_package("~/GitHub/Packages/YourPackageName", "config.R", update_citation = FALSE)
```

>  Set `update_citation` to `TRUE` if you have & want to update the CITATION file.



### Configuring Your Package

Create a `config.R` file with the following structure:

```R
config <- list(
  package.name = "YourPackageName",
  title = "Package Title",
  description = "Description of your package",
  version = "1.0.0",
  author.given = "YourFirstName",
  author.family = "YourLastName",
  author.email = "youremail@example.com",
  license = "LicenseType",
  depends = "Dependencies",
  imports = "Imports",
  bugReports = "https://github.com/yourusername/YourPackageName/issues"
)
```


# 6. PackageSetupTools
[PackageSetupTools Wiki](../../wiki/PackageSetupTools) 






---------------------------------------------------------------------------------------------------------

# List of Functions

## List of Functions in PackageTools.R (6) 

Updated: 2024/01/25 15:22

- #### 1 `  function_lines()`

  Parse Roxygen Comments. Extracts and summarizes Roxygen documentation comments from a specified R script file.  This function reads an R script, identifies Roxygen comments for function titles and descriptions,  and writes a summary to an output file. 

- #### 2 `        "functions are defined (as `()`

  Parse Roxygen Comments from R Script. This function parses a given R script for Roxygen comments, extracts function titles and descriptions,               and writes a summary to an output markdown file. The output file can have a custom name, and the               function allows specifying the header level for functions in the markdown file. 

- #### 3 `all_funs()`

  List All Functions in a Package. Lists all function names available in a specified R package. It excludes certain  internal objects and functions that are not intended for end users. 

- #### 4 `checkGlobalVarsInPackage()`

  Check for Global Variables in Package Functions.   `checkGlobalVarsInPackage` iterates over all functions in a specified package  and checks each function for the usage of global variables using `checkGlobalVars`. 

- #### 5 `checkGlobalVars()`

  Check for Use of Global Variables in a Function. This function checks whether the specified function (`f`) uses any global variables.  It returns `TRUE` if no global variables are used, and `FALSE` otherwise. If global variables are found  and `silent` is `FALSE`, a warning is issued listing the global variables. 

- #### 6 `source_file_stats_analyzer()`

  Analyze File for Code and Comment Statistics. This function analyzes a given file, counting the number of lines of code and comments.  It also identifies files that are sourced within the provided file. The function uses regular  expressions to differentiate between code and comment lines and to extract the names of sourced files. 



## List of Functions in RoxygenTools.R (3) 

Updated: 2024/01/25 15:22

- #### 1 `add_importFrom_statements()`

  Main Function to Process R Script for Package Calls. Reads an R script file, processes its content to find and add `@importFrom` statements  for package function calls in function bodies. The statements are added to the Roxygen documentation  blocks of the functions. Default: Excludes "MarkdownReports" from processing.

- #### 2 `get_function_bodies()`

  Extract Function Bodies from R Script. This function identifies the start and end lines of each function body in an R script.  Each function body is extracted for further processing. Default: Extracts function bodies from  a given R script content.

- #### 3 `find_package_calls()`

  Finding `::` Usage within Function Bodies. Searches for package function calls using the `::` operator within the given content of  function bodies, excluding specified packages. Default: Searches for `::` usage, excluding packages  listed in `exclude_packages`.



## List of Functions in ReplacementTools.R (5) 

Updated: 2024/01/25 15:22

- #### 1 `replace_tf_with_true_false()`

  Replace T and F with TRUE and FALSE in R Scripts. This function reads an R script, safely replaces all instances of `T` with `TRUE`  and `F` with `FALSE`, under specific conditions, and writes the modified script back to a file. 

- #### 2 `replace_short_calls()`

  Replace Short Function Calls with Full Names in an R Script. Reads an R script file and replaces instances of `l(` with `length(` and `p0` with `paste0(`.  It supports a strict mode to ensure accurate replacements. 

- #### 3 `replace_l_with_length()`

  Replace l() with length() in an R Script. This function reads an R script file and replaces instances of `l(` with `length(`.  It supports a strict mode to ensure accurate replacement. 

- #### 4 `.safely_replace_tf()`

  Safely Replace T and F in a Line of R Script. This helper function replaces instances of `T` and `F` in a single line of R  script based on the specified mode and character constraints. 

- #### 5 `.safely_replace_calls()`

  Safely Replace Short Function Calls in a Line of R Script. Safely replaces instances of `l(` with `length(` and `p0` with `paste0(` in a given line of R script.  Operates in strict mode to ensure that replacements are made only when not part of a larger word or variable name. 

## List of Functions in DocumentationTools.R (3) 

Updated: 2024/01/25 15:22

- #### 1 `document_and_create_package()`

  Create R Package from Configuration. Automate the creation of an R package from a configuration file.  This function automates the creation of an R package by sourcinÏg a configuration file  from the specified package directory. It assumes the presence of a `config.R` file in  the `Development` subdirectory of the package. 

- #### 2 `.parse_description()`

  Parse DESCRIPTION File. Helper function to parse the DESCRIPTION file from a configuration file. 

- #### 3 `.update_citation_file()`

  Helper function to update the CITATION file of a package.. Update the CITATION.cff file of a package based on its version. 



## List of Functions in DependencyTools.R (7) 

Updated: 2024/01/25 15:22

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

- #### 7 `  format_node()`

  Count and print the number of functions results. Private function to count and print the number of functions in results ("dependencies"). 

## List of Functions in Miscellaneous.R (2) 

Updated: 2024/01/25 15:22

- #### 1 `# from()`

  Check Script Environment. Checks if all functions and variables called in a script are found in a specified environment.               Optionally replaces missing function calls in the script with their fully qualified names. 

- #### 2 `copy_github_badge()`

  .   This function copies the Markdown code for a GitHub badge to the clipboard based on the  specified status. It supports four statuses: 'experimental', 'active', 'archive', and  'hibernate'. 



# Function relationships
 > (of connected functions)

 ```mermaid
 flowchart LR 
  
subgraph DependencyTools.R

    map_functions_to_packages(map_functions_to_packages) --> get_package_functions(get_package_functions)
    analyze_package_dependencies(analyze_package_dependencies) --> map_functions_to_packages(map_functions_to_packages)
    analyze_package_dependencies(analyze_package_dependencies) --> get_package_functions(get_package_functions)
    analyze_package_dependencies(analyze_package_dependencies) --> analyze_function_dependencies(analyze_function_dependencies)
  
    filter_dependencies(filter_dependencies) --> .count_and_print_function_summary(.count_and_print_function_summary)
    filter_conflicts(filter_conflicts) --> .count_and_print_function_summary(.count_and_print_function_summary)

end

subgraph DocumentationTools.R
    document_and_create_package(document_and_create_package) --> .update_citation_file(.update_citation_file)
    document_and_create_package(document_and_create_package) --> .parse_description(.parse_description)
end


subgraph ReplacementTools.R
    replace_tf_with_true_false(replace_tf_with_true_false) --> .safely_replace_tf(.safely_replace_tf)
end

subgraph RoxygenTools.R
add_import_from(add_import_from) --> find_package_calls(find_package_calls)
  add_importFrom_statements(add_importFrom_statements) --> get_function_bodies(get_function_bodies)
  add_importFrom_statements(add_importFrom_statements) --> add_import_from(add_import_from)
end
 ```
 *created by `convert_igraph_to_mermaid()`*

# Contributing

Contributions to DependencyTools are welcome, please communicate via issues.

---



