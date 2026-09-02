# Guidance for AI Agents

Welcome to the **PackageTools** repository. This repository hosts an R package that provides utilities for R package development, focusing on documentation automation and dependency analysis.

## Repository Structure
- `R/` – Core R functions. Each file generally maps to a feature set.
- `man/` – Generated Rd documentation. Updated via `devtools::document()`.
- `Development/` – Scripts and notes used while developing the package.
- `Examples/` – Example scripts demonstrating package usage.
- `Templates/` – Boilerplate files for creating or extending packages.
- Root files such as `DESCRIPTION`, `NAMESPACE`, and `CITATION.cff` define package metadata.

## How to Work with This Codebase
1. **Documentation**: When modifying functions, keep Roxygen comments up to date and run `R -q -e 'devtools::document()'` to regenerate Rd files.
2. **Testing/Checks**: Run `R CMD check .` before committing changes. This performs a standard R package check.
3. **Style**: Follow conventional R style (e.g., tidyverse style) and ensure code is well-commented.
4. **Commits**: Use concise commit messages describing the changes.
5. **Package version**: Never update the package version unless the user explicitly requests it, and do not raise code-review findings that ask for a version bump.
6. In `/Development/MYPACKAGE/Development/Create_the_MYPACKAGE_Package.R")`, `PackageTools::document_and_create_package()` recreates an R package’s metadata and documentation from a configuration file. It runs `devtools::document()` to regenerate package documentation, including the DESCRIPTION and NAMESPACE, from roxygen annotation and `config.R`. 

### Update the Source, Not Just the Documentation

Documentations rebuilt and overwritten from upstream sources: `.Rd` files from roxygen annotations and DESCRIPTION and NAMESPACE from  `config.R` by `PackageTools::document_and_create_package()` relying on  `devtools::document()`  when I manually, regularly run `/Development/MYPACKAGE/Development/Create_the_MYPACKAGE_Package.R")`. Thus  always update the upstream sources first, then fix the downstream documentations correspondingly.

## Pull Request Descriptions
Open each PR with a few bullets per major change: what was wrong, how it was fixed, and whether it changes the function's output or behavior.
- Scale the description to the change: a typo or comment-only fix needs one short line, not a paragraph.
- Keep the whole description under 250 words; reserve that ceiling for genuinely complex PRs. If it doesn't fit, split the PR instead of writing more.

## Dependencies
The package's core functionality depends only on the CRAN packages declared in `DESCRIPTION`.
It does not require any additional `@vertesy` libraries. If you encounter a dependency, raise this problem.

## For Newcomers
- Start by reading `README.md` for an overview of available tools.
- Explore the `R/` directory to see implementations and roxygen documentation.
- The `Examples/` folder offers scripts illustrating practical usage.
- Review the `Development/` notes to understand the package’s evolution.

### Next Steps
- Learn how `DependencyTools` analyzes function-level dependencies.
- Explore generating documentation from source files with `list_of_funs_to_markdown()`.
- Investigate how this package interfaces with other `@vertesy` libraries for extended functionality.

