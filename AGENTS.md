# Guidance for Codex AI Agents

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

