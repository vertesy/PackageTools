# Other use cases for the PackageTools package
# file.edit("~/GitHub/Packages/PackageTools/Development/_other.use.cases.R")



{
  # Define the base directory
  base_dir <- "~/GitHub/Packages"

  # Search pattern for the file
  file_pattern <- "Create_the_.*_Package\\.R"

  # Find all matching files in the Development subfolders
  matching_files <- list.files(path = base_dir, pattern = file_pattern,
                               recursive = TRUE, full.names = TRUE)

  # Filter to include only files inside /Development/ subfolders
  matching_files <- matching_files[grepl("/Development/", matching_files)]

  # Output matching file paths
  message2(paste0('PackageTools::replace_a_string_in_a_file(file_path = "', matching_files, '", from = "full.names = T", to = "full.names = T, pattern = .R$)")'))
  # message2(paste0('file.edit("', matching_files, '")'))
}
r$PackageTools()

PackageTools::replace_a_string_in_a_file(file_path = "/users/abel.vertesy/GitHub/Packages/CodeAndRoll2/Development/Create_the_CodeAndRoll2_Package.R", from = "full.names = T", to = "full.names = T, pattern = .R$)")
PackageTools::replace_a_string_in_a_file(file_path = "/users/abel.vertesy/GitHub/Packages/Connectome.tools/Development/Create_the_Connectome.tools_Package.R", from = "full.names = T", to = "full.names = T, pattern = .R$)")
PackageTools::replace_a_string_in_a_file(file_path = "/users/abel.vertesy/GitHub/Packages/DatabaseLinke.R/Development/Create_the_DatabaseLinke.R_Package.R", from = "full.names = T", to = "full.names = T, pattern = .R$)")
PackageTools::replace_a_string_in_a_file(file_path = "/users/abel.vertesy/GitHub/Packages/DataInCode/Development/Create_the_DataInCode_Package.R", from = "full.names = T", to = "full.names = T, pattern = .R$)")
PackageTools::replace_a_string_in_a_file(file_path = "/users/abel.vertesy/GitHub/Packages/FLIPR.tools/Development/Create_the_FLIPR.tools_Package.R", from = "full.names = T", to = "full.names = T, pattern = .R$)")
PackageTools::replace_a_string_in_a_file(file_path = "/users/abel.vertesy/GitHub/Packages/ggExpress/Development/Create_the_ggExpress_Package.R", from = "full.names = T", to = "full.names = T, pattern = .R$)")
PackageTools::replace_a_string_in_a_file(file_path = "/users/abel.vertesy/GitHub/Packages/isoENV/Development/Create_the_isoENV_Package.R", from = "full.names = T", to = "full.names = T, pattern = .R$)")
PackageTools::replace_a_string_in_a_file(file_path = "/users/abel.vertesy/GitHub/Packages/MarkdownHelpers/Development/Create_the_MarkdownHelpers_Package.R", from = "full.names = T", to = "full.names = T, pattern = .R$)")
PackageTools::replace_a_string_in_a_file(file_path = "/users/abel.vertesy/GitHub/Packages/MarkdownReports/Development/Create_the_MarkdownReports_Package.R", from = "full.names = T", to = "full.names = T, pattern = .R$)")
PackageTools::replace_a_string_in_a_file(file_path = "/users/abel.vertesy/GitHub/Packages/PackageTools/Development/Create_the_PackageTools_Package.R", from = "full.names = T", to = "full.names = T, pattern = .R$)")
PackageTools::replace_a_string_in_a_file(file_path = "/users/abel.vertesy/GitHub/Packages/PackageTools/Templates/Development/Create_the_PackageTools_Package.R", from = "full.names = T", to = "full.names = T, pattern = .R$)")
PackageTools::replace_a_string_in_a_file(file_path = "/users/abel.vertesy/GitHub/Packages/ReadWriter/Development/Create_the_ReadWriter_Package.R", from = "full.names = T", to = "full.names = T, pattern = .R$)")
PackageTools::replace_a_string_in_a_file(file_path = "/users/abel.vertesy/GitHub/Packages/Rocinante/Development/Create_the_Rocinante_Package.R", from = "full.names = T", to = "full.names = T, pattern = .R$)")
PackageTools::replace_a_string_in_a_file(file_path = "/users/abel.vertesy/GitHub/Packages/SCP.tools/Development/Create_the_SCP_Package.R", from = "full.names = T", to = "full.names = T, pattern = .R$)")
PackageTools::replace_a_string_in_a_file(file_path = "/users/abel.vertesy/GitHub/Packages/Seurat.utils/Development/Create_the_Seurat.utils_Package.R", from = "full.names = T", to = "full.names = T, pattern = .R$)")
PackageTools::replace_a_string_in_a_file(file_path = "/users/abel.vertesy/GitHub/Packages/Stringendo/Development/Create_the_Stringendo_Package.R", from = "full.names = T", to = "full.names = T, pattern = .R$)")
PackageTools::replace_a_string_in_a_file(file_path = "/users/abel.vertesy/GitHub/Packages/UVI.tools/Development/Create_the_UVI.tools_Package.R", from = "full.names = T", to = "full.names = T, pattern = .R$)")
>
