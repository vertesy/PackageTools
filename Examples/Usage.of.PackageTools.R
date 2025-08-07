######################################################################
# Usage.of.PackageTools.R
#####################################################################
# source('~/GitHub/Packages/PackageTools/Examples/Usage.of.PackageTools.R')
# stop(); rm(list = ls(all.names = TRUE)); try(dev.off(), silent = T); gc()

# Functions ------------------------------------------------
# source('~/GitHub/Packages/Rocinante/R/Rocinante.R')


# _____________________________________________________________________________________________ ----
## Function List from Roxygen ---------------------------------------------------------------------------




list_of_funs_to_markdown('~/GitHub/Packages/PackageTools/R/DependencyTools.R')


list_of_funs_to_markdown_simple("~/GitHub/Packages/CodeAndRoll2/R/CodeAndRoll2.R")

scripts = c(
  '~/GitHub/Packages/Stringendo/R/Stringendo.R',
  '~/GitHub/Packages/ReadWriter/R/ReadWriter.R',
  '~/GitHub/Packages/CodeAndRoll2/R/CodeAndRoll2.R',

  '~/GitHub/Packages/MarkdownHelpers/R/MarkdownHelpers.R',
  '~/GitHub/Packages/MarkdownReports/R/MarkdownReports.R',
  '~/GitHub/Packages/ggExpress/R/ggExpress.R',

  '~/GitHub/Packages/isoENV/R/isoENV.R',
  '~/GitHub/Packages/PackageTools/R/PackageTools.R',

  '~/GitHub/Packages/Seurat.utils/R/Seurat.Utils.R',
  '~/GitHub/Packages/Seurat.utils/R/Seurat.Utils.Metadata.R',

  '~/GitHub/Packages/Connectome.tools/R/Connectome.tools.R',
  '~/GitHub/Packages/Connectome.tools/R/Connectome.tools.AAV.R',
  '~/GitHub/Packages/UVI.tools/R/UVI.tools.R',
  '~/GitHub/Packages/UVI.tools/R/UVI.tools.Bulk.R',
  '~/GitHub/Packages/FLIPR.tools/R/FLIPR.tools.R',
  # '~/GitHub/Packages/UVI.tools/R/UVI.tools.iGraph.R',
  # '~/GitHub/Packages/UVI.tools/R/UVI.tools.less.used.R',
  # '~/GitHub/Packages/DatabaseLinke.R/R/DatabaseLinke.R',

  '~/GitHub/Packages/SCP.tools/R/SCP.functions.R',
  ""
)


for (i in 1:length(scripts)) {
  list_of_funs_to_markdown(file = scripts[i], write_title_field = T)
  list_of_funs_to_markdown_simple(file = scripts[i])
}
