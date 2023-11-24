######################################################################
# Usage.of.PackageTools.R
#####################################################################
# source('~/GitHub/Projects')
# stop(); rm(list = ls(all.names = TRUE)); try(dev.off(), silent = T); gc()

# Functions ------------------------------------------------
# source('~/GitHub/Packages/Rocinante/R/Rocinante.R')


# _____________________________________________________________________________________________ ----
## Function List from Roxygen ---------------------------------------------------------------------------




parse_roxygen("~/GitHub/Packages/CodeAndRoll2/R/CodeAndRoll2.R"
, write_title_field = F)


parse_roxygen_simple("~/GitHub/Packages/CodeAndRoll2/R/CodeAndRoll2.R")

scripts = c('~/GitHub/Packages/Connectome.tools/R/Connectome.tools.R',
            '~/GitHub/Packages/Connectome.tools/R/Connectome.tools.AAV.R',
            '~/GitHub/Packages/UVI.tools/R/UVI.tools.R',
            '~/GitHub/Packages/UVI.tools/R/UVI.tools.iGraph.R',
            '~/GitHub/Packages/UVI.tools/R/UVI.tools.Bulk.R',
            '~/GitHub/Packages/UVI.tools/R/UVI.tools.less.used.R',
            '~/GitHub/Packages/ggExpress/R/ggExpress.R',
            '~/GitHub/Packages/FLIPR.tools/R/FLIPR.tools.R',
            '~/GitHub/Packages/Seurat.utils/R/Seurat.Utils.R',
            '~/GitHub/Packages/Seurat.utils/R/Seurat.Utils.Metadata.R',
            '~/GitHub/Packages/DatabaseLinke.R/R/DatabaseLinke.R',
            '~/GitHub/Packages/ReadWriter/R/ReadWriter.R',
            '~/GitHub/Packages/MarkdownHelpers/R/MarkdownHelpers.R',
            '~/GitHub/Packages/Stringendo/R/Stringendo.R',
            '~/GitHub/Packages/MarkdownReports/R/MarkdownReports.R')


for (i in 1:length(scripts)) {
  parse_roxygen(file = scripts[i]
                , output_file = paste0("~/Downloads/List.of.Functions.",basename(scripts[i]),".md")
                , write_title_field = T)
}
