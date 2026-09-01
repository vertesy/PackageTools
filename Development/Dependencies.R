Dependency file generated on Tue Aug 25 17:00:28 2026 

#################################################################################################### 
DependencyTools.R
#################################################################################################### 
$`c("package:MarkdownReports", "package:stats")`
[1] "setNames"

$`c("package:PackageTools", "package:MarkdownReports", "package:clipr")`
[1] "write_clip"

$`character(0)`
[1] "format_node"

$`package:base`
 [1] "asNamespace"  "body"         "c"            "cat"          "character"    "deparse"     
 [7] "dir.exists"   "file.exists"  "file.path"    "Filter"       "get"          "gregexpr"    
[13] "grep"         "gsub"         "inherits"     "is.character" "is.function"  "is.list"     
[19] "is.logical"   "is.null"      "isFALSE"      "lapply"       "length"       "list"        
[25] "list.files"   "ls"           "message"      "names"        "nchar"        "nrow"        
[31] "paste"        "paste0"       "print"        "regmatches"   "require"      "return"      
[37] "sapply"       "setdiff"      "sprintf"      "stop"         "stopifnot"    "tryCatch"    
[43] "unique"       "unlist"      

$`package:PackageTools`
[1] ".count_and_print_function_summary" "analyze_function_dependencies"    
[3] "dependencies"                      "get_package_functions"            
[5] "get.edgelist"                      "getActiveProject"                 
[7] "map_functions_to_packages"        

$`package:utils`
[1] "browseURL"          "installed.packages"

c("MarkdownReports", "stats")
c("PackageTools", "MarkdownReports", "clipr")
character(0)
base
PackageTools
utils
#################################################################################################### 
DocumentationTools.R
#################################################################################################### 
$`c("package:PackageTools", "package:MarkdownReports", "package:clipr")`
[1] "write_clip"

$`package:base`
 [1] "basename"     "c"            "cat"          "date"         "dir.create"   "dir.exists"  
 [7] "exists"       "file.copy"    "file.exists"  "file.path"    "file.remove"  "gsub"        
[13] "is.character" "is.null"      "list"         "list.files"   "message"      "names"       
[19] "paste"        "paste0"       "print"        "rep"          "require"      "sapply"      
[25] "setwd"        "sink"         "source"       "stop"         "stopifnot"    "Sys.time"    
[31] "warning"      "warnings"     "which"        "write"       

$`package:PackageTools`
[1] ".parse_description"     ".update_citation_file"  "create"                
[4] "create_package"         "document"               "gsub_file"             
[7] "isAvailable"            "list.functions.in.file"

c("PackageTools", "MarkdownReports", "clipr")
base
PackageTools
#################################################################################################### 
Miscellaneous.R
#################################################################################################### 
$`c("package:MarkdownReports", "package:stats")`
[1] "na.omit"

$`c("package:PackageTools", "package:MarkdownReports", "package:clipr")`
[1] "write_clip"

$`character(0)`
[1] ".importPackageFunctions"

$`package:base`
 [1] "all.names"        "as.character"     "asNamespace"      "baseenv"         
 [5] "c"                "exists"           "file.exists"      "file.path"       
 [9] "Filter"           "get"              "gsub"             "is.character"    
[13] "is.na"            "is.null"          "length"           "list"            
[17] "list2env"         "message"          "mget"             "names"           
[21] "Negate"           "new.env"          "parse"            "paste"           
[25] "paste0"           "readLines"        "require"          "requireNamespace"
[29] "return"           "sapply"           "setdiff"          "shQuote"         
[33] "stop"             "stopifnot"        "Sys.info"         "system"          
[37] "tryCatch"         "typeof"           "union"            "warning"         

$`package:PackageTools`
[1] "getActiveProject"

$`package:utils`
[1] "find"

c("MarkdownReports", "stats")
c("PackageTools", "MarkdownReports", "clipr")
character(0)
base
PackageTools
utils
#################################################################################################### 
PackageSetupTools.R
#################################################################################################### 
$`package:base`
 [1] "dir.create"   "dir.exists"   "file.copy"    "file.path"    "file.rename"  "grepl"       
 [7] "gsub"         "is.character" "list.files"   "paste"        "print"        "readLines"   
[13] "stopifnot"    "warning"      "writeLines"  

base
#################################################################################################### 
PackageTools.R
#################################################################################################### 
$`c("package:MarkdownReports", "package:Stringendo")`
[1] "iprint"

$`package:base`
 [1] "any"              "as.environment"   "basename"         "c"               
 [5] "cat"              "character"        "close"            "diff"            
 [9] "dirname"          "exists"           "file"             "file.exists"     
[13] "file.path"        "format"           "getNamespace"     "grep"            
[17] "grepl"            "gsub"             "invisible"        "is.character"    
[21] "is.function"      "is.list"          "is.logical"       "is.na"           
[25] "is.null"          "is.numeric"       "lapply"           "length"          
[29] "list"             "logical"          "ls"               "match"           
[33] "message"          "min"              "names"            "nchar"           
[37] "nzchar"           "paste"            "paste0"           "print"           
[41] "readLines"        "regexec"          "regmatches"       "requireNamespace"
[45] "return"           "seq"              "seq_along"        "setdiff"         
[49] "source"           "stop"             "stopifnot"        "sub"             
[53] "sum"              "Sys.time"         "system"           "try"             
[57] "unique"           "unlist"           "vapply"           "warning"         
[61] "which"           

$`package:PackageTools`
[1] ".convertFilePathToOutput"     ".get_description_from_config" "all_funs"                    
[4] "checkGlobalVars"              "findGlobals"                 

$`package:utils`
[1] "tail"

c("MarkdownReports", "Stringendo")
base
PackageTools
utils
#################################################################################################### 
ReplacementTools.R
#################################################################################################### 
$`package:base`
 [1] "c"            "file.copy"    "file.exists"  "grepl"        "gsub"         "invisible"   
 [7] "is.character" "is.logical"   "length"       "message"      "paste0"       "readLines"   
[13] "return"       "sapply"       "stopifnot"    "sum"          "warning"      "writeLines"  

base
#################################################################################################### 
RoxygenTools.R
#################################################################################################### 
$`package:base`
 [1] "append"       "c"            "file.exists"  "grep"         "grepl"        "is.character"
 [7] "is.list"      "lapply"       "length"       "list"         "max"          "paste"       
[13] "paste0"       "readLines"    "regexpr"      "regmatches"   "return"       "stop"        
[19] "stopifnot"    "strsplit"     "unique"       "writeLines"  

$`package:PackageTools`
[1] "add_import_from"     "find_package_calls"  "get_function_bodies"

base
PackageTools
