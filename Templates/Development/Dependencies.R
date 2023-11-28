Dependency file generated on Mon Nov 27 11:01:54 2023 

#################################################################################################### 
DependencyTools.R
#################################################################################################### 
$`character(0)`
[1] "format_node"

$`package:base`
 [1] "asNamespace"  "body"         "c"            "cat"          "character"    "deparse"     
 [7] "dir.exists"   "file.path"    "Filter"       "get"          "gregexpr"     "grep"        
[13] "gsub"         "inherits"     "is.character" "is.function"  "is.list"      "is.logical"  
[19] "is.null"      "isFALSE"      "lapply"       "length"       "list"         "list.files"  
[25] "ls"           "message"      "names"        "nchar"        "nrow"         "paste"       
[31] "paste0"       "print"        "regmatches"   "return"       "sapply"       "setdiff"     
[37] "sprintf"      "stop"         "stopifnot"    "tryCatch"     "unique"       "unlist"      

$`package:PackageTools`
[1] ".count_and_print_function_summary" "analyze_function_dependencies"    
[3] "get_package_functions"             "get.edgelist"                     
[5] "map_functions_to_packages"         "write_clip"                       

$`package:stats`
[1] "setNames"

$`package:utils`
[1] "browseURL"          "installed.packages"

character(0)
base
PackageTools
stats
utils
#################################################################################################### 
DocumentationTools.R
#################################################################################################### 
$`package:base`
 [1] "basename"     "c"            "cat"          "date"         "dir.create"   "dir.exists"  
 [7] "file.copy"    "file.exists"  "file.path"    "file.remove"  "gsub"         "is.character"
[13] "list"         "list.files"   "names"        "paste"        "paste0"       "print"       
[19] "rep"          "setwd"        "sink"         "source"       "stop"         "stopifnot"   
[25] "Sys.time"     "warnings"     "write"       

$`package:PackageTools`
[1] ".parse_description"     ".update_citation_file"  "create"                
[4] "create_package"         "document"               "gsub_file"             
[7] "isAvailable"            "list.functions.in.file" "write_clip"            

$`package:utils`
[1] "person"

base
PackageTools
utils
#################################################################################################### 
Miscellaneous.R
#################################################################################################### 
$`package:base`
[1] "c"       "paste0"  "require" "return"  "stop"   

$`package:PackageTools`
[1] "write_clip"

base
PackageTools
#################################################################################################### 
PackageTools.R
#################################################################################################### 
$`package:base`
 [1] "basename"     "c"            "cat"          "character"    "close"        "dirname"     
 [7] "file"         "file.exists"  "format"       "grep"         "grepl"        "gsub"        
[13] "is.character" "is.list"      "is.logical"   "is.na"        "is.numeric"   "lapply"      
[19] "length"       "list"         "nchar"        "nzchar"       "paste"        "paste0"      
[25] "print"        "readLines"    "regexec"      "regmatches"   "return"       "seq_along"   
[31] "stopifnot"    "sub"          "sum"          "Sys.time"     "system"       "unique"      
[37] "unlist"       "warning"     

$`package:PackageTools`
[1] ".convertFilePathToOutput"

$`package:utils`
[1] "tail"

base
PackageTools
utils
#################################################################################################### 
ReplacementTools.R
#################################################################################################### 
$`package:base`
 [1] "c"            "file.exists"  "gsub"         "invisible"    "is.character" "length"      
 [7] "paste0"       "readLines"    "return"       "sapply"       "stopifnot"    "warning"     
[13] "writeLines"  

base
#################################################################################################### 
RoxygenTools.R
#################################################################################################### 
$`package:base`
 [1] "append"       "c"            "file.exists"  "grep"         "grepl"        "is.character"
 [7] "is.list"      "lapply"       "length"       "list"         "max"          "paste"       
[13] "paste0"       "readLines"    "regexpr"      "regmatches"   "return"       "stopifnot"   
[19] "strsplit"     "unique"       "writeLines"  

$`package:PackageTools`
[1] "add_import_from"     "find_package_calls"  "get_function_bodies"

base
PackageTools
