Dependency file generated on Sun Nov 26 22:47:53 2023 

#################################################################################################### 
DependencyTools.R
#################################################################################################### 
$`character(0)`
[1] "format_node"  "get.edgelist" "write_clip"  

$`package:base`
 [1] "asNamespace"  "body"         "c"            "cat"         
 [5] "character"    "deparse"      "Filter"       "get"         
 [9] "gregexpr"     "grep"         "gsub"         "inherits"    
[13] "is.character" "is.function"  "is.list"      "is.logical"  
[17] "is.null"      "lapply"       "length"       "list"        
[21] "ls"           "message"      "names"        "nchar"       
[25] "nrow"         "paste"        "paste0"       "print"       
[29] "regmatches"   "return"       "sapply"       "setdiff"     
[33] "sprintf"      "stopifnot"    "tryCatch"     "unique"      
[37] "unlist"      

$`package:PackageTools`
[1] ".count_and_print_function_summary" "analyze_function_dependencies"    
[3] "get_package_functions"             "map_functions_to_packages"        

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
$`c(".GlobalEnv", "package:PackageTools")`
[1] "create_package"

$`character(0)`
[1] "create"                 "document"              
[3] "gsub_file"              "isAvailable"           
[5] "list.functions.in.file" "write_clip"            

$`package:base`
 [1] "basename"     "c"            "cat"          "date"        
 [5] "dir.create"   "dir.exists"   "file.copy"    "file.exists" 
 [9] "file.path"    "file.remove"  "gsub"         "is.character"
[13] "list"         "list.files"   "names"        "paste"       
[17] "paste0"       "print"        "rep"          "setwd"       
[21] "sink"         "source"       "stop"         "stopifnot"   
[25] "Sys.time"     "warnings"     "write"       

$`package:PackageTools`
[1] ".parse_description"    ".update_citation_file"

$`package:utils`
[1] "person"

c(".GlobalEnv", "PackageTools")
character(0)
base
PackageTools
utils
#################################################################################################### 
Miscellaneous.R
#################################################################################################### 
$`character(0)`
[1] "write_clip"

$`package:base`
[1] "c"      "paste0" "stop"  

character(0)
base
#################################################################################################### 
PackageTools.R
#################################################################################################### 
$`package:base`
 [1] "basename"     "c"            "cat"          "character"   
 [5] "close"        "dirname"      "file"         "file.exists" 
 [9] "format"       "grep"         "grepl"        "gsub"        
[13] "is.character" "is.list"      "is.logical"   "is.na"       
[17] "is.numeric"   "lapply"       "length"       "list"        
[21] "nchar"        "nzchar"       "paste"        "paste0"      
[25] "print"        "readLines"    "regexec"      "regmatches"  
[29] "return"       "seq_along"    "stopifnot"    "sub"         
[33] "sum"          "Sys.time"     "system"       "unique"      
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
 [1] "c"            "file.exists"  "gsub"         "invisible"   
 [5] "is.character" "length"       "paste0"       "readLines"   
 [9] "return"       "sapply"       "stopifnot"    "warning"     
[13] "writeLines"  

base
#################################################################################################### 
RoxygenTools.R
#################################################################################################### 
$`package:base`
 [1] "append"       "c"            "file.exists"  "grep"        
 [5] "grepl"        "is.character" "is.list"      "lapply"      
 [9] "length"       "list"         "max"          "paste"       
[13] "paste0"       "readLines"    "regexpr"      "regmatches"  
[17] "return"       "stopifnot"    "strsplit"     "unique"      
[21] "writeLines"  

$`package:PackageTools`
[1] "add_import_from"     "find_package_calls"  "get_function_bodies"

base
PackageTools
