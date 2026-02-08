### Initialize and define some default parameters
# directories for input, output and scripts
print("Initializing default directories...")
directories <- c("data", 
                 "results", 
                 "plots", 
                 "scripts")
names(directories) <- directories
for (i in directories){
  assign(paste(i,"dir", sep = ""), i)
  if (!file.exists(i)){
  dir.create(i) 
  }
}
print("Default directories initialized.")
rm(directories)
rm(i)

print("Loading libraries...")
require(utils)
# preload needed libraries
if (file.exists("libraries.txt")){
  libraries <- unlist(read.table("libraries.txt", stringsAsFactors = FALSE))
  load.success <- lapply(libraries, require, character.only = TRUE)
  names(load.success) <- libraries
  if (all(unlist(load.success))){
    print("All libraries loaded successfully")
  } else {
    print(paste("Libraries:", paste(names(test[!test]), collapse = ","), "could not be loaded.", sep = " "))
  }
  rm(load.success)
} else {
  print("No libraries to load. If you want to load libraries at startup, create library.txt with one library name per line")
}

print("Loading R objects...")
if (file.exists("Robjects.txt")){
  Robjects <- unlist(read.table("Robjects.txt", stringsAsFactors = FALSE))
  for (i in Robjects){
    load(i)
    print(paste("loading Robject",i,sep = " "))
  }
  rm(i)
  rm(Robjects)
} else {
  print("No Robjects to load. If you want to load Robjects, create Robjects.txt containing the path and filename of one Robject per line")
}

# load functions in scriptsdir
files <- dir(scriptsdir, pattern = "function", full.names = TRUE)
for (file in files){
  source(file)
}
rm(files, file)