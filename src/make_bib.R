setwd("~/Documents/Seeing-is-Believing/")
#Data paths for input and output
source('src/data_io.R')
packs <- c("dplyr", "apaTables", "emmeans", "psych", 
              "rstatix","ggplot2","scales", "pacman","stringr")
sink(paste0(output,"package_refs.txt"))
for(p in packs){
  print(citation(p))
  cat("  \n")
}
sink()
