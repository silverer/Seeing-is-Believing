setwd("~/Documents/Seeing-is-Believing/")
#Data paths for input and output
source('src/data_io.R')
library(pacman)
p_load(dplyr, psych, tableone, apaTables, Hmisc,stringr)

outcomes <- c('z.lead.all', 'z.interest.all',
              'z.dd.belonging.all', 
              'z.dd.id.all')
unstd_outcomes <- str_replace(outcomes, "z[.]", "")
sib <- read.csv(paste0(experiment_data,"/outcomes_experimental_data_clean.csv"))

#Get raw correlations, means, SDs
pretty.varnames <- c(lead.all = 'STEM career interest',
                     interest.all = 'General STEM interest',
                     dd.belonging.all = 'STEM belonging',
                     dd.id.all = 'Identification with STEM')

col.recode <- names(pretty.varnames)
names(col.recode) <- pretty.varnames
tmp <- sib %>% 
  dplyr::select(all_of(unstd_outcomes)) %>% 
  rename(all_of(col.recode))
apa.cor.table(tmp, filename = 'output/pearson_correlations.doc',
              table.number = 2)

#Sets whether or not to generate plots of distributions 
#Switch to FALSE when calling from .Rmd file
VIEW.DISTS <- FALSE

if(VIEW.DISTS){
  #Just take a quick peek at the distributions of these variables
  #This will queue the plots up so you can flip through them
  for(o in outcomes){
    qqnorm(sib[[o]], main = pretty.varnames[[o]])
    qqline(sib[[o]], main = pretty.varnames[[o]])
    
    hist(sib[[o]], main = pretty.varnames[[o]], 
         xlab = pretty.varnames[[o]])
  }
}

#Get z-scored correlations, means, SD's
tmp.sib <- sib %>% 
  dplyr::rename(`Article topic` = article.cond,
                `Scientist gender` = gender.cond,
                `STEM Career Interest`=z.lead.all,
                `General STEM Interest` = z.interest.all,
                `STEM Belonging` = z.dd.belonging.all,
                `Image condition` = image.cond,
                `Identification with STEM` = z.dd.id.all,
                `Participant gender` = participant.gender)
vnames <- c(z.lead.all = 'STEM Career Interest',
            z.interest.all = 'General STEM Interest',
            z.dd.belonging.all = 'STEM Belonging',
            z.dd.id.all = 'Identification with STEM')

tabs <- CreateContTable(vars = vnames[names(vnames)], 
                        strata = c("Image condition", "Scientist gender", "Participant gender"),
                        data = tmp.sib, test = F)
write.csv(print(tabs, digits = 2), "output/means_by_cell.csv")
