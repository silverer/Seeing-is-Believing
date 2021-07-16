setwd("~/Documents/Seeing-is-Believing/")
#Data paths for input and output
source('src/data_io.R')
#source the file below to read in the dataset
source('src/zscore_descriptive_stats.R')

if (!require("pacman")) install.packages("pacman"); library(pacman)
devtools::install_github("silverer/statstring")
p_load(stats, psych, dplyr,
       apaTables, statstring)


### 3-way AOVs pretty output
###NOTE: tmp.sib is created by calling source("src/zscore_descriptive_stats.R")
aov_results <- list()
res <- lm(`STEM Career Interest`~`Participant gender`*`Image condition`*`Scientist gender`,
          data = tmp.sib)
tmp <- apa.aov.table(res)$table_body
tmp["stat_string"] <- format_anova_string(apa.aov.table(res), get.all=T)

tmp$stat_string[1] <- ""
aov_results[['z.lead.all']] <- tmp

apa.aov.table(res, filename = paste0(output, "/STEM Career Interest 3 way.doc"))

res <- lm(`General STEM Interest`~`Participant gender`*`Image condition`*`Scientist gender`,
          data = tmp.sib)
tmp <- apa.aov.table(res)$table_body
tmp["stat_string"] <- format_anova_string(apa.aov.table(res), get.all=T)
tmp$stat_string[1] <- ""
tmp$stat_string[nrow(tmp)] <- ""
aov_results[['z.interest.all']] <- tmp
apa.aov.table(res, filename = paste0(output, "/General STEM Interest 3 way.doc"))

res <- lm(`STEM Belonging`~`Participant gender`*`Image condition`*`Scientist gender`,
          data = tmp.sib)
tmp <- apa.aov.table(res)$table_body
tmp["stat_string"] <- format_anova_string(apa.aov.table(res), get.all=T)
tmp$stat_string[1] <- ""
tmp$stat_string[nrow(tmp)] <- ""
aov_results[['z.dd.belonging.all']] <- tmp
apa.aov.table(res, filename = paste0(output, "/STEM Belonging 3 way.doc"))

res <- lm(`Identification with STEM`~`Participant gender`*`Image condition`*`Scientist gender`,
          data = tmp.sib)
tmp <- apa.aov.table(res)$table_body
tmp["stat_string"] <- format_anova_string(apa.aov.table(res), get.all=T)
tmp$stat_string[1] <- ""
tmp$stat_string[nrow(tmp)] <- ""
aov_results[['z.dd.id.all']] <- tmp
apa.aov.table(res, filename = paste0(output, "/Identification with STEM 3 way.doc"))


### 4-way AOVs pretty output
res <- lm(`STEM Career Interest`~`Article topic`*`Participant gender`*`Image condition`*`Scientist gender`,
          data = tmp.sib)
apa.aov.table(res, filename = paste0(output, "/STEM Career Interest 4 way.doc"))

res <- lm(`General STEM Interest`~`Article topic`*`Participant gender`*`Image condition`*`Scientist gender`,
          data = tmp.sib)
apa.aov.table(res, filename = paste0(output, "/General STEM Interest 4 way.doc"))

res <- lm(`STEM Belonging`~`Article topic`*`Participant gender`*`Image condition`*`Scientist gender`,
          data = tmp.sib)
apa.aov.table(res, filename = paste0(output, "/STEM Belonging 4 way.doc"))

res <- lm(`Identification with STEM`~`Article topic`*`Participant gender`*`Image condition`*`Scientist gender`,
          data = tmp.sib)
apa.aov.table(res, filename = paste0(output, "/Identification with STEM 4 way.doc"))


