setwd("~/Documents/Seeing-is-Believing/")
#Data paths for input and output
source('src/data_io.R')
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(stats, dplyr, readxl, psych, rstatix, effsize)

df <- read.csv(paste0(article_data,'/article_data_cleaned.csv'))
#num_X_res_pics is the actual total count for an article,
#num_X_researchers_pictured is just for the first picture in an article
#delete the latter since it's confusing and meaningless
df <- df %>% 
  dplyr::select(-c(num_female_researchers_pictured,num_male_researchers_pictured,
                   num_other_researchers_pictured))

df <- df %>% 
  mutate(any_women_pic = ifelse(num_female_res_pics>0, T, F),
         any_men_pic = ifelse(num_male_res_pics>0, T, F),
         any_women_mentioned = ifelse(num_women_mentioned>0, T, F),
         any_men_mentioned = ifelse(num_men_mentioned>0, T, F),
         mf_mention_diff = num_men_mentioned - num_women_mentioned,
         mf_pic_diff = num_men_pictured - num_women_pictured,
         num_people_mentioned = num_women_mentioned+num_men_mentioned,
         prop_men = num_men_mentioned/num_people_mentioned,
         prop_women = num_women_mentioned/num_people_mentioned,
         prop_mention_diff = prop_men-prop_women,
         article_length = nchar(article_text))

#### Text representation ####
sum(df$num_people_mentioned,na.rm=T)

with(df,
     table(any_women_mentioned, any_men_mentioned))
chisq.test(df$any_men_mentioned, df$any_women_mentioned)

mentions <- df %>% 
  dplyr::summarise(`Men mentioned` = sum(num_men_mentioned), 
                   `Women mentioned` = sum(num_women_mentioned))
mentions
sum(mentions)
mentions[1]/sum(mentions)
mentions[2]/sum(mentions)
mentions[1]/mentions[2]
res <- chisq.test(mentions)
res

#### Image representation ####
pics <- df %>% 
  dplyr::summarise(`Male researchers pictured` = sum(num_male_res_pics), 
                   `Female researchers pictured` = sum(num_female_res_pics))
pics
sum(pics)
pics[1]/sum(pics)
pics[2]/sum(pics)
pics[1]/pics[2]
res <- chisq.test(pics)
res


