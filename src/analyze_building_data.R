setwd("~/Documents/Seeing-is-Believing/")
#Data paths for input and output
source('src/data_io.R')
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(stats, dplyr)

building_codes <- read.csv(paste0(building_data, "/building_recode.csv"))

df <- read.csv(paste0(building_data, '/cleaned_building_data_parsed.csv'))

#### Assign STEM vs. non-STEM buildings ####
recode_list <- building_codes$stem
names(recode_list) <- building_codes$code
df$building_code <- factor(df$building_code)
df <- df %>% 
  mutate(stem_building = recode_factor(building_code, !!!recode_list),
         stem_building = recode_factor(stem_building, 
                                       `1` = "True", `2` = "False"))

#### Create tallies for genders ####
#Count the number of women pictured for each image
tmp <- df %>% 
  dplyr::select(starts_with('gender_'))
tmp[tmp!=1] <- 0 #Give cells with a man (code = 2) a 0 to create tallies
df$total_women_counts <- rowSums(tmp, na.rm=TRUE)

#Count the number of men pictured for each image
tmp <- df %>% 
  dplyr::select(starts_with('gender_'))
tmp[tmp!=2] <- 0
tmp[tmp==2] <- 1 #Reassign the "2" code (men) with a 1 to create tallies
df$total_men_counts <- rowSums(tmp, na.rm=TRUE)
#Count the number of people w/ambiguous genders for each image
tmp <- df %>% 
  dplyr::select(starts_with('gender_'))
tmp[tmp!=0] <- NA
tmp[tmp==0] <- 1 #Reassign the "0" (ambiguous) to 1 to create tallies
df$total_ambig_counts <- rowSums(tmp, na.rm=TRUE)

#Create new count variables. If there's anything in total_men/women/etc, use that, 
#otherwise use tallies generated above
#this is because some images with many many people in them had the complete tally 
#(not enough columns to count everyone in their own cell)
df <- df %>% 
  mutate(men_count_final = ifelse(is.na(total_men), total_men_counts,
                                  total_men),
         women_count_final = ifelse(is.na(total_women), total_women_counts,
                                    total_women),
         ambig_count_final = ifelse(is.na(total_ambiguous), total_ambig_counts,
                                    total_ambiguous),
         prop_women = women_count_final/(men_count_final+ambig_count_final+women_count_final),
         prop_men = men_count_final/(men_count_final+ambig_count_final+women_count_final),
         no_men = ifelse(men_count_final==0 | is.na(men_count_final),
                         'NO MEN', 'SOME MEN'),
         no_women = ifelse(women_count_final==0 | is.na(women_count_final),
                         'NO WOMEN', 'SOME WOMEN'),
         is_stem = ifelse(stem_building=='True',
                          'STEM buildings', 'Non-STEM buidlings'))

#### Get counts of photos and buildings ####
unique_buildings = df[!duplicated(df$building_code),]
unique_buildings %>% 
  group_by(is_stem) %>% 
  count()

unique_images = df[!duplicated(df$clean_img_name),]
unique_images %>% 
  group_by(is_stem) %>% 
  count()

#Get sum of men and women overall
sum(df$men_count_final, na.rm=T)
sum(df$women_count_final, na.rm=T)
sum(df$ambig_count_final, na.rm=T)
#Get ratios of men:women overall
sum(df$men_count_final)/sum(df$women_count_final)

#### STEM buildings ####
stem <- df %>% 
  dplyr::filter(stem_building == 'True')
#Get counts of men and women in non-STEM buildings
sum(stem$men_count_final,na.rm=T)
sum(stem$women_count_final,na.rm=T)
#Get ratio of men:women in STEM buildings
sum(stem$men_count_final)/sum(stem$women_count_final)

#### Non-STEM buildings ####
non_stem <- df %>% 
  dplyr::filter(stem_building == 'False')
#Get counts of men and women in non-STEM buildings
sum(non_stem$men_count_final,na.rm=T)
sum(non_stem$women_count_final,na.rm=T)
#Get ratio of men:women in non-STEM buildings
sum(non_stem$men_count_final)/sum(non_stem$women_count_final)

grouped <- df %>% 
  dplyr::group_by(is_stem) %>% 
  dplyr::summarise(`Men pictured` = sum(men_count_final), 
            `Women pictured` = sum(women_count_final))

grouped <- as.data.frame(grouped)
rownames(grouped) <- grouped$is_stem
grouped <- grouped[,2:3]
res <- chisq.test(grouped)
res
res$observed
res$expected


