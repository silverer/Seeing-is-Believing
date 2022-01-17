if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(dplyr, psych,stringr)
setwd("~/Documents/Seeing-is-Believing")
#Data paths for input and output
source('src/data_io.R')
select <- dplyr::select
filter <- dplyr::filter
sib<-read.csv(paste0(experiment_data, '/experimental_de_id_data.csv'))
nrow(sib)
#Filter out garbage responses
sib <- sib[sib$DistributionChannel!="preview" &!is.na(sib$FL_15_DO),]


nrow(sib)
exclusion_tracker <- list()
exclusion_tracker['original_length'] <- nrow(sib)
#### Filter people who didn't finish, listed gender other than M/W, or were in research methods class ####

exclusion_tracker['unfinished'] <- nrow(sib %>% filter(Progress != 100))
sib["exclude_reason"] <- NA
sib$exclude_reason[sib$Progress != 100] <- "unfinished"
exclusion_tracker['in_research_methods_class'] <- nrow(sib %>% 
                                                         filter(in_research_methods_class==T))
sib$exclude_reason[sib$in_research_methods_class==T] <- "in research methods class"
# sib<-sib %>% 
#   filter(Progress == 100 & in_research_methods_class==FALSE)
# 
# nrow(sib)
sib$exclude_reason[(sib$gender > 2 |is.na(sib$gender))]<-"did not identify as women or men"
exclusion_tracker['not_m_f'] <- nrow(sib %>% filter(gender > 2 | is.na(gender)))
#sib<-sib[sib$gender==1|sib$gender==2,]
#nrow(sib)

#### Participant gender variable ####
#3 is self-describe, 6 is prefer not to say
#1 male, 2 female
sib$genderF<-as.factor(as.character(sib$gender)) 
#replace anything except MF with NA
sib$participant.gender<-as.factor(with(sib, ifelse(gender == "1", "Men",
                                              ifelse(gender == "2", "Women",
                                                    NA))))
#### Condition variables ####
sib$article.cond<-as.factor(with(sib, ifelse(FL_15_DO == "Stimuli-againstthetide", "against", 
                                             ifelse(FL_15_DO == "Stimuli-genesforage", "genes",
                                                    ifelse(FL_15_DO == "Stimuli-shellofplant", "shell",
                                                           NA))))) 

sib$image.cond<-as.factor(with(sib, ifelse(article.cond == "against", against.image,
                                           ifelse(article.cond == "genes", genes.image,
                                                  ifelse(article.cond == "shell", shell.image,
                                                         NA)))))

sib$image.3level.cond<-as.factor(with(sib, ifelse(grepl("M",image.cond), "M",
                                                  ifelse(grepl("F",image.cond), "F",
                                                         ifelse(grepl("O",image.cond), "O",
                                                                NA)))))

sib$image.2level.cond<-as.factor(with(sib, ifelse(grepl("M",image.cond), "P",
                                                  ifelse(grepl("F",image.cond), "P",
                                                         ifelse(grepl("O",image.cond), "O",
                                                                NA)))))

sib$gender.cond<-as.factor(with(sib, ifelse(article.cond == "against", against.gender,
                                            ifelse(article.cond == "genes", genes.gender,
                                                   ifelse(article.cond == "shell", shell.gender,
                                                          NA)))))

sib$cell<-as.factor(with(sib, paste(article.cond,gender.cond,image.cond), sep=""))

sib$article.image.cond <- as.factor(with(sib, ifelse(gender.cond=='F'&image.2level.cond=='P','Woman-Pic', 
                                                     ifelse(gender.cond=='F'&image.2level.cond=='O','Woman-No Pic', 
                                                            ifelse(gender.cond=='M'&image.2level.cond=='P',
                                                                   'Man-Pic', 'Man-No Pic')))))

#### Attention/manipulation checks ####
#attention
sib$stimuli.title.score<-with(sib, ifelse(article.cond == "against" & stimuli.title == 1, 1,
                                          ifelse(article.cond == "genes" & stimuli.title == 2, 1,
                                                 ifelse(article.cond == "shell" & stimuli.title == 3, 1,
                                                        0))))

#manipulation checks
sib$manip.gender.score<-with(sib, ifelse(gender.cond == "M" & manip.gender == 1, 1,
                                         ifelse(gender.cond == "F" & manip.gender == 2, 1,
                                                0)))
sib$manip.image.score<-with(sib, ifelse(grepl("M",image.cond) & manip.image == 1, 1,
                                        ifelse(grepl("F",image.cond) & manip.image == 2, 1,
                                               ifelse(grepl("O",image.cond) & article.cond == "against" & manip.image == 5, 1,
                                                      ifelse(grepl("O",image.cond) & article.cond == "genes" & manip.image == 3, 1,
                                                             ifelse(grepl("O",image.cond) & article.cond == "shell" & manip.image == 4, 1,
                                                                    0))))))

#Create pretty variables for plotting/analysis
sib <- sib %>% 
  mutate(perceived.gender = ifelse(manip.gender == 1, 'Perceived gender: Man',
                                   'Perceived gender: Woman'),
         actual.gender = ifelse(gender.cond == 'F', 'Article gender: Woman',
                                'Article gender: Man'),
         `Article gender` = gender.cond,
         `Participant gender` = participant.gender,
         failed_attn_check = ifelse(stimuli.title.score != 1 | manip.gender.score != 1 | manip.image.score != 1,
                                    TRUE, FALSE))
sib$exclude_reason[sib$failed_attn_check==T] <- "failed attention check"
sib.all <- sib
fail.table <- table(sib.all$article.image.cond,sib.all$failed_attn_check)
exclusion_tracker['failed_attn_check'] <- nrow(sib %>% filter(failed_attn_check==TRUE))
write.csv(exclusion_tracker, paste0(output, "/exclusion_tracker.csv"))
sib.excluded <- sib %>% 
  filter(!is.na(exclude_reason))
#sib<-sib[sib$stimuli.title.score==1 & sib$manip.gender.score == 1 & sib$manip.image.score == 1,]
sib <- sib %>% 
  filter(is.na(exclude_reason))
nrow(sib)

#### Scales #### 

#interest ----
#reverse score - 1,4,9,12,14,17
interest.reverse.items<-c("interest_1", "interest_4", "interest_9", 
                          "interest_12","interest_14", "interest_17") 
sib$interest_1R<-8-sib$interest_1
sib$interest_4R<-8-sib$interest_4
sib$interest_9R<-8-sib$interest_9 
sib$interest_12R<-8-sib$interest_12
sib$interest_14R<-8-sib$interest_14 
sib$interest_17R<-8-sib$interest_17
interest<-select(sib, starts_with(("interest_")), -all_of(interest.reverse.items))
#describe(interest)
#summary(interest)
sib$interest.all<-rowMeans(interest, na.rm=TRUE)

#leadership ----
#no reverse
#note: one item (item 7) on the stem career scale was also an accidental duplicate
lead<-sib %>% 
  select(starts_with("lead_")) %>% 
  select(!ends_with("_7"))
#describe(lead)
#psych::alpha(lead)
sib$lead.all<-rowMeans(lead, na.rm=TRUE)

#person job fit ----
#no reverse
pj.fit<-select(sib, starts_with("pj.fit_"))
#describe(pj.fit)
#psych::alpha(pj.fit)
sib$pj.fit.all<-rowMeans(pj.fit, na.rm=TRUE)

#confidence ----
#no reverse
confidence<-select(sib, starts_with("confidence_"))
#describe(confidence)
#psych::alpha(confidence)
sib$confidence.all<-rowMeans(confidence, na.rm=TRUE)

#self efficacy ----
### NOTE: there was an error in the survey so that this scale contained repeat items from the interest scale
sib$selfeff_2R<-8-sib$selfeff_2
sib$selfeff_4R<-8-sib$selfeff_4
selfeff.items <- c("selfeff_1", "selfeff_3", 
                   "selfeff_2R", "selfeff_4R")
selfeff.reverse.items <- c("selfeff_2", "selfeff_4")
selfeff <- sib %>% 
  select(all_of(selfeff.items))
# #reverse code - 2,4,9,12,14,17
# selfeff.reverse.items<-c("selfeff_2", "selfeff_4", "selfeff_9", "selfeff_12", "selfeff_14", "selfeff_17")
# 
# sib$selfeff_9R<-8-sib$selfeff_9
# sib$selfeff_12R<-8-sib$selfeff_12
# sib$selfeff_14R<-8-sib$selfeff_14
# sib$selfeff_17R<-8-sib$selfeff_17
# selfeff<-select(sib, starts_with("selfeff_"), -all_of(selfeff.reverse.items))
#describe(selfeff)
#psych::alpha(selfeff)
sib$selfeff.all<-rowMeans(selfeff, na.rm=TRUE)

#dd scales
#confidence 1
sib$dd.confidence.all<-sib$dd.interest
#describe(sib$dd.confidence.all)
#career aspirations 2,3
dd.career<-select(sib, dd.graduate, dd.prof)
#describe(dd.career)
sib$dd.career.all<-rowMeans(dd.career, na.rm=TRUE)
#explicit stereotypes 4,5 - lower values is more biased towards men
dd.stereo<-select(sib, dd.gender.good, dd.gender.career)
#describe(dd.stereo)
sib$dd.stereo.all<-rowMeans(dd.stereo, na.rm=TRUE)
#explicit identification with stem 6,7,8 ----
dd.id<-select(sib, dd.important,dd.useful, dd.care)
#describe(dd.id)
#psych::alpha(dd.id)

sib$dd.id.all<-rowMeans(dd.id, na.rm=TRUE)
#stem threat 9-13 ----
dd.threat<-select(sib, dd.school_1:dd.school_5)
#describe(dd.threat)
#psych::alpha(dd.threat)
sib$dd.threat.all<-rowMeans(dd.threat, na.rm=TRUE)
#stem challenge 14-18 ----
dd.challenge<-select(sib, dd.school_6:dd.school_10)
#describe(dd.challenge)
#psych::alpha(dd.challenge)
sib$dd.challenge.all<-rowMeans(dd.challenge, na.rm=TRUE)
#social belonging 19-22 ----
#reverse code 21,22
#dd.school_13,dd.school_14
dd.belonging.reverse <- c("dd.school_13", "dd.school_14")
sib$dd.school_13R<-8-sib$dd.school_13
sib$dd.school_14R<-8-sib$dd.school_14
dd.belonging<-select(sib, dd.school_11, dd.school_12, dd.school_13R, dd.school_14R)
#describe(dd.belonging)
#psych::alpha(dd.belonging)
sib$dd.belonging.all<-rowMeans(dd.belonging, na.rm=TRUE)
#changing major 23
#recode 8 as answer is NA
sib$dd.major.recode<-with(sib,ifelse(dd.major==8,NA,dd.major))
sib$dd.major.all<-sib$dd.major.recode
#describe(sib$dd.major.all)

#gender norms
#gender belief
#recode 6>5, 7>6, 8>7
gender.belief<-select(sib, gender.belief_1, gender.belief_2, gender.belief_3)
gender.belief[gender.belief==6]<-5
gender.belief[gender.belief==7]<-6
gender.belief[gender.belief==8]<-7
#describe(gender.belief)
#psych::alpha(gender.belief)
sib$gender.belief.all<-rowMeans(gender.belief, na.rm=TRUE)
#gender equalityf
#recode 6>5, 7>6, 8>7
gender.equality<-select(sib, gender.belief_4)
gender.equality[gender.equality==6]<-5
gender.equality[gender.equality==7]<-6
gender.equality[gender.equality==8]<-7
#describe(gender.equality$gender.belief_4)
sib$gender.equality.all<-gender.equality$gender.belief_4

#task performance
#angram
#anagram1 - mitochondria
#anagram2 - no solution
#anagram3 - no solution
#anagram4 - computation
#anagram5 - experiment
#anagram6 - no solution
#anagram7 - no solution
#anagram8 - centrifuge
#anagram9 - no solution
#anagram10 - microscope
sib$anagram1.strip<-trimws(tolower(sib$anagram1))
sib$anagram4.strip<-trimws(tolower(sib$anagram4))
sib$anagram5.strip<-trimws(tolower(sib$anagram5))
sib$anagram8.strip<-trimws(tolower(sib$anagram8))
sib$anagram10.strip<-trimws(tolower(sib$anagram10))
sib$anagram1.score<-with(sib, ifelse(anagram1.strip=="mitochondria", 1, 0))
sib$anagram4.score<-with(sib, ifelse(anagram4.strip=="computation", 1, 0))
sib$anagram5.score<-with(sib, ifelse(anagram5.strip=="experiment", 1, 0))
sib$anagram8.score<-with(sib, ifelse(anagram8.strip=="centrifuge", 1, 0))
sib$anagram10.score<-with(sib, ifelse(anagram10.strip=="microscope", 1, 0))
sib$anagram.score.all<-rowSums(sib[,c("anagram1.score","anagram4.score",
                                      "anagram5.score","anagram8.score",
                                      "anagram10.score")], na.rm=TRUE) 
sib$anagram2.strip<-trimws(tolower(sib$anagram2))
sib$anagram2.strip[sib$anagram2.strip == ''] <- NA
sib$anagram3.strip<-trimws(tolower(sib$anagram3))
sib$anagram3.strip[sib$anagram3.strip==''] <- NA
sib$anagram6.strip<-trimws(tolower(sib$anagram6))
sib$anagram6.strip[sib$anagram6.strip==''] <- NA
sib$anagram7.strip<-trimws(tolower(sib$anagram7))
sib$anagram7.strip[sib$anagram7.strip==''] <- NA
sib$anagram9.strip<-trimws(tolower(sib$anagram9))
sib$anagram9.strip[sib$anagram9.strip==''] <- NA

#anagram1 - mitochondria
#anagram2 - no solution
#anagram3 - no solution
#anagram4 - computation
#anagram5 - experiment
#anagram6 - no solution
#anagram7 - no solution
#anagram8 - centrifuge
#anagram9 - no solution
#anagram10 - microscope
#time - matters for impossible anagrams 2,3,6,7,9

#anagram2 - no solution
#anagram3 - no solution
#anagram6 - no solution
#anagram7 - no solution
#anagram9 - no solution
impossible.anagrams <- c("anagram2.time_Page.Submit",
                         "anagram3.time_Page.Submit",
                         "anagram6.time_Page.Submit",
                         "anagram7.time_Page.Submit",
                         "anagram9.time_Page.Submit")
sib$anagram.time.impossible<-rowSums(sib[,impossible.anagrams])
#Average anagram times?
#describe(sib$anagram.time.impossible)

possible.anagrams <- c('anagram1.time_Page.Submit',
                       'anagram4.time_Page.Submit',
                       'anagram5.time_Page.Submit',
                       'anagram8.time_Page.Submit',
                       'anagram10.time_Page.Submit')
sib$anagram.time.possible <- rowSums(sib[,possible.anagrams])

#Handle outliers for anagram perseverance
x <- sib[['anagram.time.impossible']]
mad.val <- mad(x, na.rm=TRUE)
med.val <- median(x, na.rm = TRUE)

sib['anagram.time.impossible.trimmed'] <- abs(x-med.val)/mad.val
sib['anagram.time.impossible.trimmed'] <- ifelse(sib[['anagram.time.impossible.trimmed']]<2.24&!is.na(sib[['anagram.time.impossible']]),
                                                 sib[['anagram.time.impossible']], NA)


sib <- sib %>% 
  mutate(gender.cond = as.factor(ifelse(gender.cond == 'F', 'Female scientist',
                                        'Male scientist')),
         `Image condition` = as.factor(ifelse(image.2level.cond=='O',
                                              'Not pictured', 'Pictured')),
         sci.image.cond = as.factor(ifelse(image.2level.cond=='O',
                                           'Not pictured', 'Pictured')),
         article.image.cond.pgender = as.factor(ifelse(participant.gender=='Women',
                                                       paste0(article.image.cond, '-F'),
                                                       paste0(article.image.cond, '-M')))
         )
#### look at item wordings and create variable key ####
variable_key <- read.csv(paste0(experiment_data, "/variable_key.csv"))
clean_variable_desc <- function(orig_desc){
  if(str_starts(orig_desc, "Please") & str_detect(orig_desc, "-")){
    start_loc = str_locate(orig_desc, "-")[2]
    return(str_sub(orig_desc, start = start_loc+1))
  }else{
    return(orig_desc)
  }
}
#clean up the variable descriptions to merge them with the factor loading df
variable_key["clean_variable_desc"] <- sapply(variable_key$var_desc, 
                                              clean_variable_desc)
variable_key["clean_varname"] <- ifelse(variable_key$var_name %in% 
                                          c(selfeff.reverse.items,
                                            interest.reverse.items,
                                            dd.belonging.reverse),
                                        paste0(variable_key$var_name, "R"),
                                        variable_key$var_name)
variable_key$clean_varname <- ifelse(variable_key$clean_varname %in% colnames(dd.threat),
                                     str_replace(variable_key$clean_varname, 
                                                 "dd.school", "dd.threat"),
                                     variable_key$clean_varname)
variable_key$clean_varname <- ifelse(variable_key$clean_varname %in% colnames(dd.challenge),
                                     str_replace(variable_key$clean_varname, 
                                                 "dd.school", "dd.challenge"),
                                     variable_key$clean_varname)
variable_key$clean_varname <- str_replace(variable_key$clean_varname,
                                                 "(dd.important|dd.useful|dd.care)",
                                                 "dd.id")
variable_key$clean_varname <- ifelse(variable_key$clean_varname %in% colnames(dd.belonging),
                                     str_replace(variable_key$clean_varname,
                                          "dd.school",
                                          "dd.belonging"),
                                     variable_key$clean_varname)
variable_key["desc_lower"] <-str_trim(str_to_lower(variable_key$clean_variable_desc))
duplicate_items <- variable_key %>% 
  filter(duplicated(desc_lower)) %>% 
  filter(str_detect(desc_lower, "timing")==F)
duplicate_inspect <- variable_key %>% 
  filter(desc_lower %in% duplicate_items$desc_lower)
duplicate_inspect
nrow(duplicate_inspect %>% filter(str_starts(clean_varname,"selfeff")))
#looks like most of the items on the self-efficacy scale were accidental duplicates
#one item on the stem career scale was also an accidental duplicate
drop_selfeff <- duplicate_inspect %>% filter(str_starts(clean_varname,"selfeff"))
drop_selfeff <- drop_selfeff$clean_varname
length(drop_selfeff)
variable_key <- variable_key %>%
  filter(clean_varname %in% drop_selfeff == F) %>%
  filter(clean_varname != "lead_7")

variable_key["scale_stem"] <- ifelse(str_detect(variable_key$clean_varname, "_\\d{1,2}"),
                                     str_replace(variable_key$clean_varname,
                                          "_\\d{1,2}R{0,1}", ""),
                                     NA)

write.csv(variable_key, paste0(experiment_data, "/variable_names_key.csv"))
scale.names <- variable_key %>% 
  filter(!is.na(scale_stem)&str_detect(scale_stem, "TEXT")==F)
keep.columns <- scale.names$clean_varname


#leadership ----
#no reverse
lead<-sib %>% 
  select(starts_with("lead_")) %>% 
  select(!ends_with("_7"))
#describe(lead)
#psych::alpha(lead)
sib$lead.all<-rowMeans(lead, na.rm=TRUE)

#### Create dataset with aggregate outcomes only ####
outcomes <- c('z.lead.all', 'z.interest.all',
              'z.dd.belonging.all', 
              'z.dd.id.all')
alphas <- list()
#Calculate reliabilities
alphas[['interest.all']] <- psych::alpha(interest)$total$raw_alpha
alphas[['lead.all']] <- psych::alpha(lead)$total$raw_alpha
alphas[['dd.id.all']] <- psych::alpha(dd.id)$total$raw_alpha
alphas[['dd.belonging.all']] <- psych::alpha(dd.belonging)$total$raw_alpha
alphas[['dd.threat.all']] <- psych::alpha(dd.threat)$total$raw_alpha
alphas[['dd.challenge.all']] <- psych::alpha(dd.challenge)$total$raw_alpha
alphas[['dd.career.all']] <- psych::alpha(dd.career)$total$raw_alpha
alphas[['selfeff.all']] <- psych::alpha(selfeff)$total$raw_alpha
alphas[['pj.fit.all']] <- psych::alpha(pj.fit)$total$raw_alpha
alphas[['confidence.all']] <- psych::alpha(confidence)$total$raw_alpha
alphas[['gender.belief.all']] <- psych::alpha(gender.belief)$total$raw_alpha
keep.columns <- c(colnames(interest),
                  colnames(lead),
                  colnames(dd.id),
                  colnames(dd.belonging),
                  colnames(dd.threat),
                  colnames(dd.challenge),
                  colnames(dd.career),
                  colnames(selfeff),
                  colnames(pj.fit),
                  colnames(confidence),
                  colnames(gender.belief))

#look at differences in responses to first vs. second administration of accidentally repeated interest items
selfeff.interest.rep.R <- c("9", "12", "14", "17")
selfeff.interest.rep <- sib %>% 
  select(all_of(drop_selfeff), starts_with("interest_")) %>% 
  #reverse-code the interest items from the self-efficacy scale
  mutate(across(all_of(str_c("selfeff_", selfeff.interest.rep.R)),
                ~ 8 - .x)) %>% 
  #rename the reverse-coded items
  rename_with(~str_c(.x, "R"), all_of(str_c("selfeff_", selfeff.interest.rep.R))) %>% 
  select(-ends_with("_1R"), - ends_with("_2"), -ends_with("_3"), - ends_with("_4R"))
selfeff.interest.rep["interest.rep.items"] <- rowMeans(selfeff.interest.rep %>% 
                                                         select(starts_with("interest")),
                                                       na.rm=T)
selfeff.interest.rep["selfeff.interest.rep.items"] <- rowMeans(selfeff.interest.rep %>% 
                                                         select(starts_with("selfeff")),
                                                       na.rm=T)
sib["interest.rep.items"] <- selfeff.interest.rep$interest.rep.items
sib["selfeff.interest.rep.items"] <- selfeff.interest.rep$selfeff.interest.rep.items
alpha.df <- as.data.frame(alphas)
#alpha's are really low for gender.belief.all, dd.career.all
#borderline alpha for selfeff.all
write.csv(alpha.df, paste0(output, 'cronbach_alphas.csv'))

#Z-score outcomes
outcome.vars <- colnames(alpha.df %>% select(-gender.belief.all,
                                             -dd.career.all))
sib <- sib %>% 
  mutate(across(all_of(outcome.vars),
                list(z = ~scale(.x)[,1]),
                .names = "z.{.col}"))

sib.clean <- sib %>% 
  select(article.cond, gender.cond, participant.gender,`Image condition`,
         starts_with("z."), all_of(outcome.vars), all_of(keep.columns),
         interest.rep.items, selfeff.interest.rep.items,
         age, race.cat.collapse) %>% 
  rename(image.cond=`Image condition`)
#Save subset of data with just variables necessary for analysis
write.csv(sib.clean, paste0(experiment_data,"/outcomes_experimental_data_clean_v1.csv"))
