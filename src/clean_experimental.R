pacman::p_load(sjlabelled, sjmisc, dplyr, psych, lavaan, 
               semPlot, car, ggplot2, lsr, apaTables, 
               readr, stringi, careless, stringr)
setwd("~/Documents/Seeing-is-Believing/")
#Data paths for input and output
source('src/data_io.R')

sib<-read.csv(paste0(experiment_data, '/cleaned_de_id_data.csv'))
nrow(sib)
#Filter out garbage responses
sib <- sib[sib$DistributionChannel!="preview" &!is.na(sib$FL_15_DO),]


nrow(sib)
exclusion_tracker <- list()
exclusion_tracker['original_length'] <- nrow(sib)
#### Filter people who didn't finish, listed gender other than M/W, or were in research methods class ####

exclusion_tracker['unfinished'] <- nrow(sib %>% dplyr::filter(Progress != 100))
exclusion_tracker['in_psyc485'] <- nrow(sib %>% dplyr::filter(in_research_methods_class==T))
sib<-sib %>% 
  dplyr::filter(Progress == 100 & in_research_methods_class==FALSE)

nrow(sib)
exclusion_tracker['not_m_f'] <- nrow(sib %>% dplyr::filter(gender > 2 | is.na(gender)))
sib<-sib[sib$gender==1|sib$gender==2,]
nrow(sib)

#### Participant gender variable ####
#3 is self-describe, 6 is prefer not to say
#1 male, 2 female
sib$genderF<-as.factor(as.character(sib$gender)) 
#replace anything except MF with NA
sib$gender.mf.F<-as.factor(with(sib, ifelse(gender == 1, "1",
                                            ifelse(gender == 2, "2",
                                                   NA))))
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

sib.all <- sib
fail.table <- table(sib.all$article.image.cond,sib.all$failed_attn_check)
exclusion_tracker['failed_attn_check'] <- nrow(sib %>% dplyr::filter(failed_attn_check==TRUE))
sib<-sib[sib$stimuli.title.score==1 & sib$manip.gender.score == 1 & sib$manip.image.score == 1,]
nrow(sib)

#### Scales #### 

#interest
#reverse score - 1,4,9,12,14,17
interest.reverse.items<-c("interest_1", "interest_4", "interest_9", 
                          "interest_12","interest_14", "interest_17") 
sib$interest_1R<-8-sib$interest_1
sib$interest_4R<-8-sib$interest_4
sib$interest_9R<-8-sib$interest_9 
sib$interest_12R<-8-sib$interest_12
sib$interest_14R<-8-sib$interest_14 
sib$interest_17R<-8-sib$interest_17
interest<-dplyr::select(sib, starts_with(("interest_")), -all_of(interest.reverse.items))
#describe(interest)
#summary(interest)
sib$interest.all<-rowMeans(interest, na.rm=TRUE)

#leadership
#no reverse
lead<-dplyr::select(sib, starts_with("lead_"))
#describe(lead)
#psych::alpha(lead)
sib$lead.all<-rowMeans(lead, na.rm=TRUE)

#person job fit
#no reverse
pj.fit<-dplyr::select(sib, starts_with("pj.fit_"))
#describe(pj.fit)
#psych::alpha(pj.fit)
sib$pj.fit.all<-rowMeans(pj.fit, na.rm=TRUE)

#confidence
#no reverse
confidence<-dplyr::select(sib, starts_with("confidence_"))
#describe(confidence)
#psych::alpha(confidence)
sib$confidence.all<-rowMeans(confidence, na.rm=TRUE)

#self efficacy 
#reverse code - 2,4,9,12,14,17
selfeff.reverse.items<-c("selfeff_2", "selfeff_4", "selfeff_9", "selfeff_12", "selfeff_14", "selfeff_17")
sib$selfeff_2R<-8-sib$selfeff_2
sib$selfeff_4R<-8-sib$selfeff_4
sib$selfeff_9R<-8-sib$selfeff_9
sib$selfeff_12R<-8-sib$selfeff_12
sib$selfeff_14R<-8-sib$selfeff_14
sib$selfeff_17R<-8-sib$selfeff_17
selfeff<-dplyr::select(sib, starts_with("selfeff_"), -all_of(selfeff.reverse.items))
#describe(selfeff)
#psych::alpha(selfeff)
sib$selfeff.all<-rowMeans(selfeff, na.rm=TRUE)

#dd scales
#confidence 1
sib$dd.confidence.all<-sib$dd.interest
#describe(sib$dd.confidence.all)
#career aspirations 2,3
dd.career<-dplyr::select(sib, dd.graduate, dd.prof)
#describe(dd.career)
sib$dd.career.all<-rowMeans(dd.career, na.rm=TRUE)
#explicit stereotypes 4,5 - lower values is more biased towards men
dd.stereo<-dplyr::select(sib, dd.gender.good, dd.gender.career)
#describe(dd.stereo)
sib$dd.stereo.all<-rowMeans(dd.stereo, na.rm=TRUE)
#explicit identification with stem 6,7,8
dd.id<-dplyr::select(sib, dd.important,dd.useful, dd.care)
#describe(dd.id)
#psych::alpha(dd.id)

sib$dd.id.all<-rowMeans(dd.id, na.rm=TRUE)
#stem threat 9-13
dd.threat<-dplyr::select(sib, dd.school_1:dd.school_5)
#describe(dd.threat)
#psych::alpha(dd.threat)
sib$dd.threat.all<-rowMeans(dd.threat, na.rm=TRUE)
#stem challenge 14-18
dd.challenge<-dplyr::select(sib, dd.school_6:dd.school_10)
#describe(dd.challenge)
#psych::alpha(dd.challenge)
sib$dd.challenge.all<-rowMeans(dd.challenge, na.rm=TRUE)
#social belonging 19-22
#reverse code 21,22
#dd.school_13,dd.school_14
sib$dd.school_13R<-8-sib$dd.school_13
sib$dd.school_14R<-8-sib$dd.school_14
dd.belonging<-dplyr::select(sib, dd.school_11, dd.school_12, dd.school_13R, dd.school_14R)
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
gender.belief<-dplyr::select(sib, gender.belief_1, gender.belief_2, gender.belief_3)
gender.belief[gender.belief==6]<-5
gender.belief[gender.belief==7]<-6
gender.belief[gender.belief==8]<-7
#describe(gender.belief)
#psych::alpha(gender.belief)
sib$gender.belief.all<-rowMeans(gender.belief, na.rm=TRUE)
#gender equalityf
#recode 6>5, 7>6, 8>7
gender.equality<-dplyr::select(sib, gender.belief_4)
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

#Add indicators for valid unsolvable anagram responses
IsAnswer <- function(response){
  ifelse(is.na(response), return(0),
         ifelse(str_length(response)< 5, return(0),
                ifelse(str_contains(response, c("n/a", 'na', 'know', "/",'-','?',
                                                'sure', 'possible', 'solution', 'answer', 'idk', 
                                                'solv', "can't", 'not ', "don't", "..."),
                                    logic = 'or'), return(0), return(1)))
         )
}
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
sib$anagram2.answer <- unlist(lapply(sib$anagram2.strip, IsAnswer))
sib$anagram3.answer <- unlist(lapply(sib$anagram3.strip, IsAnswer))
sib$anagram6.answer <- unlist(lapply(sib$anagram6.strip, IsAnswer))
sib$anagram7.answer <- unlist(lapply(sib$anagram7.strip, IsAnswer))
sib$anagram9.answer <- unlist(lapply(sib$anagram9.strip, IsAnswer))

sib$num.impossible.answers <- rowSums(sib[,c("anagram2.answer","anagram3.answer",
                                             "anagram6.answer","anagram7.answer",
                                             "anagram9.answer")], na.rm=TRUE)
sib$any.impossible.answer <- with(sib, ifelse(num.impossible.answers>=1,
                                                     TRUE, FALSE))
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

