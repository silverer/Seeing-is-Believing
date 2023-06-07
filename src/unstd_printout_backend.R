
#Data paths for input and output
setwd("~/Documents/Seeing-is-Believing/src")
if (!require("pacman")) install.packages("pacman"); library(pacman)
source("data_io.R")
p_load(stats, psych, tidyverse,
       emmeans,cowplot,rstatix,ggpubr,openxlsx,
       apaTables, scales, statstring,tableone,extrafont)
select <- dplyr::select
filter <- dplyr::filter
sib.og <- read.csv(paste0("../",experiment_data, "/outcomes_experimental_data_clean_v1.csv"))

#info on school in which major is housed
sib.og$in.engineering <- str_detect(sib.og$major.schools, "Engineering")
sib.og$in.natsci <- str_detect(sib.og$major.schools, "Natural ")
sib.og$ns.or.eng <- str_detect(sib.og$major.schools, "(Natural |Engineering)")
table(sib.og$ns.or.eng)
non.stem <- sib.og %>% 
  filter(ns.or.eng==F)
table(non.stem$major.schools)

some.stem <- sib.og %>% 
  filter(ns.or.eng==T)
table(some.stem$major.schools)
vnames <- c(lead.all = 'STEM Career Interest',
            interest.all = 'General STEM Interest',
            dd.belonging.all = 'STEM Belonging',
            dd.id.all = 'Identification with STEM')
outcomes <- names(vnames)
#og.outcomes <- str_replace(outcomes, "", "")

tmp.sib <- sib.og %>%
  dplyr::rename(`Article topic` = article.cond,
                `Scientist gender` = gender.cond,
                `Participant gender` = participant.gender,
                `Image condition` = image.cond,
                `STEM Career Interest`=lead.all,
                `General STEM Interest` = interest.all,
                `STEM Belonging` = dd.belonging.all,
                `Identification with STEM` = dd.id.all)

tabs <- CreateContTable(vars = vnames[names(vnames)], 
                        strata = c("Image condition", 
                                   "Scientist gender", 
                                   "Participant gender"),
                        data = tmp.sib, test = F)
write.csv(print(tabs, digits = 2), "../output/unstdmeans_by_cell.csv")

apa.cor.table(tmp.sib %>% 
                select(all_of(vnames)),
              filename = '../output/unstdpearson_correlations.doc',
              table.number = 2)

headerStyle <- createStyle(
  fontSize = 12, fontName = "Times New Roman", halign = "center",
  border = "bottom"
)
headerStyleIt <- createStyle(
  fontSize = 12, fontName = "Times New Roman", halign = "center",
  border = "bottom", textDecoration = 'italic'
)
col1HeaderStyle <- createStyle(
  fontSize = 12, fontName = "Times New Roman",border = "bottom", halign = "left"
)
bodyStyle <- createStyle(
  fontSize = 12, fontName = "Times New Roman", halign = "center"
)
col1Style <- createStyle(
  fontSize = 12, fontName = "Times New Roman", halign = "left"
)
itStyle <- createStyle(
  fontSize = 12, fontName = "Times New Roman", halign='left',
  textDecoration = 'italic'
)

saveFmtdReg <- function(regDf, sheetName, fname){
  wb<-createWorkbook(sheetName)
  addWorksheet(wb,"sheet1",gridLines = F)
  writeData(wb,sheet=1,regDf)
  addStyle(wb,sheet=1,headerStyle, rows=1, cols=c(1,4:ncol(regDf)))
  addStyle(wb,sheet=1,headerStyleIt, rows=1, cols=c(2,3))
  addStyle(wb,sheet=1,bodyStyle, 
           rows=1:nrow(regDf)+1, 
           cols = 2:ncol(regDf),
           gridExpand = T)
  addStyle(wb, sheet = 1, col1HeaderStyle,
           rows = 1, cols = 1)
  addStyle(wb,sheet=1,col1Style, 
           rows=1:nrow(regDf)+1, 
           cols=1)
  saveWorkbook(wb, paste0(fname, ".xlsx"), overwrite=T)
}
#get table 1 for outcomes that are relevant to the study
vnames.tmp <- c(lead.all = 'STEM Career Interest',
            interest.all = 'General STEM Interest',
            dd.belonging.all = 'STEM Belonging',
            dd.id.all = 'Identification with STEM')
rev.vnames.tmp <- names(vnames.tmp)
names(rev.vnames.tmp) <- vnames.tmp[names(vnames.tmp)]
tmp.1 <- tmp.sib %>% 
  select(all_of(vnames.tmp))

cor_tab <- apa.cor.table(tmp.1)
cor_tab <- data.frame(cor_tab$table.body)
cor_tab <- cor_tab %>% 
  filter(Variable != " ")
saveFmtdReg(cor_tab, "correlations","../output/unstdpearson_correlations")

### 3-way AOVs pretty output ####
sib.og$gender.cond<-factor(sib.og$gender.cond)
sib.og$image.cond<-factor(sib.og$image.cond)
sib.og$participant.gender<-factor(sib.og$participant.gender,
                                  levels = c("Women", "Men"))
indep.vars.og <- c("participant.gender","gender.cond", "image.cond")


aov_results <- list()
aov_result_df <- data.frame(Predictor = rep("", 9))

marginal.means <- list()
emm.objs <- list()
plots.3way <- list()
mods <- list()
for(v in names(vnames)){
  sib.sub <- sib.og %>% 
    select(all_of(c(v, "participant.gender", "image.cond",
                    "gender.cond")))
  f <- paste(names(sib.sub)[1], "~",
             paste(paste(names(sib.sub)[3],
                         names(sib.sub)[4],
                         names(sib.sub)[2],
                         sep=" * ")))
  options(contrasts = rep("contr.sum", 2))
  mod.allconds <- lm(f, data = sib.sub)
  
  apa.aov.table(mod.allconds, 
                filename = paste0("../",output, v,"unstd 3 way.doc"))
  tmp <- apa.aov.table(mod.allconds)$table_body
  tmp <- tmp %>% 
    mutate(Predictor = str_replace(Predictor, "gender.cond", "Scientist Gender"),
           Predictor = str_replace(Predictor, "[.]cond", " Condition"),
           Predictor = str_replace(Predictor, "[.]", " "),
           Predictor = str_to_title(Predictor)
    )
  tmp["stat_string"] <- format_anova_string(apa.aov.table(mod.allconds), get.all=T)
  tmp$stat_string[1] <- ""
  aov_results[[v]] <- tmp
  aov_result_df["Predictor"] <- tmp$Predictor
  aov_result_df[vnames[[v]]] <- tmp$stat_string
  o <- v
  #save model object for effect size calculation
  mods[[o]] <- mod.allconds
  #Get marginal means and standard errors for plotting
  emm.objs[[o]] <- emmeans(mod.allconds, indep.vars.og)
  marginal.means[[o]] <- as.data.frame(emm.objs[[o]])
  marginal.means[[o]] <- marginal.means[[o]] %>%
    arrange(participant.gender, gender.cond) %>%
    mutate(emmean = round(emmean, 2),
           SE = round(SE, 2),
           lower.CL = round(lower.CL, 2),
           upper.CL = round(upper.CL, 2)) %>%
    rename(`Est. marginal mean` = emmean,
           `Participant gender` = participant.gender,
           `Gender condition` = gender.cond,
           `Image condition` = image.cond)
  
  #Build plots
  y.ax <- paste0(vnames[[o]])
  plots.3way[[o]] <- marginal.means[[o]] %>%
    mutate(`Participant gender` = ifelse(`Participant gender` == "Men",
                                         "Male Participants",
                                         "Female Participants"),
           `Gender condition` = str_replace(`Gender condition`, 
                                            " scientist", "")) %>%
    ggplot() +
    aes(x = `Gender condition`, group = `Image condition`,
        y = `Est. marginal mean`) +
    geom_point()+
    geom_line()+
    geom_errorbar(aes(ymin=`Est. marginal mean`-SE,
                      ymax=`Est. marginal mean`+SE),
                  width = 0.1, alpha = 0.5)+
    aes(linetype=`Image condition`)+
    ggtitle(paste0(y.ax, ' by Participant Gender and Condition'))+
    labs(linetype = "Image Condition")+
    ylab(y.ax)+
    xlab("Gender Condition")+
    facet_wrap(~`Participant gender`)+
    theme(
      # Remove panel grid lines
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text.x = element_text(size = 12, colour = "black"),
      strip.background=element_rect(fill="white",color='black'),
      # Remove panel background
      panel.background = element_blank(),
      panel.border = element_rect(fill=NA),
      text=element_text(family="Times", size=14),
      axis.text.x.bottom = element_text(family="Times", size = 12),
      legend.key = element_rect(fill="white"),
      # Add axis line
      axis.line = element_line(colour = "black")
    )

  sib.sub <- sib.og %>% 
    select(all_of(c(v, "participant.gender", "image.cond",
                    "gender.cond", "article.cond")))
  f <- paste(names(sib.sub)[1], "~",
             paste(paste(names(sib.sub)[3],
                         names(sib.sub)[4],
                         names(sib.sub)[2],
                         names(sib.sub)[5],
                         sep=" * ")))
  options(contrasts = rep("contr.sum", 2))
  mod.allconds <- lm(f, data = sib.sub)
  aov4out <- data.frame(apa.aov.table(mod.allconds)$table_body)
  aov4out <- aov4out %>% 
    mutate(Predictor = str_replace(Predictor, "gender[.]cond", "Scientist Gender"),
           Predictor = str_replace(Predictor, "article[.]cond", "Article Topic"),
           Predictor = str_replace(Predictor, "[.]cond", " Condition"),
           Predictor = str_replace(Predictor, "[.]", " "),
           Predictor = str_to_title(Predictor)
  )
  write.xlsx(aov4out,paste0("../output/", v, "4way.xlsx"))
}

format_contrast_results <- function(mdiff, lower_ci, upper_ci, t_val,
                                    p_val,dof, return_md = TRUE){

  if(return_md==TRUE){
    return( paste0('_t_(', dof, ') = ', t_val,
                   ', ', p_val,
                   ' (M~diff~ = ', mdiff, ", ",
                   ' 95% CI: ', lower_ci, ', ',
                   upper_ci, ')'))
  }else{
    return(paste0('t(', dof, ') = ', t_val,
                  ', ', p_val,
                  ' (Mdiff = ', mdiff,
                  ' 95% CI: ', lower_ci, ', ',
                  upper_ci, ')'))
  }
}
#Recode the rows so that the reference level matches up with hypotheses
keep.rows <- c("Women Male scientist Pictured - Men Male scientist Pictured",
               "Women Female scientist Pictured - Women Male scientist Pictured",
               "Women Female scientist Not pictured - Women Female scientist Pictured",
               "Women Female scientist Pictured - Men Female scientist Pictured")

new.rows <- c("Women Male scientist Pictured - Men Male scientist Pictured",
              "Women Male scientist Pictured - Women Female scientist Pictured",
              "Women Female scientist Not pictured - Women Female scientist Pictured",
              "Men Female scientist Pictured - Women Female scientist Pictured")
names(new.rows) <- keep.rows

all_pairwise_results <- data.frame()
#Perform pairwise tests
for(o in outcomes){
  test <- pairs(emm.objs[[o]], adjust="none")
  test.cis <- confint(test)
  test.cis <- as.data.frame(test.cis)
  test.cis <- test.cis %>%
    dplyr::select(contrast, lower.CL, upper.CL)
  test.out <- as.data.frame(test)
  test.out <- dplyr::left_join(test.out, test.cis, by = "contrast")
  tmp.out <- test.out %>% 
    mutate(outcome = o)
  all_pairwise_results <- rbind(all_pairwise_results, tmp.out)
  test.out <- test.out %>%
    dplyr::filter(contrast %in% keep.rows)
  #adjust p-value using FDR
  test.out <- adjust_pvalue(test.out, p.col="p.value",
                            output.col="adj.p.value", method="fdr")
  test.out['outcome'] <- o
  #rename rows to reflect contrast reference level
  test.out <- test.out %>%
    mutate(new.contrast = recode_factor(contrast, !!!new.rows))
  tmp.effsize.df <- data.frame(eff_size(emm.objs[[o]],
                         sigma=sigma(mods[[o]]),
                         edf=df.residual(mods[[o]])))
  tmp.effsize.df["outcome"] <- o
  tmp.effsize.df <- tmp.effsize.df %>% 
    filter(contrast %in% keep.rows) %>% 
    mutate(new.contrast = recode_factor(contrast, !!!new.rows))
  for(i in 1:nrow(test.out)){
    if(test.out$contrast[i] != test.out$new.contrast[i]){
      test.out$estimate[i] <- test.out$estimate[i]*-1
      test.out$t.ratio[i] <- test.out$t.ratio[i]*-1
      new.upper <- test.out$lower.CL[i]*-1
      new.lower <- test.out$upper.CL[i]*-1
      test.out$lower.CL[i] <- new.lower
      test.out$upper.CL[i] <- new.upper
      
      tmp.effsize.df$effect.size[i] <- tmp.effsize.df$effect.size[i]*-1
      new.upper <- tmp.effsize.df$lower.CL[i]*-1
      new.lower <- tmp.effsize.df$upper.CL[i]*-1
      tmp.effsize.df$lower.CL[i] <- new.lower
      tmp.effsize.df$upper.CL[i] <- new.upper
    }else{
      test.out$new.contrast[i] <- test.out$contrast[i]
      tmp.effsize.df$new.contrast[i] <- tmp.effsize.df$contrast[i]
    }
  }
  if(o == outcomes[1]){
    pairwise_df <- test.out
    effsize_df <- tmp.effsize.df

  }else{
    pairwise_df <- rbind(pairwise_df, test.out)
    effsize_df <- rbind(effsize_df, tmp.effsize.df)
  }
}

#format outputs
pairwise_df["formatted.result"] <- mapply(statstring::format_pairwise_comparison,
                                          t_stat = pairwise_df$t.ratio,
                                          df = pairwise_df$df,
                                          p_val = pairwise_df$adj.p.value,
                                          mdiff = pairwise_df$estimate,
                                          lci = pairwise_df$lower.CL,
                                          uci = pairwise_df$upper.CL)
pairwise_df$lower.CL <- number(pairwise_df$lower.CL, accuracy = 0.01)
pairwise_df$upper.CL <- number(pairwise_df$upper.CL, accuracy = 0.01)
pairwise_df$estimate <- number(pairwise_df$estimate, accuracy = 0.01)
pairwise_df["adj.p.value.num"] <- pairwise_df$adj.p.value
pairwise_df$adj.p.value <- sapply(pairwise_df$adj.p.value.num, format_pval_apa)
pairwise_df$t.ratio <- number(pairwise_df$t.ratio, accuracy = 0.01)
pairwise_df$df <- number(pairwise_df$df)
pairwise_df['formatted.result'] <- mapply(format_contrast_results, pairwise_df$estimate,
                                          pairwise_df$lower.CL, pairwise_df$upper.CL,
                                          pairwise_df$t.ratio, pairwise_df$adj.p.value,
                                          pairwise_df$df, return_md=TRUE)

#pairwise_df
h1a_cond = "Women Male scientist Pictured - Men Male scientist Pictured"
h1b_cond = "Women Female scientist Not pictured - Women Female scientist Pictured"
h1c_cond = "Women Male scientist Pictured - Women Female scientist Pictured"

h1a_table = pairwise_df %>%
  dplyr::filter(new.contrast == h1a_cond) %>%
  dplyr::filter(adj.p.value.num < 0.1) %>%
  dplyr::select(outcome, new.contrast, formatted.result)
h1a_sigoutcomes = h1a_table$outcome
h1a_table

h1b_table = pairwise_df %>%
  dplyr::filter(new.contrast == h1b_cond) %>%
  dplyr::filter(adj.p.value.num < 0.1) %>%
  dplyr::select(outcome, new.contrast, formatted.result)
h1b_sigoutcomes = h1b_table$outcome
h1b_table

h1c_table = pairwise_df %>%
  dplyr::filter(new.contrast == h1c_cond) %>%
  dplyr::filter(adj.p.value.num < 0.1) %>%
  dplyr::select(outcome, new.contrast, formatted.result)
h1c_sigoutcomes = h1c_table$outcome
h1c_table

#robustness check: does using the first vs. second set of interest items change the overall results?
sib.og["participant.id"] <- 1:nrow(sib.og)
long_df <- sib.og %>% 
  pivot_longer(all_of(c("selfeff.interest.rep.items",
                        "interest.rep.items"))) %>% 
  select(name, value, participant.gender, gender.cond,image.cond, participant.id)
long_df$participant.gender <- factor(long_df$participant.gender)
long_df$gender.cond<-factor(long_df$gender.cond)
long_df$image.cond<-factor(long_df$image.cond)
long_df$name <- ifelse(long_df$name=="interest.rep.items",
                       "T1", "T2")
long_df$name <- factor(long_df$name, levels = c("T1",
                                                "T2"))

#' there's a significant difference between people's responses to items 5-18 on the first vs. second time it was administered
#' the "interest" scale was given before the "self efficacy" scale
t.test(long_df$value~long_df$name,
       paired=T)
#' people scored a  point lower on the first set of actual interest items than they did on the same items when they saw them a second time
long_df %>% 
  group_by(name) %>% 
  summarise(mean(value, na.rm=T))

mod <- long_df %>% 
  anova_test(dv = value,
             within = name,
             wid = participant.id,
             between = c(participant.gender,gender.cond,image.cond),
             type = 3)
get_anova_table(mod)



mod <- lm(selfeff.interest.rep.items~participant.gender*image.cond*gender.cond,
          data = sib.og)
apa.aov.table(mod)
mod.em <- emmeans(mod, indep.vars.og)
mod.pairs <- data.frame(pairs(mod.em, adjust="none"))
mod.pairs <- mod.pairs %>%
  dplyr::filter(contrast %in% keep.rows)
mod.pairs <- adjust_pvalue(mod.pairs, p.col="p.value",
                           output.col="adj.p.value", method="fdr")

mod.1 <- lm(interest.rep.items~participant.gender*image.cond*gender.cond,
            data = sib.og)
apa.aov.table(mod.1)
mod1.em <- emmeans(mod.1, indep.vars.og)
mod1.pairs <- data.frame(pairs(mod1.em, adjust="none"))
mod1.pairs <- mod1.pairs %>%
  dplyr::filter(contrast %in% keep.rows)
mod1.pairs <- adjust_pvalue(mod1.pairs, p.col="p.value",
                            output.col="adj.p.value", method="fdr")
#BUT the overall take-away is the same regardless 
#(sig diff for women in female scientist not pictured vs pictured and women in male scientist pictured vs. female scientist pictured)
mod.pairs
mod1.pairs


#### Make and save the two-panel figure ####
career_interest <- marginal.means$lead.all %>%
  mutate(`Participant gender` = ifelse(`Participant gender` == "Men",
                                       "Male Participants",
                                       "Female Participants"),
         `Gender condition` = ifelse(`Gender condition`=="Male scientist",
                                     "Male", "Female")) %>%
  ggplot() +
  aes(x = `Gender condition`, group = `Image condition`,
      y = `Est. marginal mean`) +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=`Est. marginal mean`-SE,
                    ymax=`Est. marginal mean`+SE),
                width = 0.1, alpha = 0.5)+
  aes(linetype=`Image condition`)+
  labs(linetype = "Image Condition")+
  ggtitle("STEM Career Interest")+
  ylab("STEM Career Interest")+
  xlab("Scientist Gender")+
  facet_wrap(~`Participant gender`)+
  ylim(bottom=4,top=7)+
  theme(
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text.x = element_text(size = 12, colour = "black"),
    strip.background=element_rect(fill="white",color='black'),
    # Remove panel background
    panel.background = element_blank(),
    panel.border = element_rect(fill=NA),
    text=element_text(family="Times", size=12),
    axis.text.x.bottom = element_text(family = "Times", size = 12),
    # Add axis line
    axis.line = element_line(colour = "black"),
    legend.key = element_rect(fill="white"),
    legend.box.background = element_rect(color="black")
  )

stem_id <- marginal.means$dd.id.all %>%
  mutate(`Participant gender` = ifelse(`Participant gender` == "Men",
                                       "Male Participants",
                                       "Female Participants"),
         `Gender condition` = ifelse(`Gender condition`=="Male scientist",
                                     "Male", "Female")) %>%
  ggplot() +
  aes(x = `Gender condition`, group = `Image condition`,
      y = `Est. marginal mean`) +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=`Est. marginal mean`-SE,
                    ymax=`Est. marginal mean`+SE),
                width = 0.1, alpha = 0.5)+
  aes(linetype=`Image condition`)+
  labs(linetype = "Image Condition")+
  ggtitle("Identification with STEM")+
  ylab("Identification with STEM")+
  xlab("Scientist Gender")+
  facet_wrap(~`Participant gender`)+
  ylim(bottom=4,top=7)+
  theme(
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text.x = element_text(size = 12, colour = "black"),
    strip.background=element_rect(fill="white",color='black'),
    # Remove panel background
    panel.background = element_blank(),
    panel.border = element_rect(fill=NA),
    text=element_text(family="Times", size=12),
    axis.text.x.bottom = element_text(family = "Times", size = 12),
    # Add axis line
    axis.line = element_line(colour = "black"),
    legend.key = element_rect(fill="white"),
    legend.box.background = element_rect(color="black")
  )


figure <- ggarrange(career_interest, stem_id,
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1,
                    legend = "bottom",
                    common.legend = TRUE)
#### Make and save the four-panel figure ####
stem_interest <- marginal.means$interest.all %>%
  mutate(`Participant gender` = ifelse(`Participant gender` == "Men",
                                       "Male Participants",
                                       "Female Participants"),
         `Gender condition` = ifelse(`Gender condition`=="Male scientist",
                                     "Male", "Female")) %>%
  ggplot() +
  aes(x = `Gender condition`, group = `Image condition`,
      y = `Est. marginal mean`) +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=`Est. marginal mean`-SE,
                    ymax=`Est. marginal mean`+SE),
                width = 0.1, alpha = 0.5)+
  aes(linetype=`Image condition`)+
  labs(linetype = "Image Condition")+
  ggtitle("General STEM Interest")+
  ylab("General STEM Interest")+
  xlab("Scientist Gender")+
  ylim(bottom=4,top=7)+
  facet_wrap(~`Participant gender`)+
  theme(
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text.x = element_text(size = 12, colour = "black"),
    strip.background=element_rect(fill="white",color='black'),
    # Remove panel background
    panel.background = element_blank(),
    panel.border = element_rect(fill=NA),
    text=element_text(family="Times", size=12),
    axis.text.x.bottom = element_text(family="Times", size = 12),
    # Add axis line
    axis.line = element_line(colour = "black"),
    legend.key = element_rect(fill="white"),
    legend.box.background = element_rect(color="black")
  )

stem_belong <- marginal.means$dd.belonging.all %>%
  mutate(`Participant gender` = ifelse(`Participant gender` == "Men",
                                       "Male Participants",
                                       "Female Participants"),
         `Gender condition` = ifelse(`Gender condition`=="Male scientist",
                                     "Male", "Female")) %>%
  ggplot() +
  aes(x = `Gender condition`, group = `Image condition`,
      y = `Est. marginal mean`) +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=`Est. marginal mean`-SE,
                    ymax=`Est. marginal mean`+SE),
                width = 0.1, alpha = 0.5)+
  aes(linetype=`Image condition`)+
  ylim(bottom=4,top=7)+
  labs(linetype = "Image Condition")+
  ggtitle("STEM Belongingness")+
  ylab("STEM Belongingness")+
  xlab("Scientist Gender")+
  facet_wrap(~`Participant gender`)+
  theme(
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text.x = element_text(size = 12, colour = "black"),
    strip.background=element_rect(fill="white",color='black'),
    # Remove panel background
    panel.background = element_blank(),
    panel.border = element_rect(fill=NA),
    text=element_text(family="Times", size=12),
    axis.text.x.bottom = element_text(family = "Times", size = 12),
    # Add axis line
    axis.line = element_line(colour = "black"),
    legend.key = element_rect(fill="white"),
    legend.box.background = element_rect(color="black")
  )

figure.4panel <- ggarrange(career_interest, stem_id,
                           stem_interest,stem_belong,
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2,
                    legend = "bottom",
                    common.legend = TRUE)

save_plot("../plots/unstdmultipanel_4panels.png", figure.4panel,
          base_width = 8, base_height=10)
save_plot("../plots/unstdmultipanel_4panels.tiff", figure.4panel,
          base_width = 8, base_height=10)



