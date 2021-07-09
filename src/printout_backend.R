
#Data paths for input and output
#setwd("~/Documents/Seeing-is-Believing/")
if (!require("pacman")) install.packages("pacman"); library(pacman)
source("data_io.R")
p_load(stats, psych, tidyverse,
       emmeans,cowplot,rstatix,ggpubr,
       apaTables, scales, statstring)

sib.og <- read.csv(paste0("../",experiment_data, "/outcomes_experimental_data_clean.csv"))
tmp.sib <- sib.og %>%
  dplyr::rename(`Article topic` = article.cond,
                `Scientist gender` = gender.cond,
                `Participant gender` = participant.gender,
                `Image condition` = image.cond,
                `STEM Career Interest`=z.lead.all,
                `General STEM Interest` = z.interest.all,
                `STEM Belonging` = z.dd.belonging.all,
                `Identification with STEM` = z.dd.id.all)

vnames <- c(z.lead.all = 'STEM Career Interest',
            z.interest.all = 'General STEM Interest',
            z.dd.belonging.all = 'STEM Belonging',
            z.dd.id.all = 'Identification with STEM')

### 3-way AOVs pretty output ####
aov_results <- list()
res <- lm(`STEM Career Interest`~`Participant gender`*`Image condition`*`Scientist gender`,
          data = tmp.sib)
tmp <- apa.aov.table(res)$table_body
tmp["stat_string"] <- format_anova_string(apa.aov.table(res), get.all=T)
tmp$stat_string[1] <- ""
aov_results[['z.lead.all']] <- tmp

res <- lm(`General STEM Interest`~`Participant gender`*`Image condition`*`Scientist gender`,
          data = tmp.sib)
tmp <- apa.aov.table(res)$table_body
tmp["stat_string"] <- format_anova_string(apa.aov.table(res), get.all=T)
tmp$stat_string[1] <- ""
tmp$stat_string[nrow(tmp)] <- ""
aov_results[['z.interest.all']] <- tmp

res <- lm(`STEM Belonging`~`Participant gender`*`Image condition`*`Scientist gender`,
          data = tmp.sib)
tmp <- apa.aov.table(res)$table_body
tmp["stat_string"] <- format_anova_string(apa.aov.table(res), get.all=T)
tmp$stat_string[1] <- ""
tmp$stat_string[nrow(tmp)] <- ""
aov_results[['z.dd.belonging.all']] <- tmp

res <- lm(`Identification with STEM`~`Participant gender`*`Image condition`*`Scientist gender`,
          data = tmp.sib)
tmp <- apa.aov.table(res)$table_body
tmp["stat_string"] <- format_anova_string(apa.aov.table(res), get.all=T)
tmp$stat_string[1] <- ""
tmp$stat_string[nrow(tmp)] <- ""
aov_results[['z.dd.id.all']] <- tmp

indep.vars.og <- c("participant.gender","gender.cond", "image.cond")
marginal.means <- list()
emm.objs <- list()
plots.3way <- list()
outcomes <- c('z.lead.all', 'z.interest.all',
              'z.dd.belonging.all',
              'z.dd.id.all')
for(o in outcomes){
  print(o)
  temp.all.conds <- sib.og %>%
    dplyr::select(all_of(c(o, indep.vars.og)))
  f <- paste(names(temp.all.conds)[1], "~",
             paste(paste(names(temp.all.conds)[3],
                         names(temp.all.conds)[4],
                         names(temp.all.conds)[2],
                         sep=" * ")))
  options(contrasts = rep("contr.sum", 2))
  mod.allconds <- lm(f, data = temp.all.conds)
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
                                         "Female Participants")) %>%
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
      axis.text.x.bottom = element_text(family = "Times", size = 12),
      legend.key = element_rect(fill="white"),
      # Add axis line
      axis.line = element_line(colour = "black")
    )
}



format_contrast_results <- function(mdiff, lower_ci, upper_ci, t_val,
                                    p_val,dof, return_md = TRUE){

  if(return_md==TRUE){
    return( paste0('_t_(', dof, ') = ', t_val,
                   ', _p_ = ', p_val,
                   ' (M~diff~ = ', mdiff, ", ",
                   ' 95% CI: ', lower_ci, ', ',
                   upper_ci, ')'))
  }else{
    return(paste0('t(', dof, ') = ', t_val,
                  ', p = ', p_val,
                  ' (Mdiff = ', mdiff,
                  ' 95% CI: ', lower_ci, ', ',
                  upper_ci, ')'))
  }
}
#Recode the rows so that the reference level matches up with hypotheses
keep.rows <- c("Men Male scientist Pictured - Women Male scientist Pictured",
               "Women Female scientist Pictured - Women Male scientist Pictured",
               "Women Female scientist Not pictured - Women Female scientist Pictured",
               "Men Female scientist Pictured - Women Female scientist Pictured")

new.rows <- c("Women Male scientist Pictured - Men Male scientist Pictured",
              "Women Male scientist Pictured - Women Female scientist Pictured",
              "Women Female scientist Not pictured - Women Female scientist Pictured",
              "Men Female scientist Pictured - Women Female scientist Pictured")
names(new.rows) <- keep.rows

#Perform pairwise tests
for(o in outcomes){
  test <- pairs(emm.objs[[o]], adjust="none")
  test.cis <- confint(test)
  test.cis <- as.data.frame(test.cis)
  test.cis <- test.cis %>%
    dplyr::select(contrast, lower.CL, upper.CL)
  test.out <- as.data.frame(test)
  test.out <- dplyr::left_join(test.out, test.cis, by = "contrast")
  test.out <- test.out %>%
    dplyr::filter(contrast %in% keep.rows)
  #adjust p-value using FDR
  test.out <- adjust_pvalue(test.out, p.col="p.value",
                            output.col="adj.p.value", method="fdr")
  test.out['outcome'] <- o
  #rename rows to reflect contrast reference level
  test.out <- test.out %>%
    mutate(new.contrast = recode_factor(contrast, !!!new.rows))

  for(i in 1:nrow(test.out)){
    if(test.out$contrast[i] != test.out$new.contrast[i]){
      test.out$estimate[i] <- test.out$estimate[i]*-1
      test.out$t.ratio[i] <- test.out$t.ratio[i]*-1
      new.upper <- test.out$lower.CL[i]*-1
      new.lower <- test.out$upper.CL[i]*-1
      test.out$lower.CL[i] <- new.lower
      test.out$upper.CL[i] <- new.upper
    }else{
      test.out$new.contrast[i] <- test.out$contrast[i]
    }
  }
  if(o == outcomes[1]){
    pairwise_df <- test.out

  }else{
    pairwise_df <- rbind(pairwise_df, test.out)
  }
}
#format outputs
pairwise_df$lower.CL <- number(pairwise_df$lower.CL, accuracy = 0.01)
pairwise_df$upper.CL <- number(pairwise_df$upper.CL, accuracy = 0.01)
pairwise_df$estimate <- number(pairwise_df$estimate, accuracy = 0.01)
pairwise_df$adj.p.value <- number(pairwise_df$adj.p.value, accuracy = 0.01)
pairwise_df$t.ratio <- number(pairwise_df$t.ratio, accuracy = 0.01)
pairwise_df$df <- number(pairwise_df$df)
pairwise_df['formatted.result'] <- mapply(format_contrast_results, pairwise_df$estimate,
                                          pairwise_df$lower.CL, pairwise_df$upper.CL,
                                          pairwise_df$t.ratio, pairwise_df$adj.p.value,
                                          pairwise_df$df, return_md=TRUE)
h3a_cond = "Women Male scientist Pictured - Men Male scientist Pictured"
h3b_cond = "Women Female scientist Not pictured - Women Female scientist Pictured"
h3c_cond = "Women Male scientist Pictured - Women Female scientist Pictured"

h3a_table = pairwise_df %>%
  dplyr::filter(new.contrast == h3a_cond) %>%
  dplyr::filter(adj.p.value < 0.1) %>%
  dplyr::select(outcome, new.contrast, formatted.result)
h3a_sigoutcomes = h3a_table$outcome
h3a_table

h3b_table = pairwise_df %>%
  dplyr::filter(new.contrast == h3b_cond) %>%
  dplyr::filter(adj.p.value < 0.1) %>%
  dplyr::select(outcome, new.contrast, formatted.result)
h3b_sigoutcomes = h3b_table$outcome
h3b_table

h3c_table = pairwise_df %>%
  dplyr::filter(new.contrast == h3c_cond) %>%
  dplyr::filter(adj.p.value < 0.1) %>%
  dplyr::select(outcome, new.contrast, formatted.result)
h3c_sigoutcomes = h3c_table$outcome
h3c_table

#### Make and save the two-panel figure ####

career_interest <- marginal.means$z.lead.all %>%
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
  ggtitle("STEM Career Interest by Condition")+
  ylab("STEM Career Interest")+
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
    legend.key = element_rect(fill="white")
  )

stem_id <- marginal.means$z.dd.id.all %>%
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
  ggtitle("Identification with STEM by Condition")+
  ylab("Identification with STEM")+
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
    legend.key = element_rect(fill="white")
  )

figure <- ggarrange(career_interest, stem_id,
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1,
                    legend = "bottom",
                    common.legend = TRUE)



