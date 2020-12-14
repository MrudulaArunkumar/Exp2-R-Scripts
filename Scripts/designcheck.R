###CHECKING THE DESIGN BALANCE####
library(tidyverse)
library(plyr)
library(ez)
library(schoRsch)


withWord_data1 <- rbind(withW1,withW2)
attach(withWord_data1)

table(Block,Condition)
table(Block,Condition,Validity)

withWord_data1 <- withWord_data1 %>%
  select(-consentkey.keys,-consentkey.rt,-beginexp.rt,-beginexp.keys,-checkresp.corr,-checkresp.rt,-checkresp.keys,
         -Attention.ran,-Attention.thisIndex,-Attention.thisN,-Attention.thisTrialN,-Attention.thisRepN,
         -Question,-Solution,-gotoPrac.rt,-gotoPrac.keys,-InstRep.ran,-InstRep.thisIndex,-InstRep.thisN,-InstRep.thisTrialN,-InstRep.thisRepN,
         -Prac_start.rt,-Prac_start.keys,-prctrials.ran,-prctrials.thisIndex,-prctrials.thisN,-prctrials.thisRepN,-prctrials.thisRepN,
         -pracend.keys,-pracend.rt,-PracRepeat.ran,-PracRepeat.thisIndex,-PracRepeat.thisN,-PracRepeat.thisRepN,-PracRepeat.thisTrialN)

#removing unwanted columns in the experimental side 
withWord_data1 <- withWord_data1 %>%
  select(-firstlearntrials.ran,-firstlearntrials.thisIndex,-firstlearntrials.thisRepN,-firstlearntrials.thisTrialN,
         -brkcontinue.keys,-Exptrials.ran,-Exptrials.thisIndex,-Exptrials.thisTrialN, - Exptrials.thisRepN,
         -afterpause.keys,-blocks.ran,-blocks.thisIndex,-blocks.thisTrialN,-blocks.thisRepN,
         -CAproceed.rt,-CAproceed.keys,-ContAwareness.ran,-ContAwareness.thisIndex,-ContAwareness.thisN,-ContAwareness.thisTrialN,-ContAwareness.thisRepN,
         -Questionnaire.ran,-Questionnaire.thisIndex,-Questionnaire.thisTrialN,-Questionnaire.thisRepN,-todebrief.keys,-ExpExit.keys)

withWord_data1 <- withWord_data1%>%group_by(participant)%>%fill(Screen_bg,.direction = "down")

withWord_data1 <- withWord_data1 %>% group_by(participant)%>%fill(blocks.thisN,.direction = "up")
withWord_data1 <- rename(withWord_data1, c("BlockCount" = "blocks.thisN"))




withWord_data1 <- separate(withWord_data1, col = ResponseKey.rt, into = c("RT_Trials", "RT_secondary"), sep = ',')
withWord_data1$RT_Trials <- withWord_data1$RT_Trials%>%
  str_replace_all("\\[|\\]","")%>%
  as.double(withWord_data1$RT_Trials)
withWord_data1$RT_Trials <- 1000*(withWord_data1$RT_Trials)
withWord_data1$PreTargetDisplayTime <- 1000*(withWord_data1$PreTargetDisplayTime)

withWord_data1 <- withWord_data1%>%drop_na(RT_Trials)
mean(withWord_data1$RT_Trials)
summary(withWord_data1$RT_Trials)

withWord_data1$ACC_trials <- withWord_data1$ResponseKey.corr
withWord_data1$ErrorRate <- 1 - withWord_data1$ACC_trials


#removing practice and learn trials
withWord_data1 <- withWord_data1[!grepl("Practice","HeadStartLearn", withWord_data1$Block),]

#get newly created vars to start of dataset
withWord_data1<-withWord_data1 %>%
  select(RT_Trials, ACC_trials, ErrorRate, everything())

table(withWord_data1$ACC_trials)


withWord_data1$RT_Trials[withWord_data1$ACC_trials==0] <- NA


#creating function to remove the outliers and farouts
computeTukeys <- function(x){
  P25 <- quantile(x$RT_Trials, .25, na.rm = TRUE, type = 6) #type = 6 -> used in SPSS
  P75 <- quantile(x$RT_Trials, .75, na.rm = TRUE, type = 6)
  x$Outlier <- P75 + 1.5*(P75 - P25)
  x$Farouts <- P75 + 3.0*(P75 - P25)
  return(x)
}


#identifying the outliers and farouts at individual level
withWord_data1 <- ddply(withWord_data1, .(participant), computeTukeys)

#creating new column with RT trials after removing outliers/farouts
withWord_data1$RT_ifo <- withWord_data1$RT_Trials
withWord_data1$RT_io <- withWord_data1$RT_Trials
withWord_data1$RT_ifo[withWord_data1$RT_ifo > withWord_data1$Farouts|withWord_data1$RT_ifo < 300] <- NA
withWord_data1$RT_io[withWord_data1$RT_io > withWord_data1$Outlier|withWord_data1$RT_io < 300] <- NA

summary(withWord_data1$RT_ifo)
summary(withWord_data1$RT_io)


mean_agg_fo <- aggregate(data = withWord_data1, RT_ifo~participant+Condition+Saliency+Validity,mean)

pilotWord_fo <- ezANOVA(data = mean_agg_fo,
                     dv = RT_ifo,
                     wid = participant,
                     within = .(Validity,Saliency),
                    detailed = TRUE)
anova_out(pilotWord_fo)
