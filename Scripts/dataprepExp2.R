### loading all the datafiles

library(tidyverse)
library(plyr)

dir <- setwd("D:/PhD/Experiments/Exp2 Overshadowing/Data/Exp2 Prolific")
allOldata <- list.files(path = dir, pattern = "*.csv")

Exp2data <- lapply(allOldata, read_csv)

#ysing rbind.fill because some columns do not exist in some datafiles, namely the "preResp.key" 
Exp2 <- do.call(rbind.fill,Exp2data)

write.csv(Exp2, file = "Exp2_fulldataset.csv")
