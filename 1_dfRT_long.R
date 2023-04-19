#STEP 1: create long format df and vars#
set.seed(12345)
library(readr)
library(dplyr)
library(tidyr)
library(rio)
library(gtools)
library(reshape)
library(ggplot2)

data <- read.delim("spr.txt", sep=";", dec=",") #pre-processed data framework
summary(data)

data$SUBJ <- factor(data$SUBJ, 
                    levels = c("1", "2", "3", "4", "5", "6", "7", "8",
                               "9", "10", "11", "12", "13", "14", "15", "16",
                               "17", "18", "19", "20", "21", "22", "23", "24",
                               "25", "26", "27", "28", "29", "30", "31", "32",
                               "33", "34")
                    )
data$region <- factor(data$region, 
                      levels = c("adv_t", "contr", "N1", "det_N2", "N2",
                                 "IC_preTARGET1", "prep_IC1", "N3", "prep_IC2",
                                 "det_N3", "N4", "POL_preTARGET2", "corr_N3",
                                 "verb_aux", "verb_pp", "det_preTARGET3", "N5_TARGET"),
                      labels = c("ADV", "CONTR", "N1", "N2DET", "N2",
                                 "IC", "pIC1", "N3", "pIC2", 
                                 "N3DET", "N4", "POL", "N3COREF", 
                                 "AUX", "ppVERB", "preN5CRI", "N5CRI")
                      )
data$wd_number <- factor(data$wd_number,
                         levels = c("1", "2", "3", "4", "5", "6", "7", "8",
                                    "9", "10", "11", "12", "13", "14", "15", "16",
                                    "17")
                         )
data$ITEM <- factor(data$variable, 
                    levels = c("s1", "s10", "s19", "s28", "s5", "s14", "s23", "s32",
                               "s9", "s18", "s27", "s4", "s13", "s22", "s31", "s8",
                               "s17", "s26", "s3", "s12", "s21", "s30", "s7", "s16",
                               "s25", "s2", "s11", "s20", "s29", "s6", "s15", "s24")
                    )
data$PRED <- factor(data$variable, 
                    levels = c("s1", "s10", "s19", "s28", "s5", "s14", "s23", "s32",
                               "s9", "s18", "s27", "s4", "s13", "s22", "s31", "s8",
                               "s17", "s26", "s3", "s12", "s21", "s30", "s7", "s16",
                               "s25", "s2", "s11", "s20", "s29", "s6", "s15", "s24")
)
data$COND <- factor(data$variable, 
                    levels = c("s1", "s10", "s19", "s28", "s5", "s14", "s23", "s32",
                               "s9", "s18", "s27", "s4", "s13", "s22", "s31", "s8",
                               "s17", "s26", "s3", "s12", "s21", "s30", "s7", "s16",
                               "s25", "s2", "s11", "s20", "s29", "s6", "s15", "s24")
)

#create subsets per predictability#
data$PRED
levels(data$PRED)[levels(data$PRED) == "s1"] <- "hi"
levels(data$PRED)[levels(data$PRED) == "s2"] <- "lo"
levels(data$PRED)[levels(data$PRED) == "s3"] <- "hi"
levels(data$PRED)[levels(data$PRED) == "s4"] <- "lo"
levels(data$PRED)[levels(data$PRED) == "s5"] <- "hi"
levels(data$PRED)[levels(data$PRED) == "s6"] <- "lo"
levels(data$PRED)[levels(data$PRED) == "s7"] <- "hi"
levels(data$PRED)[levels(data$PRED) == "s8"] <- "lo"
levels(data$PRED)[levels(data$PRED) == "s9"] <- "hi"
levels(data$PRED)[levels(data$PRED) == "s10"] <- "lo"
levels(data$PRED)[levels(data$PRED) == "s11"] <- "hi"
levels(data$PRED)[levels(data$PRED) == "s12"] <- "lo"
levels(data$PRED)[levels(data$PRED) == "s13"] <- "hi"
levels(data$PRED)[levels(data$PRED) == "s14"] <- "lo"
levels(data$PRED)[levels(data$PRED) == "s15"] <- "hi"
levels(data$PRED)[levels(data$PRED) == "s16"] <- "lo"
levels(data$PRED)[levels(data$PRED) == "s17"] <- "hi"
levels(data$PRED)[levels(data$PRED) == "s18"] <- "lo"
levels(data$PRED)[levels(data$PRED) == "s19"] <- "hi"
levels(data$PRED)[levels(data$PRED) == "s20"] <- "lo"
levels(data$PRED)[levels(data$PRED) == "s21"] <- "hi"
levels(data$PRED)[levels(data$PRED) == "s22"] <- "lo"
levels(data$PRED)[levels(data$PRED) == "s23"] <- "hi"
levels(data$PRED)[levels(data$PRED) == "s24"] <- "lo"
levels(data$PRED)[levels(data$PRED) == "s25"] <- "hi"
levels(data$PRED)[levels(data$PRED) == "s26"] <- "lo"
levels(data$PRED)[levels(data$PRED) == "s27"] <- "hi"
levels(data$PRED)[levels(data$PRED) == "s28"] <- "lo"
levels(data$PRED)[levels(data$PRED) == "s29"] <- "hi"
levels(data$PRED)[levels(data$PRED) == "s30"] <- "lo"
levels(data$PRED)[levels(data$PRED) == "s31"] <- "hi"
levels(data$PRED)[levels(data$PRED) == "s32"] <- "lo"

#create subsets per condition#
data$COND
levels(data$COND)[levels(data$COND) == "s1"] <- "c1"
levels(data$COND)[levels(data$COND) == "s2"] <- "c1"
levels(data$COND)[levels(data$COND) == "s3"] <- "c1"
levels(data$COND)[levels(data$COND) == "s4"] <- "c1"
levels(data$COND)[levels(data$COND) == "s5"] <- "c1"
levels(data$COND)[levels(data$COND) == "s6"] <- "c1"
levels(data$COND)[levels(data$COND) == "s7"] <- "c1"
levels(data$COND)[levels(data$COND) == "s8"] <- "c1"
levels(data$COND)[levels(data$COND) == "s9"] <- "c2"
levels(data$COND)[levels(data$COND) == "s10"] <- "c2"
levels(data$COND)[levels(data$COND) == "s11"] <- "c2"
levels(data$COND)[levels(data$COND) == "s12"] <- "c2"
levels(data$COND)[levels(data$COND) == "s13"] <- "c2"
levels(data$COND)[levels(data$COND) == "s14"] <- "c2"
levels(data$COND)[levels(data$COND) == "s15"] <- "c2"
levels(data$COND)[levels(data$COND) == "s16"] <- "c2"
levels(data$COND)[levels(data$COND) == "s17"] <- "c3"
levels(data$COND)[levels(data$COND) == "s18"] <- "c3"
levels(data$COND)[levels(data$COND) == "s19"] <- "c3"
levels(data$COND)[levels(data$COND) == "s20"] <- "c3"
levels(data$COND)[levels(data$COND) == "s21"] <- "c3"
levels(data$COND)[levels(data$COND) == "s22"] <- "c3"
levels(data$COND)[levels(data$COND) == "s23"] <- "c3"
levels(data$COND)[levels(data$COND) == "s24"] <- "c3"
levels(data$COND)[levels(data$COND) == "s25"] <- "c4"
levels(data$COND)[levels(data$COND) == "s26"] <- "c4"
levels(data$COND)[levels(data$COND) == "s27"] <- "c4"
levels(data$COND)[levels(data$COND) == "s28"] <- "c4"
levels(data$COND)[levels(data$COND) == "s29"] <- "c4"
levels(data$COND)[levels(data$COND) == "s30"] <- "c4"
levels(data$COND)[levels(data$COND) == "s31"] <- "c4"
levels(data$COND)[levels(data$COND) == "s32"] <- "c4"

#create subsets per ADV polarity#
data$POL <- factor(data$COND)
levels(data$POL)[levels(data$POL) == "c1"] <- "pos"
levels(data$POL)[levels(data$POL) == "c2"] <- "neg"
levels(data$POL)[levels(data$POL) == "c3"] <- "pos"
levels(data$POL)[levels(data$POL) == "c4"] <- "neg"

#create subsets per IC#
data$IC <- factor(data$COND)
levels(data$IC)[levels(data$IC) == "c1"] <- "pos"
levels(data$IC)[levels(data$IC) == "c2"] <- "neg"
levels(data$IC)[levels(data$IC) == "c3"] <- "pos"
levels(data$IC)[levels(data$IC) == "c4"] <- "neg"

filenew = "SPR_long_v2.txt"
write.table(data, file=filenew, sep=";", dec=",", na="", row.names=FALSE)
write.csv2(data, file = "SPR_long_v2.csv")


