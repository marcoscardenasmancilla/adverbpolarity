##Prep data for variable exploration and plotting
# Packages
library(emmeans)
library(broom.mixed)
library(dotwhisker)
library(foreign)
library(intsvy)
library(dplyr)
library(ggplot2)
library(tidyr)
library(rms)
library(lme4)
library(lmerTest)
library(GGally)
library(lattice)
library(MASS) 
library(mvoutlier)
library(car)
library(MASS) 
library(sm)
library(reshape)
library(plyr)
library(Hmisc)
library(psych) #- For function describe.
library(foreign) #- reads SPSS files
library(nlme)
library(mice)
library(Rmisc)


data <- read.csv2("data_v6.csv", header = TRUE) #trimmed DF at verb, adverb and target words
data

###Scale vars
data$wd_number <- factor(data$wd_number, levels = c("w1", "w2", "w3", "w4", "w5", "w6",
                                                    "w7", "w8", "w9", "w10", "w11", "w12",
                                                    "w13", "w14", "w15", "w16", "w17"))

data$subj <- factor(data$subj, levels = c("subj1", "subj2", "subj3", "subj4", "subj5", "subj6",
                                          "subj7", "subj8", "subj9", "subj10", "subj11", "subj12",
                                          "subj13", "subj14", "subj15", "subj16", "subj17", "subj18",
                                         "subj19", "subj20", "subj21", "subj22"))

data$condition <- factor(data$condition, levels = c("A", "B", "C", "D", "E", "F",
                                                    "G", "H"))

data$verb_polarity <- factor(data$verb_polarity, levels = c("pos", "neg"))

data$adv_polarity <- factor(data$adv_polarity, levels = c("pos", "neg"))

data$target_pred <- factor(data$target_pred, levels = c("high", "low")) # target word predictability as CATEGORICAL var

data$Target_pred <- as.numeric(data$target_pred, levels = c("high", "low"))

data$Target_pred <- recode(data$Target_pred, "1=0.5 ; 2=-0.5") # target word predictability as continuous var

data[2] <- lapply(data[2], function(x) as.factor(as.character(x)))

data[6] <- lapply(data[6], function(x) as.numeric(as.integer(x)))

data$WD_length <- (data$wd_length*1000)

data$logwd_length <- log(data$WD_length)

data$center_wdlength <- scale(data$WD_length, center= TRUE, scale = FALSE)

data$z_wdlength <- scale(data$WD_length, center= TRUE, scale = TRUE)

hist(data$center_wdlength)

hist(data$logwd_length)

data[10:14] <- lapply(data[10:14], function(x) as.numeric(as.character(x)))

data$RT <- (data$rt*1000)

hist(data$RT)

data$logRT <- log(data$RT)

hist(data$logRT)

###Create control predictors
data$Target_freq <-(data$target_freq*1000)# Target frequency
data$condtargetfreq <-(data$Target_freqdata$condition)
data$logtargetfreq <-log(data$Target_freq)
data$centertargetfreq <-scale(data$Target_freq, center=TRUE, scale=FALSE)

data$Adv_freq <-(data$adv_freq*1000)# Adverb frequency
data$logadvfreq <-log(data$Adv_freq)
data$centeradvfreq <-scale(data$Adv_freq, center=TRUE, scale=FALSE)

data$Verb_freq <-(data$verb_freq*1000)# Verb frequency
data$logverbfreq <-log(data$Verb_freq)
data$centeradvfreq <-scale(data$Verb_freq, center=TRUE, scale=FALSE)

write.csv(data,"C:/Users/marco/Desktop/Tesis/paper/R_analyses/adverbpolarity/datafull.csv", row.names = FALSE)

###Filter target data per condition and estimate mean/median Target Freq

w17 <- filter(data, wd_number %in% c("w17")) #filter target
w12 <- filter(data, wd_number %in% c("w12")) #adverb filter
w6 <- filter(data, wd_number %in% c("w6")) #verb filter
w11 <- filter(data, wd_number %in% c("w11")) #adverb -1 filter

View(w17)
View(w12)
View(w6)
View(w11)

###Plotting interactions

pd <- position_dodge(0.1) # move them .05 to the left and right, # The errorbars overlapped, so use position_dodge to move them horizontally

# Theme
My_Theme = theme(
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text.y = element_text(size = 16))

##Plotting interactions per TARGET level
##Data target level HIGH
target.high <- filter(w17, target_pred %in% c("high")) #target high
sum.target.high <- summarySE(target.high, measurevar="RT", groupvars=c("verb_polarity", "adv_polarity"))
#Figures target level HIGH
w17.high.verb.plot <- ggplot(sum.target.high, aes(x=adv_polarity, y=RT, color = verb_polarity, fill = verb_polarity, group=verb_polarity)) + 
  geom_errorbar(aes(ymin=RT-se, ymax=RT+se), size=1.2, colour="black", width=.2, position=pd) + 
  geom_line(position=pd, size=1.2) +
  geom_point(aes(shape = verb_polarity), position=pd, size=5, fill="white") + # 21 is filled circle
  xlab("Adverb Polarity") +
  ylab("RT (ms)") +
  expand_limits(y=400:600) +                        # Expand y range
  scale_y_continuous(breaks = seq(400, 600, by = 50)) +       # -10 y 10 Set tick every 2
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.justification=c(1,0), legend.position=c(.95,0.05))
w17.high.verb.plot
dev.print(png, "w17.high.verb.plot.png", res=900, height=8, width=16, units="in")

w17.high.adv.plot <- ggplot(sum.target.high, aes(x=verb_polarity, y=RT, color = adv_polarity, fill = adv_polarity, group=adv_polarity)) + 
  geom_errorbar(aes(ymin=RT-se, ymax=RT+se), size=1.2, colour="black", width=.2, position=pd) + 
  geom_line(position=pd, size=1.2) +
  geom_point(aes(shape = adv_polarity), position=pd, size=5, fill="white") + # 21 is filled circle
  xlab("Verb Polarity") +
  ylab("RT (ms)") +
  expand_limits(y=400:600) +                        # Expand y range
  scale_y_continuous(breaks = seq(400, 600, by = 50)) +       # -10 y 10 Set tick every 2
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.justification=c(1,0), legend.position=c(.95,0.05))
w17.high.adv.plot
dev.print(png, "w17.high.adv.plot.png", res=900, height=8, width=16, units="in")

##Data target level LOW
target.low <- filter(w17, target_pred %in% c("low")) #target low
sum.target.low <- summarySE(target.low, measurevar="RT", groupvars=c("verb_polarity", "adv_polarity"))
#Figures target level LOW
w17.low.verb.plot <- ggplot(sum.target.low, aes(x=adv_polarity, y=RT, color = verb_polarity, fill = verb_polarity, group=verb_polarity)) + 
  geom_errorbar(aes(ymin=RT-se, ymax=RT+se), size=1.2, colour="black", width=.2, position=pd) + 
  geom_line(position=pd, size=1.2) +
  geom_point(aes(shape = verb_polarity), position=pd, size=5, fill="white") + # 21 is filled circle
  xlab("Adverb Polarity") +
  ylab("RT (ms)") +
  expand_limits(y=400:600) +                        # Expand y range
  scale_y_continuous(breaks = seq(400, 600, by = 50)) +       # -10 y 10 Set tick every 2
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.justification=c(1,0), legend.position=c(.95,0.05))
w17.low.verb.plot
dev.print(png, "w17.low.verb.plot.png", res=900, height=8, width=16, units="in")

w17.low.adv.plot <- ggplot(sum.target.low, aes(x=verb_polarity, y=RT, color = adv_polarity, fill = adv_polarity, group=adv_polarity)) + 
  geom_errorbar(aes(ymin=RT-se, ymax=RT+se), size=1.2, colour="black", width=.2, position=pd) + 
  geom_line(position=pd, size=1.2) +
  geom_point(aes(shape = adv_polarity), position=pd, size=5, fill="white") + # 21 is filled circle
  xlab("Verb Polarity") +
  ylab("RT (ms)") +
  expand_limits(y=400:600) +                        # Expand y range
  scale_y_continuous(breaks = seq(400, 600, by = 50)) +       # -10 y 10 Set tick every 2
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.justification=c(1,0), legend.position=c(.95,0.05))
w17.low.adv.plot
dev.print(png, "w17.low.adv.plot.png", res=900, height=8, width=16, units="in")

### Plotting ADVERB interactions ###
##Data adverb polarity
sum.w12 <- summarySE(w12, measurevar="RT", groupvars=c("verb_polarity", "target_pred"))
##Figures adverb POSITIVE AND NEGATIVE
w12.verb.plot <- ggplot(sum.w12, aes(x=target_pred, y=RT, color = verb_polarity, fill = verb_polarity, group=verb_polarity)) + 
  geom_errorbar(aes(ymin=RT-se, ymax=RT+se), size=1.2, colour="black", width=.2, position=pd) + 
  geom_line(position=pd, size=1.2) +
  geom_point(aes(shape = verb_polarity), position=pd, size=5, fill="white") + # 21 is filled circle
  xlab("Target Word Predictability") +
  ylab("RT (ms)") +
  expand_limits(y=400:600) +                        # Expand y range
  scale_y_continuous(breaks = seq(400, 600, by = 50)) +       # -10 y 10 Set tick every 2
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.justification=c(1,0), legend.position=c(.95,0.05))
w12.verb.plot
dev.print(png, "w12.verb.plot.png", res=900, height=8, width=16, units="in")
dev.print(tiff, "w12.verb.plot.tiff", res=300, height=5, width=8, units="in") # to save figures in your folder. 

w12.target.plot <- ggplot(sum.w12, aes(x=verb_polarity, y=RT, colour=target_pred, fill= target_pred, group=target_pred)) + 
  geom_errorbar(aes(ymin=RT-se, ymax=RT+se), size=1.2, colour="black", width=.2, position=pd) + 
  geom_line(position=pd, size=1.2) +
  geom_point(aes(shape = target_pred), position=pd, size=5, fill="white") + # 21 is filled circle
  xlab("Verb Polarity") +
  ylab("RT (ms)") +
  expand_limits(y=400:600) +                        # Expand y range
  scale_y_continuous(breaks = seq(400, 600, by = 50)) +       # -10 y 10 Set tick every 2
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.justification=c(1,0), legend.position=c(.95,0.05))
w12.target.plot
dev.print(png, "w12.target.plot.png", res=900, height=8, width=16, units="in")
dev.print(tiff, "w12.target.plot.tiff", res=300, height=5, width=8, units="in") # to save figures in your folder. 

### Plotting interactions per adverb level
##Data adverb polarity level POSITIVE
adv.pos <- filter(w12, adv_polarity %in% c("pos")) #adverb POSITIVE "Claramente"
sum.adv.pos <- summarySE(adv.pos, measurevar="RT", groupvars=c("verb_polarity", "target_pred"))
#Figures adverb polarity POSITVE
w12.pos.verb.plot <- ggplot(sum.adv.pos, aes(x=target_pred, y=RT, color = verb_polarity, fill = verb_polarity, group=verb_polarity)) + 
  geom_errorbar(aes(ymin=RT-se, ymax=RT+se), size=1.2, colour="black", width=.2, position=pd) + 
  geom_line(position=pd, size=1.2) +
  geom_point(aes(shape = verb_polarity), position=pd, size=5, fill="white") + # 21 is filled circle
  xlab("Target Word Predictability") +
  ylab("RT (ms)") +
  expand_limits(y=400:600) +                        # Expand y range
  scale_y_continuous(breaks = seq(400, 600, by = 50)) +       # -10 y 10 Set tick every 2
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.justification=c(1,0), legend.position=c(.95,0.05))
w12.pos.verb.plot
dev.print(png, "w12.pos.verb.plot.png", res=900, height=8, width=16, units="in")

w12.pos.target.plot <- ggplot(sum.adv.pos, aes(x=verb_polarity, y=RT, color = target_pred, fill = target_pred, group=target_pred)) + 
  geom_errorbar(aes(ymin=RT-se, ymax=RT+se), size=1.2, colour="black", width=.2, position=pd) + 
  geom_line(position=pd, size=1.2) +
  geom_point(aes(shape = target_pred), position=pd, size=5, fill="white") + # 21 is filled circle
  xlab("Verb-based Implicit Causality") +
  ylab("RT (ms)") +
  expand_limits(y=450:600) +                        # Expand y range
  scale_y_continuous(breaks = seq(450, 600 , by = 10)) +       # -10 y 10 Set tick every 2
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.justification=c(1,0), legend.position=c(.95,0.05))
w12.pos.target.plot
dev.print(png, "w12.pos.target.plot.png", res=900, height=8, width=16, units="in")

##Data adverb polarity level NEGATIVE
adv.neg <- filter(w12, adv_polarity %in% c("neg")) #adverb NEGATIVE "lamentablemente"
sum.adv.neg <- summarySE(adv.neg, measurevar="RT", groupvars=c("verb_polarity", "target_pred"))
#Figures adverb polarity NEGATIVE
w12.neg.verb.plot <- ggplot(sum.adv.neg, aes(x=target_pred, y=RT, color = verb_polarity, fill = verb_polarity, group=verb_polarity)) + 
  geom_errorbar(aes(ymin=RT-se, ymax=RT+se), size=1.2, colour="black", width=.2, position=pd) + 
  geom_line(position=pd, size=1.2) +
  geom_point(aes(shape = verb_polarity), position=pd, size=5, fill="white") + # 21 is filled circle
  xlab("Target Word Predictability") +
  ylab("RT (ms)") +
  expand_limits(y=450:600) +                        # Expand y range
  scale_y_continuous(breaks = seq(450, 600 , by = 10)) +       # -10 y 10 Set tick every 2
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.justification=c(1,0), legend.position=c(.95,0.05))
w12.neg.verb.plot
dev.print(png, "w12.neg.verb.plot.png", res=900, height=8, width=16, units="in")

w12.neg.target.plot <- ggplot(sum.adv.neg, aes(x=verb_polarity, y=RT, color = target_pred, fill = target_pred, group=target_pred)) + 
  geom_errorbar(aes(ymin=RT-se, ymax=RT+se), size=1.2, colour="black", width=.2, position=pd) + 
  geom_line(position=pd, size=1.2) +
  geom_point(aes(shape = target_pred), position=pd, size=5, fill="white") + # 21 is filled circle
  xlab("Verb-based Implicit Causality") +
  ylab("RT (ms)") +
  expand_limits(y=450:600) +                        # Expand y range
  scale_y_continuous(breaks = seq(450, 600 , by = 10)) +       # -10 y 10 Set tick every 2
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.justification=c(1,0), legend.position=c(.95,0.05))
w12.neg.target.plot #COND3 and #COND4
dev.print(png, "w12.neg.target.plot.png", res=900, height=8, width=16, units="in")


### Plotting VERB interactions ###
##Data verb polarity
sum.w6 <- summarySE(w6, measurevar="RT", groupvars=c("adv_polarity", "target_pred"))
##Figures verb POSITIVE AND NEGATIVE
w6.target.plot <- ggplot(sum.w6, aes(x=adv_polarity, y=RT, color = target_pred, fill = target_pred, group=target_pred)) + 
  geom_errorbar(aes(ymin=RT-se, ymax=RT+se), size=1.2, colour="black", width=.2, position=pd) + 
  geom_line(position=pd, size=1.2) +
  geom_point(aes(shape = Target_pred), position=pd, size=5, fill="white") + # 21 is filled circle
  xlab("Adverb Polarity") +
  ylab("RT (ms)") +
  expand_limits(y=350:500) +                        # Expand y range
  scale_y_continuous(breaks = seq(350, 500 , by = 10)) +       # -10 y 10 Set tick every 2
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.justification=c(1,0), legend.position=c(.95,0.05))
w6.target.plot
dev.print(png, "w6.target.plot.png", res=900, height=8, width=16, units="in")
dev.print(tiff, "w6.target.plot.tiff", res=300, height=5, width=8, units="in") # to save figures in your folder. 

w6.adv.plot <- ggplot(sum.w6, aes(x=Target_pred, y=RT, colour=adv_polarity, fill=adv_polarity, group=adv_polarity)) + 
  geom_errorbar(aes(ymin=RT-se, ymax=RT+se), size=1.2, colour="black", width=.2, position=pd) + 
  geom_line(position=pd, size=1.2) +
  geom_point(aes(shape = adv_polarity), position=pd, size=5, fill="white") + # 21 is filled circle
  xlab("Target Word Predictability") +
  ylab("RT (ms)") +
  expand_limits(y=350:500) +                        # Expand y range
  scale_y_continuous(breaks = seq(350, 500, by = 50)) +       # -10 y 10 Set tick every 2
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.justification=c(1,0), legend.position=c(.95,0.05))
w6.adv.plot
dev.print(png, "w6.adv.plot.png", res=900, height=8, width=16, units="in")
dev.print(tiff, "w6.adv.plot.tiff", res=300, height=5, width=8, units="in") # to save figures in your folder. 

### Plotting interactions per verb level
##Data verb polarity level POSITIVE
verb.pos <- filter(w6, verb_polarity %in% c("pos")) #verb POSITIVE "felicitar"
sum.verb.pos <- summarySE(verb.pos, measurevar="RT", groupvars=c("adv_polarity", "target_pred"))
#Figures verb polarity POSITVE
w6.pos.adv.plot <- ggplot(sum.verb.pos, aes(x=target_pred, y=RT, color = adv_polarity, fill = adv_polarity, group=adv_polarity)) + 
  geom_errorbar(aes(ymin=RT-se, ymax=RT+se), size=1.2, colour="black", width=.2, position=pd) + 
  geom_line(position=pd, size=1.2) +
  geom_point(aes(shape = adv_polarity), position=pd, size=5, fill="white") + # 21 is filled circle
  xlab("Target Word Predictability") +
  ylab("RT (ms)") +
  expand_limits(y=350:500) +                        # Expand y range
  scale_y_continuous(breaks = seq(350, 500, by = 50)) +       # -10 y 10 Set tick every 2
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.justification=c(1,0), legend.position=c(.95,0.05))
w6.pos.adv.plot
dev.print(png, "w6.pos.adv.plot.png", res=900, height=8, width=16, units="in")

w6.pos.target.plot <- ggplot(sum.verb.pos, aes(x=adv_polarity, y=RT, color = target_pred, fill = target_pred, group=target_pred)) + 
  geom_errorbar(aes(ymin=RT-se, ymax=RT+se), size=1.2, colour="black", width=.2, position=pd) + 
  geom_line(position=pd, size=1.2) +
  geom_point(aes(shape = target_pred), position=pd, size=5, fill="white") + # 21 is filled circle
  xlab("Adverb polarity") +
  ylab("RT (ms)") +
  expand_limits(y=350:500) +                        # Expand y range
  scale_y_continuous(breaks = seq(350, 500, by = 50)) +       # -10 y 10 Set tick every 2
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.justification=c(1,0), legend.position=c(.95,0.05))
 w6.pos.target.plot
 dev.print(png, "w6.pos.target.plot.png", res=900, height=8, width=16, units="in")

##Data verb polarity level NEGATIVE
verb.neg <- filter(w6, verb_polarity %in% c("neg")) #verb NEGATIVE "reprochar"
sum.verb.neg <- summarySE(verb.neg, measurevar="RT", groupvars=c("adv_polarity", "target_pred"))
#Figures verb polarity NEGATIVE
w6.neg.adv.plot <- ggplot(sum.verb.neg, aes(x=target_pred, y=RT, color = adv_polarity, fill = adv_polarity, group=adv_polarity)) + 
  geom_errorbar(aes(ymin=RT-se, ymax=RT+se), size=1.2, colour="black", width=.2, position=pd) + 
  geom_line(position=pd, size=1.2) +
  geom_point(aes(shape = adv_polarity), position=pd, size=5, fill="white") + # 21 is filled circle
  xlab("Target Word Predictability") +
  ylab("RT (ms)") +
  expand_limits(y=350:500) +                        # Expand y range
  scale_y_continuous(breaks = seq(350, 500, by = 50)) +       # -10 y 10 Set tick every 2
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.justification=c(1,0), legend.position=c(.95,0.05))
w6.neg.adv.plot
dev.print(png, "w6.neg.adv.plot.png", res=900, height=8, width=16, units="in")

w6.neg.target.plot <- ggplot(sum.verb.neg, aes(x=adv_polarity, y=RT, color = target_pred, fill = target_pred, group=target_pred)) + 
  geom_errorbar(aes(ymin=RT-se, ymax=RT+se), size=1.2, colour="black", width=.2, position=pd) + 
  geom_line(position=pd, size=1.2) +
  geom_point(aes(shape = target_pred), position=pd, size=5, fill="white") + # 21 is filled circle
  xlab("Adverb polarity") +
  ylab("RT (ms)") +
  expand_limits(y=350:500) +                        # Expand y range
  scale_y_continuous(breaks = seq(350, 500, by = 50)) +       # -10 y 10 Set tick every 2
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.justification=c(1,0), legend.position=c(.95,0.05))
w6.neg.target.plot
dev.print(png, "w6.neg.target.plot.png", res=900, height=8, width=16, units="in")
