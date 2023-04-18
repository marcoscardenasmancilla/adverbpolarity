library(readxl)
library(colorspace)
library(plotrix)
library(tidyr)
library(matrixStats)
library(doBy)
library(ggplot2)
library(reshape)
library(plyr)
library(dplyr)
library(stringi)
library(Rmisc)
library(MASS)
library(Matrix)
library(ggpubr)
library(gridExtra)
library(lme4)
library(lmerTest)

df.spr <- read.delim("df.spr.txt", sep=";", dec=",") #pre-processed data framework
summary(df.spr)
data <- df.spr
data_lmer <- df.spr[complete.cases(df.spr),]

names(data_lmer)[names(data_lmer) == "IC"] <- "verb"
names(data_lmer)[names(data_lmer) == "POL"] <- "adverb"
names(data_lmer)[names(data_lmer) == "PRED"] <- "noun"

library(dplyr)
# rename the levels of a column
data$verb <- recode(data$verb, "pos" = "positive", "neg" = "negative")
data$adverb <- recode(data$adverb, "pos" = "positive", "neg" = "negative")

word_17 <- filter(data_lmer, region %in% c("N5CRI"))
data1 <- filter(data_lmer, COND %in% c("c2","c4"))
data2 <- filter(data_lmer, COND %in% c("c1","c3"))
data3 <- filter(word_17, COND %in% c("c2","c4"))
data4 <- filter(word_17, COND %in% c("c1","c3"))


sum.data <- summarySEwithin(df.spr, measurevar = "rt", withinvars = c("IC", "POL","PRED", "COND"), na.rm = T)
sum.data1 <- summarySEwithin(data1, measurevar = "rt", withinvars = c("verb", "noun"), na.rm = T)
sum.data3 <- summarySEwithin(data3, measurevar = "rt", withinvars = c("verb", "noun"), na.rm = T)
sum.data4 <- summarySEwithin(data4, measurevar = "rt", withinvars = c("verb", "noun"), na.rm = T)
sum.data2 <- summarySEwithin(data2, measurevar = "rt", withinvars = c("verb", "noun"), na.rm = T)
sum.data1e <- summarySEwithin(data1, measurevar = "rt", withinvars = c("verb", "noun", "ITEM"), na.rm = T)
sum.data5 <- summarySEwithin(df.spr, measurevar = "rt", withinvars = c("IC", "POL","PRED", "COND"), na.rm = T)

#Bar plots
p1 <- ggplot(sum.data1, aes(x=verb, y=rt, fill=noun)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=rt-ci, ymax=rt+ci),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Tiempo de respuesta promedio por condición. Barras de error representan intervalos de confianza.") +
  ylab("Tiempo de respuesta en milisegundos (ms)") +
  scale_fill_hue(name="Predictibilidad", # Legend label, use darker colors
                 breaks=c("hi", "lo"),
                 labels=c("alta", "baja")) +
  coord_cartesian(ylim = c(300, 500)) + # coord_cartesian(xlim = c(5, 20), ylim = (0, 50)) it can be used for both.
  scale_y_continuous(breaks = seq(300, 500, by = 50)) + #- y axis from -0.5 to 0.5 with .1 for breaks.
  theme_bw()
p1

#Bar plots
p2 <- ggplot(sum.data2, aes(x=verb, y=rt, fill=noun)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=rt-ci, ymax=rt+ci),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Tiempo de respuesta promedio por condición. Barras de error representan intervalos de confianza.") +
  ylab("Tiempo de respuesta en milisegundos (ms)") +
  scale_fill_hue(name="Predictibilidad", # Legend label, use darker colors
                 breaks=c("hi", "lo"),
                 labels=c("alta", "baja")) +
  coord_cartesian(ylim = c(300, 500)) + # coord_cartesian(xlim = c(5, 20), ylim = (0, 50)) it can be used for both.
  scale_y_continuous(breaks = seq(300, 500, by = 50)) + #- y axis from -0.5 to 0.5 with .1 for breaks.
  theme_bw()
p2


#Bar plots
p3 <- ggplot(sum.data3, aes(x=verb, y=rt, fill=noun)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=rt-ci, ymax=rt+ci),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Tiempo de respuesta promedio por condición. Barras de error representan intervalos de confianza.") +
  ylab("Tiempo de respuesta en milisegundos (ms)") +
  scale_fill_hue(name="Predictibilidad", # Legend label, use darker colors
                 breaks=c("hi", "lo"),
                 labels=c("alta", "baja")) +
  coord_cartesian(ylim = c(300, 600)) + # coord_cartesian(xlim = c(5, 20), ylim = (0, 50)) it can be used for both.
  scale_y_continuous(breaks = seq(300, 600, by = 50)) + #- y axis from -0.5 to 0.5 with .1 for breaks.
  theme_bw()
p3

#Bar plots
p4 <- ggplot(sum.data4, aes(x=verb, y=rt, fill=noun)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=rt-ci, ymax=rt+ci),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Tiempo de respuesta promedio por condición. Barras de error representan intervalos de confianza.") +
  ylab("Tiempo de respuesta en milisegundos (ms)") +
  scale_fill_hue(name="Predictibilidad", # Legend label, use darker colors
                 breaks=c("hi", "lo"),
                 labels=c("alta", "baja")) +
  coord_cartesian(ylim = c(300, 600)) + # coord_cartesian(xlim = c(5, 20), ylim = (0, 50)) it can be used for both.
  scale_y_continuous(breaks = seq(300, 600, by = 50)) + #- y axis from -0.5 to 0.5 with .1 for breaks.
  theme_bw()
p4

md1 <- lmer(log(rt) ~ verb * noun + (1|SUBJ) + (1|wd_number), data = data1)
summary(md1)
# No effect of verbo inicial (negativo, positivo)
# Effect of sustantivo final (congruente, incongruente con adverbio) 
# interaction verbo inicial * sustantivo final 

df_lmer <- data.frame(rt = data1$rt,
                      SUBJ = data1$SUBJ,
                      wd_number = data1$wd_number,
                      verb = data1$verb,
                      noun = data1$noun)

#view data frame
df_lmer
hist(log(data1$rt))
md1 <- lmer(rt ~ verb * noun + (1|SUBJ) + (1|wd_number), data = df_lmer)
summary(md1)

### Influence analysis
## Run a model excluding one participant at the time
ex.subj <- influence(md1, "SUBJ")
## Calculate D Cook for parameter of interest (VIR:vocabulary, VIR:textcomp, VIA:vocabulary, VIA:textcomp [6,7,8,9])
cookd <- cooks.distance(ex.subj, sort=TRUE)
plot(cookd, which="cookd", xlab ="DCook", ylab = "SUBJ")
# data frame with D Cook
cookd <- data.frame(cookd)
cookd$idpa <- as.numeric(rownames(cookd))
cookd1 <- cookd %>% filter(cookd > 4)
idpac <- c(cookd1$idpa)

# remove influential cases
df_lmer.a <- subset(df_lmer, !SUBJ%in%idpac)
View(df_lmer)
View(df_lmer.a)
md1.fit <- lmer(log(rt) ~ verb * noun + (1|SUBJ) + (1|wd_number), data = df_lmer.a)
summary(md1.fit)

library("jsPlot")
tab_model(
  md1, 
  pred.labels = c("Intercepto", "IC Positiva", "Predictibilidad", 
                  "Efecto IC-Pol-Pred"),
  dv.labels = c("Modelo Interacciones (milisegundos)"),
  string.est = "Estimado",
  string.pred = "Coeficiente",
  string.ci = "Int. Conf. (95%)",
  string.p = "Valor P"
)

md1.lm <- lm(log(rt) ~ verb * noun, data = data1)
summary(md1.lm)
ggplotInfluence(md1.lm)

# Posthoc tests
library(emmeans)
emmeans(md1, pairwise ~ verb:noun)

md2 <- lmer(log(rt) ~ verb * noun + (1|SUBJ) + (1|wd_number), data = data2)
summary(md2)
# No effect of verbo inicial (negativo, positivo)
# Effect of sustantivo final (congruente, incongruente con adverbio) 
# interaction verbo inicial * sustantivo final 

# Posthoc tests
library(emmeans)
emmeans(md2, pairwise ~ verb:noun)

tab_model(
  md2, 
  pred.labels = c("Intercepto", "IC Positiva", "Predictibilidad", 
                  "Efecto IC-Pol(Neg)-Pred"),
  dv.labels = c("Modelo Interacciones (milisegundos)"),
  string.est = "Estimado",
  string.pred = "Coeficiente",
  string.ci = "Int. Conf. (95%)",
  string.p = "Valor P"
)

md3 <- lmer(rt ~ verb * noun + (1|SUBJ) + (1|ITEM), data = data3)
summary(md3)

md4 <- lmer(rt ~ verb * noun + (1|SUBJ) + (1|ITEM), data = data4)
summary(md4)


#Line plot!
pd <- position_dodge(0.2) # move them .05 to the left and right
p1_line <- ggplot(sum.data1, aes(x=verb, y=rt, colour=noun, group=noun)) + 
  geom_errorbar(aes(ymin=rt-ci, ymax=rt+ci), colour="black", width=.1, position=pd) + 
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=3, shape=21, fill="white") +
  xlab("IC positiva") +
  ylab("Tiempo de reacción") +
  scale_colour_hue(name="Predictibilidad",
                 breaks=c("hi", "lo"),
                 labels=c("alta", "baja"),
                   l=40) +                   
  coord_cartesian(ylim = c(300, 500)) + 
  scale_y_continuous(breaks = seq(300, 500, by = 50)) +  
  theme(legend.justification=c(1,0),
        legend.position=c(1,0))  +  
  theme_bw()
 p1_line

 p2_line <- ggplot(sum.data2, aes(x=verb, y=rt, colour=noun, group=noun)) + 
   geom_errorbar(aes(ymin=rt-ci, ymax=rt+ci), colour="black", width=.1, position=pd) + 
   geom_line(position=pd, size=1) +
   geom_point(position=pd, size=3, shape=21, fill="white") +
   xlab("Verbo inicial") +
   ylab("Tiempo de reacción") +
   scale_colour_hue(name="Predictibilidad",
                    breaks=c("hi", "lo"),
                    labels=c("alta", "baja"),
                    l=40) +                   
   coord_cartesian(ylim = c(300, 500)) + 
   scale_y_continuous(breaks = seq(300, 500, by = 50)) +  
   theme(legend.justification=c(1,0),
         legend.position=c(1,0))  +  
   theme_bw()
 p2_line

 
word_12 <- filter(data, region %in% c("POL"))
data5 <- filter(word_12, COND %in% c("c2","c4"))
data6 <- filter(word_12, COND %in% c("c1","c3"))
sum.data5 <- summarySEwithin(data5, measurevar = "rt", withinvars = c("IC", "PRED"), na.rm = T)
sum.data6 <- summarySEwithin(data6, measurevar = "rt", withinvars = c("IC", "PRED"), na.rm = T)

md5 <- lmer(log(rt) ~ IC * PRED + (1|SUBJ) + (1|ITEM), data = data5)
summary(md5)
emmeans(md5, pairwise ~ verb:noun)


#line plots
pd <- position_dodge(0.2) # move them .05 to the left and right
p5_line <- ggplot(sum.data5, aes(x=IC, y=rt, colour=PRED, group=PRED)) + 
  geom_errorbar(aes(ymin=rt-se, ymax=rt+se), colour="black", width=.1, position=pd) + 
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=3, shape=21, fill="white") +
  xlab("Efecto de IC (Pos/Neg) sobre Sustantivo Crítico mediado por 'LAMENTABLEMENTE'") +
  ylab("Tiempo de respuesta (milisegundos)") +
  scale_colour_hue(name="Predictibilidad",
                   breaks=c("hi", "lo"),
                   labels=c("alta", "baja"),
                   l=60) +                   
  coord_cartesian(ylim = c(400, 650)) + 
  scale_y_continuous(breaks = seq(400, 650, by = 50)) +  
  scale_x_discrete(breaks=c("neg", "pos"),
                   labels=c("IC(neg)", "IC(pos)")) +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0))  +  
  theme_bw()
p5_line 
dev.print(jpeg, "IC_POLneg_PRED.w12.png", res=900, height=8, width=16, units="in")


p6_line <- ggplot(sum.data6, aes(x=IC, y=rt, colour=PRED, group=PRED)) + 
  geom_errorbar(aes(ymin=rt-se, ymax=rt+se), colour="black", width=.1, position=pd) + 
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=3, shape=21, fill="white") +
  xlab("Efecto de IC (Pos/Neg) sobre Sustantivo Crítico mediado por 'CLARAMENTE'") +
  ylab("Tiempo de respuesta (milisegundos)") +
  scale_colour_hue(name="Predictibilidad",
                   breaks=c("hi", "lo"),
                   labels=c("alta", "baja"),
                   l=60) +                   
  coord_cartesian(ylim = c(400, 650)) + 
  scale_y_continuous(breaks = seq(400, 650, by = 50)) +  
  scale_x_discrete(breaks=c("neg", "pos"),
                   labels=c("IC(neg)", "IC(pos)")) +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0))  +  
  theme_bw()
p6_line 
dev.print(jpeg, "IC_POLpos_PRED.w12.png", res=900, height=8, width=16, units="in")
