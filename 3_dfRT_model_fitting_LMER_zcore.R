set.seed(12345)
library(readr)
library(dplyr)
library(tidyr)
library(rio)
library(gtools)
library(reshape)
library(ggplot2)
library(Rmisc)
library(BayesFactor)
library(bayestestR)
library(sjPlot) #- To create LMER object table
library(merTools)

#Data loading and var setting
df.spr <- read.delim("SPR_long.txt", sep=";", dec=",")

df.spr[1:3] <- lapply(df.spr[1:3], function(x) as.factor(as.character(x)))
df.spr[6:10] <- lapply(df.spr[6:10], function(x) as.factor(as.character(x)))
df.spr$WDNUM <- factor(df.spr$wd_number,
                       levels = c("1", "2", "3", "4", "5", "6",
                                  "7", "8", "9", "10", "11", "12",
                                  "13", "14", "15", "16", "17")
)

df.spr[11] <- lapply(df.spr[11], function(x) as.numeric(as.factor(x)))
df.spr$RT <- (df.spr$value)*1000

#Z-score for outlier identification
df.spr$z_RT <- (df.spr$RT - mean(df.spr$RT, na.rm = TRUE)) / sd(df.spr$RT, na.rm = TRUE)
df.spr$rt <- ifelse(df.spr$z_RT >= abs(2.5), NA, df.spr$RT)
plyr::count(is.na(df.spr$rt))[[2,2]]/plyr::count(is.na(df.spr$rt))[[1,2]]
#[1] 0.02300885

### Data preparation
# data frame from long to wide by region
data_lmer <- dcast(df.spr, SUBJ + COND + ITEM + IC + POL + PRED ~ region, value.var="rt", na.rm = T)
head(data_lmer)
# create difference score
data_lmer$difrt <- data_lmer$`preN5CRI` - data_lmer$`N5CRI`
data_lmer <- data_lmer[complete.cases(data_lmer),]

### Dummy coding
#create dummy variables (C1, PREDhi, ICpos, POLpos as reference groups)
C2 <- ifelse(data_lmer$COND == 'c2', 1, 0)
C3 <- ifelse(data_lmer$COND == 'c3', 1, 0)
C4 <- ifelse(data_lmer$COND == 'c4', 1, 0)

PRED <- ifelse(data_lmer$PRED == 'lo', 1, 0)

IC <- ifelse(data_lmer$IC == 'neg', 1, 0)

POL <- ifelse(data_lmer$POL == 'neg', 1, 0)

#create data frame to use for regression
df_lmer <- data.frame(difrt = data_lmer$difrt,
                     SUBJ = data_lmer$SUBJ,
                     ITEM = data_lmer$ITEM,
                     C2 = C2,
                     C3 = C3,
                     C4 = C4,
                     PRED = PRED,
                     IC = IC,
                     POL = POL)

#view data frame
df_lmer

## LMER Analysis
### Full model
fit.full <- lmer(difrt ~ C2 + C3 + C4 * (IC + POL + PRED) +
                   (C2 + C3 + C4 || SUBJ) +
                   (C2 + C3 + C4 || ITEM),
                 data = df_lmer)
summary(fit.full)

fit.test <- lmer(difrt ~ (C2 + C3 + C4) * (PRED) +
                   (C2 + C3 + C4 || SUBJ) +
                   (C2 + C3 + C4 || ITEM),
                 data = df_lmer)
summary(fit.test)
### Zero model: no PRED
fit.0 <- lmer(difrt ~ C2 + C3 + C4 + 
                (C2 + C3 + C4 ||SUBJ) +
                (C2 + C3 + C4 ||ITEM),
              data = df_lmer)
summary(fit.0)
anova(fit.full, fit.test, fit.0)

### Model comparison for random structure
### Random structure comparison
fit.test.pp <- lmer(difrt ~ (C2 + C3 + C4) * (PRED) +
                      (1|ITEM),
                    data = df_lmer)
fit.test.no_random <- lmer(difrt ~ (C2 + C3 + C4) * (PRED) +
                             (1|SUBJ) +
                             (1|ITEM),
                           data = df_lmer)
anova(fit.test, fit.test.pp, fit.test.no_random)
fit=fit.test.no_random
summary(fit)

### Diagnostic plots
plot(fit, residuals(., scaled =T)~fitted(.), ylab="Standardized Residuals", xlab="Fitted Values")
qqmath(residuals(fit, scaled =T), ylab="Standardized Residuals", xlab="Standard normal quantiles")
plot(fit,sqrt(abs(resid(.)))~fitted(.), type=c("p","smooth"), ylab=expression(sqrt(abs(resid))), xlab="Fitted values")

dotplot(ranef(fit, condVar=T))$SUBJ
dotplot(ranef(fit, condVar=T))$ITEM
qqmath(ranef(fit, condVar=T))$SUBJ
qqmath(ranef(fit, condVar=T))$ITEM

### Influence analysis
## Run a model excluding one participant at the time
estex.subj <- influence(md1, "SUBJ")
## Calculate D Cook for parameter of interest (VIR:vocabulary, VIR:textcomp, VIA:vocabulary, VIA:textcomp [6,7,8,9])
cookd <- cooks.distance(estex.subj, sort=TRUE)
plot(estex.subj, which="cook", xlab ="DCook", ylab = "Participant")
# data frame with D Cook
cookd <- data.frame(cookd)
cookd$idpa <- as.numeric(rownames(cookd))
cookd1 <- cookd %>% filter(cookd > 0.04)
idpac <- c(cookd1$idpa)
# remove influential cases
df_lmer.a <- subset(df_lmer, !SUBJ%in%idpac)
View(df_lmer)
View(df_lmer.a)
# run model without influential cases
fit.l <- lmer(difrt ~ (C2 + C3 + C4) * (PRED) +
                (1|SUBJ) +
                (1|ITEM),
              data = df_lmer.a)
summary(fit.l)

# compare model with all cases
BF_models <- bayestestR::bayesfactor_models(fit.l, fit, verbose = FALSE)
BF_models
BF_models <- update(BF_models, reference = 2)
BF_models
as.matrix(BF_models)
effectsize::interpret_bf(exp(BF_models$log_BF[2]), include_value = TRUE)
write.csv(an,"C:/Users/marco/Desktop/Tesis/Figuras_tesis/BFmodel.csv", row.names = FALSE)

## Ordinary Least Squares (OLS) Regression
### Prepare data
# data frame from long to with by region
library("reshape2")
df_ols <- dcast(df.spr, SUBJ + COND + ITEM + IC + POL + PRED ~ region, value.var="rt", na.rm = T)
head(df_ols)
# create difference score
df_ols$difrt <- df_ols$`preN5CRI` - df_ols$`N5CRI`
df_ols <- df_ols[complete.cases(df_ols),]
# aggregate over subject and condition
data_ols <- aggregate(difrt ~ SUBJ + COND, mean, data=df_ols, na.rm=TRUE) 

#create dummy variables (C1, PREDhi, ICpos, POLpos as reference groups)
C2 <- ifelse(df_ols$COND == 'c2', 1, 0)
C3 <- ifelse(df_ols$COND == 'c3', 1, 0)
C4 <- ifelse(df_ols$COND == 'c4', 1, 0)

PRED <- ifelse(df_ols$PRED == 'lo', 1, 0)

IC <- ifelse(df_ols$IC == 'ICneg', 1, 0)

POL <- ifelse(df_ols$POL == 'POLneg', 1, 0)

#create data frame to use for regression
df_lm <- data.frame(difrt = df_ols$difrt,
                      C2 = C2,
                      C3 = C3,
                      C4 = C4,
                      PRED = PRED,
                      IC = IC,
                      POL = POL)

#view data frame
df_lm

## Get Variance Inflation Factor using OLS with aggregated data
### Dummy coding, fit and VIF
# fit lm
fit.vif <- lm(difrt ~ (C2 + C3 + C4) * (PRED),  data=df_lm)
# get maximum of the model VIF
library("car")
range(vif(fit.vif))
# get all VIFs
vif(fit.vif)
# from Cohen, Cohen, West & Aiken (2003)
# variance inflation factor (VIF) > 10 is problematic 
# also, vif < 10 don't benefit from PCR or Ridge Regreesion (p. 429)

#identify outliers LM object#
ggplotInfluence <- function (model, fill="grey", 
                             outline="black", size=30) {
  require(ggplot2)
  if(!inherits(model, "lm")) 
    stop("You need to supply an lm object.")
  df<-data.frame(Residual=rstudent(model), 
                 Leverage=hatvalues(model), 
                 Cooks=cooks.distance(model), 
                 Observation=names(hatvalues(model)), 
                 stringsAsFactors=FALSE)
  myxint<-c(2*mean(df$Leverage), 3*mean(df$Leverage))
  inds<-intersect(which(abs(df$Residual) < 2), 
                  which( df$Leverage < myxint[1]))
  if(length(inds) > 0) df$Observation[inds]<-""
  ggplot(df, aes_string(x='Leverage', y='Residual', 
                        size='Cooks', label='Observation'), 
         legend=FALSE) +
    geom_point(colour=outline, fill=fill, shape=21) + 
    scale_size_area(max_size=size) + 
    theme_bw(base_size=16) + geom_text(size=4) + 
    geom_hline(yintercept=c(2,-2), linetype="dashed") + 
    geom_vline(xintercept=myxint, linetype="dashed") + 
    ylab("Studentized Residuals") + 
    xlab("Hat-Values") + labs(size="Cook's distance")
  
}
ggplotInfluence(fit.vif) #outputs Cook's distance plot
dev.print(jpeg, "Puntos influyentes según Distancia de Cook.jpeg", res=900, height=8, width=16, units="in")

## Final LMER model
#model summary of effects
summary(fit.vif)

tab_model(df_lmer, df_lmer.a)
