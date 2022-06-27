##Prep data for variable exploration, model fitting and prediction intervals
# Packages
library(bayestestR) #calculates Bayes factor for model comparison
library(merTools)
library(repmod) #creates table report for lmer objects 
library(broom.mixed)
library(flextable) #creates table report for lm objects  (APA format)
library(officer)
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
library(car)
library(MASS) 
library(sm)
library(reshape)
library(plyr)
library(dplyr)
library(Hmisc)
library(psych) #- For function describe.
library(foreign) #- reads SPSS files
library(nlme)
library(mice)
library(sjPlot) #creates table report for lm objects  (APA format) [estimates, ci, t, df, p-value]

###Model fitting to Target Word Predictability
w17.M0 <- lmer(logRT ~ logwd_length:logadvfreq + target_pred + (1|subj), w17)
w17.M1 <- lmer(logRT ~ logwd_length:logadvfreq + verb_polarity + target_pred + (1|subj), w17)
w17.M2 <- lmer(logRT ~ logwd_length:logadvfreq + verb_polarity + adv_polarity + target_pred + (1|subj), w17)
w17.M3 <- lmer(logRT ~ logwd_length:logadvfreq + verb_polarity * adv_polarity * target_pred + (1|subj), w17)
w17.M4 <- lmer(logRT ~ logwd_length:logadvfreq + verb_polarity + adv_polarity * target_pred + (1|subj), w17) #Selected interactions model

w17.M4invRT <- lmer(invRT ~ logwd_length + logverbfreq + logadvfreq:logtargetfreq + (1|subj)+(1|condition), w17) #Selected interactions model
summary(w17.M4invRT)

summary(w17.M0)
summary(w17.M1)
summary(w17.M2)
summary(w17.M3)
summary(w17.M4)#selected model
anova(w17.M0, w17.M1, w17.M2, w17.M3, w17.M4) #anova for model comparison

#Generate APA Table
#selected model report 1
report(
  w17.M4, #selected model
  file = "table_w17.M4",
  type = "word",
  digits = 3,
  digitspvals = 3,
  info = TRUE,
  print = TRUE,
)

#selected model report 2

tab_model(w17.M4, p.val = "kr", show.df = TRUE, show.se = TRUE, show.stat = TRUE, show.ci = FALSE) # indicate model and stats

#model comparison report
tab_model(w17.M0, w17.M1, w17.M2, w17.M3, w17.M4) 

tab_model(
  m1, m2, 
  pred.labels = c("Intercept", "Age (Carer)", "Hours per Week", "Gender (Carer)", #indicate model predictors
                  "Education: middle (Carer)", "Education: high (Carer)", 
                  "Age (Older Person)"),
  dv.labels = c("First Model", "M2"),
  string.pred = "Coeffcient",
  string.ci = "Conf. Int (95%)",
  string.p = "P-Value"
)

#calculate BF for model comparison
BF_w17_models <- bayestestR::bayesfactor_models(w17.M1, w17.M2, w17.M3, w17.M4, denominator = w17.M0)
BF_w17_models
update(BF_w17_models, reference = 5)
BF_w17_models
as.matrix(BF_w17_models)
effectsize::interpret_bf(exp(BF_w17_models$log_BF[4]), include_value = TRUE)#Interpret BF for "w17.M4" - "[4] "extreme evidence (BF = 1/409.58) against" (Rules: Jeffreys, 1961)

write.csv(BF_w17_models,"C:/Users/marco/Desktop/Tesis/paper/R_analyses/adverbpolarity/BFmodelcomparison.csv", row.names = TRUE)


### Calculate Prediction Intervals for Interactions Model (w17.M4)
#Simulation-driven
w17[1,]
PI.time <- system.time(
  PI <- predictInterval(w17.M4, newdata = w17[1:238,], #Update vector value
                        which = "fixed", level = 0.95, 
                        n.sims = 2000, stat = "median", 
                        type = "linear.prediction",
                        include.resid.var = TRUE)
)
View(PI)
tempdfSIM <- cbind(w17, PI) #simulation-driven temporal dataframe
View(tempdfSIM)

#Model-driven
PI_w17.M4 <- predictInterval(w17.M4, newdata = w17[239:476,], 
                             which = "fixed", level = 0.95, 
                             n.sims = 2000, stat = "median", 
                             type = "linear.prediction",
                             include.resid.var = TRUE)
View(PI_w17.M4)
tempdfMOD <- cbind(w17, PI_w17.M4) #model-driven temporal dataframe
View(tempdfMOD)

# Plot PI and keep only points that are outside the prediction interval
plotPointsMOD <- tempdfMOD[which(!(tempdfMOD$logRT > tempdfMOD$lwr & tempdfMOD$logRT < tempdfMOD$upr)),]
plotPointsSIM <- tempdfSIM[which(!(tempdfSIM$logRT > tempdfSIM$lwr & tempdfSIM$logRT < tempdfSIM$upr)),]
ggplot(data=plotPointsSIM, aes(x = Target_pred)) + geom_point(aes(y = fit), alpha = 0.5) + 
 geom_ribbon(data=tempdfSIM, aes(ymin = lwr, ymax = upr), fill = "blue", alpha = 0.3) + #data = tempdf
  labs(x="Target Word Predictability", y="Prediction w/ 95% PI")

dev.print(jpeg, "PI_model_driven.jpeg", res=900, height=8, width=16, units="in")
dev.print(jpeg, "PI_simulation_driven.jpeg", res=900, height=8, width=16, units="in")
