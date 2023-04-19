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

##Filtrar datos en palabra crítica N5CRI:

w17 <- filter(df.spr, wd_number %in% c("17"))
hist(w17$RT)
w17$logRT <- log(w17$RT)
hist(w17$logRT)

w17.full <- lmer(logRT ~ (IC * POL * PRED) + (1|SUBJ) + (1|ITEM), w17) 
w17.test <- lmer(logRT ~ PRED + (1|SUBJ) + (1|ITEM), w17) #Selected interactions model
w17.0 <- lmer(logRT ~ 0 + (1|SUBJ) + (1|ITEM), w17)
w17.1 <- lmer(logRT ~ IC + (POL * PRED) + (1|SUBJ) + (1|ITEM), w17)
summary(w17.1)
comparison <- anova(w17.full, w17.test, w17.0)
comparison

#create dummy variables (C1, PREDhi, ICpos, POLpos as reference groups)
C2 <- ifelse(w17$COND == 'c2', 1, 0)
C3 <- ifelse(w17$COND == 'c3', 1, 0)
C4 <- ifelse(w17$COND == 'c4', 1, 0)

PRED <- ifelse(w17$PRED == 'lo', 1, 0)

IC <- ifelse(w17$IC == 'ICneg', 1, 0)

POL <- ifelse(w17$POL == 'POLneg', 1, 0)

#create data frame to use for regression
df_stan_glmer <- data.frame(rt = w17$rt,
                       SUBJ = w17$SUBJ,
                       ITEM = w17$ITEM,
                       C2 = C2,
                       C3 = C3,
                       C4 = C4,
                       PRED = PRED,
                       IC = IC,
                       POL = POL)


#view data frame
df_glmer
full.glmer <- glmer(rt ~ (IC * POL * PRED) + (1|SUBJ) + (1|ITEM), df.spr , family = Gamma(link = "log"), glmerControl(optimizer = "bobyqa"))
summary(full.glmer)
full.test <- glmer(rt ~ (PRED) + (1+region|SUBJ) + (1|ITEM), df.spr , family = Gamma(link = "log"), glmerControl(optimizer = "bobyqa"))
summary(full.test)
anova(full.glmer, full.test)

color_scheme_set("brightblue")
fit.full.glmer <- stan_glmer(rt ~ (IC * POL * PRED) + (1|SUBJ) + (1|ITEM), #no intercept, no random slope
                data = df_glmer,
                family = Gamma(link="log"),
                cores = 5,
                chains = 10, 
                iter = 10000,
                warmup = 1000,
                prior = rpois()
                diagnostic_file = file.path(tempdir(), "df.csv")
                )
#bridge_1 <- bridgesampling::bridge_sampler(fit_glmer1)
summary(fit.0.glmer)
fit_test.glmer <- update(fit.0.glmer, formula = . ~ (C2 + C3 + C4) * (IC + POL + PRED) + (1|SUBJ) + (1|ITEM))
#bridge_2 <- bridgesampling::bridge_sampler(fit_glmer2)

fit_glmer3 <- update(fit_glmer1, formula = . ~ 0 + log(lambda) + (0+log(lambda)|Var2) + (1|Var1))
#bridge_3 <- bridgesampling::bridge_sampler(fit_glmer3)

#Compare models with loo
loo1 <- loo(fit_glmer1, cores = 5)
loo2 <- loo(fit_glmer2, cores = 5)
loo3 <- loo(fit_glmer3, cores = 5) #selected model
comp <- loo_compare(loo1, loo2, loo3)
comp

#          elpd_diff se_diff
#fit_glmer3  0.0       0.0   
#fit_glmer1 -7.8       1.6   
#fit_glmer2 -8.0       0.5 

#Plot Posterior Predictive Intervals
df_PP <- full.test[complete.cases(full.test),]
ppc_intervals(
  y = df_PP$rt,
  yrep = posterior_predict(full.test), #selected model
  x = df_PP$PRED,
  prob = 0.5,
  prob_outer = 0.9
) +
  labs(
    x = "Predictibilidad RT",
    y = "Frecuencia Promedio de Futuras Observaciones",
    title = "Intervalos de PredicciÃ³n (PI 90%) \nvs Indice Cloze Calculado por CondiciÃ³n") + 
  panel_bg(fill = "gray95", color = NA) +
  grid_lines(color = "white") 

dev.print(jpeg, "PIcloze_selectedmodel.jpeg", res=900, height=8, width=16, units="in")

#BF GLMER model evaluation##
BF_glmer3 <- bayesfactor_parameters(fit.0.glmer, null= 0)
plot(BF_glmer3)
BF_glmer3
effectsize::interpret_bf(exp(BF_glmer3$log_BF[1]), include_value = TRUE)
dev.print(jpeg, "1_BF_GLMER.jpeg", res=900, height=8, width=16, units="in")
#[1] "strong evidence (BF = 13.81) in favour of"
#(Rules: jeffreys1961)

##GLMER Posterior Predictive Distributions##
plot_title0 <- ggplot2::ggtitle("Distribuciones posteriores",
                               "con mediana e intervalos predictivos (90%)")
pp_check(fit_glmer3, plotfun = "stat", stat = "mean") + plot_title0
dev.print(jpeg, "1_pp_check_GLMER.jpeg", res=900, height=8, width=16, units="in")

##Bayesian GLM object fitting##
bayesplot::color_scheme_set("brightblue")
fit_glm <- stan_glm(Freq ~ 0 + Var2,
                    data = df.cond,
                    family = poisson(link="log"), #The Poisson distribution is defined by a single parameter, lambda (Î»), which is the mean number of occurrences during an observation unit. A rate of occurrence is simply the mean count per standard observation period.
                    cores = 5,
                    chains = 10, 
                    iter = 10000,
                    warmup = 1000,
                    diagnostic_file = file.path(tempdir(), "df.csv")
)
plot(fit_glm)
em_condition <- emmeans(fit_glm, ~ Var2)
hyps <- c("c1 > c2 & c3 > c4",
          "c5 > c6 & c7 > c8"
)

##GLM Posterior predictive probability distributions##
plot_title <- ggplot2::ggtitle("Distribuciones posteriores",
                               "con media y SD e intervalos predictivos (90%)")
pp_check(fit_glm, plotfun = "stat_2d", stat = c("mean", "sd")) +
  plot_title
dev.print(jpeg, "1_pp_check_GLM.jpeg", res=900, height=8, width=16, units="in")

bayesplot::mcmc_areas(fit_glm, 
                      pars = c("Var2c1",
                                "Var2c2"),
                      prob = 0.9) + plot_title
dev.print(jpeg, "1_bayesplot_GLM_Var2.jpeg", res=900, height=8, width=16, units="in")

#BF GLM model evaluation##
BF_glm <- bayesfactor_parameters(fit_glm, null = 0)
BF_glm
plot
effectsize::interpret_bf(exp(BF_glmer3$log_BF[1]), include_value = TRUE)
dev.print(jpeg, "1_BF_glm_parameters.jpeg", res=900, height=8, width=16, units="in")

#BF GLM restricted hypothesis##
posterior <- posterior_predict(fit_glmer3)
b <- bayesfactor_restricted(em_condition, hypothesis = hyps, prior = fit_glm)
as.numeric(b)
if (require("see") && require("patchwork")) {
  i <- attr(b, "bool_results")[["posterior"]]
  
  see::plots(
    plot(estimate_density(posterior)),
    # distribution **conditional** on the restrictions
    plot(estimate_density(posterior[i[[hyps[1]]], ])) + ggplot2::ggtitle(hyps[1]),
    plot(estimate_density(posterior[i[[hyps[2]]], ])) + ggplot2::ggtitle(hyps[2]),
    guides = "collect"
  )
}
dev.print(jpeg, "1_BF_GLM_restricted.jpeg", res=900, height=8, width=16, units="in")
